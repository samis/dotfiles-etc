(use-modules (ice-9 match)
             (srfi srfi-1)
             (system repl server))

(define-syntax-rule (forever body ...)
  (while #t body ...))

(define-syntax-rule (with-fork body ...)
  (match (primitive-fork)
    (0 (begin body ...))
    (pid pid)))

(define provided-by (@@ (shepherd service) provided-by))
(define deregister-service (@@ (shepherd service) deregister-service))

(define deregister-service*
  (compose deregister-service symbol->string))

(define (register-services* . services)
  (for-each deregister-service* (append-map provided-by services))
  (apply register-services services))

(define-syntax-rule (define-service name service)
  (define name (begin (register-services* service) service)))

(define (touch-file file)
  (close-port (open-file file "a0b")))

(define (run-command command)
  (zero? (status:exit-val (apply system* command))))

(define (make-system-constructor command)
  (lambda _
    (run-command command)))

(define (make-system-destructor command)
  (lambda _
    (not (run-command command))))

(define (simple-service program)
  (make <service>
    #:provides (list (string->symbol program))
    #:requires '()
    #:start (make-forkexec-constructor (list program))
    #:stop (make-kill-destructor)))

(define %home (getenv "HOME"))

(define (rbenv-bin version)
  "Return the binary path for the VERSION of Ruby managed by rbenv."
  (string-append %home "/.rbenv/versions/" version "/bin"))

(define (ruby-environment version gemset)
  "Return the environment variables needed to run Ruby applications
that use a specific VERSION of Ruby, whose dependencies are stored in
GEMSET."
  (let ((gem-home (string-append %home "/.gems/" gemset)))
    (list (string-append "GEM_PATH=" gem-home)
          (string-append "GEM_HOME=" gem-home)
          (string-append "PATH=" gem-home "/bin:"
                         (rbenv-bin version) ":"
                         (getenv "PATH")))))

(define* (rails-service name port ruby-version #:optional (requires '()))
  "Create a service that runs the Rails application NAME located in
DIR.  The application runs with RUBY, the file name of the necessary
Ruby version, and listens on PORT."
  (define gem-home
    (string-append %home "/.gems/" name))

  (make <service>
    #:provides (list (string->symbol name))
    #:requires requires
    #:start (make-forkexec-constructor
             `("passenger" "start"
               "--address" "127.0.0.1"
               "--port" ,port
               "--ruby" ,(string-append (rbenv-bin ruby-version) "/ruby"))
             #:directory (string-append %home "/Code/" name)
             #:environment-variables (ruby-environment ruby-version name))
    #:stop (make-kill-destructor)))

;; (define-service vhl-tunnel
;;   (make <service>
;;     #:provides '(vhl-tunnel)
;;     #:requires '()
;;     #:start (make-forkexec-constructor
;;              '("ssh" "-N" "-L" "1234:7VWJD42.vhl.dom:22" "vhl"))
;;     #:stop (make-kill-destructor)))

;; (define-service vhl-proxy
;;   (make <service>
;;     #:provides '(vhl-proxy)
;;     #:requires '(vhl-tunnel)
;;     #:start (make-forkexec-constructor
;;              '("ssh" "-N" "-D8080" "-p1234" "dthompson@localhost"))
;;     #:stop (make-kill-destructor)))

(register-services
 ;; Emacs
 (make <service>
   #:provides '(emacs)
   #:requires '()
   #:start (make-system-constructor '("emacs" "--daemon"))
   #:stop (make-system-destructor '("emacsclient" "--eval" "(kill-emacs)"))))

;; Send shepherd into the background.
(action 'shepherd 'daemonize)

;; Start the REPL server.
(spawn-server (make-tcp-server-socket #:port 37148))

;; Services to start when shepherd starts:
;; Add the name of each service that should be started to the list
;; below passed to 'for-each'.
(for-each start '(emacs))
