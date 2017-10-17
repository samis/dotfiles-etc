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

(register-services
 ;; Emacs
 (make <service>
   #:provides '(emacs)
   #:requires '()
   #:start (make-system-constructor '("emacs" "--daemon"))
   #:stop (make-system-destructor '("emacsclient" "--eval" "(kill-emacs)")))

 ;; Compton X11 Compositor
 (simple-service "compton"))

;; Send shepherd into the background.
(action 'shepherd 'daemonize)

;; Start the REPL server.
(spawn-server (make-tcp-server-socket #:port 37148))

;; Services to start when shepherd starts:
;; Add the name of each service that should be started to the list
;; below passed to 'for-each'.
(for-each start '(emacs compton))
