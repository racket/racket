#lang racket/base
(require mzlib/thread
         unstable/match
         racket/match
         racket/port
         racket/sandbox)
(provide (all-defined-out))

(define (write-output-bytes obs op)
  (define bs (get-output-bytes obs))
  (write (bytes-length bs) op)
  (write-bytes bs op))

(define (handle-one-msg password log! ip op authenticated?)
  (define (is-authenticated? x) authenticated?)
  (match (with-handlers ([exn? (λ (x) x)]) (read ip))
    [(? is-authenticated? (list 'run (? number? timeout) (? path-string? command) (? string? arg) ...))
     (call-with-custodian-shutdown
      (λ ()
        (define stdout-obs (open-output-bytes 'stdout))
        (define stderr-obs (open-output-bytes 'stderr))
        (define info (list* command arg))
        (log! "Running with timeout (~a) ~S" timeout info)
        (define start-time (current-inexact-milliseconds))
        (define-values (sp stdout stdin stderr) (apply subprocess #f #f #f command arg))
        (close-output-port stdin)
        
        (define stdout-t
          (thread (λ () (copy-port stdout stdout-obs))))
        (define stderr-t
          (thread (λ () (copy-port stderr stderr-obs))))
        
        (define exit-status
          (sync
           (handle-evt sp
                       (λ _
                         (subprocess-status sp)))
           (handle-evt (alarm-evt (+ start-time (* 1000 timeout)))
                       (λ _
                         (subprocess-kill sp #f)
                         (subprocess-kill sp #t)
                         #f))))
        (define end-time (current-inexact-milliseconds))
        (log! "Finished running ~S, status was ~a" info exit-status)
        
        (thread-wait stdout-t)
        (thread-wait stderr-t)
        (close-input-port stdout)
        (close-input-port stderr)
        
        (write (vector start-time end-time exit-status) op)
        (write-output-bytes stdout-obs op)
        (write-output-bytes stderr-obs op)))
     authenticated?]
    [(list 'auth (== password string=?))
     (log! "Authenticated")
     (write #t op)
     #t]
    [(? eof-object?)
     (log! "Master disconnect")
     (void)]
    [x
     (log! "Illegal message: ~e" x)
     (write #f op)
     authenticated?]))

(define (call-with-safe-read t)
  (parameterize
      ([read-case-sensitive #t]
       [read-square-bracket-as-paren #t]
       [read-curly-brace-as-paren #t]
       [read-accept-box #f]
       [read-accept-compiled #f]
       [read-accept-bar-quote #f]
       [read-accept-graph #f]
       [read-decimal-as-inexact #t]
       [read-accept-dot #f]
       [read-accept-infix-dot #f]
       [read-accept-quasiquote #f]
       [read-accept-reader #f])
    (t)))

(define (handle ip op password log!)
  (call-with-safe-read 
   (λ ()
     (let loop ([authenticated? #f])
       (match (handle-one-msg password log! ip op authenticated?)
         [(? void?) (void)]
         [authenticated? (loop authenticated?)])))))

(define (port-closing-curry f . args)
  (λ (ip op)
    (dynamic-wind 
     void 
     (λ () (apply f ip op args))
     (λ () 
       (close-input-port ip)
       (close-output-port op)))))

(define (main)
  ; XXX commandline
  (define port 4532)
  (define *password* "foo")
  ; XXX make web server to view recent things
  (define (log! fmt . vals)
    (apply printf fmt vals))
  ; XXX use ssl
  (run-server 
   port 
   (port-closing-curry handle *password* log!)
   #f))
