#lang racket/base
(require racket/place
         racket/port
         rackunit)

(module+ test
  (main))

(define (main)

  (define-syntax-rule (with-stderr e)
    (parameterize ([current-error-port (current-output-port)])
      e))

  (test-exn
   "output port is closed"
   (lambda (x) (void))
   (lambda ()
     (define op (open-output-bytes))
     (call-with-output-file "foo.foo" #:exists 'replace 
                            (lambda (op)
                              (close-output-port op)
                              (let-values ([(p pin pout perr) (place* #:out op ch (printf "Hello3\n"))])
                                (place-wait p))))))
  
  (place-wait (place ch (printf "Hello1\n")))
  (with-stderr
   (place-wait (place ch (eprintf "Hello2\n"))))
  (place-wait (place ch (printf "~a\n" (read))))  ; #<eof>

  (let-values ([(p pin pout perr) (place* ch (printf "Hello3\n"))])
    (place-wait p))
  
  (let-values ([(p pin pout perr) (place* ch (printf "Hello4\n"))])
    (copy-port pout (current-output-port))
    (place-wait p))
  (let-values ([(p pin pout perr) (place* #:out (current-output-port) ch (printf "Hello5\n"))])
    (place-wait p))
  
  (with-stderr
   (let-values ([(p pin pout perr) (place* #:err (current-error-port) ch (eprintf "Hello6\n")
                                           (flush-output (current-error-port)))])
     (place-wait p)))
  (let-values ([(p pin pout perr) (place* #:out (current-output-port) ch (printf "Hello7 ~a\n" (read)))])
    (write "Again" pin)
    (flush-output pin)
    (place-wait p))

  (let-values ([(p pin pout perr) (place* ch (write "Hello8\n"))])
    (check-equal? "Hello8\n" (read pout))
    (place-wait p))

  (let-values ([(p pin pout perr) (place* ch (write "Hello9\n" (current-error-port)))])
    (check-equal? "Hello9\n" (read perr))
    (place-wait p))

  (let*-values ([(pipeout pipein) (make-pipe)]
                [(p pin pout perr) (place* #:out pipein 
                                           ch (write "Hello10\n") 
                                           (close-output-port (current-output-port))
                                           (close-output-port (current-error-port)))])
    (place-wait p)
    (thread (lambda ()
              (sync (place-dead-evt p))
              (close-output-port pipein)))
    (copy-port pipeout (current-output-port))
    (newline)
    (flush-output)))
