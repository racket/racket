#lang racket/base
(require racket/sandbox)

(define (exn:fail:resource:time? x)
  (and (exn:fail:resource? x)
       (eq? 'deep-time (exn:fail:resource-resource x))))

(module+ test
  (require rackunit)

  (define n 1)
  (check-not-exn
   (λ ()
     (with-deep-time-limit
      n
      (sleep (sub1 n)))))
  (check-exn
   exn:fail:resource:time?
   (λ ()
     (with-deep-time-limit
      n
      (sleep (add1 n)))))
  (check-exn
   exn:fail:resource:time?
   (λ ()
     (with-deep-time-limit
      n
      (thread (λ () (sleep (add1 n)))))))
  (check-exn
   exn:fail:resource:time?
   (λ ()
     (with-deep-time-limit
      n
      (subprocess (current-output-port) 
                  #f ; because (current-input-port) = /dev/null in DrDr mode
                  (current-error-port)
                  (find-executable-path "cat")))))
  (check-exn
   exn:fail:resource:time?
   (λ ()
     (with-deep-time-limit
      n
      (thread (λ ()
                (thread (λ () (sleep (add1 n)))))))))
  (check-exn
   exn:fail:resource:time?
   (λ ()
     (with-deep-time-limit
      n
      (parameterize ([current-custodian (make-custodian)])
        (thread (λ () (sleep (add1 n)))))))))
