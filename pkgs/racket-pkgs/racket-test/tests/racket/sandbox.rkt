#lang racket/base
(require racket/sandbox)

(define (exn:fail:resource:time? x)
  (and (exn:fail:resource? x)
       (eq? 'time (exn:fail:resource-resource x))))

(module+ test
  (require rackunit)

  (define n 1)
  (check-not-exn
   (λ ()
     (with-timeout
      n
      (sleep (sub1 n)))))
  (check-exn
   exn:fail:resource:time?
   (λ ()
     (with-timeout
      n
      (sleep (add1 n)))))
  (check-exn
   exn:fail:resource:time?
   (λ ()
     (with-timeout
      n
      (thread (λ () (sleep (add1 n)))))))
  (check-exn
   exn:fail:resource:time?
   (λ ()
     (with-timeout
      n
      (thread (λ ()
                (thread (λ () (sleep (add1 n)))))))))
  (check-exn
   exn:fail:resource:time?
   (λ ()
     (with-timeout
      n
      (parameterize ([current-custodian (make-custodian)])
        (thread (λ () (sleep (add1 n)))))))))
