#lang racket/base

;; Try to provoke a race for the internal side effect of propagating
;; scopes in a syntax object

(define stx (datum->syntax
             #'here
             (for/list ([i 10000]) i)))
(define i (make-syntax-introducer))

(define (burn n)
  (if (zero? n)
      'done
      (burn (sub1 n))))

(for ([j (in-range 100)])
  (define failed? #f)
  (define stx2 (i stx))
  (for-each
   sync
   (for/list ([i 10])
     (thread (lambda ()
               (burn (random 100000))
               (unless (eq? (syntax-e stx2)
                            (begin
                              (sleep)
                              (syntax-e stx2)))
                 (set! failed? #t)
                 (printf "CHANGED\n"))))))
  (when failed?
    (error "failed")))
