#lang racket

;; When run by itself, this example triggers a use of
;; _scheme_tail_apply_from_native_fixup_args().

(define (filt pred vs)
  (if (empty? vs)
      '()
      (let ([v (car vs)])
        (if (pred v)
            (cons v (filt pred (cdr vs)))
            (filt pred (cdr vs))))))

(define (qsort2-par vs)
  (flatten (qsort2-par/private vs)))

(define (qsort2-par/private vs)
  (if (or (null? vs) (null? (cdr vs)))
      vs
      (let* ([p-i 0]
             [p (list-ref vs p-i)]
             [lf (future (λ () (qsort2-par/private (filt (λ (v) (v . < . p)) vs))))]
             [ef (future (λ () (filt (λ (v) (= v p)) vs)))]
             [gf (future (λ () (qsort2-par/private (filt (λ (v) (v . > . p)) vs))))])
        (list (touch lf) (touch ef) (touch gf)))))


(define l (build-list 10000 (λ (x) (random 2000))))
(void (qsort2-par l))
