#lang typed/racket/base

(require racket/sequence
         "../exception.rkt")

(provide (all-defined-out))

(: sequences->weighted-samples
   ((Sequenceof Real) (Sequenceof Real) -> (Values (Listof Real) (Listof Nonnegative-Real))))
(define (sequences->weighted-samples x-seq w-seq)
  (define xs (sequence->list x-seq))
  (define n (length xs))
  (define ws
    (for/list: : (Listof Nonnegative-Real) ([w  w-seq])
      (cond [(w . >= . 0)  w]
            [else  (raise-argument-error 'sequences->weighted-samples
                                         "(Sequenceof Nonnegative-Real)"
                                         1 x-seq w-seq)])))
  (define m (length ws))
  (cond [(= n m)  (values xs ws)]
        [else  (error 'sequences->weighted-samples
                      "values and weights must be the same length; given lengths ~a and ~a"
                      n m)]))

;; bias-adjustment = #f   Return the second central moment
;; bias-adjustment = #t   Assume weights are counts of repeated samples; correct for bias
;; bias-adjustment = n    Assume n actual samples; correct for bias

(: get-bias-adjustment (Nonnegative-Real (U #t Real) Positive-Real -> Positive-Real))
(define (get-bias-adjustment c bias mn)
  (define n (if (real? bias) bias c))
  (if (n . > . mn) n +nan.0))

(: adjust-variance (Nonnegative-Real Nonnegative-Real (U #t #f Real) -> Nonnegative-Real))
(define (adjust-variance m2 n bias)
  (cond [bias
         (let ([n  (get-bias-adjustment n bias 1)])
           (define c (max 0 (/ n (- n 1))))  ; max proves c >= 0
           (* m2 c))]
        [else  m2]))

(: adjust-skewness (Real Nonnegative-Real (U #t #f Real) -> Real))
(define (adjust-skewness g n bias)
  (cond [bias
         (let ([n  (get-bias-adjustment n bias 2)])
           (* g (/ (sqrt (max 0 (* n (- n 1)))) (- n 2))))]
        [else  g]))

(: adjust-kurtosis (Nonnegative-Real Nonnegative-Real (U #t #f Real) -> Nonnegative-Real))
(define (adjust-kurtosis g n bias)
  (cond [bias
         (let ([n  (get-bias-adjustment n bias 3)])
           (define c (max 0 (/ (- n 1) (* (- n 2) (- n 3)))))  ; max proves c >= 0
           (* (+ (* (+ n 1) g) 6) c))]
        [else  g]))
