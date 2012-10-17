#lang typed/racket/base

(require racket/sequence
         "../../flonum.rkt"
         "../vector/vector.rkt")

(provide (all-defined-out))

(define-type (Moment-Fun T)
  (case-> ((Sequenceof Real) [#:bias (U #t #f Real)] -> T)
          ((Sequenceof Real) (Option (Sequenceof Real)) [#:bias (U #t #f Real)] -> T)))

(define-type (Moment/Mean-Fun T)
  (case-> (Real (Sequenceof Real) [#:bias (U #t #f Real)] -> T)
          (Real (Sequenceof Real) (Option (Sequenceof Real)) [#:bias (U #t #f Real)] -> T)))

(define-type (Correlation-Fun T)
  (case-> ((Sequenceof Real) (Sequenceof Real) [#:bias (U #t #f Real)] -> T)
          ((Sequenceof Real) (Sequenceof Real) (Option (Sequenceof Real))
                             [#:bias (U #t #f Real)] -> T)))

(define-type (Correlation/Means-Fun T)
  (case-> (Real Real (Sequenceof Real) (Sequenceof Real) [#:bias (U #t #f Real)] -> T)
          (Real Real (Sequenceof Real) (Sequenceof Real) (Option (Sequenceof Real))
                [#:bias (U #t #f Real)] -> T)))

;; ===================================================================================================

(: check-lengths! (All (A B) (Symbol String A B Index Index -> Void)))
(define (check-lengths! name what xs ys m n)
  (unless (= m n) (error name "~a must be the same length; given ~e (length ~a) and ~e (length ~a)"
                         what xs m ys n)))

(: sequences->weighted-samples
   (All (A) (Symbol (Sequenceof A) (Sequenceof Real)
                    -> (Values (Listof A) (Listof Nonnegative-Real)))))
(define (sequences->weighted-samples name x-seq w-seq)
  (define xs (sequence->list x-seq))
  (define ws
    (for/list: : (Listof Nonnegative-Real) ([w w-seq])
      (cond [(w . >= . 0)  w]
            [else  (raise-argument-error name "(Sequenceof Nonnegative-Real)" 1 x-seq w-seq)])))
  (check-lengths! name "values and weights" xs ws (length xs) (length ws))
  (values xs ws))

(: sequence->vector (All (A) ((Sequenceof A) -> (Vectorof A))))
(define (sequence->vector vs)
  (for/vector: ([v vs]) : A v))

(: sequences->weighted-sample-vectors
   (All (A) (Symbol (Sequenceof A) (Sequenceof Real)
                    -> (Values (Vectorof A) (Vectorof Nonnegative-Real)))))
(define (sequences->weighted-sample-vectors name x-seq w-seq)
  (define xs (sequence->vector x-seq))
  (define ws
    (for/vector: ([w w-seq]) : Nonnegative-Real
      (cond [(w . >= . 0)  w]
            [else  (raise-argument-error name "(Sequenceof Nonnegative-Real)" 1 x-seq w-seq)])))
  (check-lengths! name "values and weights" xs ws (vector-length xs) (vector-length ws))
  (values xs ws))

;; ===================================================================================================

;; bias = #f   Return the central moment
;; bias = #t   Assume sum of weights is the count and correct for bias normally
;; bias = n    Assume n actual samples; correct for bias

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

(: adjust-covariance (Real Nonnegative-Real (U #t #f Real) -> Real))
(define (adjust-covariance m2 n bias)
  (cond [bias
         (let ([n  (get-bias-adjustment n bias 1)])
           (* m2 (/ n (- n 1))))]
        [else  m2]))

(: adjust-skewness (Real Nonnegative-Real (U #t #f Real) -> Real))
(define (adjust-skewness g n bias)
  (cond [bias
         (let ([n  (get-bias-adjustment n bias 2)])
           (fl (* g (/ (sqrt (max 0 (* n (- n 1)))) (- n 2)))))]
        [else  (fl g)]))

(: adjust-kurtosis (Nonnegative-Real Nonnegative-Real (U #t #f Real) -> Nonnegative-Real))
(define (adjust-kurtosis g n bias)
  (cond [bias
         (let ([n  (get-bias-adjustment n bias 3)])
           (define c (max 0 (/ (- n 1) (* (- n 2) (- n 3)))))  ; max proves c >= 0
           (* (+ (* (+ n 1) g) 6) c))]
        [else  g]))
