#lang typed/racket/base

(require racket/sequence
         "../../base.rkt"
         "statistics-utils.rkt")

(provide mean
         variance/mean
         stddev/mean
         skewness/mean
         kurtosis/mean
         variance
         stddev
         skewness
         kurtosis)

(: mean (case-> ((Sequenceof Real) -> Real)
                ((Sequenceof Real) (Option (Sequenceof Real)) -> Real)))
(define (mean xs [ws #f])
  (cond [ws  (let-values ([(xs ws)  (sequences->weighted-samples 'mean xs ws)])
               (define n (sum ws))
               (cond [(zero? n)  +nan.0]
                     [else  (/ (sum (map * xs ws)) n)]))]
        [else  (let ([xs  (sequence->list xs)])
                 (define n (length xs))
                 (cond [(zero? n)  +nan.0]
                       [else  (/ (sum xs) n)]))]))

(: variance* (Symbol Real (Sequenceof Real) (Option (Sequenceof Real)) (U #t #f Real)
                     -> Nonnegative-Real))
(define (variance* name m xs ws bias)
  (define-values (xs^2 n)
    (cond [ws  (let-values ([(xs ws)  (sequences->weighted-samples name xs ws)])
                 (values (map (λ: ([x : Real] [w : Real]) (* w (sqr (- x m)))) xs ws)
                         (max 0 (sum ws))))]
          [else  (let ([xs  (sequence->list xs)])
                   (values (map (λ: ([x : Real]) (sqr (- x m))) xs)
                           (length xs)))]))
  (cond [(zero? n)  +nan.0]
        [else
         (define m2 (max 0 (/ (sum xs^2) n)))
         (adjust-variance m2 n bias)]))

(: skewness* (Symbol Real (Sequenceof Real) (Option (Sequenceof Real)) (U #t #f Real) -> Real))
(define (skewness* name m xs ws bias)
  (define-values (xs^2 xs^3 n)
    (cond [ws  (let-values ([(xs ws)  (sequences->weighted-samples name xs ws)])
                 (values (map (λ: ([x : Real] [w : Real]) (* w (sqr (- x m)))) xs ws)
                         (map (λ: ([x : Real] [w : Real]) (* w (expt (- x m) 3))) xs ws)
                         (max 0 (sum ws))))]
          [else  (let ([xs  (sequence->list xs)])
                   (values (map (λ: ([x : Real]) (sqr (- x m))) xs)
                           (map (λ: ([x : Real]) (expt (- x m) 3)) xs)
                           (length xs)))]))
  (cond [(zero? n)  +nan.0]
        [else
         (define m2 (expt (max 0 (sum xs^2)) 3/2))
         (cond [(zero? m2)  +nan.0]
               [else
                (define m3 (sum xs^3))
                (adjust-skewness (/ (* m3 (sqrt n)) m2) n bias)])]))

(: kurtosis* (Symbol Real (Sequenceof Real) (Option (Sequenceof Real)) (U #t #f Real)
                     -> Nonnegative-Real))
(define (kurtosis* name m xs ws bias)
  (define-values (xs^2 xs^4 n)
    (cond [ws  (let-values ([(xs ws)  (sequences->weighted-samples name xs ws)])
                 (values (map (λ: ([x : Real] [w : Real]) (* w (sqr (- x m)))) xs ws)
                         (map (λ: ([x : Real] [w : Real]) (* w (expt (- x m) 4))) xs ws)
                         (max 0 (sum ws))))]
          [else  (let ([xs  (sequence->list xs)])
                   (values (map (λ: ([x : Real]) (sqr (- x m))) xs)
                           (map (λ: ([x : Real]) (expt (- x m) 4)) xs)
                           (length xs)))]))
  (cond [(zero? n)  +nan.0]
        [else
         (define m2 (sum xs^2))
         (cond [(zero? m2)  +nan.0]
               [else
                (define m4 (sum xs^4))
                (adjust-kurtosis (max 0 (/ (* (/ m4 m2) n) m2)) n bias)])]))

;; ===================================================================================================
;; User-facing functions

(: variance/mean (case-> (Real (Sequenceof Real) [#:bias (U #t #f Real)] -> Nonnegative-Real)
                         (Real (Sequenceof Real) (Option (Sequenceof Real)) [#:bias (U #t #f Real)]
                               -> Nonnegative-Real)))
(define (variance/mean m xs [ws #f] #:bias [bias #f])
  (variance* 'variance/mean m xs ws bias))

(: variance (case-> ((Sequenceof Real) [#:bias (U #t #f Real)] -> Nonnegative-Real)
                    ((Sequenceof Real) (Option (Sequenceof Real)) [#:bias (U #t #f Real)]
                                       -> Nonnegative-Real)))
(define (variance xs [ws #f] #:bias [bias #f])
  (variance* 'variance (mean xs ws) xs ws bias))

(: stddev/mean (case-> (Real (Sequenceof Real) [#:bias (U #t #f Real)] -> Nonnegative-Real)
                       (Real (Sequenceof Real) (Option (Sequenceof Real)) [#:bias (U #t #f Real)]
                             -> Nonnegative-Real)))
(define (stddev/mean m xs [ws #f] #:bias [bias #f])
  (sqrt (variance* 'stddev/mean (mean xs ws) xs ws bias)))

(: stddev (case-> ((Sequenceof Real) [#:bias (U #t #f Real)] -> Nonnegative-Real)
                  ((Sequenceof Real) (Option (Sequenceof Real)) [#:bias (U #t #f Real)]
                                     -> Nonnegative-Real)))
(define (stddev xs [ws #f] #:bias [bias #f])
  (sqrt (variance* 'stddev (mean xs ws) xs ws bias)))

(: skewness/mean (case-> (Real (Sequenceof Real) [#:bias (U #t #f Real)] -> Real)
                         (Real (Sequenceof Real) (Option (Sequenceof Real)) [#:bias (U #t #f Real)]
                               -> Real)))
(define (skewness/mean m xs [ws #f] #:bias [bias #f])
  (skewness* 'skewness/mean m xs ws bias))

(: skewness (case-> ((Sequenceof Real) [#:bias (U #t #f Real)] -> Real)
                    ((Sequenceof Real) (Option (Sequenceof Real)) [#:bias (U #t #f Real)] -> Real)))
(define (skewness xs [ws #f] #:bias [bias #f])
  (skewness* 'skewness (mean xs ws) xs ws bias))

(: kurtosis/mean (case-> (Real (Sequenceof Real) [#:bias (U #t #f Real)] -> Nonnegative-Real)
                         (Real (Sequenceof Real) (Option (Sequenceof Real)) [#:bias (U #t #f Real)]
                               -> Nonnegative-Real)))
(define (kurtosis/mean m xs [ws #f] #:bias [bias #f])
  (kurtosis* 'kurtosis/mean m xs ws bias))

(: kurtosis (case-> ((Sequenceof Real) [#:bias (U #t #f Real)] -> Nonnegative-Real)
                    ((Sequenceof Real) (Option (Sequenceof Real)) [#:bias (U #t #f Real)]
                                       -> Nonnegative-Real)))
(define (kurtosis xs [ws #f] #:bias [bias #f])
  (kurtosis* 'kurtosis (mean xs ws) xs ws bias))
