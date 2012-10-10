#lang typed/racket/base

(require racket/match
         racket/sequence
         "../../base.rkt"
         "../../flonum.rkt"
         "../exception.rkt"
         "statistics-utils.rkt")

(provide (struct-out statistics)
         empty-statistics
         update-statistics
         sequence->statistics
         statistics-range
         statistics-variance
         statistics-stddev
         statistics-skewness
         statistics-kurtosis
         mean
         variance/given-mean
         stddev/given-mean
         skewness/given-mean
         kurtosis/given-mean
         variance
         stddev
         skewness
         kurtosis)

(struct: statistics ([min : Flonum]
                     [max : Flonum]
                     [count : Nonnegative-Flonum]
                     [mean : Flonum]
                     [m2 : Nonnegative-Flonum]
                     [m3 : Flonum]
                     [m4 : Nonnegative-Flonum])
  #:transparent)

(define empty-statistics (statistics +inf.0 -inf.0 0.0 0.0 0.0 0.0 0.0))

(: update-statistics (case-> (statistics Real -> statistics)
                             (statistics Real Real -> statistics)))
(define (update-statistics e x [w 1.0])
  (match-define (statistics mn mx n m1 m2 m3 m4) e)
  (cond
    [(w . < . 0)  (raise-argument-error 'update-statistics "Nonnegative-Real" 2 e x w)]
    [else
     (let* ([x  (fl x)]
            [w  (fl w)]
            [w+n  (fl+ w n)]
            [d  (fl- m1 x)]
            [dm1  (fl/ (fl* d w) w+n)]
            [new-m1  (fl- m1 dm1)]
            [new-m2  (flmax (fl+ m2 (fl* dm1 (fl* n d))) 0.0)]
            [new-m3  (fl+ m3 (fl* dm1 (fl+ (fl* 3.0 m2) (fl* d (fl* n (fl- (fl* 2.0 dm1) d))))))]
            [new-m4  (flmax
                      (fl+ m4 (fl* dm1 (fl+ (fl+ (fl* 4.0 m3) (fl* dm1 (fl* 6.0 m2)))
                                            (fl* d (fl+ (fl* dm1 (fl* dm1 (fl+ (fl* 6.0 n)
                                                                               (fl* 3.0 w))))
                                                        (fl* d (fl* d (fl+ n (fl* -3.0 w)))))))))
                      0.0)])
       (statistics (flmin x mn) (flmax x mx) w+n new-m1 new-m2 new-m3 new-m4))]))

(: sequence->statistics (case-> ((Sequenceof Real) -> statistics)
                                ((Sequenceof Real) (Sequenceof Real) -> statistics)))
(define sequence->statistics
  (case-lambda
    [(xs)  (for/fold: ([e : statistics  empty-statistics]) ([x xs])
             (update-statistics e x))]
    [(xs ws)  (for/fold: ([e : statistics  empty-statistics]) ([x xs] [w ws])
                (update-statistics e x w))]))

;; ===================================================================================================

(: statistics-range (statistics -> Nonnegative-Flonum))
(define (statistics-range e)
  (define mn (statistics-min e))
  (define mx (statistics-max e))
  (define rng (fl- mx mn))
  (cond [(rng . fl>= . 0.0)  rng]
        [else  +nan.0]))

(: statistics-variance (statistics [#:bias (U #t #f Real)] -> Nonnegative-Flonum))
(define (statistics-variance e #:bias [bias #f])
  (define n (statistics-count e))
  (fl (adjust-variance (flmax 0.0 (fl/ (statistics-m2 e) n)) n bias)))

(: statistics-stddev (statistics [#:bias (U #t #f Real)] -> Nonnegative-Flonum))
(define (statistics-stddev e #:bias [bias #f])
  (flsqrt (flmax 0.0 (statistics-variance e #:bias bias))))

(: statistics-skewness (statistics [#:bias (U #t #f Real)] -> Flonum))
(define (statistics-skewness e #:bias [bias #f])
  (define n (statistics-count e))
  (define m2 (statistics-m2 e))
  (define m3 (statistics-m3 e))
  (fl (adjust-skewness (fl/ (fl* m3 (flsqrt n)) (flexpt m2 #i3/2)) n bias)))

(: statistics-kurtosis (statistics [#:bias (U #t #f Real)] -> Nonnegative-Flonum))
(define (statistics-kurtosis e #:bias [bias #f])
  (define n (statistics-count e))
  (define m2 (statistics-m2 e))
  (define m4 (statistics-m4 e))
  (fl (adjust-kurtosis (flmax 0.0 (fl/ (fl* (fl/ m4 m2) n) m2)) n bias)))

;; ===================================================================================================

(define-type (Moment-Fun T)
  (case-> ((Sequenceof Real) [#:bias (U #t #f Real)] -> T)
          ((Sequenceof Real) (Sequenceof Real) [#:bias (U #t #f Real)] -> T)))

(define-type (Moment/Mean-Fun T)
  (case-> (Real (Sequenceof Real) [#:bias (U #t #f Real)] -> T)
          (Real (Sequenceof Real) (Sequenceof Real) [#:bias (U #t #f Real)] -> T)))

(: mean (case-> ((Sequenceof Real) -> Real)
                ((Sequenceof Real) (Sequenceof Real) -> Real)))
(define (mean xs [ws #f])
  (cond [ws  (let-values ([(xs ws)  (sequences->weighted-samples xs ws)])
               (/ (apply sum (map * xs ws)) (apply sum ws)))]
        [else  (let ([xs  (sequence->list xs)])
                 (/ (apply sum xs) (length xs)))]))

(: variance/given-mean (Moment/Mean-Fun Nonnegative-Real))
(define (variance/given-mean m xs [ws #f] #:bias [bias #f])
  (define-values (xs^2 n)
    (cond [ws  (let-values ([(xs ws)  (sequences->weighted-samples xs ws)])
                 (values (map (λ: ([x : Real] [w : Real]) (* w (sqr (- x m)))) xs ws)
                         (max 0 (apply sum ws))))]
          [else  (let ([xs  (sequence->list xs)])
                   (values (map (λ: ([x : Real]) (sqr (- x m))) xs)
                           (length xs)))]))
  (define m2 (max 0 (/ (apply sum xs^2) n)))
  (adjust-variance m2 n bias))

(: stddev/given-mean (Moment/Mean-Fun Nonnegative-Real))
(define (stddev/given-mean m xs [ws #f] #:bias [bias #f])
  (cond [ws  (sqrt (variance/given-mean m xs ws #:bias bias))]
        [else  (sqrt (variance/given-mean m xs #:bias bias))]))

(: skewness/given-mean (Moment/Mean-Fun Real))
(define (skewness/given-mean m xs [ws #f] #:bias [bias #f])
  (define-values (xs^2 xs^3 n)
    (cond [ws  (let-values ([(xs ws)  (sequences->weighted-samples xs ws)])
                 (values (map (λ: ([x : Real] [w : Real]) (* w (sqr (- x m)))) xs ws)
                         (map (λ: ([x : Real] [w : Real]) (* w (expt (- x m) 3))) xs ws)
                         (max 0 (apply sum ws))))]
          [else  (let ([xs  (sequence->list xs)])
                   (values (map (λ: ([x : Real]) (sqr (- x m))) xs)
                           (map (λ: ([x : Real]) (expt (- x m) 3)) xs)
                           (length xs)))]))
  (define m2 (max 0 (apply sum xs^2)))
  (define m3 (apply sum xs^3))
  (adjust-skewness (/ (* m3 (sqrt n)) (expt m2 3/2)) n bias))

(: kurtosis/given-mean (Moment/Mean-Fun Nonnegative-Real))
(define (kurtosis/given-mean m xs [ws #f] #:bias [bias #f])
  (define-values (xs^2 xs^4 n)
    (cond [ws  (let-values ([(xs ws)  (sequences->weighted-samples xs ws)])
                 (values (map (λ: ([x : Real] [w : Real]) (* w (sqr (- x m)))) xs ws)
                         (map (λ: ([x : Real] [w : Real]) (* w (expt (- x m) 4))) xs ws)
                         (max 0 (apply sum ws))))]
          [else  (let ([xs  (sequence->list xs)])
                   (values (map (λ: ([x : Real]) (sqr (- x m))) xs)
                           (map (λ: ([x : Real]) (expt (- x m) 4)) xs)
                           (length xs)))]))
  (define m2 (apply sum xs^2))
  (define m4 (apply sum xs^4))
  (adjust-kurtosis (max 0 (/ (* (/ m4 m2) n) m2)) n bias))

(: variance (Moment-Fun Nonnegative-Real))
(define (variance xs [ws #f] #:bias [bias #f])
  (cond [ws  (variance/given-mean (mean xs ws) xs ws #:bias bias)]
        [else  (variance/given-mean (mean xs) xs #:bias bias)]))

(: stddev (Moment-Fun Nonnegative-Real))
(define (stddev xs [ws #f] #:bias [bias #f])
  (cond [ws  (sqrt (variance xs ws #:bias bias))]
        [else  (sqrt (variance xs #:bias bias))]))

(: skewness (Moment-Fun Real))
(define (skewness xs [ws #f] #:bias [bias #f])
  (cond [ws  (skewness/given-mean (mean xs ws) xs ws #:bias bias)]
        [else  (skewness/given-mean (mean xs) xs #:bias bias)]))

(: kurtosis (Moment-Fun Nonnegative-Real))
(define (kurtosis xs [ws #f] #:bias [bias #f])
  (cond [ws  (kurtosis/given-mean (mean xs ws) xs ws #:bias bias)]
        [else  (kurtosis/given-mean (mean xs) xs #:bias bias)]))
