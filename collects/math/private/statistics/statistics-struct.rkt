#lang typed/racket/base

(require racket/match
         "../../flonum.rkt"
         "statistics-utils.rkt")

(provide statistics
         statistics?
         empty-statistics
         update-statistics
         update-statistics*
         statistics-min
         statistics-max
         statistics-count
         statistics-range
         statistics-mean
         statistics-variance
         statistics-stddev
         statistics-skewness
         statistics-kurtosis)

(struct: base-statistics
  ([min : Flonum]
   [max : Flonum]
   [count : Nonnegative-Flonum])
  #:transparent)

(struct: statistics base-statistics
  ([m1 : Flonum]
   [m2 : Nonnegative-Flonum]
   [m3 : Flonum]
   [m4 : Nonnegative-Flonum]))

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

(: update-statistics*
   (case-> (statistics (Sequenceof Real) -> statistics)
           (statistics (Sequenceof Real) (Option (Sequenceof Real)) -> statistics)))
(define (update-statistics* e xs [ws #f])
  (cond [ws
         (for/fold: ([e : statistics  e]) ([x xs] [w ws])
           (update-statistics e x w))]
        [else
         (for/fold: ([e : statistics  e]) ([x xs])
           (update-statistics e x 1.0))]))

;; ===================================================================================================

(: statistics-min (statistics -> Flonum))
(define (statistics-min e) (base-statistics-min e))

(: statistics-max (statistics -> Flonum))
(define (statistics-max e) (base-statistics-max e))

(: statistics-count (statistics -> Nonnegative-Flonum))
(define (statistics-count e) (base-statistics-count e))

(: statistics-range (statistics -> Nonnegative-Flonum))
(define (statistics-range e)
  (define mn (statistics-min e))
  (define mx (statistics-max e))
  (define rng (fl- mx mn))
  (cond [(rng . fl>= . 0.0)  rng]
        [else  +nan.0]))

(: statistics-mean (statistics -> Flonum))
(define (statistics-mean e)
  (define n (statistics-count e))
  (if (fl= n 0.0) +nan.0 (statistics-m1 e)))

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
