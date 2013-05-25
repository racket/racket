#lang typed/racket/base

(require racket/sequence
         racket/list
         "../../base.rkt"
         "../../flonum.rkt"
         "expected-values.rkt"
         "statistics-utils.rkt")

(provide covariance/means
         correlation/means
         covariance
         correlation)

(: covariance* (Symbol Real Real (Sequenceof Real) (Sequenceof Real) (Option (Sequenceof Real))
                       (U #t #f Real) -> Real))
(define (covariance* name mx my xs ys ws bias)
  (define-values (zs n)
    (cond [ws  (let-values ([(xs ws)  (sequences->weighted-samples name xs ws)]
                            [(ys)  (sequence->list ys)])
                 (check-lengths! name "value sequences" xs ys (length xs) (length ys))
                 (values (map (λ: ([x : Real] [y : Real] [w : Real]) (* w (- x mx) (- y my)))
                              xs ys ws)
                         (max 0 (sum ws))))]
          [else  (let ([xs  (sequence->list xs)]
                       [ys  (sequence->list ys)])
                   (check-lengths! name "value sequences" xs ys (length xs) (length ys))
                   (values (map (λ: ([x : Real] [y : Real]) (* (- x mx) (- y my))) xs ys)
                           (length xs)))]))
  (define m2 (/ (sum zs) n))
  (adjust-covariance m2 n bias))

(: covariance/means (case-> (Real Real (Sequenceof Real) (Sequenceof Real)
                                  [#:bias (U #t #f Real)] -> Real)
                            (Real Real (Sequenceof Real) (Sequenceof Real) (Option (Sequenceof Real))
                                  [#:bias (U #t #f Real)] -> Real)))
(define (covariance/means mx my xs ys [ws #f] #:bias [bias #f])
  (covariance* 'covariance/means mx my xs ys ws bias))

(: covariance (case-> ((Sequenceof Real) (Sequenceof Real) [#:bias (U #t #f Real)] -> Real)
                      ((Sequenceof Real) (Sequenceof Real) (Option (Sequenceof Real))
                                         [#:bias (U #t #f Real)] -> Real)))
(define (covariance xs ys [ws #f] #:bias [bias #f])
  (covariance* 'covariance (mean xs ws) (mean ys ws) xs ys ws bias))

(: correlation/means (case-> (Real Real (Sequenceof Real) (Sequenceof Real)
                                   [#:bias (U #t #f Real)] -> Real)
                             (Real Real (Sequenceof Real) (Sequenceof Real) (Option (Sequenceof Real))
                                   [#:bias (U #t #f Real)] -> Real)))
(define (correlation/means mx my xs ys [ws #f] #:bias [bias #f])
  (define g (covariance/means mx my xs ys ws #:bias bias))
  (define sx (stddev/mean mx xs ws #:bias bias))
  (define sy (stddev/mean my ys ws #:bias bias))
  (cond [(zero? sx)  +nan.0]
        [(zero? sy)  +nan.0]
        [else  (/ g sx sy)]))

(: correlation (case-> ((Sequenceof Real) (Sequenceof Real) [#:bias (U #t #f Real)] -> Real)
                       ((Sequenceof Real) (Sequenceof Real) (Option (Sequenceof Real))
                                          [#:bias (U #t #f Real)] -> Real)))
(define (correlation xs ys [ws #f] #:bias [bias #f])
  (correlation/means (mean xs ws) (mean ys ws) xs ys ws #:bias bias))
