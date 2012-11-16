#lang typed/racket/base

#|
Rational Chebyshev Approximations for the Inverse of the Error Function
J. M. Blair, C. A. Edwards and J. H. Johnson
Mathematics of Computation, Vol. 30, No. 136 (Oct., 1976), pp. 827-830

Cribbed from LGPL Fortran implementation in:

ALGORITHM 715: SPECFUN - A Portable FORTRAN Package of Special Function Routines and Test Drivers
W. J. Cody
ACM Transactions on Mathematical Software, Vol. 19 (1993), pp. 22-32

Error is <= 3 ulps for both log and non-log functions
|#

(require "../../../flonum.rkt"
         "../../../base.rkt"
         "normal-utils.rkt")

(provide standard-flnormal-log-cdf
         standard-flnormal-cdf)

(: R0 (Float Any -> Float))
(define (R0 x log?)
  (cond [log?  (if (x . >= . 0.0)
                   (lg+ (fllog 0.5) (lg/ (fllog x) logsqrt2pi.0))
                   (lg- (fllog 0.5) (lg/ (fllog (- x)) logsqrt2pi.0)))]
        [else  (+ 0.5 (/ x sqrt2pi.0))]))

(: R1 (Float Any -> Float))
(define (R1 x log?)
  (define x^2 (fl* x x))
  (let* ([r  6.5682337918207449113e-2]
         [r  (fl+ (fl* r x^2) 2.2352520354606839287e0)]
         [r  (fl+ (fl* r x^2) 1.6102823106855587881e2)]
         [r  (fl+ (fl* r x^2) 1.0676894854603709582e3)]
         [r  (fl+ (fl* r x^2) 1.8154981253343561249e4)]
         [s  1.0]
         [s  (fl+ (fl* s x^2) 4.7202581904688241870e1)]
         [s  (fl+ (fl* s x^2) 9.7609855173777669322e2)]
         [s  (fl+ (fl* s x^2) 1.0260932208618978205e4)]
         [s  (fl+ (fl* s x^2) 4.5507789335026729956e4)])
    (cond [log?  (if (x . fl>= . 0.0)
                     (lg+ (fllog 0.5) (lg* (fllog x) (lg/ (fllog r) (fllog s))))
                     (lg- (fllog 0.5) (lg* (fllog (- x)) (lg/ (fllog r) (fllog s)))))]
          [else  (fl+ 0.5 (fl* x (fl/ r s)))])))

(: R2-post (Flonum Flonum Any -> Flonum))
(define (R2-post x q log?)
  (cond [log?
         (cond [(x . fl> . 0.0)  (fllog1p (- (fl* (flexp-1/2*x^2 x) q)))]
               [else  (fl+ (fl* (fl* -0.5 x) x) (fllog q))])]
        [else
         (define z (fl* (flexp-1/2*x^2 x) q))
         (if (x . fl> . 0.0) (fl- 1.0 z) z)]))

(: R2 (Float Any -> Float))
(define (R2 x log?)
  (define y (flabs x))
  (let* ([r  1.0765576773720192317e-8]
         [r  (fl+ (fl* r y) 3.9894151208813466764e-1)]
         [r  (fl+ (fl* r y) 8.8831497943883759412e0)]
         [r  (fl+ (fl* r y) 9.3506656132177855979e1)]
         [r  (fl+ (fl* r y) 5.9727027639480026226e2)]
         [r  (fl+ (fl* r y) 2.4945375852903726711e3)]
         [r  (fl+ (fl* r y) 6.8481904505362823326e3)]
         [r  (fl+ (fl* r y) 1.1602651437647350124e4)]
         [r  (fl+ (fl* r y) 9.8427148383839780218e3)]
         [s  1.0]
         [s  (fl+ (fl* s y) 2.2266688044328115691e1)]
         [s  (fl+ (fl* s y) 2.3538790178262499861e2)]
         [s  (fl+ (fl* s y) 1.5193775994075548050e3)]
         [s  (fl+ (fl* s y) 6.4855582982667607550e3)]
         [s  (fl+ (fl* s y) 1.8615571640885098091e4)]
         [s  (fl+ (fl* s y) 3.4900952721145977266e4)]
         [s  (fl+ (fl* s y) 3.8912003286093271411e4)]
         [s  (fl+ (fl* s y) 1.9685429676859990727e4)])
    (R2-post x (fl/ r s) log?)))

(: R3 (Float Any -> Float))
(define (R3 x log?)
  (define x^2 (fl/ (fl/ 1.0 x) x))
  (let* ([r  2.307344176494017303e-2]
         [r  (fl+ (fl* r x^2) 2.158985340579569900e-1)]
         [r  (fl+ (fl* r x^2) 1.274011611602473639e-1)]
         [r  (fl+ (fl* r x^2) 2.223527787064980700e-2)]
         [r  (fl+ (fl* r x^2) 1.421619193227893466e-3)]
         [r  (fl+ (fl* r x^2) 2.9112874951168792e-5)]
         [s  1.0]
         [s  (fl+ (fl* s x^2) 1.28426009614491121e0)]
         [s  (fl+ (fl* s x^2) 4.68238212480865118e-1)]
         [s  (fl+ (fl* s x^2) 6.59881378689285515e-2)]
         [s  (fl+ (fl* s x^2) 3.78239633202758244e-3)]
         [s  (fl+ (fl* s x^2) 7.29751555083966205e-5)])
    (R2-post x (fl/ (fl- 1/sqrt2pi.0 (fl* x^2 (fl/ r s)))
                    (flabs x))
             log?)))

(: standard-flnormal-log-cdf (Float -> Float))
(define (standard-flnormal-log-cdf x)
  (define y (flabs x))
  (cond [(y . fl<= . 1e-5)  (R0 x #t)]
        [(y . fl<= . 0.6744897501960817)  (R1 x #t)]
        [(y . fl<= . (flsqrt 32.0))  (R2 x #t)]
        [(x . fl< . -standard-normal-log-cdf-max.0)  -inf.0]
        [(x . fl> . +standard-normal-log-cdf-max.0)  0.0]
        [else  (R3 x #t)]))

(: standard-flnormal-cdf (Float -> Float))
(define (standard-flnormal-cdf x)
  (define y (flabs x))
  (cond [(y . fl<= . 1e-16)  0.5]
        [(y . fl<= . 1e-6)  (R0 x #f)]
        [(y . fl<= . 0.6744897501960817)  (R1 x #f)]
        [(y . fl<= . (flsqrt 32.0))  (R2 x #f)]
        [(x . fl< . -38.456870800437045)
         (cond [(x . fl< . -standard-normal-cdf-max.0)  0.0]
               [else  +min.0])]
        [(x . fl> . 8.160707840858583)
         (cond [(x . fl> . +standard-normal-cdf-max.0)  1.0]
               [else  0.9999999999999999])]
        [else  (R3 x #f)]))
