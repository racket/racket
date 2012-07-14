#lang typed/racket/base

#|
TODO

Error on exact nonpositive integers in log-gamma
|#

(require racket/flonum racket/fixnum
         "../../constants.rkt"
         "log-factorial.rkt"
         "gamma.rkt"
         "sinpx.rkt"
         "polyfun.rkt")

(provide fllog-gamma log-gamma)

;; Computes log(Gamma(x)) using 5 terms from Stirling's series
;; For x >= 143, relative error ε < +epsilon.0
(: fllog-gamma/stirling5 (Float -> Float))
(define (fllog-gamma/stirling5 x)
  (let* ([x  (- x 1.0)]
         [log-x  (fllog x)])
    (+ (* x log-x)
       (- x)
       (* 0.5 (+ (fllog (* 2.0 pi.0)) log-x))
       (let ([1/x  (/ 1.0 x)])
         (* 1/x (+ (* #i-1/360 (* 1/x 1/x)) #i1/12))))))

;; Computes log(Gamma(x)) using 3 terms from Stirling's series
;; For x >= 1e7, relative error ε < +epsilon.0
(: fllog-gamma/stirling3 (Float -> Float))
(define (fllog-gamma/stirling3 x)
  (let* ([x  (- x 1.0)]
         [log-x  (fllog x)])
    (+ (* x log-x)
       (- x)
       (* 0.5 (+ (fllog (* 2.0 pi.0)) log-x)))))

(define lanczos-sum-numer
  (make-polyfun
   Float
   (56906521.91347156388090791033559122686859
    103794043.1163445451906271053616070238554
    86363131.28813859145546927288977868422342
    43338889.32467613834773723740590533316085
    14605578.08768506808414169982791359218571
    3481712.15498064590882071018964774556468
    601859.6171681098786670226533699352302507
    75999.29304014542649875303443598909137092
    6955.999602515376140356310115515198987526
    449.9445569063168119446858607650988409623
    19.51992788247617482847860966235652136208
    0.5098416655656676188125178644804694509993
    0.006061842346248906525783753964555936883222)))

(define lanczos-sum-denom
  (make-polyfun
   Float
   (0.0
    39916800.0
    120543840.0
    150917976.0
    105258076.0
    45995730.0
    13339535.0
    2637558.0
    357423.0
    32670.0
    1925.0
    66.0
    1.0)))

(define lanczos-g 6.024680040776729583740234375)

(: fllog-gamma/lanczos (Float -> Float))
(define (fllog-gamma/lanczos x)
  (define s (* (- x 0.5) (- (fllog (+ x lanczos-g -0.5)) 1.0)))
  (define p (lanczos-sum-numer x))
  (define q (lanczos-sum-denom x))
  (cond [(x . < . 8.39911597930119e-309)  (+ s (- (fllog p) (fllog q)))]
        [else  (+ s (fllog (/ p q)))]))

(: fllog-abs-gamma (Float -> (Values Float Float)))
(define (fllog-abs-gamma x)
  (cond #;[(integer? x)  (values (fllog-factorial (- (fl->fx x) 1)) 1.0)]
        [(x . < . 0.0)
         (define t (sinpx x))
         (cond [(t . < . 0.0)  (values (- (log pi.0) (fllog-gamma (- x)) (log (- t))) 1.0)]
               [else           (values (- (log pi.0) (fllog-gamma (- x)) (log t)) -1.0)])]
        [(x . >= . 1.0e7)  (values (fllog-gamma/stirling3 x) 1.0)]
        [(x . >= . 143.0)  (values (fllog-gamma/stirling5 x) 1.0)]
        [(x . >= . 5.0)    (values (fllog (flgamma x)) 1.0)]
        [else              (values (fllog-gamma/lanczos x) 1.0)]))

(: fllog-gamma (Float -> Float))
(define (fllog-gamma x)
  (define-values (y s) (fllog-abs-gamma x))
  y)

(: log-gamma (Real -> Real))
(define (log-gamma x)
  (cond [(double-flonum? x)  (fllog-gamma x)]
        [(exact-positive-integer? x)  (log-factorial (- x 1))]
        [(single-flonum? x)  (real->single-flonum (fllog-gamma (real->double-flonum x)))]
        [else  (fllog-gamma (real->double-flonum x))]))
