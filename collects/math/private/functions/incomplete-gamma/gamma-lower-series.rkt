#lang typed/racket/base

;; Computes the lower gamma series (found by unfolding its recursive characterization infinitely)

(require "../../../flonum.rkt"
         "../log-gamma.rkt"
         "gamma-utils.rkt")

(provide flgamma-lower-series fllog-gamma-lower-series get-series-iters)

(define: series-iters : Flonum  0.0)
(define (get-series-iters) series-iters)

(: flgamma-lower-iter (Float Float -> Float))
(define (flgamma-lower-iter k x)
  (let: loop : Float ([y  : Float  0.0]
                      [dy : Float  (fl/ x (fl+ k 1.0))]
                      [i  : Float  0.0])
    (define new-y (fl+ y dy))
    (cond [(or (not (rational? new-y))
               ((flabs dy) . fl<= . (flabs (fl* epsilon.0 new-y))))
           (set! series-iters i)
           new-y]
          [else
           (loop new-y (fl/ (fl* dy x) (fl+ (fl+ 2.0 i) k)) (fl+ i 1.0))])))

(: flgamma-lower-series (Float Float -> Float))
(define (flgamma-lower-series k x)
  (set! series-iters 0.0)
  (define z (flgamma-series-const k x))
  (cond [(fl= z 0.0)  0.0]
        [else
         (define y (flgamma-lower-iter k x))
         ;; Avoid adding 1.0 if y is near zero
         (if (y . fl< . 1.0) (fl+ z (fl* z y)) (fl* z (fl+ y 1.0)))]))

(: fllog-gamma-lower-series (Float Float -> Float))
(define (fllog-gamma-lower-series k x)
  (define y (flgamma-lower-iter k x))
  (define log-z (fllog-gamma-series-const k x))
  (fl+ log-z (fllog1p (fl y))))
