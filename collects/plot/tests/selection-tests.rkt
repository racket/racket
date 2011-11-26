#lang racket

;; These tests aren't meant to be run so much as manipulated after running

(require plot plot/utils unstable/flonum)

(parameterize ([plot-x-transform  log-transform])
  (plot (function values +min.0 1)))

(define raise-error? #f)

(plot (function (λ (x)
                  (when raise-error?
                    (error 'buh "buh buh"))
                  (sin x))
                -4 4))

(plot3d (surface3d (λ (x y)
                     (when raise-error?
                       (error 'buh "buh buh buh"))
                     (- (sqr x) (sqr y)))
                   -1 1 -1 1))

(set! raise-error? #t)
