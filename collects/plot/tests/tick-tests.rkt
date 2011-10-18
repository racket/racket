#lang racket

(require plot (only-in plot/common/math floor-log/base real-modulo))

(plot-font-family 'swiss)

(define (ticks-scale fun t)
  (match-define (invertible-function f g) fun)
  (match-define (ticks layout format) t)
  (ticks (λ (x-min x-max max-ticks transform)
           (define ts (layout (f x-min) (f x-max) max-ticks transform))
           (for/list ([t  (in-list ts)])
             (match-define (pre-tick x major?) t)
             (pre-tick (g x) major?)))
         (λ (x-min x-max ts)
           (format (f x-min) (f x-max) (map (λ (t)
                                              (match-define (pre-tick x major?) t)
                                              (pre-tick (f x) major?))
                                            ts)))))

(define (linear-scale m [b 0])
  (invertible-function (λ (x) (+ (* m x) b))
                       (λ (y) (/ (- y b) m))))

(define exp-scale
  (invertible-function exp log))

(parameterize ([plot-y-ticks  (ticks-scale (linear-scale 2 1) (plot-y-ticks))])
  (plot (list (function sqr -2 2)
              (function sin -4 4))))

(parameterize ([plot-y-ticks  (ticks-scale exp-scale (log-ticks))])
  (plot (list (function sqr -2 2)
              (function sin -4 4))))

(parameterize ([plot-y-ticks  (ticks-scale exp-scale (log-ticks))])
  (plot (function values -10 10)))

(parameterize ([plot-y-transform  log-transform]
               [plot-y-ticks  (log-ticks)])
  (plot (function exp -10 10)))

(parameterize ([plot-y-ticks  (ticks-append (plot-y-ticks)
                                            (ticks-scale (linear-scale 2 1) (currency-ticks)))])
  (plot (function values -4 4)))

#|
(plot (function (λ (x) (count pre-tick-major? ((linear-ticks) 0 x 8 id-transform)))
                0.1 10))

(plot (function (λ (x) (count pre-tick-major? ((linear-ticks) 0 x 40 id-transform)))
                1 100))

(parameterize ([plot-x-ticks  (linear-ticks #:base 2 #:divisors '(1 2))]
               #;[plot-y-ticks  (linear-ticks #:base (* 1 2 3 4 5) #:divisors '(1 2 3 4 5))])
  (plot (function cos 0.013 2.1176)))

(parameterize ([plot-x-transform  log-transform]
               [plot-x-ticks  (ticks (log-ticks-layout)
                                     (fraction-ticks-format))]
               [plot-y-ticks  (fraction-ticks)])
  (plot (function (λ (x) (+ 1 (cos x))) 0.0001 12)))

(parameterize ([plot-x-ticks  (date-ticks)]
               [plot-x-max-ticks  3]
               [plot-y-ticks  (currency-ticks)])
  (plot (function values -1 1)))

(parameterize ([plot-x-ticks  (date-ticks)]
               [currency-format-strings  uk-currency-format-strings]
               [currency-scale-suffixes  uk-currency-scale-suffixes]
               [plot-y-ticks  (currency-ticks #:kind 'GBP)])
  (plot (function values 101232512 2321236192)))

(parameterize ([currency-format-strings  eu-currency-format-strings]
               [currency-scale-suffixes  eu-currency-scale-suffixes]
               [plot-x-ticks  (currency-ticks #:kind 'EUR)]
               [plot-y-ticks  (currency-ticks)])
  (plot (function (λ (x) (* x 1.377)) 8000000 10000000)
        #:title "EUR-USD Conversion, 2011-10-13"
        #:x-label "Euros"
        #:y-label "Dollars"))

(parameterize ([plot-x-ticks  no-ticks])
  (plot (function sin -1 4)))

(parameterize ([plot-x-transform  log-transform]
               [plot-y-transform  log-transform]
               [plot-x-ticks      (log-ticks #:base 10)]
               [plot-y-ticks      (log-ticks #:base 2)])
  (plot (function values 0.1 10)))

(parameterize ([plot-x-transform  log-transform]
               [plot-y-transform  (stretch-transform -1 1 4)]
               [plot-x-ticks      (ticks (uniform-ticks-layout)
                                         (log-ticks-format #:base 10))]
               [plot-y-ticks      (ticks (uniform-ticks-layout)
                                         (currency-ticks-format #:kind 'USD))])
  (plot (function log 0.1 10)))

(parameterize ([plot-x-transform  log-transform]
               [plot-x-ticks      (log-ticks #:base 10)])
  (plot (function values 10000000000000 1000000000000000)))

(plot (polar-axes) #:x-min -1 #:x-max 1 #:y-min -1 #:y-max 1)

(plot (polar-axes) #:x-min 0 #:x-max 3 #:y-min 0 #:y-max 3)

(plot (polar-axes) #:x-min 1 #:x-max 4 #:y-min 1 #:y-max 4)

(plot (polar-axes #:number 12) #:x-min 10 #:x-max 12 #:y-min 10 #:y-max 12)

(parameterize ([plot-z-transform  log-transform]
               [plot-z-ticks      (log-ticks)]
               [contour-samples   (plot3d-samples)])
  (values
   (plot (contours (λ (x y) (exp (- (+ (sqr x) (sqr y))))) -2 2 -2 2 #:label "z"))
   (plot (contour-intervals (λ (x y) (exp (- (+ (sqr x) (sqr y))))) -2 2 -2 2 #:label "z"))
   (plot3d (contours3d (λ (x y) (exp (- (+ (sqr x) (sqr y))))) -2 2 -2 2 #:label "z"))
   (plot3d (contour-intervals3d (λ (x y) (exp (- (+ (sqr x) (sqr y))))) -2 2 -2 2 #:label "z"))))

(plot (contours (λ (x y) (* 1/2 (+ (sqr x) (sqr y)))) -1 1 -1 1 #:label "z"))
(plot3d (contours3d (λ (x y) (* 1/2 (+ (sqr x) (sqr y)))) -1 1 -1 1 #:label "z"))
|#