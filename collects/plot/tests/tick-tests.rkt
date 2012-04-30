#lang racket

(require plot plot/utils)

(plot-font-family 'swiss)

(define (get-isosurface-ticks z-min z-max)
  (cond [(z-min . >= . z-max)  empty]
        [else
         (map pre-tick-value
              (filter pre-tick-major?
                      (contour-ticks (plot-d-ticks) z-min z-max 'auto #f)))]))

;; try to verify that we always get 3-5 isosurfaces from the isosurfaces3d renderer
(time
 (plot (function (λ (x)
                   (let ([ts  (get-isosurface-ticks 1/10 (+ 1/10 x))])
                     (if (empty? ts) +nan.0 (length ts))))
                 #:samples 10000)
       #:x-min 0 #:x-max 10
       #:x-label "bounds size (min = 1/10)"
       #:y-label "number of ticks"))

;; try to verify that we always get 3-5 isosurfaces from the isosurfaces3d renderer
(time
 (plot3d (contour-intervals3d (λ (x y)
                                (let ([ts  (get-isosurface-ticks x (+ x y))])
                                  (if (empty? ts) +nan.0 (length ts))))
                              #:samples 101 #:line-styles '(transparent))
         #:x-min 0 #:x-max 10 #:y-min 0 #:y-max 10
         #:x-label "bounds min" #:y-label "bounds size"
         #:z-label "number of ticks"))

(time
 (plot (contour-intervals (λ (x y)
                            (let ([ts  (get-isosurface-ticks x (+ x y))])
                              (if (empty? ts) +nan.0 (length ts))))
                          #:samples 101)
       #:x-min 0 #:x-max 10 #:y-min 0 #:y-max 10
       #:x-label "bounds min" #:y-label "bounds size"))

(plot (function (λ (x) (count pre-tick-major? ((linear-ticks) 0 x))) #e0.1 10))
(plot (function (λ (x) (count pre-tick-major? ((linear-ticks #:number 40) 0 x))) 1 100))

(parameterize ([plot-x-ticks  (linear-ticks #:base 2 #:divisors '(1 2))]
               #;[plot-y-ticks  (linear-ticks #:base (* 1 2 3 4 5) #:divisors '(1 2 3 4 5))])
  (plot (function cos 0.013 2.1176)))

(parameterize ([plot-x-transform  log-transform]
               [plot-x-ticks  (ticks (log-ticks-layout)
                                     (fraction-ticks-format))]
               [plot-y-ticks  (fraction-ticks)])
  (plot (function (λ (x) (+ 1 (cos x))) 0.0001 12)))

(parameterize ([plot-x-ticks  (date-ticks #:number 3)]
               [plot-y-ticks  (currency-ticks)])
  (plot (function values -1 1)))

(parameterize* ([currency-ticks-formats uk-currency-formats]
                [currency-ticks-scales uk-currency-scales]
                [plot-x-ticks  (date-ticks)]
                [plot-y-ticks  (currency-ticks #:kind 'GBP)])
  (plot (function values 101232512 2321236192)))

(parameterize ([plot-x-ticks  (currency-ticks #:kind 'EUR
                                              #:scales eu-currency-scales
                                              #:formats eu-currency-formats)]
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
               [plot-x-ticks      (ticks (linear-ticks-layout)
                                         (log-ticks-format #:base 10))]
               [plot-y-ticks      (ticks (linear-ticks-layout)
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

(parameterize ([plot-y-ticks  (ticks-scale (plot-y-ticks) (linear-scale 2 1))])
  (plot (list (function sqr -2 2)
              (function sin -4 4))))

(define exp-scale (invertible-function exp log))

(parameterize ([plot-y-ticks  (ticks-scale (log-ticks) exp-scale)])
  (plot (function values -10 10)))

(parameterize ([plot-y-ticks  (ticks-add (ticks no-ticks-layout (linear-ticks-format))
                                         '(1/3 2/3))])
  (plot (function sin -4 4)))

(plot (list (function sin -4 4)
            (points '(#(-3.75 -1/4)) #:size 10)
            (x-ticks (list (tick 1.5 #t "3/2") (tick 3 #t "Three")))
            (y-ticks (list (tick 1/4 #t "1/4") (tick -1/4 #f "")))))

(parameterize ([plot-z-ticks  (linear-ticks #:number 5)])
  (plot3d (list (surface3d (λ (x y) (* 2 (+ (sin x) (cos y)))) -4 4 -4 4 #:alpha 1/2)
                (x-ticks (list (tick 1.5 #t "3/2") (tick 3 #t "Three")))
                (y-ticks (list (tick 1/3 #t "1/3") (tick -1/3 #f "1/3")))
                (z-ticks (list (tick pi #f "π") (tick (- pi) #t "-π"))))))

(parameterize ([plot-title  "Money for time in a sine wave"]
               [plot-x-far-ticks  (time-ticks)]
               [plot-y-ticks  (currency-ticks #:kind 'USD)]
               [plot-y-far-ticks  (ticks-scale (currency-ticks #:kind 'EUR) (linear-scale 1.47))]
               [plot-x-label  #f]
               [plot-y-label  #f])
  (values
   (parameterize ([plot-x-axis?  #f]
                  [plot-x-far-axis?  #f])
     (plot (list (function sin -4 4)
                 (x-axis -0.25 #:ticks? #f #:labels? #t)
                 (x-axis 0.25 #:ticks? #t #:labels? #t #:far? #t))))
   
   (parameterize ([plot-y-axis?  #f]
                  [plot-y-far-axis?  #f])
     (plot (list (function sin -4 4)
                 (y-axis -1 #:ticks? #f #:labels? #t)
                 (y-axis 1 #:ticks? #t #:labels? #t #:far? #t))))))

(parameterize ([plot-y-ticks  (fraction-ticks)])
  (plot (function sin (- pi) pi)))

(parameterize ([plot-x-far-label  "x far axis"]
               [plot-x-ticks      (linear-ticks #:number 10)]
               [plot-y-far-label  "y far axis"]
               [plot-y-far-ticks  (date-ticks)]
               [plot-z-label  "z axis"]
               [plot-z-far-label  "z far axis"]
               [plot-z-far-ticks  (currency-ticks #:number 5)])
  (plot3d (surface3d (λ (x y) (+ (sin x) (cos y))) -2 2 -2 2 #:alpha 1/2)
          #:angle 60 #:altitude 35))

(parameterize ([plot-title  "Saddle"]
               [plot-x-axis?  #f]
               [plot-y-axis?  #f]
               [plot-z-axis?  #f]
               [plot-x-far-axis?  #f]
               [plot-y-far-axis?  #f]
               [plot-z-far-axis?  #f]
               [plot-x-label  #f]
               [plot-y-label  #f]
               [plot-z-label  #f]
               [plot-x-far-label  #f]
               [plot-y-far-label  #f]
               [plot-z-far-label  #f])
  (plot3d (contour-intervals3d (λ (x y) (- (sqr x) (sqr y))) -2 2 -2 2
                               #:label "z")))

(parameterize ([plot-decorations?  #f])
  (values
   (plot (function sin -4 4)
         #:title "Hello")
   (plot3d (contour-intervals3d (λ (x y) (- (sqr x) (sqr y))) -2 2 -2 2))))

(time
 (define ((degrees-ticks-format suffix) x-min x-max ts)
   (map (λ (label) (format "~a\ub0~a" label suffix))
        ((linear-ticks-format) x-min x-max ts)))
 
 (define C-ticks (ticks (linear-ticks-layout) (degrees-ticks-format 'C)))
 
 (define F/C-ticks (ticks-scale
                    (ticks (linear-ticks-layout) (degrees-ticks-format 'F))
                    (linear-scale 9/5 32)))
 
 (define data (list #(0 0) #(15 0.6) #(30 9.5) #(45 10.0) #(60 16.6)
                    #(75 41.6) #(90 42.7) #(105 65.5) #(120 78.9)
                    #(135 78.9) #(150 131.1) #(165 151.1) #(180 176.2)))
 
 (define (temp/time-trend x) (/ (sqr x) 180))

 (define above-data (filter (λ (v) (match-let ([(vector x y)  v])
                                     (y . > . (temp/time-trend x))))
                            data))

 (parameterize ([plot-x-ticks      (time-ticks)]
                [plot-y-ticks      C-ticks]
                [plot-y-far-ticks  F/C-ticks])
   (plot (list (function temp/time-trend 0 180 #:style 'long-dash #:color 3
                         #:label "Trend")
               (lines data #:color 2 #:width 2)
               (points data #:color 2 #:line-width 2 #:fill-color 0 #:sym 'fullcircle
                       #:label "Measurement")
               (map (λ (d) (point-label d #:anchor 'bottom #:point-color 2 #:point-size 7))
                    above-data))
         #:y-min -25 #:x-label "Time" #:y-label "Temp."
         #:title "Temp./Time With Applied Heat (Measurement and Trend)")))

(parameterize ([plot-x-ticks  (fraction-ticks)]
               [plot-y-ticks  (currency-ticks)])
  (plot (list (function sin -4 4)
              (function-label sin 1/3))))

(parameterize ((plot-x-tick-label-angle 45)
               (plot-x-tick-label-anchor 'top-right)
               (plot-y-tick-label-angle 45)
               (plot-y-tick-label-anchor 'bottom-right)
               (plot-x-far-tick-label-angle 45)
               (plot-x-far-tick-label-anchor 'bottom-left)
               (plot-y-far-tick-label-angle 45)
               (plot-y-far-tick-label-anchor 'top-left)
               (plot-x-far-label "x far axis")
               (plot-y-far-label "y far axis"))
  (plot (list (discrete-histogram '(#(asdglkj 5399) #(liegjd 5390) #(pqlcxkgfj 3534)))
              (x-ticks (list (tick 1 #t "asdgwieasdgwefj")) #:far? #t)
              (y-ticks (list (tick 2500 #t "asdgwegawegfgwiej")) #:far? #t))))
