#lang racket

(require rackunit plot plot/utils)

;(plot-new-window? #t)

(plot (function / -249 250))

(time
 (define xs (build-list 10000 (λ _ (random))))
 (plot (density xs 1/2)))

(printf "Plot should be empty:~n")
(time
 (plot empty #:x-min -1 #:x-max 1 #:y-min -1 #:y-max 1))

(printf "Plot should be empty:~n")
(time
 (plot (points empty) #:x-min -1 #:x-max 1 #:y-min -1 #:y-max 1))

(printf "Plot should be empty:~n")
(plot (rectangles (list (list (ivl 0 1) (ivl 0 1))))
      #:x-min 2 #:x-max 3)

(plot (list (function values -4 4) (axes 1 2)))

(time (plot (function values 0 1000)))

(parameterize ([plot-x-transform  log-transform]
               [plot-x-ticks      (log-ticks #:base 4)]
               [plot-y-transform  log-transform]
               [plot-y-ticks      (linear-ticks #:number 10)])
  (plot (function values 1 243)))

(parameterize ([plot-background  "black"]
               [plot-foreground  "white"]
               [plot-background-alpha  1/2]
               [plot-foreground-alpha  1/2])
  (plot (function sin -4 4 #:label "y = sin(x)")))

(parameterize ([plot-x-transform  (hand-drawn-transform 200)]
               [plot-y-transform  (hand-drawn-transform 200)])
  (plot (function sqr -1 1)))

(parameterize ([plot-x-transform  log-transform]
               #;[plot-y-transform  log-transform])
  (time
   (plot (list (function (λ (x) x) 0.1 10 #:samples 2 #:label "y = x")
               (polar-axes))
         #:title "Nonlinear scaling" #:x-label "x axis" #:y-label "y-axis")))

(define (degrees->point θ [r 1])
  (vector (* r (cos (degrees->radians θ)))
          (* r (sin (degrees->radians θ)))))

(plot (list (lines (list (degrees->point 0)
                         (degrees->point 120)
                         (degrees->point 180 0)
                         (degrees->point 240)
                         (degrees->point 0)))
            (polar (λ (θ) 1) #:color 0)))

;(plot-x-transform cbrt-transform)
;(plot-y-transform cbrt-transform)
;(plot-x-transform (hand-drawn-transform 100))
;(plot-y-transform (hand-drawn-transform 100))

(time
 (plot (vector-field (λ (x y) (vector x y))
                     -5 5 -5 5 #:scale 'auto)))

(time
 (plot (vector-field (λ (x y) (vector (- y) x))
                     -2 2 -1 4 #:scale 'normalized)))

(time
 (plot (list (lines (for/list ([i  (in-range 6)])
                      (degrees->point (* 2 72 i))))
             (list (polar (λ (θ) 1) #:color 0)
                   (polar (λ (θ) 0.38) #:color 0)))))

(time
 (plot (list (lines (for/list ([i  (in-range 4)])
                      (degrees->point (* 120 i))))
             (lines (for/list ([i  (in-range 4)])
                      (degrees->point (+ 60 (* 120 i)))))
             (polar (λ (θ) 1) #:color 0)
             (polar (λ (θ) 0.5) #:color 0))))

(time
 (define xs (build-list 100 (λ _ (random))))
 (define ys (build-list 100 (λ _ (random))))
 (plot (list (points (map vector ys xs) #:sym 'full6star #:color 1)
             (points (map vector xs ys) #:sym "B" #:color 3))))

(printf "This plot should be blank:~n")
(time
 (plot (vector-field (λ (x y) (vector +nan.0 +nan.0)))
       #:x-min -2 #:x-max 2 #:y-min -2 #:y-max 2))

#;; high-contrast white-on-black:
(begin
  (plot-foreground "white")
  (plot-background "black")
  (point-color "white")
  (histogram-line-color "white")
  (histogram-fill-color "black")
  (contour-color "white"))

;; an exact rational function and a floating-point function
;; the plot of the exact rational function's graph should be smooth
(time
 (parameterize ([plot-x-tick-label-angle 15])
   (plot (list (function (λ (x) x) #:label "Exact")
               (function (λ (x) (exact->inexact x)) #:color 2 #:label "Inexact"))
         #:x-min #e100000000000000.0 #:x-max #e100000000000000.1
         #:width 450)))

(time
 (plot (function cos 0 0.0000001)
       #:width 500))

(time
 (plot (list (function sin #:label "Sine"
                       #:color "Blue" #:style 'long-dash #:width 3)
             (function sqr #:label "Square"))
       #:x-min -3 #:x-max 3
       #:legend-anchor 'top-left))

(time
 (plot (list (axes)
             (function sqr -2.1 2.1 #:label "x^2")
             (error-bars (map (λ (x) (vector x (sqr x) (/ 1/2 (+ (abs x) 1))))
                              (sequence->list (in-range -2 2.1 1/8)))
                         #:x-min -1))))

(time
 (plot (list (points '(#(1 1) #(2 2) #(3 3)) #:sym "bob" #:size 10
                     #:x-min 2 #:x-max 3 #:y-min 1 #:y-max 3))
       #:x-min 0 #:x-max 4 #:y-min 0 #:y-max 4))

(time
 (plot (list (x-axis 1) (y-axis 1)
             (function sqr #f 2 #:color 1)
             (inverse sqr #:color 2)
             (function values #:color 0 #:style 1))
       #:x-min -2 #:y-min -1))

(time
 (plot (list (polar-axes #:number 4)
             (polar (λ (θ) (+ 0.5 (cos (* 1/2 θ)))) (* -2 pi) (* 2 pi)))))

; draws both functions with x in [-1,1]
(plot (list (function sqr #f 1)
            (function (λ (x) (* 2 (sqr x))) -1 #f
                      #:color "blue")
            (axes 1 0 #:y-ticks? #f)))

(printf "Plot should be empty:~n")
(plot (list (function sqr #f -1)
            (function sqr 1 #f)))

; draws both functions with x in [-1,2] (meaning nothing is drawn)
(printf "Plot should be empty:~n")
(plot (list (function sqr #f -1)
            (function sqr 2 #f)))

; draws first function with x in [-2,-1]
(plot (list (function sqr #f -1)
            (function sqr 1 #f))
      #:x-min -2)

; draws second function with x in [1,2]
(plot (list (function sqr #f -1)
            (function sqr 1 #f))
      #:x-max 2)

; draws both functions with x in [-2,2]
(plot (list (function sqr #f -1)
            (function sqr 1 #f))
      #:x-min -2 #:x-max 2)

; draws both in full (particularly, without chopping off the top of the parabola), in [-2,2]
(plot (list (function sqr)
            (function sin -2 2)))

(time
 (plot (list (discrete-histogram
              (build-list 10 (λ (n) (vector (string-ref "abcdefghij" n)
                                            (sqr n))))
                        #:label "ord(x)^2")
             (function truncate))))

(time
 (plot (list (x-axis)
             (discrete-histogram '((a -1) (b 2.6) (c 4) (d 3.1)) #:y-min #f
                                 #:color 5 #:line-color 5 #:line-style 'long-dash
                                 #:label "Corrupt")
             (discrete-histogram '(#(a 1) #(b 2.6) #(c 4) #(d 3.1))
                                 #:x-min 5
                                 #:color 1 #:line-color 1 #:line-width 3
                                 #:label "Clean"))
       #:title "Widgetyness of Widgets"
       #:x-label "Widget"
       #:y-label "Widgetyness"
       #:legend-anchor 'bottom-right))

(time
 (plot (stacked-histogram '(#(a (1 1 1)) #(b (1.5 3)) #(c ()) #(d (1/2)))
                          #:labels '("Red" #f "Blue"))))

(time
 (parameterize ([discrete-histogram-gap  0]
                [discrete-histogram-skip  3]
                [rectangle-line-width 2])
   (plot (list (discrete-histogram '(#(a 1) #(b 2.5) #(c 2)) #:label "Blue")
               (discrete-histogram '(#(a 2) #(b 4) #(c 1)) #:x-min 2/3 #:color 1 #:line-color 1
                                   #:label "Red")
               (discrete-histogram '(#(a 3) #(b 3) #(c 2.5)) #:x-min 4/3 #:color 2 #:line-color 2
                                   #:label "Green")))))

(time
 (parameterize ([discrete-histogram-gap  0]
                [discrete-histogram-skip  2]
                [stacked-histogram-line-widths '(3)])
   (plot (list (stacked-histogram '(#(a (0.2 1)) #(b (2.5 1.2)) #(c (2 0))))
               (stacked-histogram '((a (2 1)) (b (1.1 0.9)) (c (1 1.1))) #:x-min 7/8
                                  #:colors '(3 4)
                                  #:line-colors '(3 4))))))

(time
 (parameterize ([plot-x-ticks  (currency-ticks)])
   (plot (discrete-histogram (list (vector '(a . a) 1) (vector '(a . b) 2)
                                   (vector '(b . b) 3) (vector '(b . a) 4))
                             #:invert? #t #:add-ticks? #f))))

(time
 (parameterize ([plot-x-ticks  (currency-ticks)])
   (plot (stacked-histogram (list (vector '(a . a) '(1 2 1)) (vector '(a . b) '(2 1 3))
                                  (vector '(b . b) '()) (vector '(b . a) '(4 4 2)))
                            #:invert? #t #:add-ticks? #f))))

(time
 (plot (rectangles
        (map vector
             (bounds->intervals (map log (linear-seq 10 20 10)))
             (build-list 9 (λ (n) (ivl (sqr n) (- (sqr n)))))))))

(time
 (define (f x) (* (/ 1 (sqrt (* 2 pi)))
                  (exp (* -1/2 (sqr x)))))
 (plot (list (area-histogram f (linear-seq -4 4 10))
             (function f -4 4))))

(time
 (plot (list (area-histogram sqr (map (λ (x) (* (sqrt x) (sqrt 8))) (linear-seq 0 8 10)))
             (function sqr 0 8))))

(time
 (define xs (build-list 10000 (λ _ (random))))
 (define ys (build-list 10000 (λ _ (random))))
 (plot (list
        (points (map vector xs ys)
                #:x-min -1 #:x-max 1 #:y-min 0.5 #:y-max 1
                #:sym 'fullcircle #:size 6.5 #:alpha 1/8
                #:label "Dots"))
       #:y-max 1.5))

(time
 (plot (vector-field (λ (x y) (vector x y)) -0.5 1.85 -5 0.5
                     #:color "blue" #:line-width 2/3
                     #:scale 'auto #:label "(vector x y)")
       #:x-min -1 #:x-max 5))

(time
 (plot (list (function (λ (x) (* 220 (cos (* 4 x)))) -2 2)
             (function (λ (x) (* 200 (sin (* 3 x)))) 0 #f
                       #:y-min -150 #:y-max 150
                       #:color "blue"))
       #:x-min -1/2 #:x-max 3))

(time
 (plot (lines (reverse
               (for/fold ([lst (list (vector 0 0))]) ([i  (in-range 1 400)])
                 (match-define (vector x y) (first lst))
                 (cons (vector i (+ y (* 1/100 (- (random) 1/2)))) lst)))
              #:alpha 0.5 #:label "Random walk")))

(time
 (plot (function (λ (x) (/ 1.0 (exact->inexact x))) -2 2)))

(time
 (plot (parametric (λ (t) (vector (sin t) (cos t))) (- pi) pi
                   #:x-min -0.5 #:y-max 0.5)
       #:x-min -1))

(time
 (plot (list (function sin -1/2 1)
             (parametric (λ (t) (vector (cos t) (sin t))) -2 1
                         #:color "blue" #:style 'short-dash))))

(define (norm mx my x y)
  (exp (* -1/2 (+ (sqr (- x mx)) (sqr (- y my))))))

(define (f1 x y)
  (+ (norm -1.5 -1.5 x y)
     (* 2 (norm 1 1 x y))
     (* 1.5 (norm 2 -2 x y))))

(define (f2 x y)
  (- (sqr x) (sqr y)))

(time (plot (list (contours f1 0 5 #:label "Cyan/Redness")
                  (contours f2 -5 0 #:colors '("blue") #:label "Blueness"
                            #:widths '(2) #:styles '(dot)))
            #:x-min -5 #:x-max 5 #:y-min -5 #:y-max 5))

(printf "These two plots should be empty:~n")
(plot (contours f2 -5 0) #:x-min 1 #:x-max 3 #:y-min 1 #:y-max 3)
(plot (contour-intervals f2 -5 0) #:x-min 1 #:x-max 3 #:y-min 1 #:y-max 3)

(time (plot (contour-intervals f1 -5 5 -5 5 #:label "z")))

(time (plot (contour-intervals
             (λ (x y)
               (define z (- x y))
               (cond [(< z -1) -1]
                     [(> z 1)   1]
                     [else      z]))
             -2 2 -2 2)))

(time (plot (list (tick-grid)
                  (contour-intervals f1 -5 2 -5 2
                                     #:levels 5
                                     #:contour-styles '(transparent)
                                     #:label "")
                  (contours f1 -2 5 -2 5 #:levels 5 #:label ""))
            #:x-min -5 #:x-max 5 #:y-min -5 #:y-max 5
            #:legend-anchor 'center))

(time (plot (list (tick-grid)
                  (contour-intervals f1 -5 2 -5 2
                                     #:levels '(0.25 0.5 0.75 1.0 1.25 1.5 1.75)
                                     #:colors (compose default-contour-colors (curry map ivl-center))
                                     #:styles '(0 1 2 3 4 5 6)
                                     #:contour-styles '(transparent)
                                     #:label "z")
                  (contours f1 -2 5 -2 5 #:levels '(0.25 0.5 0.75 1.0 1.25 1.5 1.75)))
            #:x-min -5 #:x-max 5 #:y-min -5 #:y-max 5
            #:legend-anchor 'top-left))

; tests contour 7-sided and 8-sided saddle facets
; contour shading should line up with contour lines, no matter how weird
(parameterize ([contour-samples  10])
  (define (f x y) (sqr (sin (- x y))))
  (time (plot (contour-intervals f)
              #:x-min -5 #:x-max 5 #:y-min -5 #:y-max 5)))

(time
 (define (f2 x) (sin (* x pi)))
 (plot (list (x-tick-lines)
             (function-interval atan (λ (x) (- (atan x)))
                                #:color 6 #:line1-color 6 #:line2-color 6)
             (function-interval sqr f2 -1 1 #:alpha 0.5)
             (function-label f2 -1/4 #:anchor 'top-left))))

(time
 (define amps (linear-seq 1/4 1 8))
 (define colors (color-seq* '("darkred" "white" "darkblue") 7))
 (plot (flatten
        (list
         (x-tick-lines)
         (for/list ([a1     (in-list amps)]
                    [a2     (in-list (rest amps))]
                    [color  (in-list colors)])
           (inverse-interval (λ (y) (* a1 (sin y)))
                             (λ (y) (* a2 (sin y)))
                             (- pi) pi #:color color #:alpha 1
                             #:label (format "f(~a,~a)" a1 a2)))
         (y-tick-lines)
         (inverse-label (λ (y) (* 1/4 (sin y))) (* -1/2 pi)
                        "x = 1/4 sin(y)")))
       #:legend-anchor 'top-left))

(time
 (define a #(0 0))
 (define b #(1 1/2))
 (define c #(0 1))
 (define d #(1 3/2))
 (plot (list
        (tick-grid)
        (lines-interval (list a b) (list c d)
                        #:color 4 #:line1-color 4 #:line2-color 4
                        #:label "Parallelogram")
        (point-label #(1/2 5/4) #:anchor 'bottom-right #:alpha 0.5))
       #:legend-anchor 'bottom-left))

(time
 (define (fa t) (vector (* 2 (cos (* 4/5 t))) (* 2 (sin (* 4/5 t)))))
 (define (fb t) (vector (cos t) (sin t)))
 (define (fc t) (vector (* 1/2 (cos (* 4/5 t))) (* 1/2 (sin (* 4/5 t)))))
 (define t1 (- pi))
 (define t2 pi)
 (plot (list
        (x-tick-lines)
        (lines (list (fa t1) (fb t1) (vector +nan.0 +nan.0) (fb t1) (fc t1))
               #:color "black" #:style 'dot)
        (lines (list (fa t2) (fb t2) (vector +nan.0 +nan.0) (fb t2) (fc t2))
               #:color "black" #:style 'dot)
        (parametric fa t1 t2 #:color 5 #:label "fa")
        (parametric-interval fa fb t1 t2 #:color 5 #:label "(fa,fb)"
                             #:line1-style 'transparent
                             #:line2-style 'transparent)
        (parametric fb t1 t2 #:color 1 #:label "fb")
        (parametric-interval fb fc t1 t2 #:color 2 #:label "(fb,fc)"
                             #:line1-style 'transparent
                             #:line2-style 'transparent)
        (parametric fc t1 t2 #:color 2 #:label "fc")
        (x-axis #:ticks? #f)
        (parametric-label fa t1 "fa(-π)"
                          #:size 14 #:anchor 'left #:point-size 5)
        (parametric-label fa t2 "fa(π)"
                          #:size 14 #:anchor 'left #:point-size 5))
       #:legend-anchor 'top-right))

(time
 (define (f1 θ) (+ 1/2 (* 1/6 (cos (* 5 θ)))))
 (define (f2 θ) (+ 1 (* 1/4 (cos (* 10 θ)))))
 (plot (list (polar-axes #:number 10)
             (polar-interval f1 f2
                             #:color 3 #:label "[f1,f2]"
                             #:line1-color 1 #:line1-width 2 #:line1-style 'dot
                             #:line2-color 2 #:line2-width 2)
             (polar-label f1 0 #:anchor 'top-left)
             (polar-label f2 (degrees->radians 36) #:anchor 'bottom-right)
             (point-label #(1/2 1/2)))))

(time
 (define (f1 θ) (/ θ pi 2))
 (define (f2 θ) (+ (/ θ pi 2) 1))
 (plot (list (tick-grid)
             (polar-interval f1 f2 0 (* 5 pi)
                             #:color 4 #:alpha 3/4
                             #:line1-color 1 #:line2-color 1
                             #:label "[f1,f2]"))
       #:legend-anchor 'center))

(time
 (define ((make-fun y) x)
   (+ y (sqr x)))
 
 (values
  (plot (build-list
         20 (λ (n) (function (make-fun n) #:color n #:style n #:width 2)))
        #:x-min -2 #:x-max 2)
  
  (plot (list
         (tick-grid)
         (function-interval (λ (x) 0) (λ (x) 16) #:color "black" #:alpha 1/20)
         (build-list
          12 (λ (n) (function-interval
                     (make-fun n) (make-fun (add1 n)) -2 0
                     #:color (->pen-color n) #:style n #:alpha 1
                     #:line1-style 'transparent #:line2-style 'transparent)))
         (build-list
          12 (λ (n) (function-interval
                     (make-fun n) (make-fun (add1 n)) 0 2
                     #:color n
                     #:line1-style 'transparent #:line2-style 'transparent)))
         (build-list
          13 (λ (n) (function (make-fun n) -2 0
                              #:color n #:width 2)))
         (build-list
          13 (λ (n) (function (make-fun n) 0 2
                              #:color n #:width 2 #:style n))))
        #:x-min -2 #:x-max 2)))

(time
 (define (f x) (/ (sin x) x))
 (parameterize ([plot-x-transform  (stretch-transform -1 1 10)]
                [plot-x-ticks      (ticks-add (plot-x-ticks) '(-1 1))]
                [plot-y-ticks      (fraction-ticks)])
   (plot (list (y-axis -1 #:ticks? #f) (y-axis 1 #:ticks? #f)
               (function f -1 1 #:width 2 #:color 4)
               (function f -14 -1 #:color 4 #:label "y = sin(x)/x")
               (function f 1 14 #:color 4)
               (point-label (vector 0 1) "y → 1 as x → 0" #:anchor 'bottom-right))
         #:y-max 1.2)))
