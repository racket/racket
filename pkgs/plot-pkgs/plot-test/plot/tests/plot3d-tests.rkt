#lang racket

(require plot plot/utils unstable/flonum)

;(plot-new-window? #t)

(printf "The following two plots should be empty:~n")
(time
 (plot3d empty #:x-min -1 #:x-max 1 #:y-min -1 #:y-max 1 #:z-min -1 #:z-max 1))

(time
 (plot3d (points3d empty) #:x-min -1 #:x-max 1 #:y-min -1 #:y-max 1 #:z-min -1 #:z-max 1))

(printf "This plot should be empty:~n")
(plot3d (rectangles3d (list (list (ivl 0 1) (ivl 0 1) (ivl 0 1))))
      #:x-min 2 #:x-max 3)

(parameterize ([plot-background  "black"]
               [plot-foreground  "white"]
               [plot-background-alpha  1/2]
               [plot-foreground-alpha  1/2])
  (plot3d (surface3d (λ (x y) (* (sin x) (sin y))) -2 2 -2 2 #:label "z = trig(x,y)")))

(time
 (plot3d (points3d '(#(0.1 0.6 0.3)))
         #:x-min 0 #:x-max 1 #:y-min 0 #:y-max 1 #:z-min 0 #:z-max 1))

(time
 (plot3d (vector-field3d (λ (x y z) (vector x z y)) -2 2 -2 2 -2 2
                               #:line-width 3)))

(time
 (define x-ivls (bounds->intervals (linear-seq 2 8 10)))
 (define y-ivls (bounds->intervals (linear-seq -5 5 10)))
 (define x-mids (linear-seq 2 8 9 #:start? #f #:end? #f))
 (define y-mids (linear-seq -5 5 9 #:start? #f #:end? #f))
 (plot3d (rectangles3d (append*
                        (for/list ([y-ivl  (in-list y-ivls)] [y  (in-list y-mids)])
                          (for/list ([x-ivl  (in-list x-ivls)] [x  (in-list x-mids)])
                            (vector x-ivl y-ivl (ivl 0 (exp (* -1/2 (+ (sqr (- x 5)) (sqr y)))))))))
                       #:alpha 3/4
                       #:label "Approximate 2D Normal")))

(time
 (plot3d (discrete-histogram3d (list (vector 'a 'a 1)
                                     (vector 'a 'b 2)
                                     (vector 'b 'b 3))
                               #:label "Missing (b,a)"
                               #:color 4 #:line-color 4)))

(time
 (define c1s #(a b c d e))
 (define c2s #(1 2 3 4 5))
 (define cat-vals (build-list 15 (λ (n) (vector (vector-ref c1s (random 5))
                                                (vector-ref c2s (random 5))
                                                n))))
 (plot3d (discrete-histogram3d cat-vals)))

(time
 (plot3d (stacked-histogram3d '(#(a a (1 1 1)) #(a b (1.5 3)) #(b b ()) #(b a (1/2)))
                              #:labels '("Red" #f "Blue") #:alphas '(2/3))))

(time
 (plot3d (stacked-histogram3d '(#(a a (1 1 1)) #(a b (1.5 3)) #(b b ()) #(b a (1/2)))
                              #:labels '("Red" #f "Blue") #:alphas '(2/3)
                              #:add-x-ticks? #f #:add-y-ticks? #f)))

(time
 (plot3d (surface3d + 0 10 0 1)
         #:angle 10 #:z-label "z axis"))

(parameterize ([plot-x-transform  log-transform]
               [plot-y-transform  log-transform])
  (time (plot3d (surface3d + .01 3 .01 1))))

;(plot-x-transform cbrt-transform)
;(plot-y-transform cbrt-transform)
;(plot-z-transform cbrt-transform)

(time
 (plot3d (surface3d (λ (x y) (+ (/ 1.0 (exact->inexact x))
                                (/ 1.0 (exact->inexact y))))
                    -2 2 -2 2
                    #:color '(255 128 128)
                    #:line-color '(255 128 128)
                    #:line-width 1.5
                    #:label "Inverse")
         #:title "Here it is!"
         #:x-label "WannaHockaLoogi"
         #:y-label "An Impossibly Long Y Axis Label"
         #:angle 330 #:altitude 0))

(time
 (plot3d (surface3d (λ (x y)
                      (+ (/ (+ (abs x) 0.01))
                         (/ (+ (abs y) 0.01))))
                    -4 4 -4 4 #:color '(128 128 255)
                    #:label "Z sort test polygons")
         #:angle 330 #:altitude 41
         #:z-label #f #:y-label #f #:x-label #f))

(let ()
  (define xs (build-list 200 (λ (n) (/ 1 (- (random) 0.5)))))
  (define ys (build-list 200 (λ (n) (/ 1 (- (random) 0.5)))))
  (define zs (build-list 200 (λ (n) (/ 1 (- (random) 0.5)))))
  (time
   (plot3d (points3d (map vector xs ys zs)
                     #:x-min -20 #:x-max 20
                     #:y-min -20 #:y-max 20
                     #:z-min -20 #:z-max 20
                     #:label "Widget Locations")
           #:angle 15 #:title "Random Points")))

(let ()
  (define xs (build-list 10000 (λ (n) (/ 1 (- (random) 0.5)))))
  (define ys (build-list 10000 (λ (n) (/ 1 (- (random) 0.5)))))
  (define zs (build-list 10000 (λ (n) (/ 1 (- (random) 0.5)))))
  (time
   (plot3d (points3d (map vector xs ys zs)
                     #:x-min -20 #:x-max 20
                     #:y-min -20 #:y-max 20
                     #:z-min -20 #:z-max 20
                     #:color "blue" #:sym 'dot ;#:size 10
                     #:alpha 0.5)
           #:angle 30 #:altitude 30
           #:title "A Bunch of Random Points Concentrated at the Origin"
           #:x-label "x" #:y-label "y" #:z-label "z")))

;; tests line clipping: should look like a sphere with six poles chopped off
(time
 (plot3d (parametric3d (λ (t)
                         (vector (* (cos (* 80 t)) (cos t))
                                 (* (sin (* 80 t)) (cos t))
                                 (sin t)))
                       (- pi) pi
                       #:x-min -0.8 #:x-max 0.8
                       #:y-min -0.8 #:y-max 0.8
                       #:z-min -0.8 #:z-max 0.8
                       #:color "blue" #:width 1/2 #:style 'long-dash
                       #:samples 3000 #:alpha 0.5
                       #:label "Sphere")
         #:altitude 22
         #:x-min -1 #:x-max 1 #:y-min -1 #:y-max 1 #:z-min -1 #:z-max 1))

(time
 (plot3d (surface3d (λ (x y) (+ x y)) -0.81 0.81 -0.81 0.81
                    #:line-color '(0 0 255) #:line-width 1 #:line-style 'dot)
         #:x-min -1 #:x-max 1 #:y-min -1 #:y-max 1 #:z-min -1 #:z-max 1))

(time
  (define xs (build-list 200 (λ (n) (* 2 (- (random) 0.5)))))
  (define ys (build-list 200 (λ (n) (* 2 (- (random) 0.5)))))
  (define zs (build-list 200 (λ (n) (* 2 (- (random) 0.5)))))
  (plot3d (list (surface3d (λ (x y) (+ x y)) -0.81 0.81 -0.81 0.81
                           #:line-color '(0 0 255) #:line-width 1
                           #:line-style 'dot)
                (points3d (map vector xs ys zs)))
          #:x-min -1 #:x-max 1 #:y-min -1 #:y-max 1 #:z-min -1 #:z-max 1))

(define (norm mx my x y)
  (exp (* -1/2 (+ (sqr (- x mx)) (sqr (- y my))))))

(define (f1 x y)
  (- (sqr x) (sqr y)))

(define (f2 x y)
  (- (sqrt (+ (abs y) (abs x)))))

(define (f3 x y)
  (define d (* 2 pi (+ (abs x) (abs y))))
  (+ (* 1/8 (cos d)) (- (sqr x) (sqr y))))

(define (f4 x y)
  (imag-part (log (make-rectangular (exact->inexact x)
                                    (exact->inexact y)))))

(define (f5 x y)
  (+ (* 1.1 (norm -1.5 -1.5 x y))
     (* 2 (norm 1 1 x y))
     (* 1.3 (norm 2 -2 x y))))

(define (f6 x y)
  (define d (sqrt (+ (sqr x) (sqr y))))
  (if (d . < . 1)
      (sqrt (- 1 (sqr d)))
      0))

(parameterize ([plot3d-diffuse-light? #f]
               [plot3d-specular-light? #f])
  (time (plot3d (surface3d f5 -5 5 -5 5 #:style 'transparent))))

(time (plot3d (contours3d f5 -4 4 -4 4 #:colors '(0)
                          #:label "z")))

(time (plot3d (contour-intervals3d f5 -4 4 -4 4 #:label "z")))

(time (plot3d (contour-intervals3d
               (λ (x y)
                 (define z (- x y))
                 (cond [(< z -1) -1]
                       [(> z 1)   1]
                       [else      z]))
               -2 2 -2 2)))

(time
 (plot3d (contour-intervals3d (λ (x y) (+ x y)))
         #:x-min #e100000000000000.0 #:x-max #e100000000000000.1
         #:y-min #e100000000000000.0 #:y-max #e100000000000000.1
         #:width 500))

(time
 (plot3d (list (surface3d f5 0 4 -4 4 #:color '(128 255 160) #:alpha 0.5
                          #:label "x pos.")
               (contour-intervals3d f5 -4 0 -4 4
                                    #:colors '(0 1 5)
                                    #:line-colors '(0 4 2)
                                    #:line-widths '(1.5) #:line-styles '(dot)
                                    #:contour-colors '(0)
                                    #:contour-widths '(0)
                                    #:contour-styles '(transparent)
                                    #:alphas '(0.75)
                                    #:label "x neg."))
         #:z-min 0.25 #:z-max 1.1
         #:legend-anchor 'top))

(time
 (parameterize ([plot3d-samples 81])
   (plot3d (contour-intervals3d
            f5 -4 4 -4 4 #:label "z"
            #:line-styles '(transparent)))))

(time
 (plot3d (list (contours3d f5 -4 4 -4 4)
               (contour-intervals3d f5 -2.5 2.5 -2.5 2.5
                                    #:z-min 0.25 #:z-max 1.5 #:label "z"))))

(time
 (plot3d (contour-intervals3d f5 -3 3 -3 3
                              #:colors '((255 128 128) (128 128 255)))))

(time
 (plot3d (list (surface3d f4 -4 4 -4 4 #:color '(255 224 0))
               (contours3d f4 -4 4 -4 4))
         #:angle -30))

(time (plot3d (contour-intervals3d f1 -4 4 -4 4)))

(time (parameterize ([plot3d-samples  101])
        (plot3d (contour-intervals3d f2 -2 2 -2 2 #:levels 10
                                     #:line-styles '(transparent)
                                     #:contour-styles '(long-dash)
                                     #:alphas '(1 2/3))
                #:altitude 20)))

(time
 (plot3d (contour-intervals3d (λ (x y) (- (sqr x) (sqr y))) -min.0 +min.0 -min.0 +min.0)))

(time
 (define (f x y) (* (sin x) (cos y)))
 (plot3d (list (contour-intervals3d f -3 3 -3 3)
               (point-label3d (list -1 1 (f -1 1))))))
