#lang typed/racket

;; These tests "pass" when they pass the type checker and raise no exceptions

(require plot/typed)

(define xs (build-list 1000 (λ (_) (random))))
(define ys (build-list 1000 (λ (_) (random))))
(define zs (build-list 1000 (λ (_) (random))))
(define xys (map (λ: ([x : Flonum] [y : Flonum]) (vector x y 3.0)) xs ys))
(define xyzs (map (λ: ([x : Flonum] [y : Flonum] [z : Flonum]) (vector x y z 2.0)) xs ys zs))

(plot (points xys #:x-min 0 #:x-max 1 #:y-min 0 #:y-max 1))

(plot (vector-field (λ: ([x : Real] [y : Real])
                      (list x y 5.0))
                    -1 1 -1 1))

(plot (error-bars '(#(1.0 2.0 1.0)
                    #(2.0 3.0 1.0))
                  #:x-min 0.75 #:x-max 2.25
                  #:y-min 0.5 #:y-max 4.5))

(plot (lines (map (λ: ([x : Real] [y : Real]) (list x y 5.0))
                  (build-list 7 (λ: ([x : Real]) x))
                  (build-list 7 (λ: ([x : Real]) (sqr x))))))

(plot (list (parametric (λ: ([t : Real])
                          (list (sin t) (cos t)))
                        0 (* 2 pi))
            (polar (λ: ([θ : Real]) 1/2) 0 (* 2 pi) #:color 3)))

(plot (list (function (λ: ([x : Real]) (- (exp x) 1)) -1.5 1.5)
            (inverse (λ: ([x : Real]) (- (exp x) 1)) -1.5 1.5 #:color 2)
            (function (λ: ([x : Real]) x) -1.5 (- (exp 1.5) 1) #:color 0 #:style 'dot)))

(plot (density xs 1))

(plot (lines-interval
       (map (λ: ([x : Real] [y : Real]) (list x y 5.0))
            (build-list 7 (λ: ([x : Real]) x))
            (build-list 7 (λ: ([x : Real]) (sqr x))))
       (map (λ: ([x : Real] [y : Real]) (list x y 5.0))
            (build-list 7 (λ: ([x : Real]) x))
            (build-list 7 (λ: ([x : Real]) (* 2 (sqr x)))))))

(plot (parametric-interval
       (λ: ([t : Real]) (list (sin t) (cos t)))
       (λ: ([t : Real]) (list (* 0.5 (sin t)) (* 0.5 (cos t))))
       0 (* 2 pi)))

(plot (polar-interval
       (λ: ([t : Real]) 1)
       (λ: ([t : Real]) 0.5)
       0 (* 2 pi)
       #:color 2
       #:style 'cross-hatch
       #:line1-color 1
       #:line2-color 5))

(plot (list (function-interval sin cos -3 3)
            (inverse-interval cos sin -3 3 #:color 1 #:line1-color 1 #:line2-color 1)))

(plot (isoline (λ: ([x : Real] [y : Real]) (* (sin x) (cos y))) 1/2 -6 6 -6 6))

(plot (contours (λ: ([x : Real] [y : Real]) (* (sin x) (cos y))) -6 6 -6 6
                #:levels '(-2/3 -1/3 0 1/3 2/3)
                #:colors '(0 1 2)
                #:alphas '(0.5 1.0)))

(plot (contour-intervals
       (λ: ([x : Real] [y : Real]) (* (sin x) (cos y))) -6 6 -6 6
       #:levels '(-2/3 -1/3 0 1/3 2/3)
       #:colors '(0 1 2)
       #:alphas '(0.5 1.0)))

(plot (rectangles (list (list (ivl 0 1) (ivl 0 1))))
      #:x-min -1 #:x-max 2 #:y-min -1 #:y-max 2)

(plot (area-histogram (λ: ([x : Real]) (* x x x)) '(-4 -3 -2 -1 0 1 2 3 4)
                      #:y-min -44))

(plot (discrete-histogram
       (list '(a 4)
             '(b 10)
             (list 'c (ivl 1 7)))))

(plot (stacked-histogram
       (list '(a (4 2 1 5))
             '(b #(10 1 1 2)))
       #:colors (λ: ([n : Natural]) (build-list n (λ: ([n : Index]) n)))
       #:styles (λ: ([n : Natural]) (build-list n (λ: ([n : Index]) n)))
       #:line-colors (λ: ([n : Natural]) (build-list n (λ: ([n : Index]) n)))
       #:line-widths (λ: ([n : Natural]) (build-list n (λ: ([n : Index]) n)))
       #:line-styles (λ: ([n : Natural]) (build-list n (λ: ([n : Index]) n)))
       #:labels '("One" "Two" "Three" "Four")))

(plot (list (function sin -4 4) (x-axis 1) (y-axis -1))
      #:y-min -4 #:y-max 4)

(plot (list (function sin -4 4) (axes 0 0))
      #:y-min -4 #:y-max 4)

(plot (list
       (polar-axes)
       (polar-interval
        (λ: ([t : Real]) 1)
        (λ: ([t : Real]) 0.5)
        0 (* 2 pi)
        #:color 2
        #:style 'cross-hatch
        #:line1-color 1
        #:line2-color 5)))

(plot (list (x-tick-lines) (y-tick-lines) (function sin -4 4)))

(plot (list (tick-grid) (function sin -4 4)))

(plot (list (function sin -4 4)
            (point-label #(1/2 1/2) "one-half, one-half")))

(plot (list (parametric (λ: ([t : Real]) (list (sin t) (cos t))) 0 (* 2 pi))
            (parametric-label (λ: ([t : Real]) (list (sin t) (cos t))) 0.4)
            (polar (λ: ([θ : Real]) 1/2) 0 (* 2 pi) #:color 3)
            (polar-label (λ: ([θ : Real]) 1/2) 0.4 "0.4")))

(plot (list (function-interval sin cos -3 3)
            (function-label sin -2)
            (inverse-interval cos sin -3 3 #:color 1 #:line1-color 1 #:line2-color 1)
            (inverse-label cos -2)))

(plot3d (surface3d (λ: ([x : Real] [y : Real]) (* (sin x) (cos y))) -6 6 -6 6))

(plot3d (for/list: : (Listof renderer3d) ([z  '(-2/3 -1/3 0 1/3 2/3)])
          (isoline3d (λ: ([x : Real] [y : Real]) (* (sin x) (cos y))) z -6 6 -6 6
                     #:z-min -1 #:z-max 1)))

(plot3d (contours3d (λ: ([x : Real] [y : Real]) (* (sin x) (cos y))) -6 6 -6 6))

(plot3d (contour-intervals3d (λ: ([x : Real] [y : Real]) (* (sin x) (cos y))) -6 6 -6 6))

(plot3d (lines3d (map (λ: ([x : Real] [y : Real] [z : Real]) (list x y z 2.2))
                      (build-list 7 (λ: ([x : Real]) x))
                      (build-list 7 (λ: ([x : Real]) (sqr x)))
                      (build-list 7 (λ: ([x : Real]) (* x x x))))))

(plot3d (parametric3d (λ: ([t : Real])
                        (list (sin t) (cos t) t))
                      0 (* 4 pi)))

(plot3d (points3d xyzs #:x-min 0 #:x-max 1 #:y-min 0 #:y-max 1 #:z-min 0 #:z-max 1))

(plot3d (vector-field3d (λ: ([x : Real] [y : Real] [z : Real])
                          (list x y z 5.0))
                        -1 1 -1 1 -1 1))

(plot3d (isosurface3d (λ: ([x : Real] [y : Real] [z : Real])
                        (* (sin x) (cos y) (sin z)))
                      1/2 -3 3 -3 3 -3 3))

(plot3d (isosurfaces3d (λ: ([x : Real] [y : Real] [z : Real])
                         (* (sin x) (cos y) (sin z)))
                       -3 3 -3 3 -3 3))

(plot3d (polar3d (λ: ([θ : Real] [ρ : Real]) 1)))

(plot3d (rectangles3d (list (list (ivl -1 1) (ivl -1 1) (ivl -1 1)))
                      #:x-min -2 #:x-max 2 #:y-min -2 #:y-max 2 #:z-min -2 #:z-max 2))

(plot3d (discrete-histogram3d
         (list (list 'a 'a 4)
               (list 'a 'b 5)
               (list 'b 'b (ivl 1 3)))))

(plot3d (stacked-histogram3d
         (list (list 'a 'a '(4 1 2))
               (list 'a 'b '(5 2 1))
               (list 'b 'b '(2 3 2)))))
