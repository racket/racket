#lang racket

(require plot
         (except-in plot/utils sum flsum sample)
         math
         profile
         contract-profile)

#;; Test under crazy transformation
(begin
  (plot-x-transform cbrt-transform)
  (plot-y-transform cbrt-transform)
  (plot-z-transform cbrt-transform))

(plot3d (list (contour-intervals3d * -1 1 -1 1 #:samples 2)
              (isosurface3d (λ (x y z) z)
                              0 -1 1 -1 1 -1 1
                              #:samples 2 #:color 3)))

(plot3d (list (parametric3d (λ (t)
                              (list (* 0.5 (cos (* 2 pi (- t))))
                                    (* 0.5 (sin (* 2 pi (- t))))
                                    t))
                            -1 1
                            #:samples 53
                            #:color "green"
                            #:width 15
                            #:alpha 0.6)
              (parametric3d (λ (t)
                              (list (* 0.75 (cos (* 2 pi t)))
                                    (* 0.75 (sin (* 2 pi t)))
                                    t))
                            -1 1
                            #:samples 53
                            #:color "red"
                            #:width 15
                            #:alpha 0.6)))

(plot3d (list (isosurface3d (λ (x y z) (+ x y z)) 0 -1 1 -1 1 -1 1
                            #:samples 2
                            #:line-width 2)
              (isosurface3d (λ (x y z) x) 0 #:samples 2
                            #:color "red"
                            #:line-width 2)
              (isosurface3d (λ (x y z) (+ x (- y) z)) 0
                            #:samples 2
                            #:line-width 2)
              (parametric3d (λ (t)
                              (list (* 0.75 (cos (* 5 pi t)))
                                    (* 0.75 (sin (* 5 pi t)))
                                    t))
                            -1 1 #:width 2 #:color 2)))

(let* ([xs  (sample (uniform-dist -1 1) 10000)]
       [ys  (sample (uniform-dist -1 1) 10000)]
       [zs  (sample (uniform-dist -1 1) 10000)]
       [xyzs  (map list xs ys zs)])
  (time
   (plot3d (list (isosurface3d (λ (x y z) (+ x y z)) 0 -1 1 -1 1 -1 1
                               #:samples 2
                               #:line-width 2)
                 (isosurface3d (λ (x y z) x) 0 #:samples 2
                               #:color "red"
                               #:line-width 2)
                 (isosurface3d (λ (x y z) (+ x (- y) z)) 0
                               #:samples 2
                               #:line-width 2)
                 (points3d xyzs #:sym 'dot)))))

(time
 (for/last ([_  (in-range 1)])
   (plot3d
    (list (contour-intervals3d
           (λ (x y)
             (* x (+ 0.1 y)))
           -1 1 -1 1
           #:samples 41
           #:alphas '(0.85)
           ;#:alpha 0.75
           ;#:line-width 2
           ;#:line-widths '(2)
           ;#:line-styles '(transparent)
           #:contour-widths '(2)
           ;#:color 1
           ;#:label ""
           )
          
          (surface3d
           (λ (x y)
             (* (- (* (flnormal-pdf 0.0 0.2 (fl x) #f)
                      (flnormal-pdf 0.0 0.2 (fl y) #f))
                   0.7)
                0.4))
           -1 1 -1 1
           #:samples 40
           ;#:alphas '(0.75)
           #:alpha 0.95
           #:color "plum"
           #:line-color 6
           ;#:line-style 'transparent
           ;#:line-width 2
           ))
    #:x-min -1 #:x-max 1
    #:y-min -1 #:y-max 1
    ;#:out-file "test.pdf"
    )))

(define (f2 x y)
  (let ([x  (fl x)] [y  (fl y)])
    (- (sqrt (+ (abs y) (abs x))))))

(plot3d (list (surface3d * -1 1 -1 1 #:samples 6 #:alpha 0.75 #:color 1)
              (surface3d (λ (x y) (+ 0.1 (* x y))) -1 1 -1 1 #:samples 6 #:alpha 0.75 #:color 2)
              (surface3d (λ (x y) (+ 0.2 (* x y))) -1 1 -1 1 #:samples 6 #:alpha 0.75 #:color 3
                         #:line-width 2)
              ))

(plot3d (list
         (isosurface3d (λ (x y z) (+ (- 1 x) (- 1 y) (- z 1.5))) 0
                       #:alpha 0.85 #:color 2 #:line-color 2
                       #:samples 4)
         (discrete-histogram3d (list (vector 'a 'a 1)
                                     (vector 'a 'b 2)
                                     (vector 'b 'b 3))
                               #:color 4 #:line-color 4 #:line-width 2
                               #:alpha 0.65)))

(plot3d (list
         ;(isosurface3d (λ (x y z) (+ x y z)) 0 #:samples 2)
         (surface3d * -1 1 -1 1)
         (vector-field3d (λ (x y z) (vector x z y))
                         #:line-width 2)))

(define (f x y) (* (sin x) (cos y)))
(time
 (for/last ([_  (in-range 1)])
   (plot3d (list (contour-intervals3d f -3 3 -3 3)
                 (point-label3d (list -1 1 (f -1 1)))))))

(parameterize ([plot-x-transform  (hand-drawn-transform 50)]
               [plot-y-transform  (hand-drawn-transform 50)]
               [plot-z-transform  (hand-drawn-transform 50)]
               )
  (plot3d (contour-intervals3d (λ (x y) (- (sqr x) (sqr y)))
                               -1 1 -1 1 #:samples 9
                               #:contour-widths '(2)
                               #:line-widths '(2))))

(define (saddle x y) (- (sqr x) (sqr y)))
(plot3d (list (surface3d saddle -1 1 -1 1)
              (isoline3d saddle 1/4 #:width 2 #:style 'long-dash)))
