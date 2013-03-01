(module balloon racket/base
  (require "mrpict.rkt"
           "utils.rkt"
           racket/draw
           mzlib/class
           mzlib/math)

  (provide wrap-balloon pip-wrap-balloon
           place-balloon
           pin-balloon
           (rename-out [mk-balloon balloon])
           make-balloon
           balloon?
           balloon-pict
           balloon-point-x
           balloon-point-y
           balloon-color
           balloon-enable-3d)

  (define-struct balloon (pict point-x point-y))
    
  (define no-pen (find-pen "white" 1 'transparent))
  (define no-brush (find-brush "white" 'transparent))
  (define black-pen (find-pen "black"))

  (define balloon-enable-3d (make-parameter #t (lambda (x) (and x #t))))

  (define (series dc steps start-c end-c f pen? brush?)
    (if (balloon-enable-3d)
        (color-series dc steps #e0.5 start-c end-c f pen? brush?)
        (color-series dc 0 0 start-c end-c f pen? brush?)))
    
  (define (mk-balloon w h corner-radius spike-pos dx dy
                      [color balloon-color])
    (let ([dw (if (< corner-radius 1)
                  (* corner-radius w)
                  corner-radius)]
          [dh (if (< corner-radius 1)
                  (* corner-radius h)
                  corner-radius)]
          [dxbig (lambda (v) (if (> (abs dx) (abs dy))
                                 v
                                 0))]
          [dybig (lambda (v) (if (<= (abs dx) (abs dy))
                                 v
                                 0))])
      (let-values ([(bx0 by0 bx1 by1 x0 y0 x1 y1 xc yc mx0 mx1 my0 my1 mfx mfy)
                    (case spike-pos
                      [(w) (values -1 -0.5 -1 0.5
                                   1 (/ (- h dh) 2)
                                   1 (/ (+ h dh) 2)
                                   1 (/ h 2)
                                   0.5 1 0.5 -1
                                   1 0)]
                      [(nw) (values 0 0 0 0
                                    0 dh
                                    dw 0
                                    0 0
                                    1 -0.5 -1 0.5
                                    (dxbig 1) (dybig 1))]
                      [(e) (values 1 -0.5 1 0.5
                                   (sub1 w) (/ (- h dh) 2)
                                   (sub1 w) (/ (+ h dh) 2)
                                   (sub1 w) (/ h 2)
                                   -1 -1 1 -1
                                   -1 0)]
                      [(ne) (values 0 0 0 0 
                                    (- w dw) 0
                                    w dh
                                    w 0
                                    0.5 -1 0.5 -1
                                    (dxbig -1) (dybig 1))]
                      [(s) (values -0.5 1 0.5 1
                                   (/ (- w dw) 2) (sub1 h)
                                   (/ (+ w dw) 2) (sub1 h)
                                   (/ w 2) (sub1 h)
                                   1 -1 -1 -1
                                   0 -1)]
                      [(n) (values -0.5 -1 0.5 -1
                                   (/ (- w dw) 2) 1
                                   (/ (+ w dw) 2) 1
                                   (/ w 2) 1
                                   1 -1 1 1
                                   0 1)]
                      [(sw) (values 0 0 0 0
                                    0 (- (sub1 h) dh)
                                    dw (sub1 h)
                                    0 (sub1 h)
                                    0.5 -1 0.5 -1
                                    (dxbig 1) (dybig -1))]
                      [(se) (values 0 1 0 1
                                    (- w dw) (sub1 h)
                                    w (- (sub1 h) dh)
                                    w (sub1 h)
                                    0.5 -1 -1 0.5
                                    (dxbig -1) (dybig -1))])])
        (let ([xf (+ xc dx)]
              [yf (+ yc dy)]
              [dark-color (scale-color #e0.6 color)])
          (make-balloon
           (dc (lambda (dc x y)
                 (let ([b (send dc get-brush)]
                       [p (send dc get-pen)]
                       [draw-once
                        (lambda (i rr?)
                          (when rr?
			    (send dc draw-rounded-rectangle 
                                  (+ x (/ i 2)) (+ y (/ i 2))
                                  (- w i) (- h i)
                                  (if (and (< (* 2 corner-radius) (- w i))
					   (< (* 2 corner-radius) (- h i)))
				      corner-radius
				      (/ (min (- w i) (- h i)) 2)))
                            (let ([p (send dc get-pen)])
                              (send dc set-pen no-pen)
                              (send dc draw-polygon (list (make-object point% (+ x0 (* i mx0)) (+ y0 (* i my0)))
                                                          (make-object point% (+ xf (* i mfx)) (+ yf (* i mfy)))
                                                          (make-object point% (+ x1 (* i mx1)) (+ y1 (* i my1))))
                                    x y)
                              (send dc set-pen p)))
                          (send dc draw-line (+ x x0 bx0 (* i mx0)) (+ y y0 by0 (* i my0)) 
                                (+ x xf (* i mfx)) (+ y yf (* i mfy)))
                          (send dc draw-line (+ x x1 bx1 (* i mx1)) (+ y y1 by1 (* i my1))
                                (+ x xf (* i mfx)) (+ y yf (* i mfy))))])
                   (series dc 5
                           dark-color
                           (if (string? color) (make-object color% color) color)
                           (lambda (i) (draw-once i #t))
                           #t #t)
                   (when (balloon-enable-3d)
                     (send dc set-brush no-brush)
                     (send dc set-pen (find-pen dark-color 0.5))
                     (draw-once 0 #f))
                   
                   (send dc set-pen p)
                   (send dc set-brush b)))
               w h 0 0)
           xf yf)))))

  (define balloon-color (make-object color% 255 255 170))
  
  (define corner-size 32)

  (define wrap-balloon
    (lambda (p corner dx dy [color balloon-color] [c-rad corner-size] 
               #:factor [factor 1])
      (let ([b (mk-balloon (+ (pict-width p) (* 2 c-rad))
			   (+ (pict-height p) c-rad)
			   c-rad
			   corner dx dy
			   color)])
	(make-balloon
         (scale (cc-superimpose
                 (balloon-pict b)
                 p)
                factor)
	 (* factor (balloon-point-x b))
	 (* factor (balloon-point-y b))))))

  (define pip-wrap-balloon
    (lambda (p corner dx dy [color balloon-color] [c-rad corner-size]
               #:factor [factor 1])
      (pin-balloon (wrap-balloon p corner dx dy color c-rad #:factor factor) (blank 0) 0 0)))
  
  (define (do-place-balloon flip-proc? balloon p to find-to)
    (let-values ([(x y) (if (and (number? to) 
				 (number? find-to)) 
			    (values to (- (pict-height p)
					  find-to))
			    (if flip-proc?
				(let-values ([(x y) (find-to p to)])
				  (values x (- (pict-height p) y)))
				(find-to p to)))])
      (cons-picture
       p
       `((place ,(- x (balloon-point-x balloon))
                ,(- y  ; up-side down!
                    (- (pict-height (balloon-pict balloon))
                       (balloon-point-y balloon)))
                ,(balloon-pict balloon))))))

  (define (place-balloon balloon p to find-to)
    (do-place-balloon #f balloon p to find-to))

  (define (pin-balloon balloon p to find-to)
    (do-place-balloon #t balloon p to find-to)))
