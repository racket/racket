#lang racket/base
(provide draw-honu)

(require racket/class
         racket/gui/base
         "palaka.rkt")

(define pi (atan 0 -1))

(define body-path (make-object dc-path%))

(define (find-arc-spot x y w h end)
  (let ([ce (cos end)]
        [se (- (sin end))])
    (values (+ x (* w 1/2) (* w 1/2 ce))
            (+ y (* h 1/2) (* h 1/2 se)))))

(define weighted-arc 
  (lambda (path x y w h start end ccw? [dx1 0.0] [dy1 0.2] [dx2 dx1] [dy2 (- dy1)])
    (let ([sweep (let loop ([s (if ccw? (- end start) (- start end))])
		   (if (< s 0)
		       (loop (+ s (* 2 pi)))
		       s))])
      (if (> sweep pi)
	  (let ([halfway ((if ccw? + -) start (/ sweep 2))])
	    (weighted-arc path x y w h start halfway ccw? dx1 dy1 dx2 dy2)
	    (weighted-arc path x y w h halfway end ccw? dx2 (- dy2) dx1 (- dy1)))
	  (let ([p (new dc-path%)])
	    ;; Set p to be the arc for a unit circle,
	    ;;  centered on the X-axis:
	    (let* ([x0 (cos (/ sweep 2))]
		   [y0 (sin (/ sweep 2))]
		   [x1 (/ (- 4 x0) 3)]
		   [y1 (/ (* (- 1 x0) (- 3 x0)) (* 3 y0))]
		   [x2 x1]
		   [y2 (- y1)]
		   [x3 x0]
		   [y3 (- y0)]
		   [sw (/ w 2)]
		   [sh (/ h 2)])
	      (send p move-to x0 y0)
	      (send p curve-to 
		    (+ x1 dx1) (+ y1 dy1)
		    (+ x2 dx2) (+ y2 dy2)
		    x3 y3)
	      ;; Rotate to match start:
	      (send p rotate (+ (if ccw? start end) (/ sweep 2)))
	      ;; Scale to match width and height:
	      (send p scale (/ w 2) (/ h 2))
	      ;; Translate to match x and y
	      (send p translate (+ x (/ w 2)) (+ y (/ h 2)))
	      (unless ccw?
		(send p reverse)))
	    (send path append p))))))

(define overall-rotation (- (* pi 1/2 3/8)))

(define body-width 100)
(define body-height 110)
(define body-thickness 12)
(define angle-offset (* pi 1/10))

(define big-fin-curve-top-offset 0)
(define big-fin-curve-bottom-offset 4)
(define big-fin-top-angle (* pi 3/12))
(define big-fin-bottom-angle (* pi 2/12))
(define big-fin-size 60)
(define big-fin-right-edge (+ body-width big-fin-size))

(define little-fin-top-angle (- (* pi (/ 3.5 12))))
(define little-fin-bottom-angle (- (* pi (/ 4.5 12))))
(define little-fin-size 20)
(define little-fin-far-y (+ body-height little-fin-size))

(define pointy-tip-offset 8)

(define head-angle-span (* pi 1/6))

(define head-cx (/ body-width 2))
(define head-cy -8)

(define head-width 30)
(define head-height 40)

(define acos-arg
  (* (/ 2 head-width) (- (* (cos (- (/ pi 2) (/ head-angle-span 2)))
                            (/ body-width 2)))))

(define head-theta-start (- (acos acos-arg)))
(define head-theta-end (- pi head-theta-start))

(define-values (head-attach-left-x head-attach-left-y)
  (find-arc-spot 0 0 body-width body-height (+ (/ pi 2) (/ head-angle-span 2))))
(define-values (head-attach-right-x head-attach-right-y)
  (find-arc-spot 0 0 body-width body-height (- (/ pi 2) (/ head-angle-span 2))))

(define right-edge-of-center-line (+ (/ body-width 2) (/ body-thickness 2)))
(define left-edge-of-center-line (- (/ body-width 2) (/ body-thickness 2)))  

(define-values (big-fin-top-x big-fin-top-y)
  (find-arc-spot 0 0 body-width body-height big-fin-top-angle))
(define-values (big-fin-bottom-x big-fin-bottom-y)
  (find-arc-spot 0 0 body-width body-height big-fin-bottom-angle))

(define-values (left-little-fin-top-x left-little-fin-top-y)
  (find-arc-spot 0 0 body-width body-height (- pi little-fin-top-angle)))
(define-values (left-little-fin-bottom-x left-little-fin-bottom-y)
  (find-arc-spot 0 0 body-width body-height (- pi little-fin-bottom-angle)))

(define-values (little-fin-top-x little-fin-top-y)
  (find-arc-spot 0 0 body-width body-height little-fin-top-angle))
(define-values (little-fin-bottom-x little-fin-bottom-y)
  (find-arc-spot 0 0 body-width body-height little-fin-bottom-angle))

(define-values (inner-right-arc-top-x inner-right-arc-top-y)
  (find-arc-spot 
   body-thickness 
   body-thickness 
   (- body-width body-thickness body-thickness)
   (- body-height body-thickness body-thickness)
   (- (* pi 1/2) angle-offset)))

(define-values (inner-right-arc-bottom-x inner-right-arc-bottom-y)
  (find-arc-spot
   body-thickness 
   body-thickness 
   (- body-width body-thickness body-thickness)
   (- body-height body-thickness body-thickness)
   (+ (* pi 3/2) angle-offset)))

(define (add-big-fin-top add)
  (let ([fin-width (- big-fin-right-edge big-fin-top-x)])
    (add big-fin-top-x
         big-fin-top-y
         
         (+ big-fin-top-x (* 1/3 fin-width))
         big-fin-curve-top-offset
         
         (+ big-fin-top-x (* 2/3 fin-width))
         big-fin-curve-top-offset
         
         big-fin-right-edge
	 (+ big-fin-bottom-y 10))))

(define (add-big-fin-bottom add)
  (let ([fin-width (- big-fin-right-edge big-fin-bottom-x)])
    (add 
     (+ big-fin-bottom-x fin-width)
     (+ big-fin-bottom-y 10)
     
     (+ big-fin-bottom-x (* 1/3 fin-width))
     (- (/ (+ big-fin-bottom-y big-fin-top-y) 2)
        big-fin-curve-bottom-offset)
     
     (+ big-fin-bottom-x (* 1/5 fin-width))
     (/ (+ big-fin-bottom-y big-fin-top-y) 2)
     
     big-fin-bottom-x
     big-fin-bottom-y)))

(define (add-little-fin-top add)
  (add
   little-fin-top-x
   little-fin-top-y
   
   (+ little-fin-top-x (* (- little-fin-top-x little-fin-bottom-x) 2/3))
   (+ little-fin-top-y (* (- little-fin-far-y little-fin-top-y) 1/3))
   
   (+ little-fin-top-x (* (- little-fin-top-x little-fin-bottom-x) 1/3))
   (+ little-fin-top-y (* (- little-fin-far-y little-fin-top-y) 2/3))
   
   little-fin-top-x
   little-fin-far-y))

(define (add-little-fin-bottom add)
  (add 
   little-fin-top-x
   little-fin-far-y
   
   (+ little-fin-top-x (* (- little-fin-bottom-x little-fin-top-x) 2/3))
   (+ little-fin-top-y (* (- little-fin-far-y little-fin-top-y) 1/3))
   
   (+ little-fin-top-x (* (- little-fin-bottom-x little-fin-top-x) 2/3))
   (+ little-fin-top-y (* (- little-fin-far-y little-fin-top-y) 1/3))
   
   little-fin-bottom-x 
   little-fin-bottom-y))

(define (add-dot path x y)
  (let ([p (new dc-path%)])
    (send p ellipse (- x 2) (- y 2) 4 4)
    (send path append p)))

(define (same-side-add x1 y1 x2 y2 x3 y3 x4 y4) 
  (send body-path curve-to x2 y2 x3 y3 x4 y4))

(define (same-side-add/dot x1 y1 x2 y2 x3 y3 x4 y4) 
  (send body-path line-to x1 y1)
  (add-dot body-path x1 y1)
  (send body-path line-to x2 y2)
  (add-dot body-path x2 y2)
  (send body-path line-to x3 y3)
  (add-dot body-path x3 y3))

(define (opposite-side-add x1 y1 x2 y2 x3 y3 x4 y4)
  (let ([conv (lambda (x y) (values (+ (- x) body-width) y))])
    (let-values ([(cx1 cy1) (conv x1 y1)]
                 [(cx2 cy2) (conv x2 y2)]
                 [(cx3 cy3) (conv x3 y3)])
      (send body-path curve-to cx3 cy3 cx2 cy2 cx1 cy1))))

(define (opposite-side-add/dot x1 y1 x2 y2 x3 y3 x4 y4)
  (let ([conv (lambda (x y) (values (+ (- x) body-width) y))])
    (let-values ([(cx1 cy1) (conv x1 y1)]
                 [(cx2 cy2) (conv x2 y2)]
                 [(cx3 cy3) (conv x3 y3)])
      (send body-path line-to cx3 cy3)
      (add-dot body-path cx3 cy3)
      (send body-path line-to cx2 cy2)
      (add-dot body-path cx2 cy2)
      (send body-path line-to cx1 cy1)
      (add-dot body-path cx1 cy1))))

(define side-perturb-y 0.0)
(define side-perturb-x -0.1)

(weighted-arc body-path 0 0 body-width body-height big-fin-bottom-angle little-fin-top-angle #f 
	      side-perturb-x side-perturb-y)
(add-little-fin-top same-side-add)
(add-little-fin-bottom same-side-add)

(send body-path line-to
      little-fin-bottom-x
      little-fin-bottom-y)

(send body-path line-to
      (/ body-width 2)
      (+ body-height pointy-tip-offset))

(send body-path line-to
      left-little-fin-bottom-x
      left-little-fin-bottom-y)
 
(add-little-fin-bottom opposite-side-add)
(add-little-fin-top opposite-side-add)

(weighted-arc body-path 0 0 body-width body-height
	      (- pi little-fin-top-angle)
	      (- pi big-fin-bottom-angle)
	      #f
	      side-perturb-x side-perturb-y)

(add-big-fin-bottom opposite-side-add)
(add-big-fin-top opposite-side-add)

(weighted-arc body-path 0 0 body-width body-height (- pi big-fin-top-angle) (+ (/ pi 2) (/ head-angle-span 2)) #f 0 0)


(weighted-arc body-path
	      (- head-cx (/ head-width 2))
	      (- head-cy (/ head-height 2))
	      head-width
	      head-height
	      head-theta-start
	      head-theta-end
	      #f 0 0 0 -0.2)

(weighted-arc body-path 0 0 body-width body-height (- (/ pi 2) (/ head-angle-span 2)) big-fin-top-angle #f 0 0)

(add-big-fin-top same-side-add)
(add-big-fin-bottom same-side-add)
(send body-path close)

(define (make-right-hole-path)
  (let ([right-hole-path (make-object dc-path%)])

    (define arc/end 
      (lambda (x y w h start end [cc? #t] [dx1 0] [dy1 0.2] [dx2 0] [dy2 -0.2])
        (weighted-arc right-hole-path x y w h start end cc? dx1 dy1 dx2 dy2)
        (find-arc-spot x y w h end)))
    
    (define-values (arc1x arc1y)
      (arc/end body-thickness 
	       body-thickness 
	       (- body-width body-thickness body-thickness)
	       (- body-height body-thickness body-thickness)
	       (- (* pi 1/2) angle-offset)
	       (+ (* pi 3/2) angle-offset)
	       #f -0.2 0.2 0 -0.2))
    
    (define little-arc-size (* 2 (- inner-right-arc-bottom-x right-edge-of-center-line)))
    
    (define-values (arc2x arc2y)
      (arc/end
       right-edge-of-center-line
       (- inner-right-arc-bottom-y little-arc-size)
       little-arc-size
       little-arc-size
       (* 3/2 pi)
       pi
       #f
       0 0 0 0))
    
    (let ([arc2y (- body-height arc2y)])
      (send right-hole-path curve-to 
	    
            (+ (/ (+ (* 2 arc1x) (* 1 arc2x)) 3) -4)
            (/ (+ (* 2 arc1y) (* 1 arc2y)) 3)
	    
            (+ (/ (+ (* 1 arc1x) (* 2 arc2x)) 3) -4)
            (/ (+ (* 1 arc1y) (* 2 arc2y)) 3)
	    
            arc2x arc2y))
    
    (weighted-arc right-hole-path
		  right-edge-of-center-line
		  inner-right-arc-top-y
		  little-arc-size
		  little-arc-size
		  pi
		  (* 1/2 pi)
		  #f
                  0 0 0 0)
      
    (send right-hole-path close)
    
    right-hole-path))

(define (make-left-hole-path)
  (let ([left-hole-path (make-right-hole-path)])
    (send left-hole-path scale -1 1)
    (send left-hole-path translate 
          (+ right-edge-of-center-line left-edge-of-center-line) 0)
    left-hole-path))

(define right-hole-path (make-right-hole-path))
(define left-hole-path (make-left-hole-path))

(define (adjust-path path)
  (send path translate (+ (- big-fin-right-edge body-width) 1) (+ (- head-cy) (/ head-height 2) 2))
  (send path rotate overall-rotation))

(adjust-path body-path)
(adjust-path left-hole-path)
(adjust-path right-hole-path)

(define pale-red-color (make-object color% 242 183 183))
(define pale-blue-color (make-object color% 183 202 242))
(define pale-background-color (make-object color% 209 220 248))

(define current-body-path body-path)
(define current-left-hole-path left-hole-path)
(define current-right-hole-path right-hole-path)

(define (draw dc main-pen-color main-color left-pen-color left-color right-pen-color right-color dx dy)
  (send dc set-brush main-color 'solid)
  (send dc set-pen main-pen-color 1 'solid)
  (send dc draw-path current-body-path dx dy)
  (draw-holes dc left-pen-color left-color right-pen-color right-color dx dy))

(define (draw-holes dc left-pen-color left-color right-pen-color right-color dx dy)
  (send dc set-brush left-color 'solid)
  (send dc set-pen left-pen-color 1 'solid)
  (send dc draw-path current-left-hole-path dx dy)
  (send dc set-brush right-color 'solid)
  (send dc set-pen right-pen-color 1 'solid)
  (send dc draw-path current-right-hole-path dx dy))

(define base-width 260)
(define base-height 240)

(define dark-x 80)
(define dark-y -20)
(define current-dark-x dark-x)
(define current-dark-y dark-y)

(define light-x 350)
(define light-y dark-y)
(define current-light-x light-x)
(define current-light-y light-y)

(define (rescale w h)
  (let ([scale (min (/ w base-width) (/ h base-height))])
    (set! current-body-path (new dc-path%))
    (send current-body-path append body-path)
    (send current-body-path scale scale scale)
    (set! current-left-hole-path (new dc-path%))
    (send current-left-hole-path append left-hole-path)
    (send current-left-hole-path scale scale scale)
    (set! current-right-hole-path (new dc-path%))
    (send current-right-hole-path append right-hole-path)
    (send current-right-hole-path scale scale scale)
    (set! current-light-x (* light-x scale))
    (set! current-light-y (* light-y scale))
    (set! current-dark-x (* dark-x scale))
    (set! current-dark-y (* dark-y scale))))

(define my-canvas%
  (class canvas%
    (define/override (on-size w h)
      (rescale w h))
    (super-new)))


(define (vector-map f v)
  (build-vector (vector-length v)
                (λ (i) (f (vector-ref v i)))))

(define color-series
  (vector-map (λ (l) (vector-map (λ (x) (send the-color-database find-color x)) l))
              '#(#("red" "blue")
                 #("red" "blue")
                 #("Magenta" "MediumOrchid")
                 #("MediumOrchid" "Magenta")
                 #("blue" "red"))))

    
    (define black-honu-bitmap 'not-yet-the-bitmap)
    (define black-honu-bdc (make-object bitmap-dc%))
    
    (define (do-draw dc left-body-color right-body-color) 
      (send dc draw-bitmap black-honu-bitmap 0 0)
      (send dc set-smoothing 'aligned)
      (draw-holes dc left-body-color left-body-color right-body-color right-body-color 
                  current-dark-x
                  current-dark-y))
    
    (define (set-size w h)
      ;; update the bitmap if the size has changed
      (unless (and (is-a? black-honu-bitmap bitmap%)
                   (equal? w (send black-honu-bitmap get-width))
                   (equal? h (send black-honu-bitmap get-height)))
        (rescale w h)
        (set! black-honu-bitmap (make-object bitmap% w h))
        (recalc-bitmap)))
    
    (define (recalc-bitmap)
      (send black-honu-bdc set-bitmap black-honu-bitmap)
      (send black-honu-bdc set-smoothing 'aligned)
      (draw-palaka black-honu-bdc (send black-honu-bitmap get-width) (send black-honu-bitmap get-height))
      (draw black-honu-bdc
            "black" "black" "black" "black" "black" "black"
            current-dark-x
            current-dark-y)
      (send black-honu-bdc set-bitmap #f))
    
    (define (set-val val left-body-color right-body-color)
      (cond
        [(and (<= 0 val)
              (< val 1))
         (let* ([scaled-val (* val (- (vector-length color-series) 1))]
                [set (floor scaled-val)]
                [in-set-val (- scaled-val set)]
                [before-colors (vector-ref color-series set)]
                [after-colors (vector-ref color-series (+ set 1))])
           (linear-color-combination (vector-ref before-colors 0)
                                     (vector-ref after-colors 0)
                                     in-set-val
                                     left-body-color)
           (linear-color-combination (vector-ref before-colors 1)
                                     (vector-ref after-colors 1)
                                     in-set-val
                                     right-body-color))]
        [else
         (let ([set (vector-ref color-series (- (vector-length color-series) 1))])
           (send left-body-color copy-from (vector-ref set 0))
           (send right-body-color copy-from (vector-ref set 1)))]))
    
    (define (linear-color-combination xc yc val uc)
      (send uc set
            (linear-combination (send xc red) (send yc red) val)
            (linear-combination (send xc green) (send yc green) val)
            (linear-combination (send xc blue) (send yc blue) val)))
    
    (define (linear-combination x y val)
      (floor (+ x (* val (- y x)))))
    
(define draw-honu
  (let ()
    ;; colors
    (define left-body-color (make-object color% 0 0 0))
    (define right-body-color (make-object color% 0 0 0))
    
    (λ (dc val range w h)
      (set-size w h)
      (set-val (/ val range) left-body-color right-body-color)
      (do-draw dc left-body-color right-body-color))))

#;
(let ()
  (define f (new frame% (label "")))
  (define c2 (new canvas% [parent f]
                  [min-width 200]
                  [min-height 200]
                  [style '(no-autoclear)]
                  [paint-callback
                   (λ (c dc)
                     (let-values ([(w h) (send c get-client-size)])
                       (draw-honu dc 
                                  (send slider get-value)
                                  100
                                  w
                                  h)))]))
  (define slider (new slider% 
                      [label #f]
                      [min-value 0]
                      [max-value 100]
                      [parent f]
                      [callback 
                       (λ (a b) 
                         (send c2 refresh))]))
  (define b (new button%
                 [label "animate"]
                 [parent f]
                 [callback
                  (λ (x y)
                    (thread
                     (λ ()
                       (let loop ([i 0])
                         (queue-callback
                          (λ ()
                            (send slider set-value i)
                            (send c2 refresh)))
                         (unless (= i 100)
                           (sleep 1/20)
                           (loop (+ i 1)))))))]))
  
  (send f show #t))  

