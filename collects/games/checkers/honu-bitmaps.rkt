(module honu-bitmaps racket
  (require racket/math
           racket/gui
           racket/class)

  (provide honu-bitmap honu-down-bitmap
           honu-rotation honu-down-rotation)
  
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
    (send path translate (+ (- big-fin-right-edge body-width) 1) (+ (- head-cy) (/ head-height 2) 2)))
  
  (adjust-path body-path)
  (adjust-path left-hole-path)
  (adjust-path right-hole-path)
  
  (define pale-red-color (make-object color% 242 183 183))
  (define pale-blue-color (make-object color% 183 202 242))
  (define pale-background-color (make-object color% 209 220 248))

  (define bitmap-size 128)
    
  (define (make-honu-bitmap main-color left-color right-color rot)
    (let ([main-bm (make-single-bitmap main-color main-color left-color right-color rot)])
      (send main-bm set-loaded-mask (make-single-bitmap "white" "black" "black" "black" rot))
      main-bm))
  
  (define (make-single-bitmap bgcolor main-color left-color right-color rot)
    (let* ([bitmap (make-object bitmap% bitmap-size bitmap-size)]
           [dc (make-object bitmap-dc% bitmap)]
           [path (rotate-path body-path rot)]
           [left-hole-path (rotate-path left-hole-path rot)]
           [right-hole-path (rotate-path right-hole-path rot)]
           [scale 1/2])
      (send dc set-smoothing 'aligned)
      (send dc set-pen "white" 1 'transparent)
      (send dc set-brush bgcolor 'solid)
      (send dc draw-rectangle 0 0 bitmap-size bitmap-size)
      (send dc set-scale scale scale)
      
      (let-values ([(x y w h) (send path get-bounding-box)])
        (let ([dx (- (/ (/ bitmap-size scale) 2) (/ w 2))]
              [dy (- (/ (/ bitmap-size scale) 2) (/ h 2))])
          (send path translate (- x) (- y))
          (send left-hole-path translate (- x) (- y))
          (send right-hole-path translate (- x) (- y))
          
          (send dc set-brush main-color 'solid)
          (send dc set-pen main-color 1 'solid)
          (send dc draw-path path dx dy)
          
          (send dc set-pen left-color 1 'solid)
          (send dc set-brush left-color 'solid)
          (send dc draw-path left-hole-path dx dy)
          
          (send dc set-brush right-color 'solid)
          (send dc set-pen right-color 1 'solid)
          (send dc draw-path right-hole-path dx dy)))
      
      (send dc set-bitmap #f)
      bitmap))
  
  (define (rotate-path path rot)
    (let ([pth (new dc-path%)])
      (send pth append path)
      (send pth rotate rot)
      pth))
  
  (define honu-rotation (* pi 1/4))
  (define honu-down-rotation (* pi (+ 1 1/4)))
  
  (define honu-bitmap (make-honu-bitmap (make-object color% 150 150 150) "red" "blue" honu-rotation))
  (define honu-down-bitmap (make-honu-bitmap "black" "orangered" "blue" honu-down-rotation))
#|
  (define dx 0)
  (define dy 0)
  (define f (new frame% (label "")))
  (define c (new canvas%
                 (parent f)
                 (paint-callback
                  (lambda (c dc)
                    (send dc draw-bitmap honu-bitmap dx dy)
                    (send dc draw-line 
                          (/ bitmap-size 2)
                          (/ bitmap-size 2)
                          (+ (/ bitmap-size 2) (* 100 (cos (+ (/ pi 2) honu-down-rotation))))
                          (+ (/ bitmap-size 2) (* 100 (- (sin (+ (/ pi 2) honu-down-rotation))))))))))
  (send c min-width bitmap-size)
  (send c min-height bitmap-size)
  (define b+ (new button%
                  (label "+1")
                  (parent f)
                  (callback (lambda (x y) (set! dy (+ dy 1)) (send c refresh)))))
  (define b- (new button%
                  (label "-1")
                  (parent f)
                  (callback (lambda (x y) (set! dy (- dy 1)) (send c refresh)))))
  (send f show #t)  
|#
  )
