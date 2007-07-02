(module stick-figures mzscheme
  (require (lib "class.ss")
           (lib "pretty.ss")
           (lib "mred.ss" "mred"))
  
  (provide running-canvas%
           get-running-bitmap)
  
  (define head-size 40)
  (define running-bitmap-factor 1/11)
  (define small-factor 1/5)
  (define line-size 2)
  
  (define waiting-points
    '((head 47 -4)
      (neck 40 14)
      (shoulders 38 29)
      (left-elbow 6 65)
      (right-elbow 63 66)
      (left-hand 59 73)
      (right-hand 58 18)
      (waist 35 77)
      (left-knee 16 116)
      (right-knee 55 113)
      (left-ankle 16 155)
      (right-ankle 61 154)
      (left-toe -3 152)
      (right-toe 80 145)) 
    
    #;
    '((head 47 -4)
      (neck 40 14)
      (shoulders 38 29)
      (left-elbow 6 65)
      (right-elbow 63 66)
      (left-hand 59 73)
      (right-hand 58 18)
      (waist 35 77)
      (left-knee 19 125)
      (right-knee 58 123)
      (left-ankle 15 161)
      (right-ankle 61 163)
      (left-toe 0 161)
      (right-toe 75 157)))
  
  (define running-points
    '((head 101 18)
      (neck 85 33)
      (shoulders 76 44)
      (left-elbow 32 42)
      (right-elbow 86 67)
      (left-hand 20 62)
      (right-hand 119 53)
      (waist 31 79)
      (left-knee 27 124)
      (right-knee 68 93)
      (left-ankle -6 141)
      (right-ankle 60 132)
      (left-toe 0 152)
      (right-toe 82 132)))
  
  (define running-canvas%
    (class canvas%
      (inherit get-dc refresh)
      (define/public (set-running r?) 
        (unless (eq? r? is-running?)
          (set! is-running? r?)
          (refresh)))
      (define is-running? #f)
      
      (define-values (w h running-dx running-dy waiting-dx waiting-dy)
        (get-size-parameters))

      (define/override (on-paint)
        (let ([dc (get-dc)])
          (if is-running?
              (draw-callback dc small-factor #f running-points running-dx running-dy line-size)
              (draw-callback dc small-factor #f waiting-points waiting-dx waiting-dy line-size))))
      (super-new [stretchable-width #f]
                 [stretchable-height #f]
                 [style '(transparent)])
      (inherit min-width min-height)
      (min-width w)
      (min-height h)))
  
  (define (get-size-parameters)
    (let-values ([(min-rx min-ry) (get-max/min-x/y min running-points)]
                 [(max-rx max-ry) (get-max/min-x/y max running-points)]
                 [(min-wx min-wy) (get-max/min-x/y min waiting-points)]
                 [(max-wx max-wy) (get-max/min-x/y max waiting-points)])
      (let* ([running-w (* small-factor (- max-rx min-rx))]
             [waiting-w (* small-factor (- max-wx min-wx))]
             [running-h (* small-factor (- max-ry min-ry))]
             [waiting-h (* small-factor (- max-wy min-wy))]
             [w (+ 2 (ceiling (max running-w waiting-w)))]
             [h (+ 2 (ceiling (max running-h waiting-h)))]
             [running-dx (+ 1 (- (/ w 2) (/ running-w 2)))]
             [running-dy (+ 1 (- (/ h 2) (/ running-h 2)))]
             [waiting-dx (+ 1 (- (/ w 2) (/ waiting-w 2)))]
             [waiting-dy (+ 1 (- (/ h 2) (/ waiting-h 2)))])
        (values w h running-dx running-dy waiting-dx waiting-dy))))
  
  (define running-bitmap #f)
  (define (get-running-bitmap)
    (unless running-bitmap
      (let-values ([(min-rx min-ry) (get-max/min-x/y min running-points)]
                   [(max-rx max-ry) (get-max/min-x/y max running-points)])
        (let* ([margin 1]
               [w (+ margin margin (ceiling (* running-bitmap-factor (- max-rx min-rx))))]
               [h (+ margin margin (ceiling (* running-bitmap-factor (- max-ry min-ry))))]
               [bm-mask (make-object bitmap% w h)]
               [bm (make-object bitmap% w h)]
               [bdc (make-object bitmap-dc% bm-mask)]
               [green (make-object color% 30 132 30)])
          (send bdc clear)
          (draw-callback bdc running-bitmap-factor #f running-points
                         (+ margin (- (* running-bitmap-factor min-rx)))
                         (+ margin (- (* running-bitmap-factor min-ry)))
                         2)
          (send bdc set-bitmap bm)
          (send bdc set-brush green 'solid)
          (send bdc set-pen green 1 'solid)
          (send bdc draw-rectangle 
                0 0 
                (+ margin margin w)
                (+ margin margin h))
          (send bdc set-bitmap #f)
          (send bm set-loaded-mask bm-mask)
          (set! running-bitmap bm))))
    running-bitmap)
  
  (define (test-running-canvas)
    (let* ([f (new frame% [label ""])]
           [c (new running-canvas% [parent f])])
      (new button% [parent f]
           [label "on"]
           [callback
            (λ (x y) (send c set-running #t))])
      (new button% [parent f]
           [label "off"]
           [callback
            (λ (x y) (send c set-running #f))])
      (send f show #t)))
  

  (define (normalize points)
    (let-values ([(min-x min-y) (get-max/min-x/y min points)])
      (map (λ (x) (list (car x) 
                        (+ (- (list-ref x 1) min-x))
                        (+ (- (list-ref x 2) min-y))))
           points)))
  
  (define (get-max/min-x/y choose points)
    (values (choose (- (list-ref (assoc 'head points) 1) (/ head-size 2))
                    (+ (list-ref (assoc 'head points) 1) (/ head-size 2))
                    (apply choose (map (λ (x) (list-ref x 1)) points)))
            (choose (- (list-ref (assoc 'head points) 2) (/ head-size 2))
                    (+ (list-ref (assoc 'head points) 2) (/ head-size 2))
                    (apply choose (map (λ (x) (list-ref x 2)) points)))))
  
  (define show-dots? #t)
  (define (draw-callback dc factor dots? points dx dy line-size)
    (send dc set-smoothing 'aligned)
    (let ([points (normalize points)])
      (send dc set-pen "orange" 1 'solid)
      (send dc set-brush "orange" 'solid)
      (when (and dots? show-dots?)
        (for-each
         (λ (x) (send dc draw-ellipse
                      (+ dx (- (list-ref x 1) 4))
                      (+ dy (- (list-ref x 2) 4))
                      9 9))
         points))
      (send dc set-pen "black" line-size 'solid)
      (send dc set-brush "black" 'transparent)
      (draw-points points dc factor dx dy)
      
      (let* ([head (assoc 'head points)]
             [hx (list-ref head 1)]
             [hy (list-ref head 2)])
        (send dc draw-ellipse 
              (+ dx (* factor (- hx (/ head-size 2))))
              (+ dy (* factor (- hy (/ head-size 2))))
              (* factor head-size)
              (* factor head-size)))))
  
  (define (draw-points points dc factor dx dy)
    (connect 'neck 'shoulders points dc factor dx dy)
    (connect 'shoulders 'left-elbow points dc factor dx dy)
    (connect 'shoulders 'right-elbow points dc factor dx dy)
    (connect 'left-elbow 'left-hand points dc factor dx dy)
    (connect 'right-elbow 'right-hand points dc factor dx dy)
    (connect 'shoulders 'waist points dc factor dx dy)
    (connect 'waist 'left-knee points dc factor dx dy)
    (connect 'waist 'right-knee points dc factor dx dy)
    (connect 'left-knee 'left-ankle points dc factor dx dy)
    (connect 'right-knee 'right-ankle points dc factor dx dy)
    (connect 'left-ankle 'left-toe points dc factor dx dy)
    (connect 'right-ankle 'right-toe points dc factor dx dy))
  
  (define (connect from to points dc factor dx dy)
    (let ([from-p (assoc from points)]
          [to-p (assoc to points)])
      (when (and from-p to-p)
        (send dc draw-line 
              (+ dx (* factor (list-ref from-p 1)))
              (+ dy (* factor (list-ref from-p 2)))
              (+ dx (* factor (list-ref to-p 1)))
              (+ dy (* factor (list-ref to-p 2)))))))
  
  ;; Use this thunk to edit the points.
  ;; Click the 'show' button to print out the pionts and then
  ;; copy and paste them back into this file.
  (define (edit-points points)
    (define c%
      (class canvas%
        (inherit get-client-size refresh get-dc)
        (define clicked-point #f)
        (define clicked-x 0)
        (define clicked-y 0)
        (define orig-x 0)
        (define orig-y 0)
        (define/override (on-paint)
          (draw-callback (get-dc) 1 #t points 0 0 line-size))
        (define/override (on-event evt)
          (cond
            [(send evt button-down? 'left)
             (let-values ([(w h) (get-client-size)])
               (let ([x (send evt get-x)]
                     [y (send evt get-y)])
                 (let ([point (find-point this x y)])
                   (when point
                     (set! clicked-x x)
                     (set! clicked-y y)
                     (set! clicked-point point)
                     (let ([orig-point (assoc point points)])
                       (set! orig-x (list-ref orig-point 1))
                       (set! orig-y (list-ref orig-point 2)))))))]
            [(and clicked-point (send evt moving?))
             (set! points 
                   (map (λ (x)
                          (if (eq? (car x) clicked-point)
                              (list (list-ref x 0)
                                    (+ orig-x (- (send evt get-x) clicked-x))
                                    (+ orig-y (- (send evt get-y) clicked-y)))
                              x))
                        points))
             (refresh)
             (send csmall refresh)]
            [(send evt button-up? 'left)
             (set! clicked-point #f)]))
        (super-new)))
    
    (define (find-point c x y)
      (let loop ([points (normalize points)])
        (cond
          [(null? points) #f]
          [else (let ([point (car points)])
                  (if (and (<= (- (list-ref point 1) 4)
                               x
                               (+ (list-ref point 1) 4))
                           (<= (- (list-ref point 2) 4)
                               y
                               (+ (list-ref point 2) 4)))
                      (car point)
                      (loop (cdr points))))])))
    
    (define f (new frame% [label ""] [width 400] [height 400]))
    (define cp (new horizontal-panel% [parent f]))
    (define cbig (new c% [parent cp]))
    (define csmall
      (new canvas%
           [parent cp]
           [paint-callback (λ (c dc) 
                             (draw-callback dc small-factor #f running-points 0 0 line-size)
                             (draw-callback dc small-factor #f waiting-points 30 0 line-size)
                             (draw-callback dc small-factor #f points 30 50 line-size)
                             (draw-callback dc small-factor #f points 0 50 line-size))]))
    (define bp (new horizontal-panel% [parent f] [stretchable-height #f]))
    (new button%
         [parent bp]
         [label "Show"]
         [callback
          (λ (x y)
            (pretty-print points))])
    (new button%
         [parent bp]
         [label "Toggle dots"]
         [callback
          (λ (x y)
            (set! show-dots? (not show-dots?))
            (send cbig refresh))])
    (send f show #t))
  
  #;(edit-points waiting-points)
  #;(edit-points running-points))
