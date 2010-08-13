#lang racket/base
  (require racket/class
           racket/pretty
           racket/gui/base)
  
  (define head-size 40)
  (define small-bitmap-factor 1/2)
  (define small-factor 1/5)
  (define line-size 2)
  
  (define waiting-points
    '((head 47 2)
      (neck 46 15)
      (shoulders 38 42)
      (left-shoulder 18 39)
      (right-shoulder 65 42)
      (left-elbow 8 74)
      (right-elbow 68 76)
      (left-hand 24 79)
      (right-hand 56 83)
      (waist 37 87)
      (left-knee 23 117)
      (right-knee 57 117)
      (left-ankle 21 149)
      (right-ankle 59 148)
      (left-toe 3 148)
      (right-toe 79 145)))
  
  (define waiting-points/2
    '((head 47 2)
      (neck 46 15)
      (shoulders 38 42)
      (left-shoulder 18 39)
      (right-shoulder 65 42)
      (left-elbow 8 74)
      (right-elbow 68 76)
      (left-hand 24 79)
      (right-hand 56 83)
      (waist 37 87)
      (left-knee 23 117)
      (right-knee 57 117)
      (left-ankle 21 149)
      (right-ankle 59 148)
      (left-toe 3 148)
      (right-toe 79 132)))
  
  (define waiting-points/old
    '((head 55 0)
      (neck 43 18)
      (shoulders 37 33)
      (left-shoulder 23 34)
      (right-shoulder 50 37)
      (left-elbow 8 74)
      (right-elbow 66 69)
      (left-hand 60 78)
      (right-hand 68 18)
      (waist 37 87)
      (left-knee 19 122)
      (right-knee 57 117)
      (left-ankle 19 154)
      (right-ankle 62 155)
      (left-toe 0 154)
      (right-toe 83 146)))
  
  (define waiting-points/2/old
    '((head 55 0)
      (neck 43 18)
      (shoulders 37 33)
      (left-shoulder 23 34)
      (right-shoulder 50 37)
      (left-elbow 8 74)
      (right-elbow 66 69)
      (left-hand 60 78)
      (right-hand 68 18)
      (waist 37 87)
      (left-knee 19 122)
      (right-knee 57 117)
      (left-ankle 19 154)
      (left-toe 0 154)
      (right-ankle 62 155)
      (right-toe 83 154)))
  
  (define running-points
    '((head 130 18)
      (neck 114 33)
      (shoulders 105 44)
      (left-shoulder 105 44)
      (right-shoulder 105 44)
      (left-elbow 71 28)
      (right-elbow 115 67)
      (left-hand 50 54)
      (right-hand 148 53)
      (waist 59 78)
      (left-knee 41 112)
      (right-knee 97 93)
      (left-ankle 0 129)
      (right-ankle 89 132)
      (left-toe 14 146)
      (right-toe 109 132)))
  
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
  
  (define (get-bitmap points green)
    (let-values ([(min-rx min-ry) (get-max/min-x/y min points)]
                 [(max-rx max-ry) (get-max/min-x/y max points)])
      (let* ([margin 2]
             [bw (+ margin margin (ceiling (* small-factor (- max-rx min-rx))))]
             [bh (+ margin margin (ceiling (* small-factor (- max-ry min-ry))))]
             [w (ceiling (* bw small-bitmap-factor))] 
             [h (ceiling (* bh small-bitmap-factor))]
             [bm-big (make-object bitmap% bw bh)]
             [bm-solid (make-object bitmap% w h)]
             [bm-small (make-object bitmap% w h)]
             [bdc-big (make-object bitmap-dc% bm-big)]
             [bdc-solid (make-object bitmap-dc% bm-solid)]
             [bdc-small (make-object bitmap-dc% bm-small)])
        (send bdc-big clear)
        (draw-callback bdc-big small-factor #f points
                       (+ margin (- (* small-factor min-rx)))
                       (+ margin #;(- (* small-factor min-ry)))
                       3)
        
        (send bdc-small clear)
        (send bdc-small set-scale small-bitmap-factor small-bitmap-factor)
        (send bdc-small draw-bitmap bm-big 0 0)
        (send bdc-small set-scale 1 1)
        
        (send bdc-solid set-brush green 'solid)
        (send bdc-solid set-pen green 1 'solid)
        (send bdc-solid draw-rectangle 0 0 w h)
        
        (send bdc-solid set-bitmap #f)
        (send bdc-small set-bitmap #f)
        (send bdc-big set-bitmap #f)
        
        (send bm-solid set-loaded-mask bm-small)
        bm-solid)))
  
  (define (get-running-bitmap) (get-bitmap running-points (make-object color% 30 100 30)))
  (define (get-waiting-bitmap) (get-bitmap waiting-points (make-object color% 30 100 30)))
  
  (define (normalize points)
    (let-values ([(min-x min-y) (get-max/min-x/y min points)])
      (map (λ (x) (list (car x) 
                        (+ (- (list-ref x 1) min-x))
                        (+ (- (list-ref x 2) min-y))))
           points)))
  
  (define (get-max/min-x/y choose points)
    (values (apply choose
                   (- (list-ref (assoc 'head points) 1) (/ head-size 2))
                   (+ (list-ref (assoc 'head points) 1) (/ head-size 2))
                   (map (λ (x) (list-ref x 1)) points))
            (apply choose
                   (- (list-ref (assoc 'head points) 2) (/ head-size 2))
                   (+ (list-ref (assoc 'head points) 2) (/ head-size 2))
                   (map (λ (x) (list-ref x 2)) points))))
  
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
    (connect 'shoulders 'left-shoulder points dc factor dx dy)
    (connect 'left-shoulder 'left-elbow points dc factor dx dy)
    (connect 'shoulders 'right-shoulder points dc factor dx dy)
    (connect 'right-shoulder 'right-elbow points dc factor dx dy)
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
  ;; Click the 'show' button to print out the points and then
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
    (define cbitmap (new message% [label (get-bitmap points (send the-color-database find-color "black"))] [parent cp]))
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
    (new button%
         [parent bp]
         [label "Bitmap"]
         [callback
          (λ (x y)
            (send cbitmap set-label (get-bitmap points (send the-color-database find-color "black"))))])
    (send f show #t))
  
  (let ()
    (define f (new frame% [label ""]))
    (define hp (new horizontal-panel% [parent f]))
    (define left-column (new vertical-panel% [parent hp]))
    (define right-column (new vertical-panel% [parent hp]))
    (define green-rb (get-running-bitmap))
    (define black (send the-color-database find-color "black"))
    (define rb (get-bitmap running-points black))
    (define wb (get-bitmap waiting-points black))
    (define wb2 (get-bitmap waiting-points/2 black))
    (define rm (new message% [label rb] [parent left-column]))
    (define grm (new message% [label green-rb] [parent right-column]))
    (new message% [label wb] [parent left-column])
    (new message% [label wb2] [parent left-column])
    (new message% [label wb2] [parent right-column])
    (new message% [label wb] [parent right-column])
    (new grow-box-spacer-pane% [parent f])
    (send green-rb save-file (collection-file-path "run.png" "icons") 'png)
    (send rb save-file (collection-file-path "b-run.png" "icons") 'png)
    (send wb save-file (collection-file-path "b-wait.png" "icons") 'png)
    (send wb2 save-file (collection-file-path "b-wait2.png" "icons") 'png)
    (send f show #t))

  #;(edit-points waiting-points/2)
  #;(edit-points running-points)
