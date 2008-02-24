(module plot mzscheme
  (require mred
           mzlib/class
           mzlib/contract)

  
  (define-struct data-set (points connected? pen min-x max-x min-y max-y))
  
  (define-struct plot-setup (axis-label-font axis-number-font axis-pen grid? grid-pen
                             x-axis-marking y-axis-marking x-axis-label y-axis-label))

  (provide/contract
   (struct data-set ((points (listof (is-a?/c point%)))
                     (connected? any/c)
                     (pen (is-a?/c pen%))
                     (min-x number?)
                     (max-x number?)
                     (min-y number?)
                     (max-y number?)))
   (struct plot-setup ((axis-label-font (is-a?/c font%))
                       (axis-number-font (is-a?/c font%))
                       (axis-pen (is-a?/c pen%))
                       (grid? any/c)
                       (grid-pen (is-a?/c pen%))
                       (x-axis-marking (listof number?))
                       (y-axis-marking (listof number?))
                       (x-axis-label string?)
                       (y-axis-label string?)))
   (plot ((is-a?/c dc<%>) (listof data-set?) plot-setup? . -> . void?)))

  (define (draw-text-sideways dc text x y font big-chars? offset)
    (let-values (((width height bot-dist top-dist)
                  (send (make-object bitmap-dc% (make-object bitmap% 1 1 #f))
                        get-text-extent text font big-chars? offset)))
      (let* ((width (inexact->exact (ceiling width)))
             (height (inexact->exact (ceiling (+ height bot-dist top-dist))))
             (bc (make-object bitmap-dc% (make-object bitmap% width height #f)))
             (new-bc (make-object bitmap-dc% (make-object bitmap% height width #f)))
             (c (make-object color%)))
        (send bc set-font font)
        (send bc clear)
        (send new-bc clear)
        (send bc draw-text text 0 0 big-chars? offset)
        (let loop ((i 0)
                   (j 0))
          (cond
            ((>= i (+ height top-dist bot-dist))
             (send dc draw-bitmap (send new-bc get-bitmap) x y))
            ((>= j width) (loop (add1 i) 0))
            (else
             (send bc get-pixel j i c)
             (send new-bc set-pixel i (- width j) c)
             (loop i (add1 j))))))))
  
  
  
  #cs
  (define (plot dc data-sets ps)
    (let-values (((canvas-width canvas-height) (send dc get-size))
                 ((_ label-height label-bottom-dist label-top-dist)
                  (send dc get-text-extent "" (plot-setup-axis-label-font ps) #f 0))
                 ((number-width number-height number-bottom-dist number-top-dist)
                  (send dc get-text-extent "00.00e-10" (plot-setup-axis-number-font ps) #f 0)))
      (let* ((label-height (+ label-height label-bottom-dist label-top-dist))
             (number-height (+ number-height number-bottom-dist number-top-dist))
             (bottom-space (+ (* 1.5 number-height) label-height))
             (side-space (+ number-width (* .5 number-height) label-height)))
        (cond
          ((not (null? data-sets))
           (let* ((mx (apply min (map data-set-min-x data-sets)))
                  (my (apply min (map data-set-min-y data-sets)))
                  (Mx (apply max (map data-set-max-x data-sets)))
                  (My (apply max (map data-set-max-y data-sets)))
                  (w-scale (/ (- canvas-width side-space) (- Mx mx)))
                  (h-scale (/ (- canvas-height bottom-space) (- my My)))
                  (transform-point
                   (lambda (p)
                     (make-object point%
                       (+ side-space (* (- (send p get-x) mx) w-scale))
                       (* (- (send p get-y) My) h-scale)))))
             
             ;; Draw axes
             (send dc set-pen (plot-setup-axis-pen ps))
             (send dc draw-lines
                   (map transform-point 
                        (list (make-object point% mx my)
                              (make-object point% Mx my))))
             (send dc draw-lines
                   (map transform-point 
                        (list (make-object point% mx my)
                              (make-object point% mx My))))
             
             ;; draw axis markings
             (send dc set-font (plot-setup-axis-number-font ps))
             (for-each 
              (lambda (x)
                (let* ((p (transform-point (make-object point% x my)))
                       (draw-x (send p get-x))
                       (draw-y-start (+ (send p get-y) (* .5 number-height)))
                       (draw-y-stop (- (send p get-y) (* .5 number-height)))
                       (str (number->string x)))
                  (send dc draw-line draw-x draw-y-start draw-x draw-y-stop)
                  (let-values (((number-width x1 x2 x3)
                                (send dc get-text-extent
                                      str (plot-setup-axis-number-font ps) #f 0)))
                    (let ((start-x (- draw-x (* .5 number-width))))
                      (send dc draw-text str start-x (+ 2 draw-y-start))))))
              (plot-setup-x-axis-marking ps))
             (for-each 
              (lambda (y)
                (let* ((p (transform-point (make-object point% mx y)))
                       (draw-y (send p get-y))
                       (draw-x-start (- (send p get-x) (* .5 number-height)))
                       (draw-x-stop (+ (send p get-x) (* .5 number-height)))
                       (str (number->string y)))
                  (send dc draw-line draw-x-start draw-y draw-x-stop draw-y)
                  (let-values (((number-width x1 x2 x3)
                                (send dc get-text-extent
                                      str (plot-setup-axis-number-font ps) #f 0)))
                    (let ((start-x (- draw-x-start 2 number-width))
                          (start-y (- draw-y (* .5 number-height))))
                      (send dc draw-text str start-x start-y)))))
              (plot-setup-y-axis-marking ps))
             
             ;; draw axis labels
             (send dc set-font (plot-setup-axis-label-font ps))
             (let-values (((x-label-width x1 x2 x3)
                           (send dc get-text-extent
                                 (plot-setup-x-axis-label ps)
                                 (plot-setup-axis-label-font ps) #f 0)))
               (send dc draw-text
                     (plot-setup-x-axis-label ps)
                     (- (+ side-space (* .5 (- canvas-width side-space))) (* .5 x-label-width))
                     (- canvas-height label-height)))
             (let-values (((y-label-width y1 y2 y3)
                           (send dc get-text-extent
                                 (plot-setup-y-axis-label ps)
                                 (plot-setup-axis-label-font ps) #f 0)))
               (draw-text-sideways dc (plot-setup-y-axis-label ps)
                                   0
                                   (- (* .5 (- canvas-height bottom-space))
                                      (* .5 y-label-width))
                                   (plot-setup-axis-label-font ps)
                                   #f
                                   0))
             
             ;; draw-grid
             (cond
               ((plot-setup-grid? ps)
                (send dc set-pen (plot-setup-grid-pen ps))
                (for-each (lambda (x)
                            (send dc draw-lines
                                  (map transform-point
                                       (list (make-object point% x my)
                                             (make-object point% x My)))))
                          (plot-setup-x-axis-marking ps))
                (for-each (lambda (y)
                            (send dc draw-lines
                                  (map transform-point
                                       (list (make-object point% mx y)
                                             (make-object point% Mx y)))))
                          (plot-setup-y-axis-marking ps))))
             
             ;; draw data
             (for-each
              (lambda (data)
                (send dc set-pen (data-set-pen data))
                (cond
                  ((data-set-connected? data)
                   (send dc draw-lines (map transform-point (data-set-points data))))
                  (else
                   (for-each
                    (lambda (p)
                      (let ((tp (transform-point p)))
                        (send dc draw-point (send tp get-x) (send tp get-y))))
                    (data-set-points data)))))
              data-sets)))))))
  )
