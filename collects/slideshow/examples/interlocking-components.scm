
(module interlocking-components slideshow/run
  (require mzlib/class
           mred
           scheme/list)
  
  (define-struct posn (x y) (make-inspector))
  
  (define between-space 20)

  (define main-piece-width 200)
  (define main-piece-height 200)
  (define right-piece-width 200)
  (define right-piece-height 200)
  (define below-piece-width (+ right-piece-width between-space main-piece-width))
  (define below-piece-height 200)
  (define below-interpose-piece-width below-piece-width)
  (define below-interpose-piece-height 50)

  (define main-piece-dark-color "crimson")
  (define main-piece-light-color "pink")
  (define right-piece-dark-color "medium blue")
  (define right-piece-light-color "sky blue")
  (define below-piece-dark-color "forest green")
  (define below-piece-light-color "pale green")
  (define below-interpose-piece-dark-color "purple")
  (define below-interpose-piece-light-color "plum")
  
  (define square-tooth-offset 50)
  (define square-tooth-height 60)
  (define square-tooth-width 50)
  (define square-tooth-accept-space 15)
  
  (define pointy-tooth-offset 50)
  (define pointy-tooth-width 100)
  (define pointy-tooth-height 50)
  (define pointy-tooth-accept-space 5)

  (define hex-tooth-offset 50)
  (define hex-tooth-width 100)
  (define hex-tooth-height 50)
  (define hex-tooth-accept-space 10)
  (define hex-tooth-inset 20)

  (slide/center
   (page-para/c
    "The following slide sequences was extracted"
    "from a talk on components and contracts"))
  
  (define (interlocking-components)
    (slide (make-orig #t #f ghost ghost ghost ghost ghost
                      "A day in the life of a component software developer."
                      "We start with one component (from web or somewhere)"))
    
    (slide (make-orig #t #f ident ident ghost ghost ghost
                      "... and compose it with other software to build a system."))
    
    (slide (make-orig #t #f ident ghost ident ghost ghost
                      "Except, of course, the composed pieces might not fit,"))
    
    (slide (make-orig #t #f ident ghost ghost ident ghost
                      "... so the programmers develop adapters."
                      "Now the program can be run, but what"
                      "happens when you run it?"))
    
    (slide (make-orig #t #f ghost ghost ghost ghost ident
                      "KABOOM!"))
    
    (slide (make-orig #t #f ident ghost ghost ident ghost
                      "What happened? Which component failed?"))
    
    (slide (make-orig #f #f ident ghost ghost ident ghost
                      "To figure that out, the programmer shouldn't have"
                      "to understand the details of all of the"
                      "component implementations."))
    
    (slide (make-orig #f #t ident ghost ghost ident ghost
                      "Instead the grey area, where the interface"
                      "contract specs are, should have enough"
                      "information to figure that out.")))
  
  (define (make-orig dark? bkg? right bot-hex bot-pointy bot-combined explosion . text)
    (let* ([below-pointy (below-pointy-piece dark?)]
           [components
            (lt-superimpose
             (at 0
                 0
                 (main-piece dark?))
             (at (+ main-piece-width between-space)
                 0 
                 (right (right-piece dark?)))
             (at 0 
                 (+ main-piece-height between-space)
                 (bot-hex (below-hex-piece dark?)))
             (at 0 
                 (+ main-piece-height between-space)
                 (bot-pointy (below-pointy-piece dark?)))
             (at 0
                 (+ main-piece-height between-space) 
                 (bot-combined (below-interpose-piece dark?)))
             (at 0
                 (+ main-piece-height between-space below-interpose-piece-height between-space) 
                 (bot-combined
                  below-pointy)))]
           [combined
            (cc-superimpose
             (if bkg?
                 (overlay-background 
                  "light gray"
                  (overlay-striped-background
                   "dark gray"
                   10 
                   components))
                 components)
             (explosion big-explosion-pict))])
      (vc-append
       40
       combined
       (apply page-para text))))

                                   
  (define big-explosion-pict (bitmap "big-explosion.jpg"))

  (define (at x y pict) (hc-append (blank x 0) (vc-append (blank 0 y) pict)))
  
  (define (main-piece dark?)
    (polygon
     (if dark? main-piece-dark-color main-piece-light-color)
     (make-posn 0 0)
     (make-posn main-piece-width 0)
     (offset-posns main-piece-width square-tooth-offset (square-tooth #t))
     (make-posn main-piece-width main-piece-height)
     (offset-posns pointy-tooth-offset main-piece-height (reverse (hex-tooth #t)))
     (make-posn 0 main-piece-height)
     (make-posn 0 0)))
  
  (define (below-pointy-piece dark?)
    (polygon
     (if dark? below-piece-dark-color below-piece-light-color)
     (make-posn 0 0)
     (offset-posns pointy-tooth-offset 0 (pointy-tooth #f))
     (make-posn below-piece-width 0)
     (make-posn below-piece-width below-piece-height)
     (make-posn 0 below-piece-height)
     (make-posn 0 0)))
  
  (define (below-hex-piece dark?)
    (polygon
     (if dark? below-piece-dark-color below-piece-light-color)
     (make-posn 0 0)
     (offset-posns hex-tooth-offset 0 (hex-tooth #f))
     (make-posn below-piece-width 0)
     (make-posn below-piece-width below-piece-height)
     (make-posn 0 below-piece-height)
     (make-posn 0 0)))
  
  (define (below-interpose-piece dark?)
    (polygon
     (if dark? below-interpose-piece-dark-color below-interpose-piece-light-color)
     (make-posn 0 0)
     (offset-posns pointy-tooth-offset 0 (hex-tooth #f))
     (make-posn below-interpose-piece-width 0)
     (make-posn below-interpose-piece-width below-interpose-piece-height)
     (offset-posns hex-tooth-offset below-interpose-piece-height (reverse (pointy-tooth #t)))
     (make-posn 0 below-interpose-piece-height)
     (make-posn 0 0)))
  
  (define (right-piece dark?)
    (polygon
     (if dark? right-piece-dark-color right-piece-light-color)
     (make-posn 0 0)
     (offset-posns 0 square-tooth-offset (square-tooth #f))
     (make-posn 0 right-piece-height)
     (make-posn right-piece-width right-piece-height)
     (make-posn right-piece-width 0)
     (make-posn 0 0)))
  
  (define (polygon color . points)
    (let ([flat-points (flatten points)])
      (when (null? flat-points)
        (error 'polygon "expected at least one point"))
      (dc
       (lambda (dc x y)
         (let ([old-brush (send dc get-brush)])
           (send dc set-brush (send the-brush-list find-or-create-brush color 'solid))
           (send dc draw-polygon 
                 (map (lambda (p) (make-object point% (posn-x p) (posn-y p))) flat-points)
                 x 
                 y)
           (send dc set-brush old-brush)))
       (get-width flat-points)
       (get-height flat-points)
       0
       0)))
  
  (define (get-width points) (apply max (map posn-x points)))
  (define (get-height points) (apply max (map posn-y points)))
       
  (define (square-tooth accept?) 
    (list (make-posn 0
                     (if accept?
                         (- square-tooth-accept-space)
                         0))
          (make-posn (- square-tooth-width)
                     (if accept?
                         (- square-tooth-accept-space)
                         0))
          (make-posn (- square-tooth-width)
                     (if accept?
                         (+ square-tooth-height square-tooth-accept-space)
                         square-tooth-height))
          (make-posn 0 
                     (if accept?
                         (+ square-tooth-height square-tooth-accept-space)
                         square-tooth-height))))
  
  (define (hex-tooth accept?)
    (list (make-posn (if accept?
                         (- hex-tooth-accept-space)
                         0) 
                     0)
          (make-posn (if accept? 
                         (- hex-tooth-inset hex-tooth-accept-space)
                         hex-tooth-inset)
                     (- hex-tooth-height))
          (make-posn (if accept? 
                         (+ (- hex-tooth-width hex-tooth-inset) hex-tooth-accept-space)
                         (- hex-tooth-width hex-tooth-inset))
                     (- hex-tooth-height))
          (make-posn (if accept?
                         (+ hex-tooth-width hex-tooth-accept-space)
                         hex-tooth-width) 
                     0)))
  
  (define (pointy-tooth accept?)
    (list
     (make-posn (if accept? (- pointy-tooth-accept-space) 0)
                0)
     (make-posn (/ pointy-tooth-width 2)
                (if accept? 
                    (- (+ pointy-tooth-height pointy-tooth-accept-space))
                    (- pointy-tooth-height)))
     (make-posn (if accept?
                    (+ pointy-tooth-accept-space pointy-tooth-width)
                    pointy-tooth-width)
                0)))

  (define (offset-posns x y posns)
    (map (lambda (pt) (make-posn (+ x (posn-x pt)) (+ y (posn-y pt)))) posns))

  (define (overlay-striped-background color diameter pict)
    (let* ([w (pict-width pict)]
           [h (pict-height pict)]
           [pt1 (make-object point% 0 0)]
           [pt2 (make-object point% 0 0)]
           [pt3 (make-object point% 0 0)]
           [poly (list pt1 pt2 pt3)]
           [bkg
            (dc
             (lambda (dc dx dy)
               (let ([old-pen (send dc get-pen)]
                     [old-brush (send dc get-brush)]
                     [pen (send the-pen-list find-or-create-pen color 1 'solid)]
                     [brush (send the-brush-list find-or-create-brush color 'solid)])
                 (unless pen
                   (error "unknown color: ~s" color))
                 (send dc set-pen pen)
                 (send dc set-brush brush)
                 (let loop ([x 0]
                            [t #f])
                   (when (< x w)
                     (let loop ([y 0]
                                [t t])
                       (when (< y h)
                         (if t
                             (begin
                               (send pt1 set-x (+ x dx))
                               (send pt1 set-y (+ y dy))
                               (send pt2 set-x (+ x dx diameter))
                               (send pt2 set-y (+ y dy))
                               (send pt3 set-x (+ x dx))
                               (send pt3 set-y (+ y dy diameter)))
                             (begin
                               (send pt1 set-x (+ x dx))
                               (send pt1 set-y (+ y dy diameter))
                               (send pt2 set-x (+ x dx diameter))
                               (send pt2 set-y (+ y dy))
                               (send pt3 set-x (+ x dx diameter))
                               (send pt3 set-y (+ y dy diameter))))
                         (send dc draw-polygon poly)
                         (loop (+ y diameter)
                               (not t))))
                     (loop (+ x diameter)
                           (not t))))
                 (send dc set-pen old-pen)
                 (send dc set-brush old-brush)))
             w
             h
             0
             0)])
      (cc-superimpose bkg pict)))
  
  (define (overlay-background color pict)
    (let* ([w (pict-width pict)]
           [h (pict-height pict)]
           [bkg
            (dc
             (lambda (dc x y)
               (let ([old-pen (send dc get-pen)]
                     [old-brush (send dc get-brush)]
                     [pen (send the-pen-list find-or-create-pen color 1 'solid)]
                     [brush (send the-brush-list find-or-create-brush color 'solid)])
                 (unless pen
                   (error "unknown color: ~s" color))
                 (send dc set-pen pen)
                 (send dc set-brush brush)
                 (send dc draw-rectangle x y w h)
                 (send dc set-pen old-pen)
                 (send dc set-brush old-brush)))
             w
             h
             0
             0)])
      (cc-superimpose bkg pict)))
  
  (define (ident x) x)

  (interlocking-components))
