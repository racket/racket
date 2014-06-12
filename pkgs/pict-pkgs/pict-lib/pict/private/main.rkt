(module pict scheme/base
  (require (rename-in texpict/mrpict
                      [hline t:hline]
                      [vline t:vline]
                      [frame t:frame])
           (rename-in texpict/utils
                      [pin-line t:pin-line]
                      [pin-arrow-line t:pin-arrow-line]
                      [pin-arrows-line t:pin-arrows-line])
           (only-in racket/draw dc-path% make-bitmap bitmap% bitmap-dc%)
           (only-in racket/class new send make-object is-a?/c))

  (define (hline w h #:segment [seg #f])
    (if seg
        (dash-hline w h seg)
        (t:hline w h)))

  (define (vline w h #:segment [seg #f])
    (if seg
        (dash-vline w h seg)
        (t:vline w h)))

  (define (frame p
                 #:color [col #f]
                 #:line-width [lw #f]
                 #:segment [seg #f])
    (let* ([f (if seg
                  (dash-frame (launder (ghost p)) seg)
                  (t:frame (launder (ghost p))))]
           [f (if col
                  (colorize f col)
                  f)]
           [f (if lw 
                  (linewidth lw f)
                  f)])
      (refocus (cc-superimpose p f)
               p)))

  (define (pict-path? p)
    (or (pict? p)
        (and (pair? p)
             (list? p)
             (andmap pict? p))))

  (define (pin-line p 
                    src src-find
                    dest dest-find
                    #:start-angle [sa #f] #:end-angle [ea #f]
                    #:start-pull [sp #f] #:end-pull [ep #f]
                    #:color [col #f]
                    #:alpha [alpha 1.0]
                    #:line-width [lw #f]
                    #:under? [under? #f]
                    #:solid? [solid? #t]
                    #:style [style #f])
    (if (not (or sa ea))
        (finish-pin (launder (t:pin-line (ghost p)
                                         src src-find 
                                         dest dest-find
                                         #:style style))
                    p lw col alpha under?)
        (pin-curve* #f #f p src src-find dest dest-find
                    sa ea sp ep 0 col lw under? #t
                    style alpha)))

  (define (pin-arrow-line sz p 
                          src src-find
                          dest dest-find
                          #:start-angle [sa #f] #:end-angle [ea #f]
                          #:start-pull [sp #f] #:end-pull [ep #f]
                          #:color [col #f]
                          #:alpha [alpha 1.0]
                          #:line-width [lw #f]
                          #:under? [under? #f]
                          #:solid? [solid? #t]
                          #:style [style #f]
                          #:hide-arrowhead? [hide-arrowhead? #f])
    (if (not (or sa ea))
        (finish-pin (launder (t:pin-arrow-line sz (ghost p)
                                               src src-find 
                                               dest dest-find
                                               #f #f #f solid?
                                               #:hide-arrowhead? hide-arrowhead?
                                               #:style style))
                    p lw col alpha under?)
        (pin-curve* #f (not hide-arrowhead?) p src src-find dest dest-find
                    sa ea sp ep sz col lw under? solid?
                    style alpha)))
  
    (define (pin-arrows-line sz p 
                             src src-find
                             dest dest-find
                             #:start-angle [sa #f] #:end-angle [ea #f]
                             #:start-pull [sp #f] #:end-pull [ep #f]
                             #:color [col #f]
                             #:alpha [alpha 1.0]
                             #:line-width [lw #f]
                             #:under? [under? #f]
                             #:solid? [solid? #t]
                             #:style [style #f]
                             #:hide-arrowhead? [hide-arrowhead? #f])
      (if (not (or sa ea))
          (finish-pin (launder (t:pin-arrows-line sz (ghost p)
                                                  src src-find 
                                                  dest dest-find
                                                  #f #f #f solid?
                                                  #:hide-arrowhead? hide-arrowhead?
                                                  #:style style))
                      p lw col alpha under?)
          (pin-curve* (not hide-arrowhead?) (not hide-arrowhead?)
                      p src src-find dest dest-find
                      sa ea sp ep sz col lw under? solid? 
                      style alpha)))
    
  (define (pin-curve* start-arrow? end-arrow? p 
                      src src-find
                      dest dest-find
                      sa ea sp ep
                      sz col lw
                      under? solid?
                      style alpha)
    (let-values ([(sx0 sy0) (src-find p src)]
                 [(dx0 dy0) (dest-find p dest)])
      (let* ([sa (or sa
                     (atan (- sy0 dy0) (- dx0 sx0)))]
             [ea (or ea
                     (atan (- sy0 dy0) (- dx0 sx0)))]
             [d (sqrt (+ (* (- dy0 sy0) (- dy0 sy0)) (* (- dx0 sx0) (- dx0 sx0))))]
             [sp (* (or sp 1/4) d)]
             [ep (* (or ep 1/4) d)])
        (let ([dx (if end-arrow? (- dx0 (* sz (cos ea))) dx0)]
              [dy (if end-arrow? (+ dy0 (* sz (sin ea))) dy0)]
              [sx (if start-arrow? (+ sx0 (* sz (cos sa))) sx0)]
              [sy (if start-arrow? (- sy0 (* sz (sin sa))) sy0)]
              [path (new dc-path%)]
              [maybe-pin-line
               (lambda (arrow? p sx sy dx dy)
                 (if arrow?
                     (pin-arrow-line 
                      sz
                      p
                      p (lambda (a b) (values sx sy))
                      p (lambda (a b) (values dx dy))
                      #:line-width lw
                      #:color col
                      #:under? under?
                      #:solid? solid?
                      #:style style)
                     p))])
          (send path move-to sx sy)
          (send path curve-to
                (+ sx (* sp (cos sa)))
                (- sy (* sp (sin sa)))
                (- dx (* ep (cos ea)))
                (+ dy (* ep (sin ea)))
                dx
                dy)
          (maybe-pin-line 
           start-arrow?
           (maybe-pin-line 
            end-arrow?
            ((if under? pin-under pin-over)
             p
             0 0
             (let* ([p (dc (lambda (dc x y)
                             (let ([b (send dc get-brush)])
                               (send dc set-brush "white" 'transparent)
                               (send dc draw-path path x y)
                               (send dc set-brush b)))
                           0 0)]
                    [p (if col
                           (colorize p col)
                           p)]
                    [p (if (= alpha 1.0)
                           p
                           (cellophane p alpha))]
                    [p (if lw
                           (linewidth lw p)
                           p)]
                    [p (if style
                           (linestyle style p)
                           p)])
               p))
            dx dy dx0 dy0)
           sx sy sx0 sy0)))))


  (define (finish-pin l p lw col alpha under?)
    (let* ([l (if lw
                  (linewidth lw l)
                  l)]
           [l (if col
                  (colorize l col)
                  l)]
           [l (if (= alpha 1.0)
                  l
                  (cellophane l alpha))])
      (if under?
          (cc-superimpose l p)
          (cc-superimpose p l))))

  (define fish
    (let ([standard-fish
           (lambda (w h 
                      #:direction [direction 'left]
                      #:color [color "blue"]
                      #:eye-color [eye-color "black"]
                      #:open-mouth [open-mouth #f])
             (standard-fish w h direction color eye-color open-mouth))])
      standard-fish))

  (define (pict->bitmap p [smoothing 'aligned])
    (define w (pict-width p))
    (define h (pict-height p))
    (define bm (make-bitmap (max 1 (inexact->exact (ceiling w)))
                            (max 1 (inexact->exact (ceiling h)))))
    (unless (send bm ok?)
      (error 'pict->bitmap
             (string-append "bitmap creation failed\n"
                            "  possible reason: out of memory\n"
                            "  pict width: ~a\n"
                            "  pict height: ~a")
             w
             h))
    (define dc (make-object bitmap-dc% bm))
    (send dc set-smoothing smoothing)
    (draw-pict p dc 0 0)
    bm)
  
  (define (pict->argb-pixels p [smoothing 'aligned])
    (define bm (pict->bitmap p smoothing))
    (define w (send bm get-width)) 
    (define h (send bm get-height))
    (define bytes (make-bytes (* w h 4)))
    (send bm get-argb-pixels 0 0 w h bytes)
    bytes)
  
  (define (argb-pixels->pict b w)
    (define h (/ (bytes-length b) w 4))
    (define bm (make-bitmap w (/ (bytes-length b) w 4)))
    (send bm set-argb-pixels 0 0 w h b)
    (bitmap bm))

  (provide hline vline
           frame
           pict-path?
           pin-line pin-arrow-line pin-arrows-line
           (except-out (all-from-out texpict/mrpict)
                       
                       dash-hline dash-vline

                       dash-frame oval oval/radius

                       caps-text
                       big-circle

                       picture
                       cons-picture
                       cons-picture*
                       place-over
                       place-under

                       record
                       thick
                       thin

                       find-lt
                       find-lc
                       find-lb
                       find-ltl
                       find-lbl
                       find-ct
                       find-cc
                       find-cb
                       find-ctl
                       find-cbl
                       find-rt
                       find-rc
                       find-rb
                       find-rtl
                       find-rbl

                       drop
                       lift)
           (rename-out [drop drop-below-ascent]
                       [lift lift-above-baseline])

           (except-out (all-from-out texpict/utils)
                   
                       color-frame color-dash-frame
                       round-frame color-round-frame
    
                       cons-colorized-picture
                       arrow-line
                       arrows-line

                       add-line
                       add-arrow-line
                       add-arrows-line

                       explode-star

                       standard-fish

                       find-pen find-brush)
           (rename-out [fish standard-fish])
           pict->bitmap
           pict->argb-pixels
           argb-pixels->pict))
