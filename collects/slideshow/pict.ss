(module pict scheme/base
  (require (rename-in texpict/mrpict
                      [hline t:hline]
                      [vline t:vline]
                      [frame t:frame])
           (rename-in texpict/utils
                      [pin-line t:pin-line]
                      [pin-arrow-line t:pin-arrow-line]
                      [pin-arrows-line t:pin-arrows-line]))

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

  (define (pin-line p src find-src dest find-dest
                    #:line-width [lw #f]
                    #:color [col #f]
                    #:under? [under? #f])
    (finish-pin (launder (t:pin-line (ghost p)
                                     src find-src 
                                     dest find-dest))
                p lw col under?))

  (define (pin-arrow-line sz p src find-src dest find-dest
                          #:line-width [lw #f]
                          #:color [col #f]
                          #:under? [under? #f]
                          #:solid? [solid? #t])
    (finish-pin (launder (t:pin-arrow-line sz (ghost p)
                                           src find-src 
                                           dest find-dest
                                           #f #f #f solid?))
                p lw col under?))

  (define (pin-arrows-line sz p src find-src dest find-dest
                           #:line-width [lw #f]
                           #:color [col #f]
                           #:under? [under? #f]
                           #:solid? [solid? #t])
    (finish-pin (launder (t:pin-arrows-line sz (ghost p)
                                            src find-src 
                                            dest find-dest
                                            #f #f #f solid?))
                p lw col under?))

  (define (finish-pin l p lw col under?)
    (let* ([l (if lw
                  (linewidth lw l)
                  l)]
           [l (if col
                  (colorize l col)
                  l)])
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
           (rename-out [fish standard-fish])))
