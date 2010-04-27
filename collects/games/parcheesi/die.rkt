(module die mzscheme
  (require mred
           mzlib/class)
  
  (provide die%)

  (define die%
    (class canvas%
      (inherit get-dc get-client-size refresh)
      (init-field [digit #f])
      (define/public (set-digit d) 
        (unless (equal? digit d)
          (set! digit d)
          (refresh)))
      (init-field [dim? #f])
      (define/public (set-dim d)
        (unless (equal? dim? d)
          (set! dim? d)
          (refresh)))
      (define/override (on-paint)
        (let ([dc (get-dc)])
          (let-values ([(w h) (get-client-size)])
            (when digit
              (send dc set-pen (send the-pen-list find-or-create-pen (if dim? "dark gray" "black") 1 'solid))
              (send dc set-brush (send the-brush-list find-or-create-brush "white" 'solid))
              (send dc draw-rounded-rectangle 0 0 w h)
              (send dc set-brush (send the-brush-list find-or-create-brush (if dim? "dark gray" "black") 'solid))
              (let ([draw-circle
                     (lambda (mx my)
                       (send dc draw-ellipse
                             (- (* mx w) (/ w 12)) 
                             (- (* my h) (/ h 12))
                             (/ w 6)
                             (/ h 6)))]
                    [in (- 1/3 1/24)]
                    [out (+ 2/3 1/24)]
                    [draw-text
                     (lambda (str)
                       (let-values ([(tw th _1 _2) (send dc get-text-extent str)])
                         (send dc draw-text 
                               str
                               (- (/ w 2) (/ tw 2))
                               (- (/ h 2) (/ th 2)))))])
                (case digit
                  [(1) (draw-circle 1/2 1/2)]
                  [(2) (draw-circle in in)
                       (draw-circle out out)]
                  [(3) (draw-circle in in)
                       (draw-circle 1/2 1/2)
                       (draw-circle out out)]
                  [(4) (draw-circle in in)
                       (draw-circle in out)
                       (draw-circle out in)
                       (draw-circle out out)]
                  [(5) (draw-circle in in)
                       (draw-circle in out)
                       (draw-circle 1/2 1/2)
                       (draw-circle out in)
                       (draw-circle out out)]
                  [(6) (draw-circle in in)
                       (draw-circle in 1/2)
                       (draw-circle in out)
                       (draw-circle out in)
                       (draw-circle out 1/2)
                       (draw-circle out out)]
                  [(10) (draw-text "10")]
                  [(20) (draw-text "20")]))))))
      (super-new (style '(transparent)))
      (send (get-dc) set-smoothing 'aligned)
      (inherit min-width min-height stretchable-width stretchable-height)
      (min-width 48)
      (min-height 48)
      (stretchable-width #f)
      (stretchable-height #f))))
