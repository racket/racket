#lang racket/base
(require texpict/mrpict
         racket/draw
         racket/class
         racket/contract)

(provide/contract
 [make-arrow-pict
  (-> string?
      (or/c 'curvy 'straight 'straight-double 'map)
      symbol?
      number?
      (-> pict?))])

(define (make-arrow-pict sample-str style font-family font-size)
  (let ([ans #f])
    (λ ()
      (or ans
          (begin
            (set! ans (raw-make-arrow-pict sample-str style font-family font-size))
            ans)))))

(define (raw-make-arrow-pict sample-str style font-family font-size)
  (let-values ([(w h d a) (send (dc-for-text-size) get-text-extent sample-str
                                (send the-font-list
                                      find-or-create-font
                                      font-size
                                      font-family
                                      'normal
                                      'normal))])
    (let* ([pen-width-factor 0.042] ;; factor of the height to get the pen width
           [line-pos (+ a (/ (- h a) 2))]
           [head-width (/ w 5)]
           [head-height (* (- h a) 9/16)]
           [path (and (eq? style 'curvy)
                      (let* ([b (blank w (- h a d) d)]
                             [a-sz (* head-width 1)]
                             [p (new dc-path%)]
                             [inc (/ (- w head-width) 3)])
                        (send p move-to 0 line-pos)
                        (let ([y (- line-pos (/ a-sz 2))])
                          (send p curve-to 
                                0 line-pos
                                (/ inc 2) y
                                inc y)
                          (let ([y2 (+ line-pos (/ a-sz 2))])
                            (send p curve-to
                                  (* 3/2 inc) y
                                  (* 3/2 inc) y2
                                  (* 2 inc) y2)
                            (send p curve-to
                                  (* 5/2 inc) y2
                                  (* 5/2 inc) line-pos
                                  (* 3 inc) line-pos)
                            (send p line-to w line-pos)))
                        p))])
      (inset
       (dc
        (λ (dc dx dy)
          (let ([old-pen (send dc get-pen)]
                [old-brush (send dc get-brush)]
                [old-smoothing (send dc get-smoothing)])
            (send dc set-smoothing 'smoothed)
            (send dc set-pen (send old-pen get-color) (* h pen-width-factor) 'solid)
            
            ;; main line of arrow
            (case style
              [(curvy)
               (send dc draw-path path dx dy)]
              [(map)
               (send dc draw-line
                     dx
                     (- (+ dy line-pos) (/ head-height 2))
                     dx
                     (+ (+ dy line-pos) (/ head-height 2)))
               (send dc draw-line 
                     dx
                     (+ dy line-pos)
                     (+ dx w)
                     (+ dy line-pos))]
              [(straight)
               (send dc draw-line 
                     dx
                     (+ dy line-pos)
                     (+ dx w)
                     (+ dy line-pos))]
              [(straight-double)
               (send dc draw-line 
                     dx 
                     (+ dy line-pos -1)
                     (+ dx w -2)
                     (+ dy line-pos -1))
               (send dc draw-line 
                     dx
                     (+ dy line-pos 1)
                     (+ dx w -2)
                     (+ dy line-pos 1))])
            
            (unless (eq? style 'straight-double)
              ;; when a single line arrow, make the arrow head's lines a tiny bit thinner
              (send dc set-pen (send old-pen get-color) (* h pen-width-factor .8) 'solid))
            
            ;; upper line of arrowhead
            (send dc draw-spline
                  (+ dx w)
                  (+ dy line-pos)
                  
                  (+ dx w (- head-width) (* head-width 1/5))
                  (+ dy line-pos (- (* head-height 4/16)))
                  
                  (+ dx w (- head-width))
                  (+ dy line-pos (- (/ head-height 2))))
            
            ;; lower line of arrowhead
            (send dc draw-spline
                  (+ dx w)
                  (+ dy line-pos)
                  
                  (+ dx w (- head-width) (* head-width 1/5))
                  (+ dy line-pos (+ (* head-height 4/16)))
                  
                  (+ dx w (- head-width))
                  (+ dy line-pos (+ (/ head-height 2))))
            
            (send dc set-smoothing old-smoothing)
            (send dc set-pen old-pen)
            (send dc set-brush old-brush)))
        w h (- h d) d)
       2 0))))
