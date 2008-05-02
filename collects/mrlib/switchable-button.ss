#lang scheme/base
(require scheme/gui/base
         scheme/class)

(require string-constants/string-constant)
(provide switchable-button%)
(define gap 2)
(define margin 2)
(define w-circle-space 6)
(define h-circle-space 6)

(define switchable-button%
  (class canvas%
    (init-field label bitmap callback)
    
    (inherit get-dc min-width min-height get-client-size refresh)
    
    (define down? #f)
    (define in? #f)
    (define/override (on-event evt)
      (cond
        [(send evt button-down? 'left)
         (set! down? #t)
         (set! in? #t)
         (refresh)]
        [(send evt button-up? 'left)
         (set! down? #f)
         (update-in evt)
         (refresh)
         (when in?
           (callback this))]
        [(send evt entering?)
         (set! in? #t)
         (refresh)]
        [(send evt leaving?)
         (set! in? #f)
         (refresh)]
        [(send evt moving?)
         (update-in evt)]))
    
    (define/private (update-in evt)
      (let-values ([(cw ch) (get-client-size)])
        (let ([new-in?
               (and (<= 0 (send evt get-x) cw)
                    (<= 0 (send evt get-y) ch))])
          (unless (equal? new-in? in?)
            (set! in? new-in?)
            (refresh)))))
    
    (define horizontal? #t)
    (define/override (on-paint)
      (let ([dc (get-dc)])
        (let-values ([(cw ch) (get-client-size)])
          (let ([alpha (send dc get-alpha)]
                [pen (send dc get-pen)]
                [brush (send dc get-brush)])
            
            (send dc set-alpha 
                  (cond
                    [in? (if down?
                             .5
                             .2)]
                    [else 0]))
            (send dc set-pen "black" 1 'transparent)
            (send dc set-brush "black" 'solid)
            (send dc draw-rounded-rectangle 
                  margin
                  margin 
                  (- cw margin margin)
                  (- ch margin margin))
            (send dc set-alpha alpha)
	    (send dc set-font normal-control-font)
            
            (cond
              [horizontal? 
               (let-values ([(tw th _1 _2) (send dc get-text-extent label)])
                 (let ([text-start (+ (/ cw 2) (- (/ tw 2)) (- (/ (send bitmap get-width) 2)))])
                   (send dc draw-text label text-start (- (/ ch 2) (/ th 2)))
                   (draw-the-bitmap (+ text-start tw gap) (- (/ ch 2) (/ (send bitmap get-height) 2)))))]
              [else
               (draw-the-bitmap (- (/ cw 2) (/ (send bitmap get-width) 2))
                                (- (/ ch 2) (/ (send bitmap get-height) 2)))])
            (send dc set-pen pen)
            (send dc set-alpha alpha)
            (send dc set-brush brush)))))
    
    (define/private (draw-the-bitmap x y)
      (send (get-dc)
            draw-bitmap
            bitmap
            x y
            'solid
            (send the-color-database find-color "black")
            (send bitmap get-loaded-mask)))
    
    (define/public (set-label-visible h?) 
      (unless (equal? horizontal? h?)
        (set! horizontal? h?)
        (update-sizes)
        (refresh)))
    
    (define/private (update-sizes)
      (let ([dc (get-dc)])
        (cond
          [horizontal?
           (let-values ([(w h _1 _2) (send dc get-text-extent label normal-control-font)])
             (do-w/h (+ w gap (send bitmap get-width))
                     (max h (send bitmap get-height))))]
          [else
           (do-w/h (send bitmap get-width)
                   (send bitmap get-height))])))
    
    (define/private (do-w/h w h)
      (let ([w (floor (inexact->exact w))]
            [h (floor (inexact->exact h))])
        (min-width (+ w w-circle-space margin margin))
        (min-height (+ h h-circle-space margin margin))))
    
    (super-new [style '(transparent)])
    (send (get-dc) set-smoothing 'aligned)
    
    (inherit stretchable-width stretchable-height)
    (stretchable-width #f)
    (stretchable-height #f)
    (inherit get-graphical-min-size)
    (update-sizes)))

#;
(begin
  (define f (new frame% [label ""]))
  (define p (new horizontal-panel% [parent f] [alignment '(right top)]))
  
  (define label (string-constant execute-button-label))
  (define bitmap (make-object bitmap% (build-path (collection-path "icons") "run.png") 'png/mask))

  (define b1 (new switchable-button% [parent p] [label label] [bitmap bitmap] [callback void]))
  (define b2 (new switchable-button% [parent p] [label label] [bitmap bitmap] [callback void]))
  (new button% [parent p] [stretchable-width #t] [label "b"])
  (define swap-button
    (new button% 
         [parent f] 
         [label "swap"]
         [callback
          (let ([state #t])
            (λ (a b)
              (set! state (not state))
              (send b1 set-orientation state)
              (send b2 set-orientation state)
              '(send p set-orientation state)))]))
  (send f show #t)) 