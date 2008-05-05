#lang scheme/base
(require scheme/gui/base
         scheme/class)

(provide switchable-button%)
(define gap 2)
(define margin 2)
(define w-circle-space 6)
(define h-circle-space 6)

(define switchable-button%
  (class canvas%
    (init-field label 
                bitmap
                callback
                [alternate-bitmap bitmap])
    
    (define disable-bitmap (make-dull-mask bitmap))
    
    (define alternate-disable-bitmap
      (if (eq? bitmap alternate-bitmap)
          disable-bitmap
          (make-dull-mask alternate-bitmap)))
    
    (inherit get-dc min-width min-height get-client-size refresh)
    
    (define down? #f)
    (define in? #f)
    (define disabled? #f)
    
    (define/override (enable e?)
      (unless (equal? disabled? (not e?))
        (set! disabled? (not e?))
        (refresh)))
    (define/override (is-enabled?) (not disabled?))
    
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
         (when (and in?
                    (not disabled?))
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
    
    (define with-label? #t)
    (define/override (on-paint)
      (let ([dc (get-dc)])
        (let-values ([(cw ch) (get-client-size)])
          (let ([alpha (send dc get-alpha)]
                [pen (send dc get-pen)]
                [brush (send dc get-brush)])
            
            (send dc set-alpha 
                  (cond
                    [disabled? 0]
                    [in? (if down?
                             .5
                             .2)]
                    [else 0]))
            (send dc set-pen "black" 1 'transparent)
            (send dc set-brush "black" 'solid)
            (send dc draw-rounded-rectangle 
                  margin
                  margin 
                  (max 0 (- cw margin margin))
                  (max 0 (- ch margin margin)))
            (send dc set-alpha alpha)
	    (send dc set-font normal-control-font)
            
            (when disabled?
              (send dc set-alpha .5))
            
            (cond
              [with-label? 
               (let-values ([(tw th _1 _2) (send dc get-text-extent label)])
                 (let ([text-start (+ (/ cw 2) (- (/ tw 2)) (- (/ (send bitmap get-width) 2)))])
                   (send dc draw-text label text-start (- (/ ch 2) (/ th 2)))
                   (draw-the-bitmap (+ text-start tw gap) (- (/ ch 2) (/ (send bitmap get-height) 2)))))]
              [else
               (draw-the-bitmap (- (/ cw 2) (/ (send (if with-label? bitmap alternate-bitmap) get-width) 2))
                                (- (/ ch 2) (/ (send (if with-label? bitmap alternate-bitmap) get-height) 2)))])
            
            
            #;
            (when disabled?
              (send dc set-alpha .5)
              (send dc set-pen "white" 1 'transparent)
              (send dc set-brush "white" 'solid)
              (send dc draw-rectangle 0 0 cw ch))
            
            (send dc set-pen pen)
            (send dc set-alpha alpha)
            (send dc set-brush brush)))))
            
    (define/private (draw-the-bitmap x y)
      (let ([bm (if with-label? bitmap alternate-bitmap)])
        (send (get-dc)
              draw-bitmap
              bm
              x y
              'solid
              (send the-color-database find-color "black")
              (if disabled?
                  (if with-label? disable-bitmap alternate-disable-bitmap)
                  (send bm get-loaded-mask)))))
    
    (define/public (set-label-visible h?)
      (unless (equal? with-label? h?)
        (set! with-label? h?)
        (update-sizes)
        (refresh)))
    
    (define/private (update-sizes)
      (let ([dc (get-dc)])
        (cond
          [with-label?
           (let-values ([(w h _1 _2) (send dc get-text-extent label normal-control-font)])
             (do-w/h (+ w gap (send bitmap get-width))
                     (max h (send bitmap get-height))))]
          [else
           (do-w/h (send alternate-bitmap get-width)
                   (send alternate-bitmap get-height))])))
    
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

(define (make-dull-mask bitmap)
  (let ([alpha-bm (send bitmap get-loaded-mask)])
    (and alpha-bm
         (let* ([w (send alpha-bm get-width)]
                [h (send alpha-bm get-height)]
                [disable-bm (make-object bitmap% w h)]
                [pixels (make-bytes (* 4 w h))]
                [bdc (make-object bitmap-dc% alpha-bm)])
           (send bdc get-argb-pixels 0 0 w h pixels)
           (let loop ([i 0])
             (when (< i (* 4 w h))
               (bytes-set! pixels i (- 255 (quotient (- 255 (bytes-ref pixels i)) 2)))
               (loop (+ i 1))))
           (send bdc set-bitmap disable-bm)
           (send bdc set-argb-pixels 0 0 w h pixels)
           (send bdc set-bitmap #f)
           disable-bm))))

#;
(begin
  (define f (new frame% [label ""]))
  (define p (new horizontal-panel% [parent f] [alignment '(right top)]))
  
  (define label "Run")
  (define bitmap (make-object bitmap% (build-path (collection-path "icons") "run.png") 'png/mask))
  (define foot (make-object bitmap% (build-path (collection-path "icons") "foot.png") 'png/mask))
  (define foot-up (make-object bitmap% (build-path (collection-path "icons") "foot-up.png") 'png/mask))
  
  (define b1 (new switchable-button% [parent p] [label label] [bitmap bitmap] [callback void]))
  (define b2 (new switchable-button% [parent p] [label label] [bitmap bitmap] [callback void]))
  (define b3 (new switchable-button% [parent p] [label "Step"] [bitmap foot] [alternate-bitmap foot-up] [callback void]))
  (define sb (new button% [parent p] [stretchable-width #t] [label "b"]))
  (define swap-button
    (new button% 
         [parent f] 
         [label "swap"]
         [callback
          (let ([state #t])
            (λ (a b)
              (set! state (not state))
              (send b1 set-label-visible state)
              (send b2 set-label-visible state)
              (send b3 set-label-visible state)))]))
  (define disable-button
    (new button% 
         [parent f] 
         [label "disable"]
         [callback
          (λ (a b)
            (send sb enable (not (send sb is-enabled?)))
            (send b1 enable (not (send b1 is-enabled?))))]))
  (send f show #t))