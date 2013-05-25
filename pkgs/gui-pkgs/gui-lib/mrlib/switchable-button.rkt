#lang racket/base
(require racket/gui/base
         racket/class)

(provide switchable-button%)
(define gap 4) ;; space between the text and the icon
(define margin 2)
(define w-circle-space 6)
(define h-circle-space 6)

;; extra space outside the bitmap, 
;; but inside the mouse highlighting (on the right)
(define rhs-pad 2) 

(define half-gray (make-object color% 127 127 127))
(define one-fifth-gray (make-object color% 200 200 200))

(define yellow-message%
  (class canvas%
    (init-field label)
    
    (define/override (on-paint)
      (let ([dc (get-dc)])
        (let ([pen (send dc get-pen)]
              [brush (send dc get-brush)]
              [font (send dc get-font)]
              [yellow (make-object color% 255 255 200)])
          
          (send dc set-pen yellow 1 'transparent)
          (send dc set-brush yellow 'solid)
          (let-values ([(cw ch) (get-client-size)])
            (send dc draw-rectangle 0 0 cw ch)
          
            (send dc set-font small-control-font)
            
            (let-values ([(tw th _1 _2) (send dc get-text-extent label)])
              (send dc draw-text 
                    label
                    (- (/ cw 2) (/ tw 2))
                    (- (/ ch 2) (/ th 2)))))
          
          (send dc set-pen pen)
          (send dc set-brush brush)
          (send dc set-font font))))
    
    (define/override (on-event evt)
      (send (get-top-level-window) show #f))
        
    (inherit stretchable-width stretchable-height
             min-width min-height
             get-client-size get-dc
             get-top-level-window)
    (super-new)
    (let-values ([(tw th _1 _2) (send (get-dc) get-text-extent label small-control-font)])
      (min-width (floor (inexact->exact (+ tw 4))))
      (min-height (floor (inexact->exact (+ th 4)))))))

(define switchable-button%
  (class canvas%
    (init-field label 
                bitmap
                callback
                [alternate-bitmap bitmap]
                [vertical-tight? #f]
                [min-width-includes-label? #f])
    
    (define/public (get-button-label) label)
    
    (when (and (is-a? label bitmap%)
               (not (send label ok?)))
      (error 'switchable-button% "label bitmap is not ok?"))
    
    (define/override (get-label) label)
    
    (define disable-bitmap (make-dull-mask bitmap))
    
    (define alternate-disable-bitmap
      (if (eq? bitmap alternate-bitmap)
          disable-bitmap
          (make-dull-mask alternate-bitmap)))
    
    (inherit get-dc min-width min-height get-client-size refresh
	     client->screen)
    
    (define down? #f)
    (define in? #f)
    (define disabled? #f)
    (define with-label? (string? label))
  
    (define/override (enable e?)
      (unless (equal? disabled? (not e?))
        (set! disabled? (not e?))
        (set! down? #f)
        (set! in? #f)
        (refresh)))
    (define/override (is-enabled?) (not disabled?))

    (define/override (on-superwindow-show show?)
      (unless show?
        (set! in? #f)
        (set! down? #f)
                (update-float #f)
        (refresh))
      (super on-superwindow-show show?))
    
    (define/override (on-event evt)
      (cond
        [(send evt button-down? 'left)
         (set! down? #t)
         (set! in? #t)
         (refresh)
         (update-float #t)]
        [(send evt button-up? 'left)
         (set! down? #f)
         (update-in evt)
         (refresh)
         (when (and in?
                    (not disabled?))
           (update-float #f)
           (callback this))]
        [(send evt entering?)
         (set! in? #t)
         (update-float #t)
         (unless disabled?
           (refresh))]
        [(send evt leaving?)
         (set! in? #f)
         (update-float #f)
         (unless disabled?
           (refresh))]
        [(send evt moving?)
         (update-in evt)]))

    (define/public (command)
      (callback this)
      (void))
    
    (define float-window #f)
    (inherit get-width get-height)
    (define timer (new timer% 
                       [just-once? #t] 
                       [notify-callback
                        (λ ()
                          (unless with-label?
                            (unless (equal? (send float-window is-shown?) in?)
                              (send float-window show in?)))
                          (set! timer-running? #f))]))
    (define timer-running? #f)
    
    (define/private (update-float new-value?)
      (when label
        (cond
          [with-label?
           (when float-window
             (send float-window show #f))]
          [else
           (unless (and float-window
                        (equal? new-value? (send float-window is-shown?)))
             (cond
               [new-value?
                (unless float-window
                  (set! float-window (new frame% 
                                          [label ""]
                                          [style '(no-caption no-resize-border float)]
                                          [stretchable-width #f]
                                          [stretchable-height #f]))
                  (new yellow-message% [parent float-window] [label (or label "")]))
                
                (send float-window reflow-container)
                
                ;; position the floating window
                (let-values ([(dw dh) (get-display-size)]
                             [(x y) (client->screen (floor (get-width))
                                                    (floor
                                                     (- (/ (get-height) 2)
                                                        (/ (send float-window get-height) 2))))]
                             [(dx dy) (get-display-left-top-inset)])
                  (let ([rhs-x (- x dx)]
                        [rhs-y (- y dy)])
                    (cond
                      [(< (+ rhs-x (send float-window get-width)) dw)
                       (send float-window move rhs-x rhs-y)]
                      [else
                       (send float-window move
                             (- rhs-x (send float-window get-width) (get-width))
                             rhs-y)])))
                (unless timer-running?
                  (set! timer-running? #t)
                  (send timer start 500 #t))]
               [else
                (when float-window
                  (send float-window show #f))]))])))
    
    (define/private (update-in evt)
      (let-values ([(cw ch) (get-client-size)])
        (let ([new-in?
               (and (<= 0 (send evt get-x) cw)
                    (<= 0 (send evt get-y) ch))])
          (unless (equal? new-in? in?)
            (set! in? new-in?)
            (refresh))
          (update-float new-in?))))
    
    (define/override (on-paint)
      (let ([dc (get-dc)])
        (let-values ([(cw ch) (get-client-size)])
          (let ([alpha (send dc get-alpha)]
                [pen (send dc get-pen)]
                [brush (send dc get-brush)])
            
            ;; Draw background. Use alpha blending if it can work,
            ;;  otherwise fall back to a suitable color.
            (let ([color (cond
                           [disabled? #f]
                           [in? (if (eq? (send dc get-smoothing) 'aligned)
                                    (if down? 0.5 0.2)
                                    (if down?
                                        half-gray
                                        one-fifth-gray))]
                           [else #f])])
              (when color
                (send dc set-pen "black" 1 'transparent)
                (send dc set-brush (if (number? color) "black" color) 'solid)
                (when (number? color)
                  (send dc set-alpha color))
                (send dc draw-rounded-rectangle 
                      margin
                      margin 
                      (max 0 (- cw margin margin))
                      (max 0 (- ch margin margin)))
                (when (number? color)
                  (send dc set-alpha alpha))))
            
            (send dc set-font normal-control-font)
            
            (when disabled?
              (send dc set-alpha .5))
            
            (cond
              [with-label?
               (cond
                 [(<= cw (get-small-width))
                  (draw-the-bitmap (- (/ cw 2) (/ (send bitmap get-width) 2)) 
                                   (- (/ ch 2) (/ (send bitmap get-height) 2)))]
                 [else
                  (define-values (tw th _1 _2) (send dc get-text-extent label))
                  (define text-start (+ (/ cw 2) 
                                        (- (/ tw 2))
                                        (- (/ (send bitmap get-width) 2))
                                        (- rhs-pad)))
                  (send dc draw-text label text-start (- (/ ch 2) (/ th 2)))
                  (draw-the-bitmap (+ text-start tw gap) 
                                   (- (/ ch 2) (/ (send bitmap get-height) 2)))])]
              [else
               (draw-the-bitmap 
                (- (/ cw 2) 
                   (/ (send (if with-label? bitmap alternate-bitmap) get-width) 
                      2))
                (- (/ ch 2)
                   (/ (send (if with-label? bitmap alternate-bitmap) get-height)
                      2)))])
            
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
    
    (define/public (set-label-visible in-h?)
      (define h? (and in-h? #t))
      (unless (equal? with-label? h?)
        (set! with-label? h?)
        (update-sizes)
        (update-float (and with-label? in?))
        (refresh)))
    (define/public (get-label-visible) with-label?)
    
    (define/private (update-sizes)
      (define dc (get-dc))
      (define-values (tw th _1 _2) (send dc get-text-extent label normal-control-font))
      (define h 
        (inexact->exact 
         (floor
          (+ (max th
                  (send alternate-bitmap get-height)
                  (send bitmap get-height))
             h-circle-space margin margin 
             (if vertical-tight? -6 0)))))
      (cond
        [with-label?
         (cond
           [min-width-includes-label?
            (min-width (get-large-width))]
           [else
            (min-width (get-small-width))])
         (min-height h)]
        [else
         (min-width (get-without-label-small-width))
         (min-height h)]))
    
    (define/public (get-large-width)
      (define dc (get-dc))
      (define-values (tw th _1 _2) (send dc get-text-extent label normal-control-font))
      (inexact->exact
       (floor
        (+ (+ tw gap (send bitmap get-width) rhs-pad)
           w-circle-space
           margin
           margin))))
    
    (define/public (get-without-label-small-width)
      (inexact->exact
       (floor
        (+ (send alternate-bitmap get-width)
           w-circle-space
           margin
           margin))))
    
    (define/public (get-small-width)
      (inexact->exact
       (floor
        (+ (send bitmap get-width)
           w-circle-space
           margin
           margin))))
    
    (super-new [style '(transparent no-focus)])
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
  (define vp (new vertical-pane% [parent f]))
  (define p (new horizontal-panel% [parent vp] [alignment '(right top)]))
  
  (define label "Run")
  (define bitmap (make-object bitmap% (build-path (collection-path "icons") "run.png") 'png/mask))
  (define foot (make-object bitmap% (build-path (collection-path "icons") "foot.png") 'png/mask))
  (define foot-up 
    (make-object bitmap% (build-path (collection-path "icons") "foot-up.png") 'png/mask))
  
  (define b1 (new switchable-button% [parent p] [label label] [bitmap bitmap] [callback void]))
  (define b2 (new switchable-button% [parent p] [label label] [bitmap bitmap] [callback void]))
  (define b3 (new switchable-button% [parent p] [label "Step"] [bitmap foot]
                  [alternate-bitmap foot-up] 
                  [callback void]))
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
