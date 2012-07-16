#lang racket/gui
(require framework 
         slideshow/pict
         "display.rkt" 
         "constants.rkt")
(provide pict-canvas%
         label 
         mt-label 
         bold-label 
         mt-bold-label 
         section-header
         (struct-out event-target) 
         make-listener-table 
         add-receiver 
         post-event)

(define pict-canvas%
  (class canvas%       
    (init redraw-on-resize pict-builder hover-handler click-handler overlay-builder)
    (inherit get-dc get-client-size refresh get-view-start)
    (define bp pict-builder) ;Builds the main pict for the canvas
    (define mh hover-handler) ;Mouse hover handler
    (define ob overlay-builder) ;Hover overlay pict builder
    (define ch click-handler) ;Mouse click handler  
    (define draw-on-resize redraw-on-resize)
    (define do-logging #f)
    (define redraw-overlay #f) ;Whether we should redraw the overlay pict in the canvas 
    (define redo-bitmap-on-paint #t) ;Redraw the base bitmap on paint? #f for mouse events
    (define scale-factor 1)
    
    (define/public (set-redo-bitmap-on-paint! v) 
      (set! redo-bitmap-on-paint v))
    
    (define/public (set-do-logging! v) 
      (set! do-logging v))
    
    ;;set-build-pict! : (viewable-region -> pict) -> void
    (define/public (set-build-pict! f) 
      (set! bp f)) 
    
    ;;set-mouse-handler! : (uint uint -> segment) -> void
    (define/public (set-mouse-handler! f) 
      (set! mh f)) 
    
    ;;set-overlay-builder! : (viewable-region -> pict) -> void
    (define/public (set-overlay-builder! f) 
      (set! ob f))
    
    ;;set-click-handler! : (uint uint -> segment) -> void
    (define/public (set-click-handler! f) 
      (set! ch f))
    
    ;;set-redraw-overlay! : bool -> void
    (define/public (set-redraw-overlay! b) 
      (set! redraw-overlay b))
    
    (define/public (set-scale-factor! s) 
      (set! scale-factor s))
    
    (define the-drawer #f)
    (define img-width 0)
    (define bm #f)
    (define overlay-pict #f) 
    
    (define/private (get-viewable-region) 
      (define-values (x y) (get-view-start)) 
      (define-values (w h) (get-client-size)) 
      (scale-viewable-region (viewable-region x y w h) (/ 1 scale-factor)))
    
    (define/private (overlay-drawer dc vregion) 
      (when ob 
        (define p (ob vregion scale-factor))
        (unless (or (not p) (void? p))
          (draw-pict p
                     dc 
                     (viewable-region-x vregion)
                     (viewable-region-y vregion)))))
    
    (define/private (redo-bitmap vregion)  
      (when bp 
        (define p (scale (bp vregion) scale-factor)) 
        (set! bm (pict->bitmap p))))
    
    (define/public (redraw-everything) 
      (redo-bitmap (get-viewable-region)) 
      (refresh))
    
    (define/override (on-size width height)
      (when (or draw-on-resize 
                (not bm)) 
        (set! bm #f)
        (refresh)) 
      (set! redraw-overlay #t))
    
    (define/override (on-paint) 
      (define vregion (get-viewable-region))
      (when (or redo-bitmap-on-paint (not bm))
        (redo-bitmap vregion))
      (unless redo-bitmap-on-paint
        (set! redo-bitmap-on-paint #t))
      (when bm
        (let ([dc (get-dc)])
          (send dc draw-bitmap 
                bm 
                (viewable-region-x vregion) 
                (viewable-region-y vregion)) 
          (overlay-drawer dc vregion))))
    
    (define/override (on-event event)
      (define vregion (get-viewable-region))
      (define x (+ (viewable-region-x vregion) (/ (send event get-x) scale-factor))) 
      (define y (+ (viewable-region-y vregion) (/ (send event get-y) scale-factor)))
      (case (send event get-event-type) 
        [(motion)
         (set! redo-bitmap-on-paint #f)
         (when mh (mh x y vregion))] 
        [(left-up)
         (set! redo-bitmap-on-paint #f)
         (when ch (ch x y vregion))]) 
      (when redraw-overlay 
          (refresh)))
    
    (super-new) 
    (send (get-dc) set-smoothing 'aligned)))

(define bold-system-font
  (send the-font-list find-or-create-font	 	
        (send normal-control-font get-point-size)	 
        (send normal-control-font get-family)	 
        (send normal-control-font get-style)	 
        'bold))

(define (label p str) 
  (new message% [parent p] 
       [label str] 
       [stretchable-width #t]))

(define (mt-label p) 
  (label p ""))

(define (bold-label p str) 
  (new message% [parent p] 
       [label str] 
       [font bold-system-font]
       [stretchable-width #t])) 

(define (mt-bold-label p) 
  (bold-label p ""))

(define (section-header par name orientation) 
  (let* ([text-pict (colorize (text name) (header-forecolor))]
         [text-container (pin-over (colorize (rectangle (+ 10 (pict-width text-pict)) 
                                                        (+ 10 (pict-height text-pict))) 
                                             (header-backcolor))
                                   5 
                                   5 
                                   text-pict)]
         [c (case orientation 
             [(horizontal) 
              (let ([canv (new pict-canvas% 
                               [parent par] 
                               [redraw-on-resize #t] 
                               [pict-builder (λ (vregion)
                                               (lc-superimpose (colorize (filled-rectangle (viewable-region-width vregion) 
                                                                                           HEADER-HEIGHT) 
                                                                         (header-backcolor))
                                                               text-container))] 
                               [hover-handler #f]
                               [click-handler #f] 
                               [overlay-builder #f]
                               [min-height HEADER-HEIGHT] 
                               [stretchable-width #t] 
                               [stretchable-height #f])]) 
                canv)]
             [(vertical) 
              (let ([canv (new pict-canvas% 
                               [parent par] 
                               [redraw-on-resize #t] 
                               [pict-builder (λ (vregion)
                                             (rotate (lc-superimpose (colorize (filled-rectangle (viewable-region-height vregion) 
                                                                                                 HEADER-HEIGHT) 
                                                                               (header-backcolor))
                                                                     text-container)
                                                     -1.57079633))] 
                               [hover-handler #f]
                               [click-handler #f] 
                               [overlay-builder #f]
                               [min-width HEADER-HEIGHT] 
                               [stretchable-width #f] 
                               [stretchable-height #t])]) 
                canv)])]) 
    c))

;Events
;receiver : any 
;handler : (any -> void)
(struct event-target (receiver handler) #:transparent)

(define (make-listener-table) (make-hash))

(define (add-receiver table evt-name object handler) 
  (hash-update! table 
                evt-name 
                (λ (old) 
                  (cons (event-target object handler) old)) 
                (list (event-target object handler))))

(define (post-event table name sender arg) 
    (let ([targets (hash-ref table name)]) 
      (for ([target (in-list targets)]) 
        (let ([receiver (event-target-receiver target)] 
              [handler (event-target-handler target)]) 
          (unless (eq? receiver sender) 
            (handler arg))))))
                                  
    
    
    
    
    
    