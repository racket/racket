#lang racket/gui
(require framework 
         slideshow/pict 
         "display.rkt") 
(provide pict-canvas%) 


(define pict-canvas%
  (class canvas%      
    (init pict-builder [hover-handler #f] 
                       [click-handler #f] 
                       [overlay-builder #f] 
                       [redraw-on-resize #f])
    (inherit get-dc get-client-size refresh get-view-start)
    (define bp pict-builder) ;Builds the main pict for the canvas
    (define mh hover-handler) ;Mouse hover handler
    (define ob overlay-builder) ;Hover overlay pict builder
    (define ch click-handler) ;Mouse click handler  
    (define redraw-on-size redraw-on-resize) ;Whether we should rebuild the pict for on-size events
    
    (define redraw-overlay #f) ;Whether we should redraw the overlay pict in the canvas 
    (define redo-bitmap-on-paint #t) ;Redraw the base bitmap on paint? #f for mouse events
    (define scale-factor 1)
    
    ;;set-redraw-overlay! : bool -> void
    (define/public (set-redraw-overlay! b) 
      (set! redraw-overlay b))
    
    (define/public (set-scale-factor! s) 
      (set! scale-factor s))
    
    (define needs-redraw #f)
    (define delaying-redraw #f)
    (define cached-bitmap #f)
    (define cached-base-pict #f)
    
    (define/private (get-viewable-region) 
      (define-values (x y) (get-view-start)) 
      (define-values (w h) (get-client-size)) 
      (scale-viewable-region (viewable-region x y w h) (/ 1 scale-factor)))
    
    (define/public (redraw-everything) 
      (redraw-the-bitmap/maybe-delayed! (get-viewable-region)))
    
    ;Rebuild both the bottom (base) and overlay (top) 
    ;pict layers for the canvas
    ;;rebuild-the-pict : viewable-region -> void
    (define/private (rebuild-the-pict vregion #:only-the-overlay? [only-the-overlay? #f]) 
      (define p (cond 
                  [(or (not cached-base-pict) (not only-the-overlay?)) 
                   (define base (scale (bp vregion) scale-factor)) 
                   (set! cached-base-pict base) 
                   (if ob 
                       (pin-over base 
                                 0 
                                 0 
                                 (ob vregion scale-factor)) 
                       base)] 
                  [else (if ob 
                            (pin-over cached-base-pict 
                                      0 
                                      0 
                                      (ob vregion scale-factor))
                            cached-base-pict)]))                   
      (pict->bitmap p))
    
    ;Rebuilds the pict and stashes in a bitmap 
    ;to be drawn to the canvas later
    ;;redraw-the-bitmap : viewable-region -> void
    (define/private (redraw-the-bitmap! vregion #:only-the-overlay? [only-the-overlay? #f]) 
      (set! cached-bitmap (rebuild-the-pict vregion #:only-the-overlay? only-the-overlay?)) 
      (set! needs-redraw #f))
    
    ;;redraw-the-bitmap/maybe-delayed! : viewable-region -> void
    (define/private (redraw-the-bitmap/maybe-delayed! vregion 
                                                      #:only-the-overlay? [only-the-overlay? #f]) 
      (cond 
        [needs-redraw (redraw-the-bitmap! vregion #:only-the-overlay? only-the-overlay?)] 
        [(not delaying-redraw) 
         (new timer% [notify-callback (λ () 
                                        (set! delaying-redraw #f) 
                                        (set! needs-redraw #t)
                                        (redraw-the-bitmap/maybe-delayed! (get-viewable-region)) 
                                        (refresh))]
              [interval 100] 
              [just-once? #t]) 
         (set! delaying-redraw #t)]))
    
    ;If we haven't already introduced a 100ms delay, 
    ;add one.  If the delay's expired, rebuild the pict
    ;;on-size : uint uint -> void
    (define/override (on-size width height)
      (when redraw-on-size
        (redraw-the-bitmap/maybe-delayed! (get-viewable-region))))
    
    (define/override (on-paint)
      (define vregion (get-viewable-region)) 
      (redraw-the-bitmap/maybe-delayed! vregion)
      (define dc (get-dc))
      (when cached-bitmap
        (send dc 
              draw-bitmap 
              cached-bitmap 
              (viewable-region-x vregion) 
              (viewable-region-y vregion))))
    
    (define/override (on-event event) 
      (define vregion (get-viewable-region)) 
      (define x (+ (viewable-region-x vregion) (/ (send event get-x) scale-factor))) 
      (define y (+ (viewable-region-y vregion) (/ (send event get-y) scale-factor))) 
      (case (send event get-event-type) 
        [(motion) 
         (when mh 
           (when (mh x y vregion) ;Mouse handler returns non-false if a state change requiring redraw occurred
             (redraw-the-bitmap/maybe-delayed! vregion #:only-the-overlay? #t)))]
        [(left-up) 
         (when ch (ch x y vregion)) ;Ditto for click handler
         (redraw-the-bitmap/maybe-delayed! vregion #:only-the-overlay? #t)]))
    
    (super-new) 
    (send (get-dc) set-smoothing 'aligned)))










