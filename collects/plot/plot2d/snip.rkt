#lang racket/base

(require racket/gui/base racket/class racket/match racket/list racket/math unstable/parameter-group
         "../common/snip.rkt"
         "../common/plot-device.rkt"
         "../common/math.rkt"
         "../common/format.rkt"
         "../common/ticks.rkt"
         "../common/parameters.rkt")

(provide 2d-plot-snip% make-2d-plot-snip)

(define zoom-delay 16)  ; about 60 fps (just over)

(define 2d-plot-snip%
  (class plot-snip%
    (init bm saved-plot-parameters)
    (init-field make-plot plot-bounds-rect area-bounds-rect area-bounds->plot-bounds)
    
    (inherit set-bitmap get-bitmap
             get-saved-plot-parameters
             refresh set-message reset-message-timeout get-admin)
    
    (super-make-object bm saved-plot-parameters)
    
    (define/override (copy)
      (make-object this%
        (get-bitmap) (get-saved-plot-parameters)
        make-plot plot-bounds-rect area-bounds-rect area-bounds->plot-bounds))
    
    (define left-click-x 0)
    (define left-click-y 0)
    (define left-drag-x 0)
    (define left-drag-y 0)
    
    (define plot-bounds-rects empty)
    
    (define (get-area-bounds-rect)
      (rect-meet area-bounds-rect
                 (rect-inexact->exact
                  (vector (ivl left-click-x left-drag-x) (ivl left-click-y left-drag-y)))))
    
    (define dragging? #f)
    (define left-down? #f)  ; only #t if left-down happened on this snip
    (define zoom-timer #f)
    
    (define (set-zoom-timer)
      (when (not zoom-timer)
        (set! zoom-timer (make-object timer% (λ ()
                                               (set! zoom-timer #f)
                                               (refresh))
                           zoom-delay #t))))
    
    (define zoomed? #f)
    (define unzoomed? #f)
    (define (set-click-message)
      (cond [(and zoomed? unzoomed?)  (void)]
            [zoomed?  (set-message "Click to unzoom once")]
            [unzoomed?  (set-message "Click and drag to zoom")]
            [else  (set-message "Click and drag to zoom\n Click to unzoom once")]))
    
    (define (update-plot new-plot-bounds-rect)
      (define-values (new-bm new-area-bounds-rect new-area-bounds->plot-bounds)
        (make-plot new-plot-bounds-rect))
      (set! plot-bounds-rect new-plot-bounds-rect)
      (set! area-bounds-rect new-area-bounds-rect)
      (set! area-bounds->plot-bounds new-area-bounds->plot-bounds)
      (set-bitmap new-bm))
    
    (define/override (on-event dc x y editorx editory evt)
      (define evt-type (send evt get-event-type))
      (define mouse-x (- (send evt get-x) x))
      (define mouse-y (- (send evt get-y) y))
      (case evt-type
        [(left-down)  (set! left-click-x mouse-x)
                      (set! left-click-y mouse-y)
                      (set! left-drag-x mouse-x)
                      (set! left-drag-y mouse-y)
                      (set! dragging? #f)
                      (set! left-down? #t)
                      (set-message #f)
                      (set-zoom-timer)]
        [(left-up)    (set! left-drag-x mouse-x)
                      (set! left-drag-y mouse-y)
                      (set! left-down? #f)
                      (cond [dragging?
                             (set! dragging? #f)
                             (define new-rect (area-bounds->plot-bounds (get-area-bounds-rect)))
                             (cond [(and (rect-rational? new-rect) (not (rect-zero-area? new-rect)))
                                    #;(printf "~a: new-plot-bounds-rect = ~v~n"
                                            (current-milliseconds) new-rect)
                                    (set! plot-bounds-rects (cons plot-bounds-rect plot-bounds-rects))
                                    (update-plot new-rect)
                                    (set! zoomed? #t)]
                                   [else  (refresh)])]
                            [(not (empty? plot-bounds-rects))
                             (define new-rect (first plot-bounds-rects))
                             (set! plot-bounds-rects (rest plot-bounds-rects))
                             (update-plot new-rect)
                             (set! unzoomed? #t)])]
        [(motion)     (cond [left-down?  ; not event's left-down: only #t if clicked on snip
                             (when (not (and (= left-drag-x mouse-x)
                                             (= left-drag-y mouse-y)))
                               (set! left-drag-x mouse-x)
                               (set! left-drag-y mouse-y)
                               (set! dragging? #t)
                               (set-zoom-timer))]
                            [(and (not (send evt get-left-down))
                                  (<= 0 mouse-x (send (get-bitmap) get-width))
                                  (<= 0 mouse-y (send (get-bitmap) get-height)))
                             (set-click-message)])]))
    
    (define/override (draw dc dc-x-min dc-y-min left top right bottom dx dy draw-caret)
      ;(printf "~a: drawing~n" (current-milliseconds))
      (super draw dc dc-x-min dc-y-min left top right bottom dx dy draw-caret)
      (when dragging?
        (define new-rect (get-area-bounds-rect))
        (when (and (rect-rational? new-rect) (not (rect-zero-area? new-rect)))
          (define width (send (get-bitmap) get-width))
          (define height (send (get-bitmap) get-height))
          
          (define pd (make-object plot-device% dc dc-x-min dc-y-min width height))
          (send pd reset-drawing-params #f)
          
          (define select-color (get-highlight-background-color))
          (define draw-rect (rect-translate new-rect (vector dc-x-min dc-y-min)))
          
          ;; inside of selection box
          (send pd set-pen select-color 1 'transparent)
          (send pd set-brush select-color 'solid)
          (send pd set-alpha 1/8)
          (send pd draw-rect draw-rect)
          
          ;; border of selection box
          (send pd set-minor-pen)
          (send pd set-brush select-color 'transparent)
          (send pd set-alpha 3/4)
          (send pd draw-rect draw-rect)
          
          ;; side labels
          (parameterize/group ([plot-parameters  (get-saved-plot-parameters)])
            (match-define (vector (ivl x-min x-max) (ivl y-min y-max)) plot-bounds-rect)
            (match-define (vector (ivl new-area-x-min new-area-x-max)
                                  (ivl new-area-y-min new-area-y-max))
              new-rect)
            (match-define (vector (ivl new-x-min new-x-max) (ivl new-y-min new-y-max))
              (area-bounds->plot-bounds new-rect))
            
            (define new-area-x-mid (* 1/2 (+ new-area-x-min new-area-x-max)))
            (define new-area-y-mid (* 1/2 (+ new-area-y-min new-area-y-max)))
            
            ;; format new-x-min and new-x-max
            (match-define (list new-x-min-str new-x-max-str)
              ((ticks-format (plot-x-ticks))
               x-min x-max (list (pre-tick new-x-min #t) (pre-tick new-x-max #t))))
            ;; draw new-x-min
            (send pd draw-text new-x-min-str
                  (vector (+ dc-x-min new-area-x-min) (+ dc-y-min new-area-y-mid))
                  'center (* 1/2 pi) #:outline? #t)
            ;; draw new-x-max
            (send pd draw-text new-x-max-str
                  (vector (+ dc-x-min new-area-x-max) (+ dc-y-min new-area-y-mid))
                  'center (* 1/2 pi) #:outline? #t)
            
            ;; format new-y-min and new-y-max
            (match-define (list new-y-min-str new-y-max-str)
              ((ticks-format (plot-y-ticks))
               y-min y-max (list (pre-tick new-y-min #t) (pre-tick new-y-max #t))))
            ;; draw new-y-min
            (send pd draw-text new-y-min-str
                  (vector (+ dc-x-min new-area-x-mid) (+ dc-y-min new-area-y-max))
                  'center #:outline? #t)
            ;; draw new-y-max
            (send pd draw-text new-y-max-str
                  (vector (+ dc-x-min new-area-x-mid) (+ dc-y-min new-area-y-min))
                  'center #:outline? #t))
          
          (send pd restore-drawing-params))))
    
    (define cross-cursor (make-object cursor% 'cross))
    (define/override (adjust-cursor dc x y editorx editory evt) cross-cursor)
    
    (send this set-flags (list* 'handles-events 'handles-all-mouse-events (send this get-flags)))))

(define (make-2d-plot-snip bm saved-plot-parameters
                           make-plot plot-bounds-rect area-bounds-rect area-bounds->plot-bounds)
  (make-object 2d-plot-snip%
    bm saved-plot-parameters
    make-plot plot-bounds-rect area-bounds-rect area-bounds->plot-bounds))
