#lang racket/base

(require racket/gui/base racket/class racket/list unstable/parameter-group
         "math.rkt"
         "parameters.rkt"
         "plot-device.rkt")

(provide plot-snip%)

(define message-timeout 2000)

(define plot-snip%
  (class image-snip%
    (init bm)
    (init-field saved-plot-parameters)
    (inherit set-bitmap get-bitmap get-admin)
    
    (super-make-object bm)
    
    (define/override (copy) (make-object this% (get-bitmap) saved-plot-parameters))
    
    (define/public (get-saved-plot-parameters) saved-plot-parameters)
    
    (define x-mid (* 1/2 (send bm get-width)))
    (define y-mid (* 1/2 (send bm get-height)))
    
    (define/public (set-message-center new-x-mid new-y-mid)
      (set! x-mid new-x-mid)
      (set! y-mid new-y-mid))
    
    (define/public (refresh)
      ;(printf "~a: refresh~n" (current-milliseconds))
      (set-bitmap (get-bitmap)))
    
    (define message #f)
    (define message-timer (make-object timer% (λ () (stop-message))))
    
    (define/public (stop-message)
      ;(printf "~a: stop-message~n" (current-milliseconds))
      (send message-timer stop)
      (set! message #f)
      (refresh))
    
    (define/public (reset-message-timeout)
      (send message-timer start message-timeout #t))
    
    (define/public (set-message msg #:refresh? [refresh? #t])
      (define refresh? (and refresh? (not (equal? msg message))))
      (set! message msg)
      (reset-message-timeout)
      (when refresh? (refresh)))
    
    (define (draw-message dc dc-x-min dc-y-min)
      (define bm (get-bitmap))
      (define width (send bm get-width))
      (define height (send bm get-height))
      
      (define pd (make-object plot-device% dc dc-x-min dc-y-min width height))
      (send pd reset-drawing-params #f)
      
      (define lines (map (λ (line) (format " ~a " line)) (regexp-split "\n" message)))
      
      (define-values (_1 char-height baseline _2) (send pd get-text-extent (first lines)))
      (define line-widths (map (λ (line) (send pd get-text-width line)) lines))
      
      (define box-x-size (apply max line-widths))
      (define box-y-size (+ baseline (* (length lines) (+ char-height baseline))))
      (define box-x-min (- x-mid (* 1/2 box-x-size)))
      (define box-x-max (+ x-mid (* 1/2 box-x-size)))
      (define box-y-min (- y-mid (* 1/2 box-y-size)))
      (define box-y-max (+ y-mid (* 1/2 box-y-size)))
      
      (define box-rect (vector (ivl box-x-min box-x-max) (ivl box-y-min box-y-max)))
      
      ;; inside selection
      (send pd set-pen (plot-foreground) 1 'transparent)
      (send pd set-brush (plot-background) 'solid)
      (send pd set-alpha 1/4)
      (send pd draw-rect box-rect)
      
      ;; selection border
      (send pd set-minor-pen)
      (send pd set-brush (plot-background) 'transparent)
      (send pd set-alpha 3/4)
      (send pd draw-rect box-rect)
      
      (send pd set-alpha 1)
      (for ([line  (in-list lines)] [i  (in-naturals)])
        (send pd draw-text
              line (vector box-x-min (+ box-y-min baseline (* i (+ char-height baseline))))
              'top-left #:outline? #t))
      (send pd restore-drawing-params))
    
    (define/override (draw dc x y left top right bottom dx dy draw-caret)
      ;(printf "~a: drawing~n" (current-milliseconds))
      (super draw dc x y left top right bottom dx dy draw-caret)
      ;(send dc draw-bitmap-section bm x y 0 0 width height)
      (when message
        (parameterize/group ([plot-parameters  saved-plot-parameters])
          (draw-message dc x y))))
    
    (define left-down-admin #f)
    (define/public (get-left-down-here?)
      (eq? (get-admin) left-down-admin))
    
    (send this set-flags (list* 'handles-events 'handles-all-mouse-events (send this get-flags)))
    
    (define/override (on-event dc x y editorx editory evt)
      (define admin (get-admin))
      (define editor (send admin get-editor))
      (case (send evt get-event-type)
        ;; The editor gives ownership to the snip as soon as the mouse left-clicks on it, before the
        ;; snip handles any events. On one hand, this is good: it means that mouse movement events are
        ;; sent to the snip even after the mouse leaves. On the other hand, it's bad: we don't want
        ;; the snip to *retain* ownership after the button is up, because it'll continue to hog all
        ;; the mouse movement events, keeping other plot snips from displaying hover messages. Also,
        ;; a plot snip has no selectable text, focusable controls, or any other reason to own the
        ;; caret.
        ;; So on left button up, if the left button went down on this snip, we give ownership to the
        ;; snip's editor.
        [(left-down)  (set! left-down-admin admin)]
        [(left-up)    (when (get-left-down-here?)
                        (set! left-down-admin #f)
                        (send editor set-caret-owner #f))]
        ;; The 'handles-events flag keeps the editor from handling right-click events, so the pop-up
        ;; menu won't pop up. So we call the editor's "local" event handler, which would have been
        ;; called had this snip not trapped events.
        ;; We also don't allow right/middle mouse clicks to transfer caret ownership, ever. We would
        ;; have similar rules to left-up, but the pop-up menu apparently gets right-up events, not
        ;; the snip.
        [(right-down middle-down)  (send editor set-caret-owner #f)
                                   (send editor on-local-event evt)]
        ;; Just in case (we don't want these events anyway):
        [(right-up middle-up)  (send editor on-local-event evt)]
        ))
    
    (define cross-cursor (make-object cursor% 'cross))
    (define/override (adjust-cursor dc x y editorx editory evt) cross-cursor)
    ))
