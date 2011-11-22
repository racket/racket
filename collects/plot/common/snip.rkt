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
    (inherit set-bitmap get-bitmap)
    
    (super-make-object bm)
    
    (define/override (copy) (make-object this% (get-bitmap) saved-plot-parameters))
    
    (define/public (get-saved-plot-parameters) saved-plot-parameters)
    
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
      (define box-x-min (+ dc-x-min (* 1/2 (- width box-x-size))))
      (define box-y-min (+ dc-y-min (* 1/2 (- height box-y-size))))
      (define box-x-max (+ box-x-min box-x-size))
      (define box-y-max (+ box-y-min box-y-size))
      
      (send pd set-alpha 2/3)
      (send pd set-minor-pen)
      (send pd draw-rect (vector (ivl box-x-min box-x-max) (ivl box-y-min box-y-max)))
      
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
          (draw-message dc x y))))))
