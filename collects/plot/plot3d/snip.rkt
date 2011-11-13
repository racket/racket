#lang racket/base

(require racket/gui/base racket/class racket/match racket/list unstable/parameter-group
         "../common/gui.rkt"
         "../common/math.rkt"
         "../common/worker-thread.rkt"
         "../common/plot-device.rkt"
         "../common/parameters.rkt"
         "plot-area.rkt")

(provide 3d-plot-snip% make-3d-plot-snip)

(define update-delay 16)  ; about 60 fps (just over)
(define message-timeout 2000)

(struct draw-command (animating? angle altitude) #:transparent)

(define (make-render-thread make-bm saved-plot-parameters)
  (make-worker-thread
   (match-lambda
     [(draw-command animating? angle altitude)
      (parameterize/group ([plot-parameters  saved-plot-parameters])
        (make-bm animating? angle altitude))])))

(define (clamp x mn mx) (min* (max* x mn) mx))

(define 3d-plot-snip%
  (class image-snip%
    (init-field make-bm angle altitude saved-plot-parameters
                [bm (make-bm #f angle altitude)])
    (inherit set-bitmap)
    
    (super-make-object bm)
    
    (define/override (copy)
      (make-object this% make-bm angle altitude saved-plot-parameters bm))
    
    (define width (send bm get-width))
    (define height (send bm get-height))
    
    (define left-click-x 0)
    (define left-click-y 0)
    (define left-drag-x 0)
    (define left-drag-y 0)
    
    (define (new-angle) (real-modulo (+ angle (* (- left-drag-x left-click-x) (/ 180 width))) 360))
    (define (new-altitude) (clamp (+ altitude (* (- left-drag-y left-click-y) (/ 180 height))) 0 90))
    
    (define (refresh)
      ;(printf "refreshing ~a~n" (current-milliseconds))
      (send this set-bitmap bm))
    
    (define draw? #t)
    (define update-timer #f)
    (define rth (make-render-thread make-bm saved-plot-parameters))
    
    (define (stop-update-timer)
      (when update-timer
        (send update-timer stop)
        (set! update-timer #f)))
    
    (define (start-update-timer)
      (stop-update-timer)
      (set! update-timer (make-object timer% update update-delay)))
    
    (define (update)
      ;(printf "update ~a~n" (current-milliseconds))
      (define can-draw?
        (cond [(worker-thread-working? rth)
               (define new-bm (worker-thread-try-get rth))
               (cond [(is-a? new-bm bitmap%)  (set! bm new-bm)
                                              (set-angles-message (new-angle) (new-altitude))
                                              (set-bitmap bm)
                                              #t]
                     [else  #f])]
              [else  #t]))
      (when (and draw? can-draw?)
        (set! draw? #f)
        (worker-thread-put rth (draw-command #t (new-angle) (new-altitude))))
      (refresh-message-timer))
    
    (define message #f)
    (define message-timer #f)
    
    (define (stop-message)
      ;(printf "stop-message ~a~n" (current-milliseconds))
      (when message-timer
        (send message-timer stop)
        (set! message-timer #f)
        (set! message #f)
        (refresh)))
    
    (define (refresh-message-timer)
      (when message-timer
        (send message-timer stop))
      (set! message-timer (make-object timer% stop-message message-timeout)))
    
    (define (set-message msg)
      (refresh-message-timer)
      (set! message msg))
    
    (define (set-angles-message angle altitude)
      (set-message (format "angle = ~a\naltitude = ~a"
                           (number->string (round angle))
                           (number->string (round altitude)))))
    
    (define (start-message msg)
      (define refresh? (not (equal? msg message)))
      (set-message msg)
      (when refresh? (refresh)))
    
    (define dragged? #f)
    (define (start-click-message)
      (unless dragged?
        (start-message "Click and drag to rotate")))
    
    (define/override (on-event dc x y editorx editory evt)
      (define evt-type (send evt get-event-type))
      #;(when (not (eq? evt-type 'motion))
        (printf "evt-type = ~v~n" evt-type))
      #;(when (eq? evt-type 'motion)
        (printf "motion for ~a; x,y = ~a,~a~n" (eq-hash-code this) (send evt get-x) (send evt get-y)))
      (case evt-type
        [(left-down)  (worker-thread-wait rth)
                      (set! angle (new-angle))
                      (set! altitude (new-altitude))
                      (set-angles-message angle altitude)
                      (set! left-click-x (send evt get-x))
                      (set! left-click-y (send evt get-y))
                      (set! left-drag-x left-click-x)
                      (set! left-drag-y left-click-y)
                      (set! draw? #t)
                      (start-update-timer)]
        [(left-up)    (when update-timer
                        (stop-update-timer)
                        (set! draw? #f)
                        (worker-thread-wait rth)
                        (set! left-drag-x (send evt get-x))
                        (set! left-drag-y (send evt get-y))
                        (set! angle (new-angle))
                        (set! altitude (new-altitude))
                        (set-angles-message angle altitude)
                        (set! left-click-x 0)
                        (set! left-click-y 0)
                        (set! left-drag-x 0)
                        (set! left-drag-y 0)
                        (worker-thread-put rth (draw-command #f angle altitude))
                        (define new-bm (worker-thread-get rth))
                        (when (is-a? new-bm bitmap%)
                          (set! bm new-bm)
                          (set-bitmap bm)))]
        [(motion)     (when (and update-timer (send evt get-left-down))
                        (when (not (and (= left-drag-x (send evt get-x))
                                        (= left-drag-y (send evt get-y))))
                          (set! left-drag-x (send evt get-x))
                          (set! left-drag-y (send evt get-y))
                          (set! draw? #t)
                          (set! dragged? #t)))
                      (when (and (not (send evt get-left-down))
                                 (<= x (send evt get-x) (+ x width))
                                 (<= y (send evt get-y) (+ y height)))
                        (start-click-message))]))
    
    (define (draw-message dc dc-x-min dc-y-min)
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
      ;(printf "drawing ~a~n" (current-milliseconds))
      (super draw dc x y left top right bottom dx dy draw-caret)
      ;(send dc draw-bitmap-section bm x y 0 0 width height)
      (when message
        (parameterize/group ([plot-parameters  saved-plot-parameters])
          (draw-message dc x y))))
    
    (define cross-cursor (make-object cursor% 'cross))
    (define/override (adjust-cursor dc x y editorx editory evt) cross-cursor)
    
    (send this set-flags (list* 'handles-events 'handles-all-mouse-events (send this get-flags)))))

;; make-3d-plot-snip : (real real real -> bitmap) real real -> 3d-plot-snip%
(define (make-3d-plot-snip make-bm angle altitude saved-plot-parameters)
  (make-object 3d-plot-snip% make-bm angle altitude saved-plot-parameters))
