#lang racket/base

(require racket/gui/base racket/class racket/match unstable/parameter-group
         "../common/snip.rkt"
         "../common/math.rkt"
         "../common/worker-thread.rkt"
         "../common/parameters.rkt")

(provide 3d-plot-snip% make-3d-plot-snip)

(define update-delay 16)  ; about 60 fps (just over)

(struct draw-command (animating? angle altitude) #:transparent)

(define (make-render-thread make-bm saved-plot-parameters)
  (make-worker-thread
   (match-lambda
     [(draw-command animating? angle altitude)
      (make-bm animating? angle altitude)])))

(define (clamp x mn mx) (min* (max* x mn) mx))

(define 3d-plot-snip%
  (class plot-snip%
    (init bm saved-plot-parameters)
    (init-field make-bm angle altitude)
    
    (inherit set-bitmap get-bitmap get-saved-plot-parameters set-message reset-message-timeout)
    
    (super-make-object bm saved-plot-parameters)
    
    (define/override (copy)
      (make-object this%
        (get-bitmap) (get-saved-plot-parameters)
        make-bm angle altitude))
    
    (define left-click-x 0)
    (define left-click-y 0)
    (define left-drag-x 0)
    (define left-drag-y 0)
    
    (define (new-angle)
      (define degrees-per-pixel (/ 180 (send (get-bitmap) get-width)))
      (define dx (- left-drag-x left-click-x))
      (real-modulo (+ angle (* dx degrees-per-pixel)) 360))
    
    (define (new-altitude)
      (define degrees-per-pixel (/ 180 (send (get-bitmap) get-height)))
      (define dy (- left-drag-y left-click-y))
      (clamp (+ altitude (* dy degrees-per-pixel)) 0 90))
    
    (define draw? #t)
    (define left-down? #f)  ; only #t if left-down happened on this snip
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
      (reset-message-timeout))
    
    (define (set-angles-message angle altitude)
      (set-message (format "angle = ~a\naltitude = ~a"
                           (number->string (inexact->exact (round angle)))
                           (number->string (inexact->exact (round altitude))))
                   #:refresh? #f))
    
    (define rotated? #f)
    (define (set-click-message)
      (unless rotated?
        (set-message "Click and drag to rotate")))
    
    (define/override (on-event dc x y editorx editory evt)
      (define evt-type (send evt get-event-type))
      (define mouse-x (- (send evt get-x) x))
      (define mouse-y (- (send evt get-y) y))
      (case evt-type
        [(left-down)  (worker-thread-wait rth)
                      (set! left-click-x mouse-x)
                      (set! left-click-y mouse-y)
                      (set! left-drag-x mouse-x)
                      (set! left-drag-y mouse-y)
                      (set! angle (new-angle))
                      (set! altitude (new-altitude))
                      (set-angles-message angle altitude)
                      (set! left-down? #t)
                      (set! draw? #t)
                      (start-update-timer)]
        [(left-up)    (when left-down?
                        (stop-update-timer)
                        (set! draw? #f)
                        (set! left-down? #f)
                        (worker-thread-wait rth)
                        (set! left-drag-x mouse-x)
                        (set! left-drag-y mouse-y)
                        (set! angle (new-angle))
                        (set! altitude (new-altitude))
                        (set-angles-message angle altitude)
                        (define new-bm (worker-thread-send rth (draw-command #f angle altitude)))
                        (when (is-a? new-bm bitmap%)
                          (set-bitmap new-bm)))]
        [(motion)     (cond [left-down?
                             (when (not (and (= left-drag-x mouse-x)
                                             (= left-drag-y mouse-y)))
                               (set! left-drag-x mouse-x)
                               (set! left-drag-y mouse-y)
                               (set! draw? #t)
                               (set! rotated? #t))]
                            [else (and (not (send evt get-left-down))
                                       (<= 0 mouse-x (send (get-bitmap) get-width))
                                       (<= 0 mouse-y (send (get-bitmap) get-height)))
                                  (set-click-message)])]))
    
    (define cross-cursor (make-object cursor% 'cross))
    (define/override (adjust-cursor dc x y editorx editory evt) cross-cursor)
    
    (send this set-flags (list* 'handles-events 'handles-all-mouse-events (send this get-flags)))))

(define (make-3d-plot-snip bm saved-plot-parameters make-bm angle altitude)
  (make-object 3d-plot-snip% bm saved-plot-parameters make-bm angle altitude))
