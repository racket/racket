#lang racket/base

(require racket/gui/base racket/class racket/match racket/list
         "../common/gui.rkt"
         "../common/math.rkt"
         "../common/worker-thread.rkt"
         "plot-area.rkt")

(provide 3d-plot-snip% make-3d-plot-snip)

(define update-delay 33)  ; about 30 fps (just over)

(struct draw-command (animating? angle altitude) #:transparent)
(struct copy-command () #:transparent)

(define (make-render-thread make-bm)
  (make-worker-thread
   (match-lambda
     [(draw-command animating? angle altitude)  (make-bm animating? angle altitude)]
     [(copy-command)  (make-render-thread make-bm)])))

(define (clamp x mn mx) (min* (max* x mn) mx))

(define 3d-plot-snip%
  (class image-snip%
    (init-field make-bm angle altitude
                [bm (make-bm #f angle altitude)]
                [rth (make-render-thread make-bm)])
    (inherit set-bitmap)
    
    (super-make-object bm)
    
    (define width (send bm get-width))
    (define height (send bm get-height))
    
    (define left-click-x 0)
    (define left-click-y 0)
    (define left-drag-x 0)
    (define left-drag-y 0)
    
    (define (new-angle) (real-modulo (+ angle (* (- left-drag-x left-click-x) (/ 180 width))) 360))
    (define (new-altitude) (clamp (+ altitude (* (- left-drag-y left-click-y) (/ 180 height))) 0 90))
    
    (define draw? #t)
    (define timer #f)
    
    (define ((update animating?))
      (define can-draw?
        (cond [(worker-thread-working? rth)
               (define new-bm (worker-thread-try-get rth))
               (cond [(is-a? new-bm bitmap%)  (set! bm new-bm)
                                              (set-bitmap bm)
                                              #t]
                     [else  #f])]
              [else  #t]))
      (when (and draw? can-draw?)
        (set! draw? #f)
        (worker-thread-put rth (draw-command animating? (new-angle) (new-altitude)))))
    
    (define (stop-timer)
      (when timer
        (send timer stop)
        (set! timer #f)))
    
    (define (start-timer)
      (stop-timer)
      (set! timer (make-object timer% (update #t) update-delay)))
    
    (define/override (on-event dc x y editorx editory evt)
      (case (send evt get-event-type)
        [(left-down)  (worker-thread-wait rth)
                      (set! angle (new-angle))
                      (set! altitude (new-altitude))
                      (set! left-click-x (send evt get-x))
                      (set! left-click-y (send evt get-y))
                      (set! left-drag-x left-click-x)
                      (set! left-drag-y left-click-y)
                      (set! draw? #t)
                      (start-timer)]
        [(left-up)    (stop-timer)
                      (set! draw? #f)
                      (worker-thread-wait rth)
                      (set! left-drag-x (send evt get-x))
                      (set! left-drag-y (send evt get-y))
                      (set! angle (new-angle))
                      (set! altitude (new-altitude))
                      (set! left-click-x 0)
                      (set! left-click-y 0)
                      (set! left-drag-x 0)
                      (set! left-drag-y 0)
                      (worker-thread-put rth (draw-command #f angle altitude))
                      (define new-bm (worker-thread-get rth))
                      (when (is-a? new-bm bitmap%)
                        (set! bm new-bm)
                        (set-bitmap bm))]
        [(motion)     (when timer
                        (cond [(send evt get-left-down)
                               (set! left-drag-x (send evt get-x))
                               (set! left-drag-y (send evt get-y))
                               (set! draw? #t)]))]))
    
    (define/override (copy)
      (make-object this%
        make-bm angle altitude bm (worker-thread-send rth (copy-command))))
    
    (define cross-cursor (make-object cursor% 'cross))
    (define/override (adjust-cursor dc x y editorx editory evt) cross-cursor)
    
    (send this set-flags (list* 'handles-events (send this get-flags)))))

;; make-3d-plot-snip : (real real real -> bitmap) real real -> 3d-plot-snip%
(define (make-3d-plot-snip make-bm angle altitude)
  (make-object 3d-plot-snip% make-bm angle altitude))
