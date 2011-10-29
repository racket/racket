#lang racket/base

(require racket/gui/base racket/class racket/match racket/bool racket/async-channel
         "../common/gui.rkt"
         "../common/math.rkt"
         "area.rkt")

(provide 3d-plot-snip% make-3d-plot-snip)

(define update-delay 33)  ; about 30 fps (just over)

(struct render-thread (state command-channel response-channel thread) #:mutable #:transparent)

(struct draw-command (angle altitude animating?) #:transparent)
(struct copy-command () #:transparent)

(define (make-render-thread make-bm)
  (define com-ch (make-channel))
  (define res-ch (make-async-channel))
  (define th
    (thread
     (λ ()
       (let loop ()
         (match (channel-get com-ch)
           [(draw-command angle altitude animating?)
            (define bm (with-handlers ([exn?  (λ (e) (async-channel-put res-ch e))])
                         (make-bm angle altitude animating?)))
            (async-channel-put res-ch bm)]
           [(copy-command)  (async-channel-put res-ch (make-render-thread make-bm))])
         (loop)))))
  (render-thread 'wait com-ch res-ch th))

(define (render-thread-get-bitmap r)
  (match-define (render-thread state com-ch res-ch th) r)
  (define res (async-channel-get res-ch))
  (set-render-thread-state! r 'wait)
  (if (exn? res) (raise res) res))

(define (render-thread-try-get-bitmap r)
  (match-define (render-thread state com-ch res-ch th) r)
  (define res (async-channel-try-get res-ch))
  (when res (set-render-thread-state! r 'wait))
  (if (exn? res) (raise res) res))

(define (render-thread-wait r)
  (match-define (render-thread state com-ch res-ch th) r)
  (when (symbol=? state 'drawing)
    (render-thread-get-bitmap r)))

(define (render-thread-draw r angle altitude animating?)
  (render-thread-wait r)
  (match-define (render-thread state com-ch res-ch th) r)
  (channel-put com-ch (draw-command angle altitude animating?))
  (set-render-thread-state! r 'drawing))

(define (render-thread-copy r)
  (render-thread-wait r)
  (match-define (render-thread state com-ch res-ch th) r)
  (channel-put com-ch (copy-command))
  (async-channel-get res-ch))

(define (clamp x mn mx) (min* (max* x mn) mx))

(define 3d-plot-snip%
  (class image-snip%
    (init-field make-bm angle altitude
                [bm (make-bm angle altitude #f)]
                [rth (make-render-thread make-bm)])
    (inherit set-bitmap)
    
    (super-make-object bm)
    
    (define width (send bm get-width))
    (define height (send bm get-height))
    
    (define click-x 0)
    (define click-y 0)
    (define drag-x 0)
    (define drag-y 0)
    
    (define (new-angle) (real-modulo (+ angle (* (- drag-x click-x) (/ 180 width))) 360))
    (define (new-altitude) (clamp (+ altitude (* (- drag-y click-y) (/ 180 height))) 0 90))
    
    (define draw? #t)
    (define timer #f)
    
    (define ((update animating?))
      (define can-draw? (case (render-thread-state rth)
                          [(wait)  #t]
                          [(drawing)  (define new-bm (render-thread-try-get-bitmap rth))
                                      (cond [(is-a? new-bm bitmap%)  (set! bm new-bm)
                                                                     (set-bitmap bm)
                                                                     #t]
                                            [else  #f])]))
      (when (and draw? can-draw?)
        (set! draw? #f)
        (render-thread-draw rth (new-angle) (new-altitude) animating?)))
    
    (define (stop-timer)
      (when timer
        (send timer stop)
        (set! timer #f)))
    
    (define (start-timer)
      (stop-timer)
      (set! timer (make-object timer% (update #t) update-delay)))
    
    (define/override (on-event dc x y editorx editory evt)
      (case (send evt get-event-type)
        [(left-down)  (render-thread-wait rth)
                      (set! angle (new-angle))
                      (set! altitude (new-altitude))
                      (set! click-x (send evt get-x))
                      (set! click-y (send evt get-y))
                      (set! drag-x click-x)
                      (set! drag-y click-y)
                      (set! draw? #t)
                      (start-timer)]
        [(left-up)    (stop-timer)
                      (set! draw? #f)
                      (render-thread-wait rth)
                      (set! drag-x (send evt get-x))
                      (set! drag-y (send evt get-y))
                      (set! angle (new-angle))
                      (set! altitude (new-altitude))
                      (set! click-x 0)
                      (set! click-y 0)
                      (set! drag-x 0)
                      (set! drag-y 0)
                      (render-thread-draw rth angle altitude #f)
                      (define new-bm (render-thread-get-bitmap rth))
                      (when (is-a? new-bm bitmap%)
                        (set! bm new-bm)
                        (set-bitmap bm))]
        [(motion)     (when (and timer (send evt get-left-down))
                        (set! drag-x (send evt get-x))
                        (set! drag-y (send evt get-y))
                        (set! draw? #t))]))
    
    (define/override (copy)
      (make-object this% make-bm angle altitude bm (render-thread-copy rth)))
    
    (define cross-cursor (make-object cursor% 'cross))
    (define/override (adjust-cursor dc x y editorx editory evt) cross-cursor)
    
    (send this set-flags (list* 'handles-events (send this get-flags)))))

;; make-3d-plot-snip : (real real real -> bitmap) real real -> 3d-plot-snip%
(define (make-3d-plot-snip make-bm angle altitude)
  (make-object 3d-plot-snip% make-bm angle altitude))
