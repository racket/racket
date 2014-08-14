#lang racket/base
(require racket/class
         racket/draw
         "../common/queue.rkt"
         "backing-dc.rkt")

(provide 
 (protect-out canvas-autoscroll-mixin
              canvas-mixin
              fix-bitmap-size))

;; Implements canvas autoscroll, applied *before* platform-specific canvas
;; methods:
(define (canvas-autoscroll-mixin %)
  (class %
    (super-new)

    (inherit get-client-size
             refresh)

    (define any-scroll? #f)
    (define auto-scroll? #f)
    (define virtual-height #f)
    (define virtual-width #f)

    (define/public (is-auto-scroll?) auto-scroll?)
    (define/public (is-disabled-scroll?) (not any-scroll?))
    (define/public (get-virtual-height) virtual-height)
    (define/public (get-virtual-width) virtual-width)
    
    (define/public (set-scrollbars h-step v-step
                                   h-len v-len
                                   h-page v-page
                                   h-pos v-pos
                                   auto?)
       (set! any-scroll? #t)
       (cond
        [auto?
         (set! auto-scroll? #t)
         (set! virtual-width (and (positive? h-len) h-len))
         (set! virtual-height (and (positive? v-len) v-len))
         (reset-auto-scroll h-pos v-pos)
         (refresh-for-autoscroll)]
        [else
         (let ([a? auto-scroll?])
           (set! auto-scroll? #f)
           (set! virtual-width #f)
           (set! virtual-height #f)
           (when a? (reset-dc-for-autoscroll))) ; disable scroll offsets
         (do-set-scrollbars h-step v-step
                            h-len v-len
                            h-page v-page
                            h-pos v-pos)]))

    ;; To be overridden:
    (define/public (do-set-scrollbars h-step v-step
                                      h-len v-len
                                      h-page v-page
                                      h-pos v-pos)
      (void))

    (define/public (reset-auto-scroll [h-pos -1] [v-pos -1])
      (let ([xb (box 0)]
            [yb (box 0)])
        (get-client-size xb yb)
        (let ([cw (unbox xb)]
              [ch (unbox yb)])
          (let ([h-len (if virtual-width
                           (max 0 (- virtual-width cw))
                           0)]
                [v-len (if virtual-height
                           (max 0 (- virtual-height ch))
                           0)]
                [h-page (if virtual-width
                            cw
                            0)]
                [v-page (if virtual-height
                            ch
                            0)])
            (do-set-scrollbars 1 1
                               h-len v-len
                               h-page v-page
                               h-pos v-pos)))))

    ;; To be overridden:
    (define/public (reset-dc-for-autoscroll)
      (void))

    (define/public (refresh-for-autoscroll)
      (reset-dc-for-autoscroll)
      (refresh))
    
    (define/public (view-start xb yb)
      (if auto-scroll?
          (begin
            (set-box! xb (if virtual-width
                             (get-virtual-h-pos)
                             0))
            (set-box! yb (if virtual-height
                             (get-virtual-v-pos)
                             0)))
          (begin
            (set-box! xb 0)
            (set-box! yb 0))))
    
    ;; To be overridden:
    (define/public (get-virtual-h-pos) 0)
    (define/public (get-virtual-v-pos) 0)
    
    (define/public (get-virtual-size xb yb)
      (get-client-size xb yb)
      (when virtual-width (set-box! xb virtual-width))
      (when virtual-height (set-box! yb virtual-height)))))

;; Implements canvas refresh, applied *after* platform-specific canvas
;; methods:
(define (canvas-mixin %)
  (class %
    (inherit request-canvas-flush-delay
             cancel-canvas-flush-delay
             queue-canvas-refresh-event
             is-shown-to-root?
             on-paint
             queue-backing-flush
             get-dc
             get-canvas-background-for-backing
             skip-pre-paint?)
    
    ;; Avoid multiple queued paints, and also allow cancel
    ;; of queued paint:
    (define paint-queued #f) ; #f or (box #t)

    (super-new)

    (define/override (queue-paint)
      ;; can be called from any thread, including the event-pump thread
      (unless paint-queued
        (let ([b (box #t)])
          (set! paint-queued b)
          (let ([req (request-canvas-flush-delay)])
            (queue-canvas-refresh-event
             (lambda () (do-on-paint req b)))))))

    (define/private (do-on-paint req b)
      ;; only called in the handler thread
      (when (or (not b) (unbox b))
        (let ([pq paint-queued])
          (when pq (set-box! pq #f)))
        (set! paint-queued #f)
        (cond
         [(or (not b) (is-shown-to-root?))
          (let ([dc (get-dc)])
            (send dc suspend-flush)
            (send dc ensure-ready)
            (send dc clean-slate)
            (let ([bg (get-canvas-background-for-backing)])
              (when bg 
                (let ([old-bg (send dc get-background)])
                  (send dc set-background bg)
                  (send dc clear)
                  (send dc set-background old-bg))))
            (on-paint)
            (send dc resume-flush)
            (queue-backing-flush))]
         [b ; => not shown to root
          ;; invalidate dc so that it's refresh
          ;; when it's shown again
          (send (get-dc) reset-backing-retained)]))
      (when req
        (cancel-canvas-flush-delay req)))

    (define/override (paint-children)
      (unless (skip-pre-paint?)
        (when (or paint-queued
                  (not (send (get-dc) can-backing-flush?)))
          (do-on-paint #f #f))))


    (define flush-box #f)
    
    ;; Periodic flush is needed for Windows, where
    ;; updates otherwise happen only via the eventspace's queue
    (define/override (schedule-periodic-backing-flush)
      (unless flush-box
        (set! flush-box (box #t))
        (add-event-boundary-sometimes-callback! 
         flush-box
         (lambda (b)
           (when (unbox b)
             (do-canvas-backing-flush #f))))))

     (define/override (do-canvas-backing-flush ctx)
       ;; cancel scheduled flush, if any:
       (when flush-box 
         (set-box! flush-box #f) 
         (set! flush-box #f))
       (super do-canvas-backing-flush ctx))))

;; useful for fixing the size of a collecting blit:
(define (fix-bitmap-size on w h on-x on-y)
  (if (and (zero? on-x)
           (zero? on-y)
           (= (send on get-width) w)
           (= (send on get-height) h))
      on
      (let ([bm (make-object bitmap% w h)])
        (let ([dc (make-object bitmap-dc% on)])
          (send dc draw-bitmap-section on 0 0 on-x on-y w h)
          (send dc set-bitmap #f)
          bm))))
