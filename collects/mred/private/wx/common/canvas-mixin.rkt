#lang racket/base
(require racket/class
         "backing-dc.rkt")

(provide canvas-mixin)

(define (canvas-mixin %)
  (class %
    (super-new)
    (inherit request-canvas-flush-delay
             cancel-canvas-flush-delay
             queue-canvas-refresh-event
             is-shown-to-root?
             on-paint
             queue-backing-flush
             get-dc
             get-canvas-background)
    
    ;; Avoid multiple queued paints, and also allow cancel
    ;; of queued paint:
    (define paint-queued #f) ; #f or (box #t)

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
        (when (or (not b) (is-shown-to-root?))
          (let ([dc (get-dc)])
            (send dc suspend-flush)
            (send dc ensure-ready)
            (send dc erase) ; start with a clean slate
            (let ([bg (get-canvas-background)])
              (when bg 
                (let ([old-bg (send dc get-background)])
                  (send dc set-background bg)
                  (send dc clear)
                  (send dc set-background old-bg))))
            (on-paint)
            (send dc resume-flush)
            (queue-backing-flush))))
      (when req
        (cancel-canvas-flush-delay req)))

    (define/override (paint-children)
      (when (or paint-queued
                (not (send (get-dc) can-backing-flush?)))
        (do-on-paint #f #f)))))
