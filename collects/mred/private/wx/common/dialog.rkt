#lang racket/base
(require racket/class
         "../../lock.rkt"
         "queue.rkt")

(provide (protect-out dialog-mixin))

(define dialog-level-counter 0)

(define (dialog-mixin %)
  (class %
    (super-new)

    (define close-sema #f)

    (define dialog-level 0)
    (define/override (get-dialog-level) dialog-level)
    
    (define/override (frame-relative-dialog-status win) 
      (let ([dl (send win get-dialog-level)])
        (cond
         [(= dl dialog-level) 'same]
         [(dl . > . dialog-level) #f]
         [else 'other])))
  
    (define/override (direct-show on?)
      ;; atomic mode
      (when on?
        (set! dialog-level-counter (add1 dialog-level-counter))
        (set! dialog-level dialog-level-counter))
      (unless on?
        (set! dialog-level 0))
      (unless on?
        (when close-sema
          (semaphore-post close-sema)
          (set! close-sema #f)))
      (super direct-show on?))

    (define/override (show on?)
      (if on?
          (let ([s (atomically
                    (let ([s (or close-sema (make-semaphore))])
                      (unless close-sema (set! close-sema s))
                      (semaphore-peek-evt s)))])
            (super show on?)
            (yield s)
            (void))
          (super show on?)))))
