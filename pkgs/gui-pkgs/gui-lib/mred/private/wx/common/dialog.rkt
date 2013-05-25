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
    (define close-sema-ready? #f)

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
        (when close-sema-ready?
          (semaphore-post close-sema)
          (set! close-sema #f)
          (set! close-sema-ready? #f)))
      (when on?
        ;; mark `close-sema' as having a corresponding `show #t'
        ;; so that a future `show #f' can clear it; without this
        ;; extra flag, `close-sema' could be set, `show #f' could
        ;; post it, and then the `show #t' that created hte sema
        ;; could happen after the `show #f'
        (set! close-sema-ready? #t))
      (super direct-show on?))

    (define/private (get-show-semaphore)
      (atomically
       (let ([s (or close-sema (make-semaphore))])
         (unless close-sema (set! close-sema s))
         (semaphore-peek-evt s))))

    (define/public (show-without-yield)
      (get-show-semaphore) ; in case some other thread wants to wait
      (super show #t))

    (define/override (show on?)
      (if on?
          (let ([s (get-show-semaphore)])
            (super show on?)
            (yield s)
            (void))
          (super show on?)))))
