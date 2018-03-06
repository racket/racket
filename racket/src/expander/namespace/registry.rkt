#lang racket/base

(provide (struct-out module-registry)
         make-module-registry
         registry-call-with-lock)

(struct module-registry (declarations  ; resolved-module-path -> module
                         lock-box))    ; reentrant lock to guard registry for use by on-demand visits

(define (make-module-registry)
  (module-registry (make-hasheq) (box #f)))

(define (registry-call-with-lock r proc)
  (define lock-box (module-registry-lock-box r))
  (let loop ()
    (define v (unbox lock-box))
    (cond
     [(or (not v)
          (sync/timeout 0 (car v) (cdr v)))
      (define sema (make-semaphore))
      (define lock (cons (semaphore-peek-evt sema) (current-thread)))
      ((dynamic-wind
        void
        (lambda ()
          (cond
           [(box-cas! lock-box v lock)
            (proc)
            void]
           [else
            ;; CAS failed; take it from the top
            (lambda () (loop))]))
        (lambda ()
          (semaphore-post sema))))]
     [(eq? (current-thread) (cdr v))
      ;; This thread already holds the lock
      (proc)]
     [else
      ; Wait and try again:
      (sync (car v) (cdr v))
      (loop)])))
