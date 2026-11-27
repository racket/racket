#lang racket/base

(provide (struct-out module-registry)
         make-module-registry
         registry-call-with-lock)

(struct module-registry (declarations  ; resolved-module-path -> module
                         cross-phase-instances ; resolved-module-path -> module-instance
                         lock-box))    ; reentrant lock to guard registry for use by on-demand visits

;; A cross-phase persistent module has a single instance per registry,
;; independent of the phase or the namespace that requests it; the
;; instance can be shared with other registries by `namespace-attach-module`

(define (make-module-registry)
  (module-registry (make-hasheq) (make-hasheq) (box #f)))

(define (registry-call-with-lock r proc)
  (define lock-box (module-registry-lock-box r))
  (let loop ()
    (define v (unbox lock-box))
    (cond
     [(or (not v)
          (sync/timeout 0 (car v) (let ([t (weak-box-value (cdr v))])
                                    (if (eq? t (current-thread))
                                        never-evt
                                        (or t always-evt)))))
      ;; Lock holder is released its semaphore or terminated
      (define sema (make-semaphore))
      (define lock (cons (semaphore-peek-evt sema) (make-weak-box (current-thread))))
      ((dynamic-wind
        void
        (lambda ()
          (cond
            [(box-cas! lock-box v lock)
             ;; This thread became the lock holder
             (call-with-values
              proc
              (lambda results
                (lambda () (apply values results))))]
            [else
             ;; CAS failed; take it from the top
             (lambda () (loop))]))
        (lambda ()
          (semaphore-post sema))))]
     [(eq? (current-thread) (weak-box-value (cdr v)))
      ;; This thread already holds the lock
      (proc)]
     [else
      ; Wait and try again:
      (sync (car v) (or (weak-box-value (cdr v)) always-evt))
      (loop)])))
