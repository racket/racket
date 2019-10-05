;; We need a little support for futures, because they interact with
;; continuation operations that may need to block the future.

(define-syntax (current-future stx)
  (syntax-case stx ()
    [(_) (with-syntax ([pos current-future-virtual-register])
           #'(virtual-register pos))]))

(define block-future (lambda () (void)))
(define sync-future (lambda (who thunk) (thunk)))
(define current-future-prompt (lambda () (void)))

(define (set-future-callbacks! block sync current-prompt)
  (set! block-future block)
  (set! sync-future sync)
  (set! current-future-prompt current-prompt))

;; Call `thunk` in the main thread to synchronizel the thunk must be
;; constant-time, never rasse an exception, and return a single value
(define (future-sync who thunk)
  (let ([disabled? (> (disable-interrupts) 1)])
    (enable-interrupts)
    (cond
     [disabled?
      ;; Interrupts were already disabled, so we're holding the global
      ;; lock, in a garbage collection, or something like that --- as
      ;; synchronized as possible already
      (thunk)]
     [else
      (sync-future who thunk)])))
