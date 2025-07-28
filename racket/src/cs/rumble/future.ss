;; We need a little support for futures, because they interact with
;; continuation operations that may need to block the future.

;; This is a lower-level `current-future` than the one exposed via
;; `racket/future`. It doesn't reflect the current future when
;; it's being `touch`ed by a Racket thread, and it can be a future
;; that represents a `thread/parallel` computation.
(define-syntax (current-future stx)
  (syntax-case stx ()
    [(_) (with-syntax ([pos current-future-virtual-register])
           #'(virtual-register pos))]))

(define block-future (lambda () (void)))
(define unblock-future (lambda () (void)))
(define current-future-prompt (lambda () (void)))

(define (set-future-callbacks! block unblock current-prompt)
  (set! block-future block)
  (set! unblock-future unblock)
  (set! current-future-prompt current-prompt))
