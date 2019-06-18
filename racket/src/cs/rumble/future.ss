;; We need a little support for futures, because they interact with
;; continuation operations that may need to block the future.

(define-syntax (current-future stx)
  (syntax-case stx ()
    [(_) (with-syntax ([pos current-future-virtual-register])
           #'(virtual-register pos))]))

(define block-future (lambda () (void)))
(define current-future-prompt (lambda () (void)))

(define (set-future-callbacks! block current-prompt)
  (set! block-future block)
  (set! current-future-prompt current-prompt))
