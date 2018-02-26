;; Futures API

(define future? (lambda (f) #f))
(define current-future (lambda () #f))
(define block (lambda () (void)))
(define current-future-prompt (lambda () (void)))
(define future-wait (lambda () (void)))

(define (set-future-callbacks! _future? _current-future _block wait cfp)
  (set! future? _future?)
  (set! current-future _current-future)
  (set! block _block)
  (set! future-wait wait)
  (set! current-future-prompt cfp))
