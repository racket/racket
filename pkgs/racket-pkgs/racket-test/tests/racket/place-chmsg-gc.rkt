#lang racket/base
(require racket/place)

;; Make sure that place channels waiting in a channel
;; are kept properly. N should be large enough to trigger
;; a master collection.

(define N 10000)
(define-values (i o) (place-channel))
(for ([j N])
  (define-values (i2 o2) (place-channel))
  (place-channel-put i2 j)
  (place-channel-put i o2))
(for ([j N])
  (unless (eq? j (place-channel-get (place-channel-get o)))
    ;; just as likely to crash as get here
    (error "failed")))
