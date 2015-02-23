#lang racket/base
(require racket/place)

;; Check finalization of place channels that are
;; abandoned in unreferenced place channels. 
(let loop ([n 0])
  (unless (= n 50000)
    (define-values (i o) (place-channel))
    (define-values (i2 o2) (place-channel))
    (place-channel-put i o2)
    (place-channel-put o i2)
    (loop (add1 n))))
'ok


