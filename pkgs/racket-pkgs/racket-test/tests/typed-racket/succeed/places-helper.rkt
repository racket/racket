#lang typed/racket/no-check

(provide double-place echo-place)


(: double-place (Place-Channel -> Void))
(define (double-place pch)
 (place-channel-put pch (* 2 (place-channel-get pch))))


(: echo-place (Place-Channel -> Void))
(define (echo-place pch)
 (place-channel-put pch (place-channel-get pch)))
