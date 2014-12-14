#lang racket/base
(require racket/place)
(provide start)
(define (start pch)
  (let loop ()
    (place-channel-put pch (place-channel-get pch))
    (loop)))

