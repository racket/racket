#lang racket/base
(require "../string/convert.rkt")

(provide more-pending)

;; See "write-with-max.rkt"

;; might be called in atomic mode
(define (more-pending max-length start end str)
  (define prev-pending (car max-length))
  (define len (- end start))
  (define new-pending (+ len prev-pending))
  (cond
    [(new-pending . > . 3) 'full]
    [else (cons new-pending
                (bytes-append (cdr max-length)
                              (if (string? str)
                                  (string->bytes/utf-8 str #f start end)
                                  (subbytes str start end))))]))
