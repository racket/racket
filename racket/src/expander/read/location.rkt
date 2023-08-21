#lang racket/base

(provide port-next-location*)

(define (port-next-location* in init-c)
  ;; If we've already read `init-c`, then back up by one column and
  ;; position; we assume that `init-c` is not a newline character
  (cond
   [(not init-c) (port-next-location in)]
   [else
    (define-values (line col pos) (port-next-location in))
    (define delta
      (cond
        [(port-counts-lines? in) 1]
        [(char? init-c)
         ;; port counts in bytes, so back up by bytes in UTF-8 encoding
         (char-utf-8-length init-c)]
        [else 1]))
    (values line
            (and col (max 0 (- col delta)))
            (and pos (max 1 (- pos delta))))]))
