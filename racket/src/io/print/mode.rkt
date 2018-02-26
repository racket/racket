#lang racket/base

(provide DISPLAY-MODE
         WRITE-MODE
         PRINT-MODE/UNQUOTED
         PRINT-MODE/QUOTED

         print-mode?)

;; These are fixed by the `prop:custom-write` and `print` APIs:
(define DISPLAY-MODE #f)
(define WRITE-MODE #t)
(define PRINT-MODE/UNQUOTED 0)
(define PRINT-MODE/QUOTED 1)

(define (print-mode? mode)
  (or (eq? mode PRINT-MODE/UNQUOTED)
      (eq? mode PRINT-MODE/QUOTED)))
