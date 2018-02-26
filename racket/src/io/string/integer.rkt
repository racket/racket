#lang racket/base

;; Simple string->number conversion, since the geenral one is
;; implemented at the expander level

(provide string->integer)

(define (string->integer s)
  (for/fold ([v 0]) ([c (in-string s)])
    (+ (* v 10) (- (char->integer c) (char->integer #\0)))))
