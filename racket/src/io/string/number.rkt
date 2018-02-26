#lang racket/base

;; The `string->number` function is implemented at the reader+expander
;; level, but the printer needs `string->number` for checking whether
;; to quote a symbol. Tie the knot with `set-string->number?!`.

(provide string->number?
         set-string->number?!)

(define string->number? (lambda (str) #f))

(define (set-string->number?! proc)
  (set! string->number? proc))
