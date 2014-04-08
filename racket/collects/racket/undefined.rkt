#lang racket/base

(provide undefined)

;; In a future version of Racket, this `letrec` pattern
;; will not work, but the `racket/undefined` library will
;; still export an `undefined`:
(define undefined (letrec ([x x]) x))
