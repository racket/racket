#lang racket/base
(require '#%kernel)

(provide check-not-undefined
         undefined
         undefined?)

;; In a future version of Racket, this `letrec` pattern
;; will not work, but the `racket/undefined` library will
;; still export an `undefined`:
(define undefined (letrec ([x x]) x))

(define (undefined? v) (eq? v undefined))
(define (check-not-undefined v s)
  (unless (symbol? s) (raise-argument-error 'check-not-undefined "symbol?" 1 v s))
  (if (eq? v undefined)
      (raise (make-exn:fail:contract:variable
              (format "~a: variable used before its definition" s)
              (current-continuation-marks)
              s))
      v))
