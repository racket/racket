#lang typed-scheme
(require/typed
 scheme/base
 [values (All (T) ((Any -> Boolean) -> (Any -> Boolean : T)))])

(: number->string? (Any -> Boolean : (Number -> String)))
(define (number->string? x)
  (((inst values (Number -> String)) procedure?) x))

(: f (Number -> String))
(define f
  (if (number->string? +) + number->string))
