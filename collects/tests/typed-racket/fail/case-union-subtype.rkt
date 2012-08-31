#lang typed/racket #:no-optimize


(define-type T
     (case->
       (String Symbol -> Symbol)
       (Symbol String -> Symbol)))

(define-type S ((U String Symbol) (U String Symbol) -> Symbol))

(: f T)
(define (f x y)
  (if (and (string? x) (string? y))
      "BROKEN"
      'ok))

(: g S)
(define g f)

(g "Hello" "World")
