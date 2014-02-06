#;#;
#<<END
TR info: let-loop-effects.rkt 10:34 displayln -- hidden parameter
TR info: let-loop-effects.rkt 11:34 displayln -- hidden parameter
TR info: let-loop-effects.rkt 9:34 displayln -- hidden parameter
TR opt: let-loop-effects.rkt 10:48 (add1 y) -- fixnum add1
TR opt: let-loop-effects.rkt 11:26 (begin (displayln (quote z)) z) -- unbox float-complex
TR opt: let-loop-effects.rkt 11:48 z -- unboxed complex variable
TR opt: let-loop-effects.rkt 12:11 x -- leave var unboxed
TR opt: let-loop-effects.rkt 12:13 z -- leave var unboxed
TR opt: let-loop-effects.rkt 12:8 (+ x z) -- unboxed binary float complex
TR opt: let-loop-effects.rkt 4:0 (real-part (let: loop : Float-Complex ((x : Float-Complex 0.0+0.0i) (y : Integer 0) (z : Float-Complex 0.0+0.0i)) (if (zero? y) (+ 1.0+0.0i (loop (begin (displayln (quote x)) x) (begin (displayln (quote y)) (add1 y)) (begin (displayln (quote z)) z))) (+ x z)))) -- complex accessor elimination
TR opt: let-loop-effects.rkt 5:2 (let: loop : Float-Complex ((x : Float-Complex 0.0+0.0i) (y : Integer 0) (z : Float-Complex 0.0+0.0i)) (if (zero? y) (+ 1.0+0.0i (loop (begin (displayln (quote x)) x) (begin (displayln (quote y)) (add1 y)) (begin (displayln (quote z)) z))) (+ x z))) -- unbox float-complex
TR opt: let-loop-effects.rkt 5:2 (let: loop : Float-Complex ((x : Float-Complex 0.0+0.0i) (y : Integer 0) (z : Float-Complex 0.0+0.0i)) (if (zero? y) (+ 1.0+0.0i (loop (begin (displayln (quote x)) x) (begin (displayln (quote y)) (add1 y)) (begin (displayln (quote z)) z))) (+ x z))) -- unboxed call site
TR opt: let-loop-effects.rkt 5:31 x -- unboxed var -> table
TR opt: let-loop-effects.rkt 5:49 0.0+0.0i -- unboxed literal
TR opt: let-loop-effects.rkt 5:8 loop -- fun -> unboxed fun
TR opt: let-loop-effects.rkt 5:8 loop -- unboxed let loop
TR opt: let-loop-effects.rkt 7:31 z -- unboxed var -> table
TR opt: let-loop-effects.rkt 7:49 0.0+0.0i -- unboxed literal
TR opt: let-loop-effects.rkt 9:11 1.0+0.0i -- unboxed literal
TR opt: let-loop-effects.rkt 9:20 (loop (begin (displayln (quote x)) x) (begin (displayln (quote y)) (add1 y)) (begin (displayln (quote z)) z)) -- call to fun with unboxed args
TR opt: let-loop-effects.rkt 9:20 (loop (begin (displayln (quote x)) x) (begin (displayln (quote y)) (add1 y)) (begin (displayln (quote z)) z)) -- unbox float-complex
TR opt: let-loop-effects.rkt 9:20 (loop (begin (displayln (quote x)) x) (begin (displayln (quote y)) (add1 y)) (begin (displayln (quote z)) z)) -- unboxed call site
TR opt: let-loop-effects.rkt 9:26 (begin (displayln (quote x)) x) -- unbox float-complex
TR opt: let-loop-effects.rkt 9:48 x -- unboxed complex variable
TR opt: let-loop-effects.rkt 9:8 (+ 1.0+0.0i (loop (begin (displayln (quote x)) x) (begin (displayln (quote y)) (add1 y)) (begin (displayln (quote z)) z))) -- unboxed binary float complex
END
#<<END
x
y
z
1.0

END
#lang typed/racket
#reader tests/typed-racket/optimizer/reset-port


;; Ensure that loop unboxing doesn't change order of effects
(real-part
  (let: loop : Float-Complex ((x : Float-Complex 0.0+0.0i)
                              (y : Integer 0)
                              (z : Float-Complex 0.0+0.0i))
    (if (zero? y)
        (+ 1.0+0.0i (loop (begin (displayln 'x) x)
                          (begin (displayln 'y) (add1 y))
                          (begin (displayln 'z) z)))
        (+ x z))))
