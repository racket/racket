#;#;
#<<END
TR info: let-loop-effects.rkt 45:34 displayln -- hidden parameter
TR info: let-loop-effects.rkt 46:34 displayln -- hidden parameter
TR info: let-loop-effects.rkt 47:34 displayln -- hidden parameter
TR opt: let-loop-effects.rkt 40:0 (real-part (let: loop : Float-Complex ((x : Float-Complex 0.0+0.0i) (y : Integer 0) (z : Float-Complex 0.0+0.0i)) (if (zero? y) (+ 1.0+0.0i (loop (begin (displayln (quote x)) x) (begin (displayln (quote y)) (add1 y)) (begin (displayln (quote z)) z))) (+ x z)))) -- complex accessor elimination
TR opt: let-loop-effects.rkt 41:2 (let: loop : Float-Complex ((x : Float-Complex 0.0+0.0i) (y : Integer 0) (z : Float-Complex 0.0+0.0i)) (if (zero? y) (+ 1.0+0.0i (loop (begin (displayln (quote x)) x) (begin (displayln (quote y)) (add1 y)) (begin (displayln (quote z)) z))) (+ x z))) -- unbox float-complex
TR opt: let-loop-effects.rkt 41:2 (let: loop : Float-Complex ((x : Float-Complex 0.0+0.0i) (y : Integer 0) (z : Float-Complex 0.0+0.0i)) (if (zero? y) (+ 1.0+0.0i (loop (begin (displayln (quote x)) x) (begin (displayln (quote y)) (add1 y)) (begin (displayln (quote z)) z))) (+ x z))) -- unboxed call site
TR opt: let-loop-effects.rkt 41:31 x -- unboxed var -> table
TR opt: let-loop-effects.rkt 41:49 0.0+0.0i -- unboxed literal
TR opt: let-loop-effects.rkt 41:8 loop -- fun -> unboxed fun
TR opt: let-loop-effects.rkt 41:8 loop -- unboxed let loop
TR opt: let-loop-effects.rkt 43:31 z -- unboxed var -> table
TR opt: let-loop-effects.rkt 43:49 0.0+0.0i -- unboxed literal
TR opt: let-loop-effects.rkt 45:11 1.0+0.0i -- unboxed literal
TR opt: let-loop-effects.rkt 45:20 (loop (begin (displayln (quote x)) x) (begin (displayln (quote y)) (add1 y)) (begin (displayln (quote z)) z)) -- call to fun with unboxed args
TR opt: let-loop-effects.rkt 45:20 (loop (begin (displayln (quote x)) x) (begin (displayln (quote y)) (add1 y)) (begin (displayln (quote z)) z)) -- unbox float-complex
TR opt: let-loop-effects.rkt 45:20 (loop (begin (displayln (quote x)) x) (begin (displayln (quote y)) (add1 y)) (begin (displayln (quote z)) z)) -- unboxed call site
TR opt: let-loop-effects.rkt 45:26 (begin (displayln (quote x)) x) -- unbox float-complex
TR opt: let-loop-effects.rkt 45:48 x -- unboxed complex variable
TR opt: let-loop-effects.rkt 45:8 (+ 1.0+0.0i (loop (begin (displayln (quote x)) x) (begin (displayln (quote y)) (add1 y)) (begin (displayln (quote z)) z))) -- unboxed binary float complex
TR opt: let-loop-effects.rkt 46:48 (add1 y) -- fixnum add1
TR opt: let-loop-effects.rkt 47:26 (begin (displayln (quote z)) z) -- unbox float-complex
TR opt: let-loop-effects.rkt 47:48 z -- unboxed complex variable
TR opt: let-loop-effects.rkt 48:11 x -- leave var unboxed
TR opt: let-loop-effects.rkt 48:13 z -- leave var unboxed
TR opt: let-loop-effects.rkt 48:8 (+ x z) -- unboxed binary float complex
END
#<<END
x
y
z
1.0

END
#lang typed/racket


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
