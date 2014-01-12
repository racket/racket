#;#;
#<<END
TR info: pr14284.rkt 26:26 displayln -- hidden parameter
TR opt: pr14284.rkt 24:13 0.0+2.0i -- unboxed literal
TR opt: pr14284.rkt 24:6 (+ 1.0 0.0+2.0i) -- unboxed binary float complex
TR opt: pr14284.rkt 24:9 1.0 -- float-arg-expr in complex ops
TR opt: pr14284.rkt 25:27 x -- unboxed var -> table
TR opt: pr14284.rkt 25:4 optimizable -- fun -> unboxed fun
TR opt: pr14284.rkt 27:25 (+ x y) -- unboxed binary float complex
TR opt: pr14284.rkt 27:28 x -- leave var unboxed
TR opt: pr14284.rkt 27:30 y -- unbox float-complex
TR opt: pr14284.rkt 28:15 3.0+3.0i -- unboxed literal
TR opt: pr14284.rkt 28:2 (optimizable 3.0+3.0i "a" "b") -- call to fun with unboxed args
TR opt: pr14284.rkt 28:2 (optimizable 3.0+3.0i "a" "b") -- unboxed call site
END
#<<END
a
4.0+5.0i

END
#lang typed/racket

(letrec:
  ([y (+ 1.0 0.0+2.0i)]
   [optimizable (lambda: ([x : Float-Complex] [a : String] [b : String])
                         (displayln a)
                         (+ x y))])
  (optimizable 3.0+3.0i "a" "b"))
