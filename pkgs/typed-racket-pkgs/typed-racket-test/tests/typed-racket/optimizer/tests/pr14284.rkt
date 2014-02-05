#;#;
#<<END
TR info: pr14284.rkt 5:26 displayln -- hidden parameter
TR opt: pr14284.rkt 3:13 0.0+2.0i -- unboxed literal
TR opt: pr14284.rkt 3:6 (+ 1.0 0.0+2.0i) -- unboxed binary float complex
TR opt: pr14284.rkt 3:9 1.0 -- float in complex ops
TR opt: pr14284.rkt 4:27 x -- unboxed var -> table
TR opt: pr14284.rkt 4:4 optimizable -- fun -> unboxed fun
TR opt: pr14284.rkt 6:25 (+ x y) -- unboxed binary float complex
TR opt: pr14284.rkt 6:28 x -- leave var unboxed
TR opt: pr14284.rkt 6:30 y -- unbox float-complex
TR opt: pr14284.rkt 7:15 3.0+3.0i -- unboxed literal
TR opt: pr14284.rkt 7:2 (optimizable 3.0+3.0i "a" "b") -- call to fun with unboxed args
TR opt: pr14284.rkt 7:2 (optimizable 3.0+3.0i "a" "b") -- unboxed call site
END
#<<END
a
4.0+5.0i

END
#lang typed/racket
#reader tests/typed-racket/optimizer/reset-port

(letrec:
  ([y (+ 1.0 0.0+2.0i)]
   [optimizable (lambda: ([x : Float-Complex] [a : String] [b : String])
                         (displayln a)
                         (+ x y))])
  (optimizable 3.0+3.0i "a" "b"))
