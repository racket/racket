#;
(
TR opt: unboxed-for.rkt 40:0 #%module-begin -- in-list
TR opt: unboxed-for.rkt 45:9 i -- unbox float-complex
TR opt: unboxed-for.rkt 45:11 sum -- unbox float-complex
TR opt: unboxed-for.rkt 45:6 (+ i sum) -- unboxed binary float complex
TR opt: unboxed-for.rkt 45:9 i -- unbox float-complex
TR opt: unboxed-for.rkt 45:11 sum -- unbox float-complex
TR opt: unboxed-for.rkt 45:6 (+ i sum) -- unboxed binary float complex
TR opt: unboxed-for.rkt 43:31 sum -- unboxed var -> table
TR opt: unboxed-for.rkt 43:0 (for/fold: : Float-Complex ((sum : Float-Complex 0.0+0.0i)) ((i : Float-Complex (quote (1.0+2.0i 2.0+4.0i)))) (+ i sum)) -- fun -> unboxed fun
TR opt: unboxed-for.rkt 43:31 sum -- unboxed complex variable
TR opt: unboxed-for.rkt 45:9 i -- unbox float-complex
TR opt: unboxed-for.rkt 45:11 sum -- unbox float-complex
TR opt: unboxed-for.rkt 45:6 (+ i sum) -- unboxed binary float complex
TR opt: unboxed-for.rkt 43:0 (for/fold: : Float-Complex ((sum : Float-Complex 0.0+0.0i)) ((i : Float-Complex (quote (1.0+2.0i 2.0+4.0i)))) (+ i sum)) -- unbox float-complex
TR opt: unboxed-for.rkt 43:0 (for/fold: : Float-Complex ((sum : Float-Complex 0.0+0.0i)) ((i : Float-Complex (quote (1.0+2.0i 2.0+4.0i)))) (+ i sum)) -- unboxed let bindings
TR opt: unboxed-for.rkt 44:13 i -- unboxed complex variable
TR opt: unboxed-for.rkt 44:13 i -- unboxed complex variable
TR opt: unboxed-for.rkt 45:9 i -- leave var unboxed
TR opt: unboxed-for.rkt 45:11 sum -- unbox float-complex
TR opt: unboxed-for.rkt 45:6 (+ i sum) -- unboxed binary float complex
TR opt: unboxed-for.rkt 43:31 sum -- leave var unboxed
TR opt: unboxed-for.rkt 43:0 (for/fold: : Float-Complex ((sum : Float-Complex 0.0+0.0i)) ((i : Float-Complex (quote (1.0+2.0i 2.0+4.0i)))) (+ i sum)) -- unboxed let bindings
TR opt: unboxed-for.rkt 45:9 i -- leave var unboxed
TR opt: unboxed-for.rkt 45:11 sum -- leave var unboxed
TR opt: unboxed-for.rkt 45:6 (+ i sum) -- unboxed binary float complex
TR opt: unboxed-for.rkt 44:13 i -- unboxed complex variable
TR opt: unboxed-for.rkt 43:31 sum -- unbox float-complex
TR opt: unboxed-for.rkt 43:0 (for/fold: : Float-Complex ((sum : Float-Complex 0.0+0.0i)) ((i : Float-Complex (quote (1.0+2.0i 2.0+4.0i)))) (+ i sum)) -- unboxed call site
TR opt: unboxed-for.rkt 43:0 (for/fold: : Float-Complex ((sum : Float-Complex 0.0+0.0i)) ((i : Float-Complex (quote (1.0+2.0i 2.0+4.0i)))) (+ i sum)) -- call to fun with unboxed args
TR opt: unboxed-for.rkt 43:31 sum -- unboxed complex variable
TR opt: unboxed-for.rkt 43:31 sum -- unboxed complex variable
TR opt: unboxed-for.rkt 43:53 0.0+0.0i -- unboxed literal
TR opt: unboxed-for.rkt 43:0 (for/fold: : Float-Complex ((sum : Float-Complex 0.0+0.0i)) ((i : Float-Complex (quote (1.0+2.0i 2.0+4.0i)))) (+ i sum)) -- unboxed call site
TR opt: unboxed-for.rkt 43:0 (for/fold: : Float-Complex ((sum : Float-Complex 0.0+0.0i)) ((i : Float-Complex (quote (1.0+2.0i 2.0+4.0i)))) (+ i sum)) -- unboxed let loop
3.0+6.0i
)

#lang typed/scheme
#:optimize

(for/fold: : Float-Complex   ((sum : Float-Complex   0.0+0.0i))
           ((i : Float-Complex   '(1.0+2.0i 2.0+4.0i)))
      (+ i sum))
