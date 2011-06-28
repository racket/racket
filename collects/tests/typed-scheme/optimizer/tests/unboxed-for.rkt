#;
(
TR opt: unboxed-for.rkt 33:0 (for/fold: : Float-Complex ((sum : Float-Complex 0.0+0.0i)) ((i : Float-Complex (quote (1.0+2.0i 2.0+4.0i)))) (+ i sum)) -- call to fun with unboxed args
TR opt: unboxed-for.rkt 33:0 (for/fold: : Float-Complex ((sum : Float-Complex 0.0+0.0i)) ((i : Float-Complex (quote (1.0+2.0i 2.0+4.0i)))) (+ i sum)) -- fun -> unboxed fun
TR opt: unboxed-for.rkt 30:0 #%module-begin -- in-list
TR opt: unboxed-for.rkt 33:0 (for/fold: : Float-Complex ((sum : Float-Complex 0.0+0.0i)) ((i : Float-Complex (quote (1.0+2.0i 2.0+4.0i)))) (+ i sum)) -- unbox float-complex
TR opt: unboxed-for.rkt 33:0 (for/fold: : Float-Complex ((sum : Float-Complex 0.0+0.0i)) ((i : Float-Complex (quote (1.0+2.0i 2.0+4.0i)))) (+ i sum)) -- unboxed call site
TR opt: unboxed-for.rkt 33:0 (for/fold: : Float-Complex ((sum : Float-Complex 0.0+0.0i)) ((i : Float-Complex (quote (1.0+2.0i 2.0+4.0i)))) (+ i sum)) -- unboxed function -> table
TR opt: unboxed-for.rkt 33:0 (for/fold: : Float-Complex ((sum : Float-Complex 0.0+0.0i)) ((i : Float-Complex (quote (1.0+2.0i 2.0+4.0i)))) (+ i sum)) -- unboxed let bindings
TR opt: unboxed-for.rkt 33:0 (for/fold: : Float-Complex ((sum : Float-Complex 0.0+0.0i)) ((i : Float-Complex (quote (1.0+2.0i 2.0+4.0i)))) (+ i sum)) -- unboxed let bindings
TR opt: unboxed-for.rkt 33:0 (for/fold: : Float-Complex ((sum : Float-Complex 0.0+0.0i)) ((i : Float-Complex (quote (1.0+2.0i 2.0+4.0i)))) (+ i sum)) -- unboxed let loop
TR opt: unboxed-for.rkt 33:0 (for/fold: : Float-Complex ((sum : Float-Complex 0.0+0.0i)) ((i : Float-Complex (quote (1.0+2.0i 2.0+4.0i)))) (+ i sum)) -- unboxed call site
TR opt: unboxed-for.rkt 33:31 sum -- leave var unboxed
TR opt: unboxed-for.rkt 33:31 sum -- unbox float-complex
TR opt: unboxed-for.rkt 33:31 sum -- unboxed complex variable
TR opt: unboxed-for.rkt 33:31 sum -- unboxed complex variable
TR opt: unboxed-for.rkt 33:31 sum -- unboxed complex variable
TR opt: unboxed-for.rkt 33:31 sum -- unboxed var -> table
TR opt: unboxed-for.rkt 33:53 0.0+0.0i -- unboxed literal
TR opt: unboxed-for.rkt 34:13 i -- unboxed complex variable
TR opt: unboxed-for.rkt 34:13 i -- unboxed complex variable
TR opt: unboxed-for.rkt 35:6 (+ i sum) -- unboxed binary float complex
TR opt: unboxed-for.rkt 35:9 i -- leave var unboxed
TR opt: unboxed-for.rkt 35:9 i -- unbox float-complex
TR opt: unboxed-for.rkt 35:11 sum -- leave var unboxed
TR opt: unboxed-for.rkt 35:11 sum -- unbox float-complex
3.0+6.0i
)

#lang typed/scheme
#:optimize

(for/fold: : Float-Complex   ((sum : Float-Complex   0.0+0.0i))
           ((i : Float-Complex   '(1.0+2.0i 2.0+4.0i)))
      (+ i sum))
