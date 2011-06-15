#;
(
TR opt: #f (no location) (#%app pos->vals pos) -- unbox float-complex
TR opt: #f (no location) (let-values (((i) (#%app pos->vals pos))) (if (#%expression (if val-cont? (#%app val-cont? i) (quote #t))) (let-values (((sum) (let-values (((sum) sum)) (let-values () (#%app + i sum))))) (if (#%expression (if all-cont? (#%app all-cont? pos i) (quote #t))) (#%app for-loop sum (#%app pos-next pos)) sum)) sum)) -- unboxed let bindings
TR opt: #f (no location) (let-values (((sum) sum)) (let-values () (#%app + i sum))) -- unboxed let bindings
TR opt: #f (no location) for-loop -- call to fun with unboxed args
TR opt: #f (no location) for-loop -- fun -> unboxed fun
TR opt: #f (no location) for-loop -- unboxed call site
TR opt: #f (no location) for-loop -- unboxed function -> table
TR opt: #f (no location) for-loop -- unboxed let loop
TR opt: #f (no location) make-sequence -- in-list
TR opt: unboxed-for.rkt 31:0 (letrec-values (((for-loop) (lambda (sum pos) (if (#%expression (if pos-cont? (#%app pos-cont? pos) (quote #t))) (let-values (((i) (#%app pos->vals pos))) (if (#%expression (if val-cont? (#%app val-cont? i) (quote #t))) (let-values (((sum) (let-values (((sum) sum)) (let-values () (#%app + i sum))))) (if (#%expression (if all-cont? (#%app all-cont? pos i) (quote #t))) (#%app for-loop sum (#%app pos-next pos)) sum)) sum)) sum)))) for-loop) -- unboxed call site
TR opt: unboxed-for.rkt 31:31 sum -- leave var unboxed
TR opt: unboxed-for.rkt 31:31 sum -- unbox float-complex
TR opt: unboxed-for.rkt 31:31 sum -- unboxed complex variable
TR opt: unboxed-for.rkt 31:31 sum -- unboxed var -> table
TR opt: unboxed-for.rkt 31:53 0.0+0.0i -- unboxed literal
TR opt: unboxed-for.rkt 32:13 i -- unboxed complex variable
TR opt: unboxed-for.rkt 33:6 (#%app + i sum) -- unboxed float complex
TR opt: unboxed-for.rkt 33:7 + -- unboxed binary float complex
TR opt: unboxed-for.rkt 33:9 i -- leave var unboxed
TR opt: unboxed-for.rkt 33:9 i -- unbox float-complex
TR opt: unboxed-for.rkt 33:11 sum -- leave var unboxed
TR opt: unboxed-for.rkt 33:11 sum -- unbox float-complex
3.0+6.0i
)

#lang typed/scheme
#:optimize

(for/fold: : Float-Complex   ((sum : Float-Complex   0.0+0.0i))
           ((i : Float-Complex   '(1.0+2.0i 2.0+4.0i)))
      (+ i sum))
