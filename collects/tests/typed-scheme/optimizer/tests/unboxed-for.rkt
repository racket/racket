#;
(
#f line #f col #f - make-sequence - in-list
#f line #f col #f - (let-values (((pos->vals pos-next init pos-cont? val-cont? all-cont?) (#%app make-sequence (quote (i)) (quote (1.0+2.0i 2.0+4.0i))))) (#%app void) (#%app (letrec-values (((for-loop) (lambda (sum pos) (if (#%expression (#%app pos-cont? pos)) (let-values (((i) (#%app pos->vals pos))) (if (#%expression (#%app val-cont? i)) (let-values (((sum) (let-values (((sum) sum)) (let-values () (#%app + i sum))))) (if (#%expression (#%app all-cont? pos i)) (#%app for-loop sum (#%app pos-next pos)) sum)) sum)) sum)))) for-loop) (quote 0.0+0.0i) init)) - unboxed let bindings
unboxed-for.rkt line 57 col 9 - i - unbox inexact-complex
unboxed-for.rkt line 57 col 11 - sum - unbox inexact-complex
unboxed-for.rkt line 57 col 7 - + - unboxed binary inexact complex
unboxed-for.rkt line 57 col 6 - (#%app + i sum) - unboxed inexact complex
unboxed-for.rkt line 57 col 9 - i - unbox inexact-complex
unboxed-for.rkt line 57 col 11 - sum - unbox inexact-complex
unboxed-for.rkt line 57 col 7 - + - unboxed binary inexact complex
unboxed-for.rkt line 57 col 6 - (#%app + i sum) - unboxed inexact complex
unboxed-for.rkt line 55 col 31 - sum - unboxed var -> table
#f line #f col #f - for-loop - unboxed function -> table
#f line #f col #f - for-loop - fun -> unboxed fun
unboxed-for.rkt line 55 col 31 - sum - unboxed complex variable
unboxed-for.rkt line 57 col 9 - i - unbox inexact-complex
unboxed-for.rkt line 57 col 11 - sum - unbox inexact-complex
unboxed-for.rkt line 57 col 7 - + - unboxed binary inexact complex
unboxed-for.rkt line 57 col 6 - (#%app + i sum) - unboxed inexact complex
#f line #f col #f - (#%app pos->vals pos) - unbox inexact-complex
#f line #f col #f - (let-values (((i) (#%app pos->vals pos))) (if (#%expression (#%app val-cont? i)) (let-values (((sum) (let-values (((sum) sum)) (let-values () (#%app + i sum))))) (if (#%expression (#%app all-cont? pos i)) (#%app for-loop sum (#%app pos-next pos)) sum)) sum)) - unboxed let bindings
unboxed-for.rkt line 56 col 13 - i - unboxed complex variable
unboxed-for.rkt line 56 col 13 - i - unboxed complex variable
unboxed-for.rkt line 57 col 9 - i - leave var unboxed
unboxed-for.rkt line 57 col 11 - sum - unbox inexact-complex
unboxed-for.rkt line 57 col 7 - + - unboxed binary inexact complex
unboxed-for.rkt line 57 col 6 - (#%app + i sum) - unboxed inexact complex
unboxed-for.rkt line 55 col 31 - sum - leave var unboxed
#f line #f col #f - (let-values (((sum) sum)) (let-values () (#%app + i sum))) - unboxed let bindings
#f line #f col #f - (let-values () (#%app + i sum)) - unboxed let bindings
unboxed-for.rkt line 57 col 9 - i - leave var unboxed
unboxed-for.rkt line 57 col 11 - sum - leave var unboxed
unboxed-for.rkt line 57 col 7 - + - unboxed binary inexact complex
unboxed-for.rkt line 57 col 6 - (#%app + i sum) - unboxed inexact complex
#f line #f col #f - (let-values (((sum) (let-values (((sum) sum)) (let-values () (#%app + i sum))))) (if (#%expression (#%app all-cont? pos i)) (#%app for-loop sum (#%app pos-next pos)) sum)) - unboxed let bindings
unboxed-for.rkt line 56 col 13 - i - unboxed complex variable
unboxed-for.rkt line 55 col 31 - sum - unbox inexact-complex
#f line #f col #f - for-loop - unboxed call site
#f line #f col #f - for-loop - call to fun with unboxed args
unboxed-for.rkt line 55 col 31 - sum - unboxed complex variable
unboxed-for.rkt line 55 col 31 - sum - unboxed complex variable
unboxed-for.rkt line 55 col 0 - (letrec-values (((for-loop) (lambda (sum pos) (if (#%expression (#%app pos-cont? pos)) (let-values (((i) (#%app pos->vals pos))) (if (#%expression (#%app val-cont? i)) (let-values (((sum) (let-values (((sum) sum)) (let-values () (#%app + i sum))))) (if (#%expression (#%app all-cont? pos i)) (#%app for-loop sum (#%app pos-next pos)) sum)) sum)) sum)))) for-loop) - unboxed let bindings
unboxed-for.rkt line 55 col 53 - 0.0+0.0i - unboxed literal
unboxed-for.rkt line 55 col 0 - (letrec-values (((for-loop) (lambda (sum pos) (if (#%expression (#%app pos-cont? pos)) (let-values (((i) (#%app pos->vals pos))) (if (#%expression (#%app val-cont? i)) (let-values (((sum) (let-values (((sum) sum)) (let-values () (#%app + i sum))))) (if (#%expression (#%app all-cont? pos i)) (#%app for-loop sum (#%app pos-next pos)) sum)) sum)) sum)))) for-loop) - unboxed call site
#f line #f col #f - for-loop - unboxed let loop
3.0+6.0i
)

#lang typed/scheme
#:optimize



(for/fold: : Inexact-Complex ((sum : Inexact-Complex 0.0+0.0i))
           ((i : Inexact-Complex '(1.0+2.0i 2.0+4.0i)))
      (+ i sum))
