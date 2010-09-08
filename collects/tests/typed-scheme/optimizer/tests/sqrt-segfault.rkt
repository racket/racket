#;
(
sqrt-segfault.rkt line 22 col 15 - - - binary float
sqrt-segfault.rkt line 22 col 0 - (let-values (((dx) (#%app - (quote 0.0) (quote 0.0)))) (let-values (((dist2) (#%app * dx dx))) (let-values (((mag) (let-values (((val) (#%app * dist2 (#%app sqrt dist2)))) (if (#%app inexact-real? val) val (#%app error (quote Assertion failed)))))) (#%app void)))) - unboxed let bindings
sqrt-segfault.rkt line 23 col 15 - * - binary float
sqrt-segfault.rkt line 22 col 0 - (let-values (((dist2) (#%app * dx dx))) (let-values (((mag) (let-values (((val) (#%app * dist2 (#%app sqrt dist2)))) (if (#%app inexact-real? val) val (#%app error (quote Assertion failed)))))) (#%app void))) - unboxed let bindings
sqrt-segfault.rkt line 24 col 14 - (let-values (((val) (#%app * dist2 (#%app sqrt dist2)))) (if (#%app inexact-real? val) val (#%app error (quote Assertion failed)))) - unboxed let bindings
sqrt-segfault.rkt line 22 col 0 - (let-values (((mag) (let-values (((val) (#%app * dist2 (#%app sqrt dist2)))) (if (#%app inexact-real? val) val (#%app error (quote Assertion failed)))))) (#%app void)) - unboxed let bindings
)

#lang typed/scheme
#:optimize

(require racket/unsafe/ops)

;; from the nbody-generic benchmark.
;; the result of sqrt was an Inexact-Complex, so inexact complex opts kicked
;; in but they resulted in segfaulting code.
;; the problem was that having Float be a subtype of Inexact-Complex was wrong
;; since you can't do unsafe-flreal-part of a float

(let* ([dx    (- 0.0 0.0)]
       [dist2 (* dx dx)]
       [mag   (assert (* dist2 (sqrt dist2)) inexact-real?)])
  (void))
