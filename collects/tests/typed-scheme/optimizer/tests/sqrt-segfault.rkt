#;
(
sqrt-segfault.rkt line 18 col 15 - - - binary float
sqrt-segfault.rkt line 19 col 15 - * - binary float
)

#lang typed/scheme
#:optimize



;; from the nbody-generic benchmark.
;; the result of sqrt was an Inexact-Complex, so float complex opts kicked
;; in but they resulted in segfaulting code.
;; the problem was that having Float be a subtype of Inexact-Complex was wrong
;; since you can't do unsafe-flreal-part of a float

(let* ([dx    (- 0.0 0.0)]
       [dist2 (* dx dx)]
       [mag   (assert (* dist2 (sqrt dist2)) flonum?)])
  (void))
