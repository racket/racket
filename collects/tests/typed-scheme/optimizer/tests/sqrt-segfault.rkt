#;
(
sqrt-segfault.rkt 18:15 - -- binary float
sqrt-segfault.rkt 19:15 * -- binary float
sqrt-segfault.rkt 20:31 (#%app sqrt dist2) -- unexpected complex type
)

#lang typed/scheme
#:optimize


;; from the nbody-generic benchmark.
;; the result of sqrt was an Float-Complex, so float complex opts kicked
;; in but they resulted in segfaulting code.
;; the problem was that having Float be a subtype of Float-Complex was wrong
;; since you can't do unsafe-flreal-part of a float

(let* ([dx    (- 0.0 0.0)]
       [dist2 (* dx dx)]
       [mag   (assert (* dist2 (sqrt dist2)) flonum?)])
  (void))
