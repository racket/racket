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
