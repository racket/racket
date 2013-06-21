#;#;
#<<END
TR opt: sqrt-segfault.rkt 20:14 (- 0.0 0.0) -- binary float
TR opt: sqrt-segfault.rkt 21:14 (* dx dx) -- binary float
TR missed opt: sqrt-segfault.rkt 22:31 (sqrt dist2) -- unexpected complex type

END
""

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
