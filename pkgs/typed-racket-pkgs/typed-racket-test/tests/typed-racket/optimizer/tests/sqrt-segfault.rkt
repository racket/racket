#;#;
#<<END
TR missed opt: sqrt-segfault.rkt 10:31 (sqrt dist2) -- unexpected complex type
TR opt: sqrt-segfault.rkt 8:14 (- 0.0 0.0) -- binary float
TR opt: sqrt-segfault.rkt 9:14 (* dx dx) -- binary float
END
""
#lang typed/scheme
#:optimize
#reader tests/typed-racket/optimizer/reset-port

;; from the nbody-generic benchmark.
;; the result of sqrt was an Float-Complex, so float complex opts kicked
;; in but they resulted in segfaulting code.
;; the problem was that having Float be a subtype of Float-Complex was wrong
;; since you can't do unsafe-flreal-part of a float

(let* ([dx    (- 0.0 0.0)]
       [dist2 (* dx dx)]
       [mag   (assert (* dist2 (sqrt dist2)) flonum?)])
  (void))
