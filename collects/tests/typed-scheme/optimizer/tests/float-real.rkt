#;
(
float-real.rkt line 13 col 1 - + - binary float
float-real.rkt line 14 col 1 - + - binary float
5.3
8.7
14.26
 )

#lang typed/racket

;; reals within float expressions should be coerced when it's safe to do so
(+ 2.3 (ann 3 Real)) ; safe
(+ 2.3 (* (ann 2 Integer) 3.2)) ; inner = unsafe, outer = safe
(* 2.3 (* (ann 2 Integer) 3.1)) ; all unsafe
