#;#;
#<<END
TR info: dead-inf-comp.rkt 110:41 displayln -- hidden parameter
TR missed opt: dead-inf-comp.rkt 10:4 (< rat -inf.0) -- generic comparison -- caused by: 10:7 rat
TR missed opt: dead-inf-comp.rkt 110:0 (for: ((i (in-range 5 +inf.0 2)) (j 3)) (displayln i)) -- generic comparison -- caused by: 110:0 (for: ((i (in-range 5 +inf.0 2)) (j 3)) (displayln i))
TR missed opt: dead-inf-comp.rkt 13:4 (< -inf.0 rat) -- generic comparison -- caused by: 13:14 rat
TR missed opt: dead-inf-comp.rkt 17:4 (> +inf.0 rat) -- generic comparison -- caused by: 17:14 rat
TR missed opt: dead-inf-comp.rkt 20:4 (> rat +inf.0) -- generic comparison -- caused by: 20:7 rat
TR missed opt: dead-inf-comp.rkt 23:4 (> -inf.0 rat) -- generic comparison -- caused by: 23:14 rat
TR missed opt: dead-inf-comp.rkt 26:4 (> rat -inf.0) -- generic comparison -- caused by: 26:7 rat
TR missed opt: dead-inf-comp.rkt 30:4 (<= rat +inf.0) -- generic comparison -- caused by: 30:8 rat
TR missed opt: dead-inf-comp.rkt 33:4 (<= +inf.0 rat) -- generic comparison -- caused by: 33:15 rat
TR missed opt: dead-inf-comp.rkt 36:4 (<= rat -inf.0) -- generic comparison -- caused by: 36:8 rat
TR missed opt: dead-inf-comp.rkt 39:4 (<= -inf.0 rat) -- generic comparison -- caused by: 39:15 rat
TR missed opt: dead-inf-comp.rkt 43:4 (>= +inf.0 rat) -- generic comparison -- caused by: 43:15 rat
TR missed opt: dead-inf-comp.rkt 46:4 (>= rat +inf.0) -- generic comparison -- caused by: 46:8 rat
TR missed opt: dead-inf-comp.rkt 49:4 (>= -inf.0 rat) -- generic comparison -- caused by: 49:15 rat
TR missed opt: dead-inf-comp.rkt 4:4 (< rat +inf.0) -- generic comparison -- caused by: 4:7 rat
TR missed opt: dead-inf-comp.rkt 52:4 (>= rat -inf.0) -- generic comparison -- caused by: 52:8 rat
TR missed opt: dead-inf-comp.rkt 7:4 (< +inf.0 rat) -- generic comparison -- caused by: 7:14 rat
TR opt: dead-inf-comp.rkt 100:4 (quote dead) -- dead then branch
TR opt: dead-inf-comp.rkt 103:4 (quote dead) -- dead then branch
TR opt: dead-inf-comp.rkt 107:4 (quote dead) -- dead else branch
TR opt: dead-inf-comp.rkt 110:0 (for: ((i (in-range 5 +inf.0 2)) (j 3)) (displayln i)) -- dead else branch
TR opt: dead-inf-comp.rkt 110:0 (for: ((i (in-range 5 +inf.0 2)) (j 3)) (displayln i)) -- dead else branch
TR opt: dead-inf-comp.rkt 110:0 (for: ((i (in-range 5 +inf.0 2)) (j 3)) (displayln i)) -- dead else branch
TR opt: dead-inf-comp.rkt 110:0 (for: ((i (in-range 5 +inf.0 2)) (j 3)) (displayln i)) -- dead else branch
TR opt: dead-inf-comp.rkt 110:0 (for: ((i (in-range 5 +inf.0 2)) (j 3)) (displayln i)) -- dead else branch
TR opt: dead-inf-comp.rkt 110:0 (for: ((i (in-range 5 +inf.0 2)) (j 3)) (displayln i)) -- dead else branch
TR opt: dead-inf-comp.rkt 110:36 3 -- in-range
TR opt: dead-inf-comp.rkt 11:4 (quote dead) -- dead then branch
TR opt: dead-inf-comp.rkt 15:4 (quote dead) -- dead else branch
TR opt: dead-inf-comp.rkt 19:4 (quote dead) -- dead else branch
TR opt: dead-inf-comp.rkt 21:4 (quote dead) -- dead then branch
TR opt: dead-inf-comp.rkt 24:4 (quote dead) -- dead then branch
TR opt: dead-inf-comp.rkt 28:4 (quote dead) -- dead else branch
TR opt: dead-inf-comp.rkt 32:4 (quote dead) -- dead else branch
TR opt: dead-inf-comp.rkt 34:4 (quote dead) -- dead then branch
TR opt: dead-inf-comp.rkt 37:4 (quote dead) -- dead then branch
TR opt: dead-inf-comp.rkt 41:4 (quote dead) -- dead else branch
TR opt: dead-inf-comp.rkt 45:4 (quote dead) -- dead else branch
TR opt: dead-inf-comp.rkt 47:4 (quote dead) -- dead then branch
TR opt: dead-inf-comp.rkt 50:4 (quote dead) -- dead then branch
TR opt: dead-inf-comp.rkt 54:4 (quote dead) -- dead else branch
TR opt: dead-inf-comp.rkt 59:4 (quote dead) -- dead else branch
TR opt: dead-inf-comp.rkt 61:4 (quote dead) -- dead then branch
TR opt: dead-inf-comp.rkt 64:4 (quote dead) -- dead then branch
TR opt: dead-inf-comp.rkt 68:4 (quote dead) -- dead else branch
TR opt: dead-inf-comp.rkt 6:4 (quote dead) -- dead else branch
TR opt: dead-inf-comp.rkt 72:4 (quote dead) -- dead else branch
TR opt: dead-inf-comp.rkt 74:4 (quote dead) -- dead then branch
TR opt: dead-inf-comp.rkt 77:4 (quote dead) -- dead then branch
TR opt: dead-inf-comp.rkt 81:4 (quote dead) -- dead else branch
TR opt: dead-inf-comp.rkt 85:4 (quote dead) -- dead else branch
TR opt: dead-inf-comp.rkt 87:4 (quote dead) -- dead then branch
TR opt: dead-inf-comp.rkt 8:4 (quote dead) -- dead then branch
TR opt: dead-inf-comp.rkt 90:4 (quote dead) -- dead then branch
TR opt: dead-inf-comp.rkt 94:4 (quote dead) -- dead else branch
TR opt: dead-inf-comp.rkt 98:4 (quote dead) -- dead else branch
END
#<<END
'live
'live
'live
'live
'live
'live
'live
'live
'live
'live
'live
'live
'live
'live
'live
'live
'live
'live
'live
'live
'live
'live
'live
'live
'live
'live
'live
'live
'live
'live
'live
'live
5
7
9

END
#lang typed/racket/base
#reader tests/typed-racket/optimizer/reset-port

(define: rat : Exact-Rational 2/3)

(if (< rat +inf.0)
    'live
    'dead)
(if (< +inf.0 rat)
    'dead
    'live)
(if (< rat -inf.0)
    'dead
    'live)
(if (< -inf.0 rat)
    'live
    'dead)

(if (> +inf.0 rat)
    'live
    'dead)
(if (> rat +inf.0)
    'dead
    'live)
(if (> -inf.0 rat)
    'dead
    'live)
(if (> rat -inf.0)
    'live
    'dead)

(if (<= rat +inf.0)
    'live
    'dead)
(if (<= +inf.0 rat)
    'dead
    'live)
(if (<= rat -inf.0)
    'dead
    'live)
(if (<= -inf.0 rat)
    'live
    'dead)

(if (>= +inf.0 rat)
    'live
    'dead)
(if (>= rat +inf.0)
    'dead
    'live)
(if (>= -inf.0 rat)
    'dead
    'live)
(if (>= rat -inf.0)
    'live
    'dead)

;; single flonums
(if (< rat +inf.f)
    'live
    'dead)
(if (< +inf.f rat)
    'dead
    'live)
(if (< rat -inf.f)
    'dead
    'live)
(if (< -inf.f rat)
    'live
    'dead)

(if (> +inf.f rat)
    'live
    'dead)
(if (> rat +inf.f)
    'dead
    'live)
(if (> -inf.f rat)
    'dead
    'live)
(if (> rat -inf.f)
    'live
    'dead)

(if (<= rat +inf.f)
    'live
    'dead)
(if (<= +inf.f rat)
    'dead
    'live)
(if (<= rat -inf.f)
    'dead
    'live)
(if (<= -inf.f rat)
    'live
    'dead)

(if (>= +inf.f rat)
    'live
    'dead)
(if (>= rat +inf.f)
    'dead
    'live)
(if (>= -inf.f rat)
    'dead
    'live)
(if (>= rat -inf.f)
    'live
    'dead)


(for: ([i (in-range 5 +inf.0 2)] [j 3]) (displayln i))
