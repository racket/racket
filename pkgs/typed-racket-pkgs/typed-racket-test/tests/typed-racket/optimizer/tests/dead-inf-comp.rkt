#;
#<<END
TR opt: dead-inf-comp.rkt 106:4 (quote dead) -- dead else branch
TR opt: dead-inf-comp.rkt 108:4 (quote dead) -- dead then branch
TR opt: dead-inf-comp.rkt 111:4 (quote dead) -- dead then branch
TR opt: dead-inf-comp.rkt 115:4 (quote dead) -- dead else branch
TR opt: dead-inf-comp.rkt 119:4 (quote dead) -- dead else branch
TR opt: dead-inf-comp.rkt 121:4 (quote dead) -- dead then branch
TR opt: dead-inf-comp.rkt 124:4 (quote dead) -- dead then branch
TR opt: dead-inf-comp.rkt 128:4 (quote dead) -- dead else branch
TR opt: dead-inf-comp.rkt 132:4 (quote dead) -- dead else branch
TR opt: dead-inf-comp.rkt 134:4 (quote dead) -- dead then branch
TR opt: dead-inf-comp.rkt 137:4 (quote dead) -- dead then branch
TR opt: dead-inf-comp.rkt 141:4 (quote dead) -- dead else branch
TR opt: dead-inf-comp.rkt 145:4 (quote dead) -- dead else branch
TR opt: dead-inf-comp.rkt 147:4 (quote dead) -- dead then branch
TR opt: dead-inf-comp.rkt 150:4 (quote dead) -- dead then branch
TR opt: dead-inf-comp.rkt 154:4 (quote dead) -- dead else branch
TR opt: dead-inf-comp.rkt 159:4 (quote dead) -- dead else branch
TR info: dead-inf-comp.rkt 157:4 (< rat +inf.f) -- exact real arith
TR opt: dead-inf-comp.rkt 161:4 (quote dead) -- dead then branch
TR info: dead-inf-comp.rkt 160:4 (< +inf.f rat) -- exact real arith
TR opt: dead-inf-comp.rkt 164:4 (quote dead) -- dead then branch
TR info: dead-inf-comp.rkt 163:4 (< rat -inf.f) -- exact real arith
TR opt: dead-inf-comp.rkt 168:4 (quote dead) -- dead else branch
TR info: dead-inf-comp.rkt 166:4 (< -inf.f rat) -- exact real arith
TR opt: dead-inf-comp.rkt 172:4 (quote dead) -- dead else branch
TR info: dead-inf-comp.rkt 170:4 (> +inf.f rat) -- exact real arith
TR opt: dead-inf-comp.rkt 174:4 (quote dead) -- dead then branch
TR info: dead-inf-comp.rkt 173:4 (> rat +inf.f) -- exact real arith
TR opt: dead-inf-comp.rkt 177:4 (quote dead) -- dead then branch
TR info: dead-inf-comp.rkt 176:4 (> -inf.f rat) -- exact real arith
TR opt: dead-inf-comp.rkt 181:4 (quote dead) -- dead else branch
TR info: dead-inf-comp.rkt 179:4 (> rat -inf.f) -- exact real arith
TR opt: dead-inf-comp.rkt 185:4 (quote dead) -- dead else branch
TR info: dead-inf-comp.rkt 183:4 (<= rat +inf.f) -- exact real arith
TR opt: dead-inf-comp.rkt 187:4 (quote dead) -- dead then branch
TR info: dead-inf-comp.rkt 186:4 (<= +inf.f rat) -- exact real arith
TR opt: dead-inf-comp.rkt 190:4 (quote dead) -- dead then branch
TR info: dead-inf-comp.rkt 189:4 (<= rat -inf.f) -- exact real arith
TR opt: dead-inf-comp.rkt 194:4 (quote dead) -- dead else branch
TR info: dead-inf-comp.rkt 192:4 (<= -inf.f rat) -- exact real arith
TR opt: dead-inf-comp.rkt 198:4 (quote dead) -- dead else branch
TR info: dead-inf-comp.rkt 196:4 (>= +inf.f rat) -- exact real arith
TR opt: dead-inf-comp.rkt 200:4 (quote dead) -- dead then branch
TR info: dead-inf-comp.rkt 199:4 (>= rat +inf.f) -- exact real arith
TR opt: dead-inf-comp.rkt 203:4 (quote dead) -- dead then branch
TR info: dead-inf-comp.rkt 202:4 (>= -inf.f rat) -- exact real arith
TR opt: dead-inf-comp.rkt 207:4 (quote dead) -- dead else branch
TR info: dead-inf-comp.rkt 205:4 (>= rat -inf.f) -- exact real arith
TR opt: dead-inf-comp.rkt 100:0 #%module-begin -- in-range
TR opt: dead-inf-comp.rkt 100:0 #%module-begin -- dead else branch
TR opt: dead-inf-comp.rkt 100:0 #%module-begin -- dead else branch
TR opt: dead-inf-comp.rkt 100:0 #%module-begin -- dead else branch
TR opt: dead-inf-comp.rkt 210:0 (for: ((i (in-range 5 +inf.0 2)) (j 3)) (displayln i)) -- dead else branch
TR opt: dead-inf-comp.rkt 210:0 (for: ((i (in-range 5 +inf.0 2)) (j 3)) (displayln i)) -- dead else branch
TR info: dead-inf-comp.rkt 210:41 displayln -- hidden parameter
TR opt: dead-inf-comp.rkt 210:0 (for: ((i (in-range 5 +inf.0 2)) (j 3)) (displayln i)) -- dead else branch
TR opt: dead-inf-comp.rkt 210:0 (for: ((i (in-range 5 +inf.0 2)) (j 3)) (displayln i)) -- dead else branch
TR opt: dead-inf-comp.rkt 210:0 (for: ((i (in-range 5 +inf.0 2)) (j 3)) (displayln i)) -- dead else branch
TR info: dead-inf-comp.rkt 210:41 displayln -- hidden parameter
TR opt: dead-inf-comp.rkt 210:0 (for: ((i (in-range 5 +inf.0 2)) (j 3)) (displayln i)) -- dead else branch
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
