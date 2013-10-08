#;#;
#<<END
TR info: dead-inf-comp.rkt 159:4 (< rat +inf.f) -- exact real arith
TR info: dead-inf-comp.rkt 162:4 (< +inf.f rat) -- exact real arith
TR info: dead-inf-comp.rkt 165:4 (< rat -inf.f) -- exact real arith
TR info: dead-inf-comp.rkt 168:4 (< -inf.f rat) -- exact real arith
TR info: dead-inf-comp.rkt 172:4 (> +inf.f rat) -- exact real arith
TR info: dead-inf-comp.rkt 175:4 (> rat +inf.f) -- exact real arith
TR info: dead-inf-comp.rkt 178:4 (> -inf.f rat) -- exact real arith
TR info: dead-inf-comp.rkt 181:4 (> rat -inf.f) -- exact real arith
TR info: dead-inf-comp.rkt 185:4 (<= rat +inf.f) -- exact real arith
TR info: dead-inf-comp.rkt 188:4 (<= +inf.f rat) -- exact real arith
TR info: dead-inf-comp.rkt 191:4 (<= rat -inf.f) -- exact real arith
TR info: dead-inf-comp.rkt 194:4 (<= -inf.f rat) -- exact real arith
TR info: dead-inf-comp.rkt 198:4 (>= +inf.f rat) -- exact real arith
TR info: dead-inf-comp.rkt 201:4 (>= rat +inf.f) -- exact real arith
TR info: dead-inf-comp.rkt 204:4 (>= -inf.f rat) -- exact real arith
TR info: dead-inf-comp.rkt 207:4 (>= rat -inf.f) -- exact real arith
TR info: dead-inf-comp.rkt 212:41 displayln -- hidden parameter
TR info: dead-inf-comp.rkt 212:41 displayln -- hidden parameter
TR opt: dead-inf-comp.rkt 102:0 #%module-begin -- dead else branch
TR opt: dead-inf-comp.rkt 102:0 #%module-begin -- dead else branch
TR opt: dead-inf-comp.rkt 102:0 #%module-begin -- dead else branch
TR opt: dead-inf-comp.rkt 108:4 (quote dead) -- dead else branch
TR opt: dead-inf-comp.rkt 110:4 (quote dead) -- dead then branch
TR opt: dead-inf-comp.rkt 113:4 (quote dead) -- dead then branch
TR opt: dead-inf-comp.rkt 117:4 (quote dead) -- dead else branch
TR opt: dead-inf-comp.rkt 121:4 (quote dead) -- dead else branch
TR opt: dead-inf-comp.rkt 123:4 (quote dead) -- dead then branch
TR opt: dead-inf-comp.rkt 126:4 (quote dead) -- dead then branch
TR opt: dead-inf-comp.rkt 130:4 (quote dead) -- dead else branch
TR opt: dead-inf-comp.rkt 134:4 (quote dead) -- dead else branch
TR opt: dead-inf-comp.rkt 136:4 (quote dead) -- dead then branch
TR opt: dead-inf-comp.rkt 139:4 (quote dead) -- dead then branch
TR opt: dead-inf-comp.rkt 143:4 (quote dead) -- dead else branch
TR opt: dead-inf-comp.rkt 147:4 (quote dead) -- dead else branch
TR opt: dead-inf-comp.rkt 149:4 (quote dead) -- dead then branch
TR opt: dead-inf-comp.rkt 152:4 (quote dead) -- dead then branch
TR opt: dead-inf-comp.rkt 156:4 (quote dead) -- dead else branch
TR opt: dead-inf-comp.rkt 161:4 (quote dead) -- dead else branch
TR opt: dead-inf-comp.rkt 163:4 (quote dead) -- dead then branch
TR opt: dead-inf-comp.rkt 166:4 (quote dead) -- dead then branch
TR opt: dead-inf-comp.rkt 170:4 (quote dead) -- dead else branch
TR opt: dead-inf-comp.rkt 174:4 (quote dead) -- dead else branch
TR opt: dead-inf-comp.rkt 176:4 (quote dead) -- dead then branch
TR opt: dead-inf-comp.rkt 179:4 (quote dead) -- dead then branch
TR opt: dead-inf-comp.rkt 183:4 (quote dead) -- dead else branch
TR opt: dead-inf-comp.rkt 187:4 (quote dead) -- dead else branch
TR opt: dead-inf-comp.rkt 189:4 (quote dead) -- dead then branch
TR opt: dead-inf-comp.rkt 192:4 (quote dead) -- dead then branch
TR opt: dead-inf-comp.rkt 196:4 (quote dead) -- dead else branch
TR opt: dead-inf-comp.rkt 200:4 (quote dead) -- dead else branch
TR opt: dead-inf-comp.rkt 202:4 (quote dead) -- dead then branch
TR opt: dead-inf-comp.rkt 205:4 (quote dead) -- dead then branch
TR opt: dead-inf-comp.rkt 209:4 (quote dead) -- dead else branch
TR opt: dead-inf-comp.rkt 212:0 (for: ((i (in-range 5 +inf.0 2)) (j 3)) (displayln i)) -- dead else branch
TR opt: dead-inf-comp.rkt 212:0 (for: ((i (in-range 5 +inf.0 2)) (j 3)) (displayln i)) -- dead else branch
TR opt: dead-inf-comp.rkt 212:0 (for: ((i (in-range 5 +inf.0 2)) (j 3)) (displayln i)) -- dead else branch
TR opt: dead-inf-comp.rkt 212:0 (for: ((i (in-range 5 +inf.0 2)) (j 3)) (displayln i)) -- dead else branch
TR opt: dead-inf-comp.rkt 212:0 (for: ((i (in-range 5 +inf.0 2)) (j 3)) (displayln i)) -- dead else branch
TR opt: dead-inf-comp.rkt 212:0 (for: ((i (in-range 5 +inf.0 2)) (j 3)) (displayln i)) -- dead else branch
TR opt: dead-inf-comp.rkt 212:36 3 -- in-range
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
