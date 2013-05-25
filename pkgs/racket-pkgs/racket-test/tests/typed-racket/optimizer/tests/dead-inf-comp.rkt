#;
(
TR opt: dead-inf-comp.rkt 105:4 (quote dead) -- dead else branch
TR opt: dead-inf-comp.rkt 107:4 (quote dead) -- dead then branch
TR opt: dead-inf-comp.rkt 110:4 (quote dead) -- dead then branch
TR opt: dead-inf-comp.rkt 114:4 (quote dead) -- dead else branch
TR opt: dead-inf-comp.rkt 118:4 (quote dead) -- dead else branch
TR opt: dead-inf-comp.rkt 120:4 (quote dead) -- dead then branch
TR opt: dead-inf-comp.rkt 123:4 (quote dead) -- dead then branch
TR opt: dead-inf-comp.rkt 127:4 (quote dead) -- dead else branch
TR opt: dead-inf-comp.rkt 131:4 (quote dead) -- dead else branch
TR opt: dead-inf-comp.rkt 133:4 (quote dead) -- dead then branch
TR opt: dead-inf-comp.rkt 136:4 (quote dead) -- dead then branch
TR opt: dead-inf-comp.rkt 140:4 (quote dead) -- dead else branch
TR opt: dead-inf-comp.rkt 144:4 (quote dead) -- dead else branch
TR opt: dead-inf-comp.rkt 146:4 (quote dead) -- dead then branch
TR opt: dead-inf-comp.rkt 149:4 (quote dead) -- dead then branch
TR opt: dead-inf-comp.rkt 153:4 (quote dead) -- dead else branch
TR opt: dead-inf-comp.rkt 158:4 (quote dead) -- dead else branch
TR info: dead-inf-comp.rkt 156:4 (< rat +inf.f) -- exact real arith
TR opt: dead-inf-comp.rkt 160:4 (quote dead) -- dead then branch
TR info: dead-inf-comp.rkt 159:4 (< +inf.f rat) -- exact real arith
TR opt: dead-inf-comp.rkt 163:4 (quote dead) -- dead then branch
TR info: dead-inf-comp.rkt 162:4 (< rat -inf.f) -- exact real arith
TR opt: dead-inf-comp.rkt 167:4 (quote dead) -- dead else branch
TR info: dead-inf-comp.rkt 165:4 (< -inf.f rat) -- exact real arith
TR opt: dead-inf-comp.rkt 171:4 (quote dead) -- dead else branch
TR info: dead-inf-comp.rkt 169:4 (> +inf.f rat) -- exact real arith
TR opt: dead-inf-comp.rkt 173:4 (quote dead) -- dead then branch
TR info: dead-inf-comp.rkt 172:4 (> rat +inf.f) -- exact real arith
TR opt: dead-inf-comp.rkt 176:4 (quote dead) -- dead then branch
TR info: dead-inf-comp.rkt 175:4 (> -inf.f rat) -- exact real arith
TR opt: dead-inf-comp.rkt 180:4 (quote dead) -- dead else branch
TR info: dead-inf-comp.rkt 178:4 (> rat -inf.f) -- exact real arith
TR opt: dead-inf-comp.rkt 184:4 (quote dead) -- dead else branch
TR info: dead-inf-comp.rkt 182:4 (<= rat +inf.f) -- exact real arith
TR opt: dead-inf-comp.rkt 186:4 (quote dead) -- dead then branch
TR info: dead-inf-comp.rkt 185:4 (<= +inf.f rat) -- exact real arith
TR opt: dead-inf-comp.rkt 189:4 (quote dead) -- dead then branch
TR info: dead-inf-comp.rkt 188:4 (<= rat -inf.f) -- exact real arith
TR opt: dead-inf-comp.rkt 193:4 (quote dead) -- dead else branch
TR info: dead-inf-comp.rkt 191:4 (<= -inf.f rat) -- exact real arith
TR opt: dead-inf-comp.rkt 197:4 (quote dead) -- dead else branch
TR info: dead-inf-comp.rkt 195:4 (>= +inf.f rat) -- exact real arith
TR opt: dead-inf-comp.rkt 199:4 (quote dead) -- dead then branch
TR info: dead-inf-comp.rkt 198:4 (>= rat +inf.f) -- exact real arith
TR opt: dead-inf-comp.rkt 202:4 (quote dead) -- dead then branch
TR info: dead-inf-comp.rkt 201:4 (>= -inf.f rat) -- exact real arith
TR opt: dead-inf-comp.rkt 206:4 (quote dead) -- dead else branch
TR info: dead-inf-comp.rkt 204:4 (>= rat -inf.f) -- exact real arith
TR opt: dead-inf-comp.rkt 99:0 #%module-begin -- in-range
TR opt: dead-inf-comp.rkt 99:0 #%module-begin -- dead else branch
TR opt: dead-inf-comp.rkt 99:0 #%module-begin -- dead else branch
TR opt: dead-inf-comp.rkt 99:0 #%module-begin -- dead else branch
TR opt: dead-inf-comp.rkt 209:0 (for: ((i (in-range 5 +inf.0 2)) (j 3)) (displayln i)) -- dead else branch
TR opt: dead-inf-comp.rkt 209:0 (for: ((i (in-range 5 +inf.0 2)) (j 3)) (displayln i)) -- dead else branch
TR info: dead-inf-comp.rkt 209:41 displayln -- hidden parameter
TR opt: dead-inf-comp.rkt 209:0 (for: ((i (in-range 5 +inf.0 2)) (j 3)) (displayln i)) -- dead else branch
TR opt: dead-inf-comp.rkt 209:0 (for: ((i (in-range 5 +inf.0 2)) (j 3)) (displayln i)) -- dead else branch
TR opt: dead-inf-comp.rkt 209:0 (for: ((i (in-range 5 +inf.0 2)) (j 3)) (displayln i)) -- dead else branch
TR info: dead-inf-comp.rkt 209:41 displayln -- hidden parameter
TR opt: dead-inf-comp.rkt 209:0 (for: ((i (in-range 5 +inf.0 2)) (j 3)) (displayln i)) -- dead else branch
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
)
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
