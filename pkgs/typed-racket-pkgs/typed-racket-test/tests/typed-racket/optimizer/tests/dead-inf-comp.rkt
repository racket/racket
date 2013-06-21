#;#;
#<<END
TR opt: dead-inf-comp.rkt 109:4 (quote dead) -- dead else branch
TR opt: dead-inf-comp.rkt 111:4 (quote dead) -- dead then branch
TR opt: dead-inf-comp.rkt 114:4 (quote dead) -- dead then branch
TR opt: dead-inf-comp.rkt 118:4 (quote dead) -- dead else branch
TR opt: dead-inf-comp.rkt 122:4 (quote dead) -- dead else branch
TR opt: dead-inf-comp.rkt 124:4 (quote dead) -- dead then branch
TR opt: dead-inf-comp.rkt 127:4 (quote dead) -- dead then branch
TR opt: dead-inf-comp.rkt 131:4 (quote dead) -- dead else branch
TR opt: dead-inf-comp.rkt 135:4 (quote dead) -- dead else branch
TR opt: dead-inf-comp.rkt 137:4 (quote dead) -- dead then branch
TR opt: dead-inf-comp.rkt 140:4 (quote dead) -- dead then branch
TR opt: dead-inf-comp.rkt 144:4 (quote dead) -- dead else branch
TR opt: dead-inf-comp.rkt 148:4 (quote dead) -- dead else branch
TR opt: dead-inf-comp.rkt 150:4 (quote dead) -- dead then branch
TR opt: dead-inf-comp.rkt 153:4 (quote dead) -- dead then branch
TR opt: dead-inf-comp.rkt 157:4 (quote dead) -- dead else branch
TR opt: dead-inf-comp.rkt 162:4 (quote dead) -- dead else branch
TR info: dead-inf-comp.rkt 160:4 (< rat +inf.f) -- exact real arith
TR opt: dead-inf-comp.rkt 164:4 (quote dead) -- dead then branch
TR info: dead-inf-comp.rkt 163:4 (< +inf.f rat) -- exact real arith
TR opt: dead-inf-comp.rkt 167:4 (quote dead) -- dead then branch
TR info: dead-inf-comp.rkt 166:4 (< rat -inf.f) -- exact real arith
TR opt: dead-inf-comp.rkt 171:4 (quote dead) -- dead else branch
TR info: dead-inf-comp.rkt 169:4 (< -inf.f rat) -- exact real arith
TR opt: dead-inf-comp.rkt 175:4 (quote dead) -- dead else branch
TR info: dead-inf-comp.rkt 173:4 (> +inf.f rat) -- exact real arith
TR opt: dead-inf-comp.rkt 177:4 (quote dead) -- dead then branch
TR info: dead-inf-comp.rkt 176:4 (> rat +inf.f) -- exact real arith
TR opt: dead-inf-comp.rkt 180:4 (quote dead) -- dead then branch
TR info: dead-inf-comp.rkt 179:4 (> -inf.f rat) -- exact real arith
TR opt: dead-inf-comp.rkt 184:4 (quote dead) -- dead else branch
TR info: dead-inf-comp.rkt 182:4 (> rat -inf.f) -- exact real arith
TR opt: dead-inf-comp.rkt 188:4 (quote dead) -- dead else branch
TR info: dead-inf-comp.rkt 186:4 (<= rat +inf.f) -- exact real arith
TR opt: dead-inf-comp.rkt 190:4 (quote dead) -- dead then branch
TR info: dead-inf-comp.rkt 189:4 (<= +inf.f rat) -- exact real arith
TR opt: dead-inf-comp.rkt 193:4 (quote dead) -- dead then branch
TR info: dead-inf-comp.rkt 192:4 (<= rat -inf.f) -- exact real arith
TR opt: dead-inf-comp.rkt 197:4 (quote dead) -- dead else branch
TR info: dead-inf-comp.rkt 195:4 (<= -inf.f rat) -- exact real arith
TR opt: dead-inf-comp.rkt 201:4 (quote dead) -- dead else branch
TR info: dead-inf-comp.rkt 199:4 (>= +inf.f rat) -- exact real arith
TR opt: dead-inf-comp.rkt 203:4 (quote dead) -- dead then branch
TR info: dead-inf-comp.rkt 202:4 (>= rat +inf.f) -- exact real arith
TR opt: dead-inf-comp.rkt 206:4 (quote dead) -- dead then branch
TR info: dead-inf-comp.rkt 205:4 (>= -inf.f rat) -- exact real arith
TR opt: dead-inf-comp.rkt 210:4 (quote dead) -- dead else branch
TR info: dead-inf-comp.rkt 208:4 (>= rat -inf.f) -- exact real arith
TR opt: dead-inf-comp.rkt 103:0 #%module-begin -- in-range
TR opt: dead-inf-comp.rkt 103:0 #%module-begin -- dead else branch
TR opt: dead-inf-comp.rkt 103:0 #%module-begin -- dead else branch
TR opt: dead-inf-comp.rkt 103:0 #%module-begin -- dead else branch
TR opt: dead-inf-comp.rkt 213:0 (for: ((i (in-range 5 +inf.0 2)) (j 3)) (displayln i)) -- dead else branch
TR opt: dead-inf-comp.rkt 213:0 (for: ((i (in-range 5 +inf.0 2)) (j 3)) (displayln i)) -- dead else branch
TR info: dead-inf-comp.rkt 213:41 displayln -- hidden parameter
TR opt: dead-inf-comp.rkt 213:0 (for: ((i (in-range 5 +inf.0 2)) (j 3)) (displayln i)) -- dead else branch
TR opt: dead-inf-comp.rkt 213:0 (for: ((i (in-range 5 +inf.0 2)) (j 3)) (displayln i)) -- dead else branch
TR opt: dead-inf-comp.rkt 213:0 (for: ((i (in-range 5 +inf.0 2)) (j 3)) (displayln i)) -- dead else branch
TR info: dead-inf-comp.rkt 213:41 displayln -- hidden parameter
TR opt: dead-inf-comp.rkt 213:0 (for: ((i (in-range 5 +inf.0 2)) (j 3)) (displayln i)) -- dead else branch

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
