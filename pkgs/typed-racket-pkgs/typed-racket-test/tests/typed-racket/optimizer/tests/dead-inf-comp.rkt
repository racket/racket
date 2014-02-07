#;#;
#<<END
TR info: dead-inf-comp.rkt 177:4 (< rat +inf.f) -- possible exact real arith
TR info: dead-inf-comp.rkt 180:4 (< +inf.f rat) -- possible exact real arith
TR info: dead-inf-comp.rkt 183:4 (< rat -inf.f) -- possible exact real arith
TR info: dead-inf-comp.rkt 186:4 (< -inf.f rat) -- possible exact real arith
TR info: dead-inf-comp.rkt 190:4 (> +inf.f rat) -- possible exact real arith
TR info: dead-inf-comp.rkt 193:4 (> rat +inf.f) -- possible exact real arith
TR info: dead-inf-comp.rkt 196:4 (> -inf.f rat) -- possible exact real arith
TR info: dead-inf-comp.rkt 199:4 (> rat -inf.f) -- possible exact real arith
TR info: dead-inf-comp.rkt 203:4 (<= rat +inf.f) -- possible exact real arith
TR info: dead-inf-comp.rkt 206:4 (<= +inf.f rat) -- possible exact real arith
TR info: dead-inf-comp.rkt 209:4 (<= rat -inf.f) -- possible exact real arith
TR info: dead-inf-comp.rkt 212:4 (<= -inf.f rat) -- possible exact real arith
TR info: dead-inf-comp.rkt 216:4 (>= +inf.f rat) -- possible exact real arith
TR info: dead-inf-comp.rkt 219:4 (>= rat +inf.f) -- possible exact real arith
TR info: dead-inf-comp.rkt 222:4 (>= -inf.f rat) -- possible exact real arith
TR info: dead-inf-comp.rkt 225:4 (>= rat -inf.f) -- possible exact real arith
TR info: dead-inf-comp.rkt 230:41 displayln -- hidden parameter
TR info: dead-inf-comp.rkt 230:41 displayln -- hidden parameter
TR missed opt: dead-inf-comp.rkt 124:4 (< rat +inf.0) -- generic comparison -- caused by: 124:7 rat
TR missed opt: dead-inf-comp.rkt 127:4 (< +inf.0 rat) -- generic comparison -- caused by: 127:14 rat
TR missed opt: dead-inf-comp.rkt 130:4 (< rat -inf.0) -- generic comparison -- caused by: 130:7 rat
TR missed opt: dead-inf-comp.rkt 133:4 (< -inf.0 rat) -- generic comparison -- caused by: 133:14 rat
TR missed opt: dead-inf-comp.rkt 137:4 (> +inf.0 rat) -- generic comparison -- caused by: 137:14 rat
TR missed opt: dead-inf-comp.rkt 140:4 (> rat +inf.0) -- generic comparison -- caused by: 140:7 rat
TR missed opt: dead-inf-comp.rkt 143:4 (> -inf.0 rat) -- generic comparison -- caused by: 143:14 rat
TR missed opt: dead-inf-comp.rkt 146:4 (> rat -inf.0) -- generic comparison -- caused by: 146:7 rat
TR missed opt: dead-inf-comp.rkt 150:4 (<= rat +inf.0) -- generic comparison -- caused by: 150:8 rat
TR missed opt: dead-inf-comp.rkt 153:4 (<= +inf.0 rat) -- generic comparison -- caused by: 153:15 rat
TR missed opt: dead-inf-comp.rkt 156:4 (<= rat -inf.0) -- generic comparison -- caused by: 156:8 rat
TR missed opt: dead-inf-comp.rkt 159:4 (<= -inf.0 rat) -- generic comparison -- caused by: 159:15 rat
TR missed opt: dead-inf-comp.rkt 163:4 (>= +inf.0 rat) -- generic comparison -- caused by: 163:15 rat
TR missed opt: dead-inf-comp.rkt 166:4 (>= rat +inf.0) -- generic comparison -- caused by: 166:8 rat
TR missed opt: dead-inf-comp.rkt 169:4 (>= -inf.0 rat) -- generic comparison -- caused by: 169:15 rat
TR missed opt: dead-inf-comp.rkt 172:4 (>= rat -inf.0) -- generic comparison -- caused by: 172:8 rat
TR missed opt: dead-inf-comp.rkt 230:0 (for: ((i (in-range 5 +inf.0 2)) (j 3)) (displayln i)) -- generic comparison -- caused by: 230:0 (for: ((i (in-range 5 +inf.0 2)) (j 3)) (displayln i))
TR missed opt: dead-inf-comp.rkt 230:0 (for: ((i (in-range 5 +inf.0 2)) (j 3)) (displayln i)) -- generic comparison -- caused by: 230:0 (for: ((i (in-range 5 +inf.0 2)) (j 3)) (displayln i))
TR opt: dead-inf-comp.rkt 126:4 (quote dead) -- dead else branch
TR opt: dead-inf-comp.rkt 128:4 (quote dead) -- dead then branch
TR opt: dead-inf-comp.rkt 131:4 (quote dead) -- dead then branch
TR opt: dead-inf-comp.rkt 135:4 (quote dead) -- dead else branch
TR opt: dead-inf-comp.rkt 139:4 (quote dead) -- dead else branch
TR opt: dead-inf-comp.rkt 141:4 (quote dead) -- dead then branch
TR opt: dead-inf-comp.rkt 144:4 (quote dead) -- dead then branch
TR opt: dead-inf-comp.rkt 148:4 (quote dead) -- dead else branch
TR opt: dead-inf-comp.rkt 152:4 (quote dead) -- dead else branch
TR opt: dead-inf-comp.rkt 154:4 (quote dead) -- dead then branch
TR opt: dead-inf-comp.rkt 157:4 (quote dead) -- dead then branch
TR opt: dead-inf-comp.rkt 161:4 (quote dead) -- dead else branch
TR opt: dead-inf-comp.rkt 165:4 (quote dead) -- dead else branch
TR opt: dead-inf-comp.rkt 167:4 (quote dead) -- dead then branch
TR opt: dead-inf-comp.rkt 170:4 (quote dead) -- dead then branch
TR opt: dead-inf-comp.rkt 174:4 (quote dead) -- dead else branch
TR opt: dead-inf-comp.rkt 179:4 (quote dead) -- dead else branch
TR opt: dead-inf-comp.rkt 181:4 (quote dead) -- dead then branch
TR opt: dead-inf-comp.rkt 184:4 (quote dead) -- dead then branch
TR opt: dead-inf-comp.rkt 188:4 (quote dead) -- dead else branch
TR opt: dead-inf-comp.rkt 192:4 (quote dead) -- dead else branch
TR opt: dead-inf-comp.rkt 194:4 (quote dead) -- dead then branch
TR opt: dead-inf-comp.rkt 197:4 (quote dead) -- dead then branch
TR opt: dead-inf-comp.rkt 201:4 (quote dead) -- dead else branch
TR opt: dead-inf-comp.rkt 205:4 (quote dead) -- dead else branch
TR opt: dead-inf-comp.rkt 207:4 (quote dead) -- dead then branch
TR opt: dead-inf-comp.rkt 210:4 (quote dead) -- dead then branch
TR opt: dead-inf-comp.rkt 214:4 (quote dead) -- dead else branch
TR opt: dead-inf-comp.rkt 218:4 (quote dead) -- dead else branch
TR opt: dead-inf-comp.rkt 220:4 (quote dead) -- dead then branch
TR opt: dead-inf-comp.rkt 223:4 (quote dead) -- dead then branch
TR opt: dead-inf-comp.rkt 227:4 (quote dead) -- dead else branch
TR opt: dead-inf-comp.rkt 230:0 (for: ((i (in-range 5 +inf.0 2)) (j 3)) (displayln i)) -- dead else branch
TR opt: dead-inf-comp.rkt 230:0 (for: ((i (in-range 5 +inf.0 2)) (j 3)) (displayln i)) -- dead else branch
TR opt: dead-inf-comp.rkt 230:0 (for: ((i (in-range 5 +inf.0 2)) (j 3)) (displayln i)) -- dead else branch
TR opt: dead-inf-comp.rkt 230:0 (for: ((i (in-range 5 +inf.0 2)) (j 3)) (displayln i)) -- dead else branch
TR opt: dead-inf-comp.rkt 230:0 (for: ((i (in-range 5 +inf.0 2)) (j 3)) (displayln i)) -- dead else branch
TR opt: dead-inf-comp.rkt 230:0 (for: ((i (in-range 5 +inf.0 2)) (j 3)) (displayln i)) -- dead else branch
TR opt: dead-inf-comp.rkt 230:0 (for: ((i (in-range 5 +inf.0 2)) (j 3)) (displayln i)) -- dead else branch
TR opt: dead-inf-comp.rkt 230:0 (for: ((i (in-range 5 +inf.0 2)) (j 3)) (displayln i)) -- dead else branch
TR opt: dead-inf-comp.rkt 230:0 (for: ((i (in-range 5 +inf.0 2)) (j 3)) (displayln i)) -- dead else branch
TR opt: dead-inf-comp.rkt 230:36 3 -- in-range
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
