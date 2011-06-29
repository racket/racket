#;
(
TR opt: fixnum-bounded-expr.rkt 67:2 (+ x (sqr y)) -- fixnum bounded expr
TR opt: fixnum-bounded-expr.rkt 73:2 (- x (* y y)) -- fixnum bounded expr
TR opt: fixnum-bounded-expr.rkt 73:7 (* y y) -- fixnum bounded expr
TR opt: fixnum-bounded-expr.rkt 80:2 (+ x y) -- fixnum bounded expr
TR opt: fixnum-bounded-expr.rkt 83:2 (+ x y) -- fixnum bounded expr
TR missed opt: fixnum-bounded-expr.rkt 86:2 (+ x y) -- out of fixnum range
TR opt: fixnum-bounded-expr.rkt 88:0 (abs 45) -- fixnum fxabs
TR missed opt: fixnum-bounded-expr.rkt 89:0 (abs (ann -3 Fixnum)) -- out of fixnum range
TR opt: fixnum-bounded-expr.rkt 91:0 (fx+ 5 2) -- fixnum fx+
TR opt: fixnum-bounded-expr.rkt 92:0 (fx+ (+ 34 231) (* 24 25)) -- fixnum fx+
TR opt: fixnum-bounded-expr.rkt 92:5 (+ 34 231) -- fixnum bounded expr
TR opt: fixnum-bounded-expr.rkt 92:16 (* 24 25) -- fixnum bounded expr
TR opt: fixnum-bounded-expr.rkt 93:0 (fx+ (+ (+ 34 231) 23) -4) -- fixnum fx+
TR opt: fixnum-bounded-expr.rkt 93:5 (+ (+ 34 231) 23) -- fixnum bounded expr
TR opt: fixnum-bounded-expr.rkt 93:8 (+ 34 231) -- fixnum bounded expr
TR opt: fixnum-bounded-expr.rkt 94:0 (fx+ -4 (+ (+ 34 231) 23)) -- fixnum fx+
TR opt: fixnum-bounded-expr.rkt 94:8 (+ (+ 34 231) 23) -- fixnum bounded expr
TR opt: fixnum-bounded-expr.rkt 94:11 (+ 34 231) -- fixnum bounded expr
TR missed opt: fixnum-bounded-expr.rkt 95:0 (fx+ (+ 300 301) (+ 301 302)) -- out of fixnum range
TR opt: fixnum-bounded-expr.rkt 95:5 (+ 300 301) -- fixnum bounded expr
TR opt: fixnum-bounded-expr.rkt 95:17 (+ 301 302) -- fixnum bounded expr
TR opt: fixnum-bounded-expr.rkt 97:0 (fx- (+ 300 301) (+ 301 302)) -- fixnum fx-
TR opt: fixnum-bounded-expr.rkt 97:5 (+ 300 301) -- fixnum bounded expr
TR opt: fixnum-bounded-expr.rkt 97:17 (+ 301 302) -- fixnum bounded expr
TR missed opt: fixnum-bounded-expr.rkt 98:0 (fx- (ann 3 Fixnum) (ann 4 Fixnum)) -- out of fixnum range
TR opt: fixnum-bounded-expr.rkt 100:0 (fx* 4 5) -- fixnum fx*
TR missed opt: fixnum-bounded-expr.rkt 101:0 (fx* 300 300) -- out of fixnum range
TR opt: fixnum-bounded-expr.rkt 103:0 (fxquotient (ann 34 Nonnegative-Fixnum) (ann -4 Fixnum)) -- fixnum fxquotient
TR missed opt: fixnum-bounded-expr.rkt 104:0 (fxquotient -4 -5) -- out of fixnum range
TR opt: fixnum-bounded-expr.rkt 106:0 (fxabs (ann 64235 Nonnegative-Fixnum)) -- fixnum fxabs
TR missed opt: fixnum-bounded-expr.rkt 107:0 (fxabs -4) -- out of fixnum range
28
89525
28
89525
291
291
291
45
3
7
865
284
284
1204
-2
-1
20
90000
-8
0
64235
4
)



#lang typed/racket
#:optimize

(require racket/fixnum)

(: f : Index Byte -> Nonnegative-Fixnum)
(define (f x y)
  (+ x (sqr y)))
(f 3 5)
(f 35236 233)

(: g : Index Byte -> Fixnum)
(define (g x y)
  (- x (* y y)))
(f 3 5)
(f 35236 233)


(let: ([x : Byte 45]
       [y : Byte 246])
  (+ x y))
(let: ([x : Index 45]
       [y : Index 246])
  (+ x y))
(let: ([x : Fixnum 45]
       [y : Fixnum 246])
  (+ x y)) ; this one can't be optimized, return type is not Fixnum

(abs 45) ; ok
(abs (ann -3 Fixnum)) ; not ok, result is not a fixnum

(fx+ 5 2)
(fx+ (+ 34 231) (* 24 25)) ; ok, (+ Index Index)
(fx+ (+ (+ 34 231) 23) -4) ; ok, (+ Nonnegative-Fixnum Nonnegative-Fixnum)
(fx+ -4 (+ (+ 34 231) 23)) ; ok, mirror case
(fx+ (+ 300 301) (+ 301 302)) ; not ok, (+ Fixnum Fixnum)

(fx- (+ 300 301) (+ 301 302)) ; ok, (+ Nonnegative-Fixnum Nonnegative-Fixnum)
(fx- (ann 3 Fixnum) (ann 4 Fixnum)) ; not ok

(fx* 4 5) ; ok, (* Byte Byte)
(fx* 300 300) ; not ok

(fxquotient (ann 34 Nonnegative-Fixnum) (ann -4 Fixnum))
(fxquotient -4 -5) ; not ok

(fxabs (ann 64235 Nonnegative-Fixnum)) ; ok
(fxabs -4) ; not ok
