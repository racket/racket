#;
(
TR opt: fixnum-bounded-expr.rkt 60:2 (+ x (sqr y)) -- fixnum bounded expr
TR opt: fixnum-bounded-expr.rkt 66:2 (- x (* y y)) -- fixnum bounded expr
TR opt: fixnum-bounded-expr.rkt 66:7 (* y y) -- fixnum bounded expr
TR opt: fixnum-bounded-expr.rkt 73:2 (+ x y) -- fixnum bounded expr
TR opt: fixnum-bounded-expr.rkt 76:2 (+ x y) -- fixnum bounded expr
TR opt: fixnum-bounded-expr.rkt 81:0 (abs 45) -- fixnum fxabs
TR opt: fixnum-bounded-expr.rkt 84:0 (fx+ 5 2) -- fixnum fx+
TR opt: fixnum-bounded-expr.rkt 85:0 (fx+ (+ 34 231) (* 24 25)) -- fixnum fx+
TR opt: fixnum-bounded-expr.rkt 85:5 (+ 34 231) -- fixnum bounded expr
TR opt: fixnum-bounded-expr.rkt 85:16 (* 24 25) -- fixnum bounded expr
TR opt: fixnum-bounded-expr.rkt 86:0 (fx+ (+ (+ 34 231) 23) -4) -- fixnum fx+
TR opt: fixnum-bounded-expr.rkt 86:5 (+ (+ 34 231) 23) -- fixnum bounded expr
TR opt: fixnum-bounded-expr.rkt 86:8 (+ 34 231) -- fixnum bounded expr
TR opt: fixnum-bounded-expr.rkt 87:0 (fx+ -4 (+ (+ 34 231) 23)) -- fixnum fx+
TR opt: fixnum-bounded-expr.rkt 87:8 (+ (+ 34 231) 23) -- fixnum bounded expr
TR opt: fixnum-bounded-expr.rkt 87:11 (+ 34 231) -- fixnum bounded expr
TR opt: fixnum-bounded-expr.rkt 88:5 (+ 300 301) -- fixnum bounded expr
TR opt: fixnum-bounded-expr.rkt 88:17 (+ 301 302) -- fixnum bounded expr
TR opt: fixnum-bounded-expr.rkt 90:0 (fx- (+ 300 301) (+ 301 302)) -- fixnum fx-
TR opt: fixnum-bounded-expr.rkt 90:5 (+ 300 301) -- fixnum bounded expr
TR opt: fixnum-bounded-expr.rkt 90:17 (+ 301 302) -- fixnum bounded expr
TR opt: fixnum-bounded-expr.rkt 93:0 (fx* 4 5) -- fixnum fx*
TR opt: fixnum-bounded-expr.rkt 96:0 (fxquotient (ann 34 Nonnegative-Fixnum) (ann -4 Fixnum)) -- fixnum fxquotient
TR opt: fixnum-bounded-expr.rkt 99:0 (fxabs (ann 64235 Nonnegative-Fixnum)) -- fixnum fxabs
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
