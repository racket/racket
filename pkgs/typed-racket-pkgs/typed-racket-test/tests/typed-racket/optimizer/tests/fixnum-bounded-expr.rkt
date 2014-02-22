#;#;
#<<END
TR info: fixnum-bounded-expr.rkt 34:0 (fx+ (+ 300 301) (+ 301 302)) -- non-optimized fixnum op
TR info: fixnum-bounded-expr.rkt 37:0 (fx- (ann 3 Fixnum) (ann 4 Fixnum)) -- non-optimized fixnum op
TR info: fixnum-bounded-expr.rkt 40:0 (fx* 300 300) -- non-optimized fixnum op
TR info: fixnum-bounded-expr.rkt 43:0 (fxquotient -4 -5) -- non-optimized fixnum op
TR missed opt: fixnum-bounded-expr.rkt 25:2 (+ x y) -- out of fixnum range
TR opt: fixnum-bounded-expr.rkt 12:2 (- x (* y y)) -- fixnum bounded expr
TR opt: fixnum-bounded-expr.rkt 12:7 (* y y) -- fixnum bounded expr
TR opt: fixnum-bounded-expr.rkt 19:2 (+ x y) -- fixnum bounded expr
TR opt: fixnum-bounded-expr.rkt 22:2 (+ x y) -- fixnum bounded expr
TR opt: fixnum-bounded-expr.rkt 27:0 (abs 45) -- unary number
TR opt: fixnum-bounded-expr.rkt 30:0 (fx+ 5 2) -- fixnum fx+
TR opt: fixnum-bounded-expr.rkt 31:0 (fx+ (+ 34 231) (* 24 25)) -- fixnum fx+
TR opt: fixnum-bounded-expr.rkt 31:16 (* 24 25) -- fixnum bounded expr
TR opt: fixnum-bounded-expr.rkt 31:5 (+ 34 231) -- fixnum bounded expr
TR opt: fixnum-bounded-expr.rkt 32:0 (fx+ (+ (+ 34 231) 23) -4) -- fixnum fx+
TR opt: fixnum-bounded-expr.rkt 32:5 (+ (+ 34 231) 23) -- fixnum bounded expr
TR opt: fixnum-bounded-expr.rkt 32:8 (+ 34 231) -- fixnum bounded expr
TR opt: fixnum-bounded-expr.rkt 33:0 (fx+ -4 (+ (+ 34 231) 23)) -- fixnum fx+
TR opt: fixnum-bounded-expr.rkt 33:11 (+ 34 231) -- fixnum bounded expr
TR opt: fixnum-bounded-expr.rkt 33:8 (+ (+ 34 231) 23) -- fixnum bounded expr
TR opt: fixnum-bounded-expr.rkt 34:17 (+ 301 302) -- fixnum bounded expr
TR opt: fixnum-bounded-expr.rkt 34:5 (+ 300 301) -- fixnum bounded expr
TR opt: fixnum-bounded-expr.rkt 36:0 (fx- (+ 300 301) (+ 301 302)) -- fixnum fx-
TR opt: fixnum-bounded-expr.rkt 36:17 (+ 301 302) -- fixnum bounded expr
TR opt: fixnum-bounded-expr.rkt 36:5 (+ 300 301) -- fixnum bounded expr
TR opt: fixnum-bounded-expr.rkt 39:0 (fx* 4 5) -- fixnum fx*
TR opt: fixnum-bounded-expr.rkt 42:0 (fxquotient (ann 34 Nonnegative-Fixnum) (ann -4 Negative-Fixnum)) -- fixnum fxquotient
TR opt: fixnum-bounded-expr.rkt 45:0 (fxabs (ann 64235 Nonnegative-Fixnum)) -- fixnum fxabs
TR opt: fixnum-bounded-expr.rkt 6:2 (+ x (sqr y)) -- fixnum bounded expr
END
#<<END
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

END
#lang typed/racket
#:optimize
#reader tests/typed-racket/optimizer/reset-port

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

(fxquotient (ann 34 Nonnegative-Fixnum) (ann -4 Negative-Fixnum))
(fxquotient -4 -5) ; not ok

(fxabs (ann 64235 Nonnegative-Fixnum)) ; ok
(fxabs -4) ; not ok
