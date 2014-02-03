#;#;
#<<END
TR info: fixnum.rkt 105:28 (- 1 (expt 2 (- (system-type (quote word)) 2))) -- possible exact real arith
TR info: fixnum.rkt 73:0 (fx* (values (ann x Index)) (values (ann y Index))) -- non-optimized fixnum op
TR info: fixnum.rkt 76:0 (fx+ (+ 300 301) (+ 301 302)) -- non-optimized fixnum op
TR info: fixnum.rkt 77:0 (fxquotient -4 -5) -- non-optimized fixnum op
TR missed opt: fixnum.rkt 101:3 (+ max-fixnum max-fixnum) -- out of fixnum range
TR missed opt: fixnum.rkt 102:3 (+ min-fixnum min-fixnum) -- out of fixnum range
TR missed opt: fixnum.rkt 106:3 (quotient min-fixnum -1) -- out of fixnum range
TR missed opt: fixnum.rkt 71:0 (+ (ann z Fixnum) 234) -- out of fixnum range
TR missed opt: fixnum.rkt 72:0 (* (ann x Index) (ann y Index)) -- out of fixnum range
TR missed opt: fixnum.rkt 75:0 (+ (+ 300 301) (+ 301 302)) -- out of fixnum range
TR missed opt: fixnum.rkt 88:0 (sub1 min-fixnum) -- out of fixnum range
TR missed opt: fixnum.rkt 94:0 (add1 max-fixnum) -- out of fixnum range
TR missed opt: fixnum.rkt 98:3 (- max-fixnum min-fixnum) -- out of fixnum range
TR missed opt: fixnum.rkt 99:3 (- min-fixnum max-fixnum) -- out of fixnum range
TR opt: fixnum.rkt 103:0 (= (+ max-fixnum min-fixnum) -1) -- binary fixnum comp
TR opt: fixnum.rkt 103:3 (+ max-fixnum min-fixnum) -- fixnum bounded expr
TR opt: fixnum.rkt 105:3 (quotient max-fixnum -1) -- nonzero fixnum bounded expr
TR opt: fixnum.rkt 68:10 (* x y) -- fixnum bounded expr
TR opt: fixnum.rkt 75:15 (+ 301 302) -- fixnum bounded expr
TR opt: fixnum.rkt 75:3 (+ 300 301) -- fixnum bounded expr
TR opt: fixnum.rkt 76:17 (+ 301 302) -- fixnum bounded expr
TR opt: fixnum.rkt 76:5 (+ 300 301) -- fixnum bounded expr
TR opt: fixnum.rkt 89:0 (add1 min-fixnum) -- fixnum add1
TR opt: fixnum.rkt 91:0 (- max-fixnum) -- unary fixnum
TR opt: fixnum.rkt 92:0 (abs max-fixnum) -- fixnum fxabs
TR opt: fixnum.rkt 93:0 (sub1 max-fixnum) -- fixnum sub1
TR opt: fixnum.rkt 96:0 (= (- max-fixnum max-fixnum) 0) -- binary fixnum comp
TR opt: fixnum.rkt 96:3 (- max-fixnum max-fixnum) -- fixnum bounded expr
TR opt: fixnum.rkt 97:0 (= (- min-fixnum min-fixnum) 0) -- binary fixnum comp
TR opt: fixnum.rkt 97:3 (- min-fixnum min-fixnum) -- fixnum bounded expr
END
#<<END
468
234
234
3
1204
1204
0
4611686018427387904
4611686018427387904
-4611686018427387905
-4611686018427387903
-4611686018427387903
4611686018427387903
4611686018427387902
4611686018427387904
#t
#t
#t
#t
#t
#t
#t
#t
#t

END
#lang typed/racket

(require racket/fixnum)


(define x 3)
(define y 78)
(define z (* x y)) ; this should be optimized

;; this should not, (+ Fixnum Byte), but it may look like it should
(+ (ann z Fixnum) 234)
(* (ann x Index) (ann y Index))
(fx* (values (ann x Index)) (values (ann y Index))) ; not reported, by design
(abs (ann -3 Fixnum))
(+ (+ 300 301) (+ 301 302))
(fx+ (+ 300 301) (+ 301 302)) ; not reported, by design
(fxquotient -4 -5) ; not reported, by design

(: min-fixnum Negative-Fixnum)
(define min-fixnum
  (cast (- (expt 2 (- (system-type 'word) 2))) Negative-Fixnum))
(: max-fixnum Positive-Fixnum)
(define max-fixnum
  (cast (- (expt 2 (- (system-type 'word) 2)) 1) Positive-Fixnum))

(- min-fixnum)
(abs min-fixnum)
(sub1 min-fixnum)
(add1 min-fixnum)

(- max-fixnum)
(abs max-fixnum)
(sub1 max-fixnum)
(add1 max-fixnum)

(= (- max-fixnum max-fixnum) 0)
(= (- min-fixnum min-fixnum) 0)
(= (- max-fixnum min-fixnum) (- (expt 2 (sub1 (system-type 'word))) 1))
(= (- min-fixnum max-fixnum) (- 1 (expt 2 (sub1 (system-type 'word)))))

(= (+ max-fixnum max-fixnum) (- (expt 2 (sub1 (system-type 'word))) 2))
(= (+ min-fixnum min-fixnum) (- (expt 2 (sub1 (system-type 'word)))))
(= (+ max-fixnum min-fixnum) -1)

(= (quotient max-fixnum -1) (- 1 (expt 2 (- (system-type 'word) 2))))
(= (quotient min-fixnum -1) (expt 2 (- (system-type 'word) 2)))
