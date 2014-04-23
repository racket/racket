#;#;
#<<END
TR info: fixnum.rkt 12:0 (fx* (values (ann x Index)) (values (ann y Index))) -- non-optimized fixnum op
TR info: fixnum.rkt 15:0 (fx+ (+ 300 301) (+ 301 302)) -- non-optimized fixnum op
TR info: fixnum.rkt 16:0 (fxquotient -4 -5) -- non-optimized fixnum op
TR info: fixnum.rkt 44:28 (- 1 (expt 2 (- (system-type (quote word)) 2))) -- possible exact real arith
TR info: fixnum.rkt 44:33 (expt 2 (- (system-type (quote word)) 2)) -- possible exact real arith
TR info: fixnum.rkt 45:28 (expt 2 (- (system-type (quote word)) 2)) -- possible exact real arith
TR missed opt: fixnum.rkt 10:0 (+ (ann z Fixnum) 234) -- out of fixnum range
TR missed opt: fixnum.rkt 11:0 (* (ann x Index) (ann y Index)) -- out of fixnum range
TR missed opt: fixnum.rkt 14:0 (+ (+ 300 301) (+ 301 302)) -- out of fixnum range
TR missed opt: fixnum.rkt 27:0 (sub1 min-fixnum) -- out of fixnum range
TR missed opt: fixnum.rkt 33:0 (add1 max-fixnum) -- out of fixnum range
TR missed opt: fixnum.rkt 37:3 (- max-fixnum min-fixnum) -- out of fixnum range
TR missed opt: fixnum.rkt 38:3 (- min-fixnum max-fixnum) -- out of fixnum range
TR missed opt: fixnum.rkt 40:3 (+ max-fixnum max-fixnum) -- out of fixnum range
TR missed opt: fixnum.rkt 41:3 (+ min-fixnum min-fixnum) -- out of fixnum range
TR missed opt: fixnum.rkt 45:3 (quotient min-fixnum -1) -- out of fixnum range
TR opt: fixnum.rkt 14:15 (+ 301 302) -- fixnum bounded expr
TR opt: fixnum.rkt 14:3 (+ 300 301) -- fixnum bounded expr
TR opt: fixnum.rkt 15:17 (+ 301 302) -- fixnum bounded expr
TR opt: fixnum.rkt 15:5 (+ 300 301) -- fixnum bounded expr
TR opt: fixnum.rkt 28:0 (add1 min-fixnum) -- fixnum add1
TR opt: fixnum.rkt 30:0 (- max-fixnum) -- unary fixnum
TR opt: fixnum.rkt 31:0 (abs max-fixnum) -- unary number
TR opt: fixnum.rkt 32:0 (sub1 max-fixnum) -- fixnum sub1
TR opt: fixnum.rkt 35:0 (= (- max-fixnum max-fixnum) 0) -- binary fixnum comp
TR opt: fixnum.rkt 35:3 (- max-fixnum max-fixnum) -- fixnum bounded expr
TR opt: fixnum.rkt 36:0 (= (- min-fixnum min-fixnum) 0) -- binary fixnum comp
TR opt: fixnum.rkt 36:3 (- min-fixnum min-fixnum) -- fixnum bounded expr
TR opt: fixnum.rkt 42:0 (= (+ max-fixnum min-fixnum) -1) -- binary fixnum comp
TR opt: fixnum.rkt 42:3 (+ max-fixnum min-fixnum) -- fixnum bounded expr
TR opt: fixnum.rkt 44:3 (quotient max-fixnum -1) -- nonzero fixnum bounded expr
TR opt: fixnum.rkt 7:10 (* x y) -- fixnum bounded expr
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
#reader tests/typed-racket/optimizer/reset-port

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
