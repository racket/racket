#;
(
TR opt: pr13468.rkt 86:13 6.0+2.3i -- unboxed literal
TR opt: pr13468.rkt 86:13 6.0+2.3i -- unboxed literal
TR opt: pr13468.rkt 86:5 (- (ann 6.0+2.3i Float-Complex)) -- unboxed unary float complex
TR opt: pr13468.rkt 87:13 6.0+2.3i -- unboxed literal
TR opt: pr13468.rkt 87:13 6.0+2.3i -- unboxed literal
TR opt: pr13468.rkt 87:5 (/ (ann 6.0+2.3i Float-Complex)) -- unboxed unary float complex
TR missed opt: pr13468.rkt 88:5 (/ (ann 0.0+0.0i Float-Complex) (ann 1 Number)) -- Float-Complex division, potential exact 0s on the rhss -- caused by: 88:42 1
TR missed opt: pr13468.rkt 91:5 (expt (ann -5.0 Flonum) (ann 2.0 Flonum)) -- unexpected complex type
TR opt: pr13468.rkt 112:21 0.0 -- float-arg-expr in complex ops
TR opt: pr13468.rkt 112:5 (magnitude (ann 0.0 Flonum-Zero)) -- unboxed unary float complex
TR opt: pr13468.rkt 113:21 6.0 -- float-arg-expr in complex ops
TR opt: pr13468.rkt 113:5 (magnitude (ann 6.0 Positive-Flonum)) -- unboxed unary float complex
TR opt: pr13468.rkt 114:21 6.0 -- float-arg-expr in complex ops
TR opt: pr13468.rkt 114:5 (magnitude (ann 6.0 Nonnegative-Flonum)) -- unboxed unary float complex
TR opt: pr13468.rkt 115:21 -6.0 -- float-arg-expr in complex ops
TR opt: pr13468.rkt 115:5 (magnitude (ann -6.0 Nonpositive-Flonum)) -- unboxed unary float complex
TR opt: pr13468.rkt 116:21 -6.0 -- float-arg-expr in complex ops
TR opt: pr13468.rkt 116:5 (magnitude (ann -6.0 Negative-Flonum)) -- unboxed unary float complex
TR opt: pr13468.rkt 117:21 6.0 -- float-arg-expr in complex ops
TR opt: pr13468.rkt 117:5 (magnitude (ann 6.0 Flonum)) -- unboxed unary float complex
TR opt: pr13468.rkt 138:21 0.0 -- float-arg-expr in complex ops
TR opt: pr13468.rkt 138:5 (real-part (ann 0.0 Flonum-Zero)) -- unboxed unary float complex
TR opt: pr13468.rkt 139:21 6.0 -- float-arg-expr in complex ops
TR opt: pr13468.rkt 139:5 (real-part (ann 6.0 Positive-Flonum)) -- unboxed unary float complex
TR opt: pr13468.rkt 140:21 6.0 -- float-arg-expr in complex ops
TR opt: pr13468.rkt 140:5 (real-part (ann 6.0 Nonnegative-Flonum)) -- unboxed unary float complex
TR opt: pr13468.rkt 141:21 -6.0 -- float-arg-expr in complex ops
TR opt: pr13468.rkt 141:5 (real-part (ann -6.0 Nonpositive-Flonum)) -- unboxed unary float complex
TR opt: pr13468.rkt 142:21 -6.0 -- float-arg-expr in complex ops
TR opt: pr13468.rkt 142:5 (real-part (ann -6.0 Negative-Flonum)) -- unboxed unary float complex
TR opt: pr13468.rkt 143:21 6.0 -- float-arg-expr in complex ops
TR opt: pr13468.rkt 143:5 (real-part (ann 6.0 Flonum)) -- unboxed unary float complex
-6.0-2.3i
0.1453136352627755-0.055703560184063944i
0.0+0.0i
25.0
0
1
6
6
6
6
6
6
6
6
6
6
6
6
0.0
6.0
6.0
6.0
6.0
6.0
0
1
6
6
6
6
-6
-6
6
6
6
-6
-6
6
0.0
6.0
6.0
-6.0
-6.0
6.0
0)

#lang typed/racket



;; Most bothersome missing cases:
(ann (- (ann 6.0+2.3i Float-Complex)) Float-Complex)
(ann (/ (ann 6.0+2.3i Float-Complex)) Float-Complex)
(ann (/ (ann 0.0+0.0i Float-Complex) (ann 1+0i Number)) Float-Complex)

;; Would be nice to maintain flonum-ness here:
(ann (expt (ann -5.0 Flonum) (ann 2.0 Flonum)) (U Flonum Float-Complex))

;; In general, for real numbers, `magnitude' should have the same cases as `abs'

(ann (magnitude (ann 0 Zero)) Zero)
(ann (magnitude (ann 1 One)) One)
(ann (magnitude (ann 6 Byte)) Byte)
(ann (magnitude (ann 6 Index)) Index)

(ann (magnitude (ann 6 Positive-Fixnum)) Positive-Fixnum)
(ann (magnitude (ann 6 Nonnegative-Fixnum)) Nonnegative-Fixnum)
(ann (magnitude (ann -6 Nonpositive-Fixnum)) Nonnegative-Integer)
(ann (magnitude (ann -6 Negative-Fixnum)) Positive-Integer)
(ann (magnitude (ann 6 Fixnum)) Integer)

(ann (magnitude (ann 6 Positive-Integer)) Positive-Integer)
(ann (magnitude (ann 6 Nonnegative-Integer)) Nonnegative-Integer)
(ann (magnitude (ann -6 Nonpositive-Integer)) Nonnegative-Integer)
(ann (magnitude (ann -6 Negative-Integer)) Positive-Integer)
(ann (magnitude (ann 6 Integer)) Integer)

(ann (magnitude (ann 0.0 Flonum-Zero)) Flonum-Zero)
(ann (magnitude (ann 6.0 Positive-Flonum)) Positive-Flonum)
(ann (magnitude (ann 6.0 Nonnegative-Flonum)) Nonnegative-Flonum)
(ann (magnitude (ann -6.0 Nonpositive-Flonum)) Nonnegative-Flonum)
(ann (magnitude (ann -6.0 Negative-Flonum)) Positive-Flonum)
(ann (magnitude (ann 6.0 Flonum)) Flonum)

;; In general, for real numbers, `real-part' should return the same type

(ann (real-part (ann 0 Zero)) Zero)
(ann (real-part (ann 1 One)) One)
(ann (real-part (ann 6 Byte)) Byte)
(ann (real-part (ann 6 Index)) Index)

(ann (real-part (ann 6 Positive-Fixnum)) Positive-Fixnum)
(ann (real-part (ann 6 Nonnegative-Fixnum)) Nonnegative-Fixnum)
(ann (real-part (ann -6 Nonpositive-Fixnum)) Nonpositive-Fixnum)
(ann (real-part (ann -6 Negative-Fixnum)) Negative-Fixnum)
(ann (real-part (ann 6 Fixnum)) Fixnum)

(ann (real-part (ann 6 Positive-Integer)) Positive-Integer)
(ann (real-part (ann 6 Nonnegative-Integer)) Nonnegative-Integer)
(ann (real-part (ann -6 Nonpositive-Integer)) Nonpositive-Integer)
(ann (real-part (ann -6 Negative-Integer)) Negative-Integer)
(ann (real-part (ann 6 Integer)) Integer)

(ann (real-part (ann 0.0 Flonum-Zero)) Flonum-Zero)
(ann (real-part (ann 6.0 Positive-Flonum)) Positive-Flonum)
(ann (real-part (ann 6.0 Nonnegative-Flonum)) Nonnegative-Flonum)
(ann (real-part (ann -6.0 Nonpositive-Flonum)) Nonpositive-Flonum)
(ann (real-part (ann -6.0 Negative-Flonum)) Negative-Flonum)
(ann (real-part (ann 6.0 Flonum)) Flonum)

;; This one surprised me with the exactness of its return value, but it seems
;; to be true:
(ann (imag-part (ann 5.6 Real)) Zero)
