#;#;
#<<END
TR missed opt: pr13468.rkt 5:5 (/ (ann 0.0+0.0i Float-Complex) (ann 1 Number)) -- Float-Complex division, potential exact 0s on the rhss -- caused by: 5:42 1
TR missed opt: pr13468.rkt 8:5 (expt (ann -5.0 Flonum) (ann 2.0 Flonum)) -- unexpected complex type
TR opt: pr13468.rkt 12:5 (magnitude (ann 0 Zero)) -- unary number
TR opt: pr13468.rkt 13:5 (magnitude (ann 1 One)) -- unary number
TR opt: pr13468.rkt 14:5 (magnitude (ann 6 Byte)) -- unary number
TR opt: pr13468.rkt 15:5 (magnitude (ann 6 Index)) -- unary number
TR opt: pr13468.rkt 17:5 (magnitude (ann 6 Positive-Fixnum)) -- unary number
TR opt: pr13468.rkt 18:5 (magnitude (ann 6 Nonnegative-Fixnum)) -- unary number
TR opt: pr13468.rkt 23:5 (magnitude (ann 6 Positive-Integer)) -- unary number
TR opt: pr13468.rkt 24:5 (magnitude (ann 6 Nonnegative-Integer)) -- unary number
TR opt: pr13468.rkt 29:5 (magnitude (ann 0.0 Flonum-Zero)) -- unary float
TR opt: pr13468.rkt 30:5 (magnitude (ann 6.0 Positive-Flonum)) -- unary number
TR opt: pr13468.rkt 31:5 (magnitude (ann 6.0 Nonnegative-Flonum)) -- unary float
TR opt: pr13468.rkt 32:5 (magnitude (ann -6.0 Nonpositive-Flonum)) -- unary float
TR opt: pr13468.rkt 33:5 (magnitude (ann -6.0 Negative-Flonum)) -- unary float
TR opt: pr13468.rkt 34:5 (magnitude (ann 6.0 Flonum)) -- unary float
TR opt: pr13468.rkt 38:5 (real-part (ann 0 Zero)) -- unary number
TR opt: pr13468.rkt 39:5 (real-part (ann 1 One)) -- unary number
TR opt: pr13468.rkt 3:13 6.0+2.3i -- unbox float-complex
TR opt: pr13468.rkt 3:13 6.0+2.3i -- unbox float-complex
TR opt: pr13468.rkt 3:5 (- (ann 6.0+2.3i Float-Complex)) -- unboxed unary float complex
TR opt: pr13468.rkt 40:5 (real-part (ann 6 Byte)) -- unary number
TR opt: pr13468.rkt 41:5 (real-part (ann 6 Index)) -- unary number
TR opt: pr13468.rkt 43:5 (real-part (ann 6 Positive-Fixnum)) -- unary number
TR opt: pr13468.rkt 44:5 (real-part (ann 6 Nonnegative-Fixnum)) -- unary number
TR opt: pr13468.rkt 45:5 (real-part (ann -6 Nonpositive-Fixnum)) -- unary number
TR opt: pr13468.rkt 46:5 (real-part (ann -6 Negative-Fixnum)) -- unary number
TR opt: pr13468.rkt 47:5 (real-part (ann 6 Fixnum)) -- unary number
TR opt: pr13468.rkt 49:5 (real-part (ann 6 Positive-Integer)) -- unary number
TR opt: pr13468.rkt 4:13 6.0+2.3i -- unbox float-complex
TR opt: pr13468.rkt 4:13 6.0+2.3i -- unbox float-complex
TR opt: pr13468.rkt 4:5 (/ (ann 6.0+2.3i Float-Complex)) -- unboxed unary float complex
TR opt: pr13468.rkt 50:5 (real-part (ann 6 Nonnegative-Integer)) -- unary number
TR opt: pr13468.rkt 51:5 (real-part (ann -6 Nonpositive-Integer)) -- unary number
TR opt: pr13468.rkt 52:5 (real-part (ann -6 Negative-Integer)) -- unary number
TR opt: pr13468.rkt 53:5 (real-part (ann 6 Integer)) -- unary number
TR opt: pr13468.rkt 55:5 (real-part (ann 0.0 Flonum-Zero)) -- unary number
TR opt: pr13468.rkt 56:5 (real-part (ann 6.0 Positive-Flonum)) -- unary number
TR opt: pr13468.rkt 57:5 (real-part (ann 6.0 Nonnegative-Flonum)) -- unary number
TR opt: pr13468.rkt 58:5 (real-part (ann -6.0 Nonpositive-Flonum)) -- unary number
TR opt: pr13468.rkt 59:5 (real-part (ann -6.0 Negative-Flonum)) -- unary number
TR opt: pr13468.rkt 60:5 (real-part (ann 6.0 Flonum)) -- unary number
TR opt: pr13468.rkt 64:5 (imag-part (ann 5.6 Real)) -- unary number
END
#<<END
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
0

END
#lang typed/racket
#reader tests/typed-racket/optimizer/reset-port

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
