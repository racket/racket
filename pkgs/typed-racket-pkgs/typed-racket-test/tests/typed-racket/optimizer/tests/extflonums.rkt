#;#;
#<<END
TR opt: extflonums.rkt 10:5 (extfl* 1.0t0 1.0t0) -- binary extflonum
TR opt: extflonums.rkt 11:5 (extfl/ 1.0t0 1.0t0) -- binary extflonum
TR opt: extflonums.rkt 12:0 (extfl= 1.0t0 1.0t0) -- binary extflonum
TR opt: extflonums.rkt 13:0 (extfl< 1.0t0 1.0t0) -- binary extflonum
TR opt: extflonums.rkt 14:0 (extfl> 1.0t0 1.0t0) -- binary extflonum
TR opt: extflonums.rkt 15:0 (extfl<= 1.0t0 1.0t0) -- binary extflonum
TR opt: extflonums.rkt 16:0 (extfl>= 1.0t0 1.0t0) -- binary extflonum
TR opt: extflonums.rkt 17:0 (extflmin 1.0t0 1.0t0) -- binary extflonum
TR opt: extflonums.rkt 18:0 (extflmax 1.0t0 1.0t0) -- binary extflonum
TR opt: extflonums.rkt 19:0 (extflexpt 1.0t0 1.0t0) -- binary extflonum
TR opt: extflonums.rkt 21:5 (extflabs 1.0t0) -- unary extflonum
TR opt: extflonums.rkt 22:5 (extflround 1.0t0) -- unary extflonum
TR opt: extflonums.rkt 23:5 (extflfloor 1.0t0) -- unary extflonum
TR opt: extflonums.rkt 24:5 (extflceiling 1.0t0) -- unary extflonum
TR opt: extflonums.rkt 25:5 (extfltruncate 1.0t0) -- unary extflonum
TR opt: extflonums.rkt 26:5 (extflsin 1.0t0) -- unary extflonum
TR opt: extflonums.rkt 27:5 (extflcos 1.0t0) -- unary extflonum
TR opt: extflonums.rkt 28:5 (extfltan 1.0t0) -- unary extflonum
TR opt: extflonums.rkt 29:5 (extflasin 1.0t0) -- unary extflonum
TR opt: extflonums.rkt 30:5 (extflacos 1.0t0) -- unary extflonum
TR opt: extflonums.rkt 31:5 (extflatan 1.0t0) -- unary extflonum
TR opt: extflonums.rkt 32:5 (extfllog 1.0t0) -- unary extflonum
TR opt: extflonums.rkt 33:5 (extflexp 1.0t0) -- unary extflonum
TR opt: extflonums.rkt 34:5 (extflsqrt 1.0t0) -- unary extflonum
TR opt: extflonums.rkt 36:14 (bitwise-and 1 2) -- binary fixnum
TR opt: extflonums.rkt 36:5 (->extfl (bitwise-and 1 2)) -- fixnum to extflonum conversion
TR opt: extflonums.rkt 37:18 (bitwise-and 1 2) -- binary fixnum
TR opt: extflonums.rkt 37:5 (real->extfl (bitwise-and 1 2)) -- fixnum to extflonum conversion
TR opt: extflonums.rkt 8:5 (extfl+ 1.0t0 1.0t0) -- binary extflonum
TR opt: extflonums.rkt 9:5 (extfl- 1.0t0 1.0t0) -- binary extflonum
END
#<<END
"2.00000"
"0.00000"
"1.00000"
"1.00000"
#t
#f
#f
#t
#t
1.0t0
1.0t0
1.0t0
"1.00000"
"1.00000"
"1.00000"
"1.00000"
"1.00000"
"0.84147"
"0.54030"
"1.55741"
"1.57080"
"0.00000"
"0.78540"
"0.00000"
"2.71828"
"1.00000"
"0.00000"
"0.00000"

END

#lang typed/racket
#reader tests/typed-racket/optimizer/reset-port

(require racket/extflonum)

;; to avoid machine-specific precision issues
(: out : ExtFlonum -> String)
(define (out v) (real->decimal-string (extfl->inexact v) 5))

(out (extfl+ 1.0t0 1.0t0))
(out (extfl- 1.0t0 1.0t0))
(out (extfl* 1.0t0 1.0t0))
(out (extfl/ 1.0t0 1.0t0))
(extfl= 1.0t0 1.0t0)
(extfl< 1.0t0 1.0t0)
(extfl> 1.0t0 1.0t0)
(extfl<= 1.0t0 1.0t0)
(extfl>= 1.0t0 1.0t0)
(extflmin 1.0t0 1.0t0)
(extflmax 1.0t0 1.0t0)
(extflexpt 1.0t0 1.0t0)

(out (extflabs 1.0t0))
(out (extflround 1.0t0))
(out (extflfloor 1.0t0))
(out (extflceiling 1.0t0))
(out (extfltruncate 1.0t0))
(out (extflsin 1.0t0))
(out (extflcos 1.0t0))
(out (extfltan 1.0t0))
(out (extflasin 1.0t0))
(out (extflacos 1.0t0))
(out (extflatan 1.0t0))
(out (extfllog 1.0t0))
(out (extflexp 1.0t0))
(out (extflsqrt 1.0t0))

(out (->extfl (bitwise-and 1 2)))
(out (real->extfl (bitwise-and 1 2)))
