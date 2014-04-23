#;#;
#<<END
TR opt: extflonums.rkt 10:0 (extfl> 1.0t0 1.0t0) -- binary extflonum
TR opt: extflonums.rkt 11:0 (extfl<= 1.0t0 1.0t0) -- binary extflonum
TR opt: extflonums.rkt 12:0 (extfl>= 1.0t0 1.0t0) -- binary extflonum
TR opt: extflonums.rkt 13:0 (extflmin 1.0t0 1.0t0) -- binary extflonum
TR opt: extflonums.rkt 14:0 (extflmax 1.0t0 1.0t0) -- binary extflonum
TR opt: extflonums.rkt 15:0 (extflexpt 1.0t0 1.0t0) -- binary extflonum
TR opt: extflonums.rkt 17:0 (extflabs 1.0t0) -- unary extflonum
TR opt: extflonums.rkt 18:0 (extflround 1.0t0) -- unary extflonum
TR opt: extflonums.rkt 19:0 (extflfloor 1.0t0) -- unary extflonum
TR opt: extflonums.rkt 20:0 (extflceiling 1.0t0) -- unary extflonum
TR opt: extflonums.rkt 21:0 (extfltruncate 1.0t0) -- unary extflonum
TR opt: extflonums.rkt 22:0 (extflsin 1.0t0) -- unary extflonum
TR opt: extflonums.rkt 23:0 (extflcos 1.0t0) -- unary extflonum
TR opt: extflonums.rkt 24:0 (extfltan 1.0t0) -- unary extflonum
TR opt: extflonums.rkt 25:0 (extflasin 1.0t0) -- unary extflonum
TR opt: extflonums.rkt 26:0 (extflacos 1.0t0) -- unary extflonum
TR opt: extflonums.rkt 27:0 (extflatan 1.0t0) -- unary extflonum
TR opt: extflonums.rkt 28:0 (extfllog 1.0t0) -- unary extflonum
TR opt: extflonums.rkt 29:0 (extflexp 1.0t0) -- unary extflonum
TR opt: extflonums.rkt 30:0 (extflsqrt 1.0t0) -- unary extflonum
TR opt: extflonums.rkt 32:0 (->extfl (bitwise-and 1 2)) -- fixnum to extflonum conversion
TR opt: extflonums.rkt 32:9 (bitwise-and 1 2) -- binary fixnum
TR opt: extflonums.rkt 33:0 (real->extfl (bitwise-and 1 2)) -- fixnum to extflonum conversion
TR opt: extflonums.rkt 33:13 (bitwise-and 1 2) -- binary fixnum
TR opt: extflonums.rkt 4:0 (extfl+ 1.0t0 1.0t0) -- binary extflonum
TR opt: extflonums.rkt 5:0 (extfl- 1.0t0 1.0t0) -- binary extflonum
TR opt: extflonums.rkt 6:0 (extfl* 1.0t0 1.0t0) -- binary extflonum
TR opt: extflonums.rkt 7:0 (extfl/ 1.0t0 1.0t0) -- binary extflonum
TR opt: extflonums.rkt 8:0 (extfl= 1.0t0 1.0t0) -- binary extflonum
TR opt: extflonums.rkt 9:0 (extfl< 1.0t0 1.0t0) -- binary extflonum
END
#<<END
2.0t0
0.0t0
1.0t0
1.0t0
#t
#f
#f
#t
#t
1.0t0
1.0t0
1.0t0
1.0t0
1.0t0
1.0t0
1.0t0
1.0t0
0.84147098480789650666t0
0.5403023058681397174t0
1.5574077246549022306t0
1.5707963267948966193t0
0.0t0
0.78539816339744830963t0
0.0t0
2.7182818284590452354t0
1.0t0
0.0t0
0.0t0

END

#lang typed/racket
#reader tests/typed-racket/optimizer/reset-port

(require racket/extflonum)

(extfl+ 1.0t0 1.0t0)
(extfl- 1.0t0 1.0t0)
(extfl* 1.0t0 1.0t0)
(extfl/ 1.0t0 1.0t0)
(extfl= 1.0t0 1.0t0)
(extfl< 1.0t0 1.0t0)
(extfl> 1.0t0 1.0t0)
(extfl<= 1.0t0 1.0t0)
(extfl>= 1.0t0 1.0t0)
(extflmin 1.0t0 1.0t0)
(extflmax 1.0t0 1.0t0)
(extflexpt 1.0t0 1.0t0)

(extflabs 1.0t0)
(extflround 1.0t0)
(extflfloor 1.0t0)
(extflceiling 1.0t0)
(extfltruncate 1.0t0)
(extflsin 1.0t0)
(extflcos 1.0t0)
(extfltan 1.0t0)
(extflasin 1.0t0)
(extflacos 1.0t0)
(extflatan 1.0t0)
(extfllog 1.0t0)
(extflexp 1.0t0)
(extflsqrt 1.0t0)

(->extfl (bitwise-and 1 2))
(real->extfl (bitwise-and 1 2))
