#;#;
#<<END
TR opt: extflonums.rkt 70:0 (extfl+ 1.0t0 1.0t0) -- binary extflonum
TR opt: extflonums.rkt 71:0 (extfl- 1.0t0 1.0t0) -- binary extflonum
TR opt: extflonums.rkt 72:0 (extfl* 1.0t0 1.0t0) -- binary extflonum
TR opt: extflonums.rkt 73:0 (extfl/ 1.0t0 1.0t0) -- binary extflonum
TR opt: extflonums.rkt 74:0 (extfl= 1.0t0 1.0t0) -- binary extflonum
TR opt: extflonums.rkt 75:0 (extfl< 1.0t0 1.0t0) -- binary extflonum
TR opt: extflonums.rkt 76:0 (extfl> 1.0t0 1.0t0) -- binary extflonum
TR opt: extflonums.rkt 77:0 (extfl<= 1.0t0 1.0t0) -- binary extflonum
TR opt: extflonums.rkt 78:0 (extfl>= 1.0t0 1.0t0) -- binary extflonum
TR opt: extflonums.rkt 79:0 (extflmin 1.0t0 1.0t0) -- binary extflonum
TR opt: extflonums.rkt 80:0 (extflmax 1.0t0 1.0t0) -- binary extflonum
TR opt: extflonums.rkt 81:0 (extflexpt 1.0t0 1.0t0) -- binary extflonum
TR opt: extflonums.rkt 83:0 (extflabs 1.0t0) -- unary extflonum
TR opt: extflonums.rkt 84:0 (extflround 1.0t0) -- unary extflonum
TR opt: extflonums.rkt 85:0 (extflfloor 1.0t0) -- unary extflonum
TR opt: extflonums.rkt 86:0 (extflceiling 1.0t0) -- unary extflonum
TR opt: extflonums.rkt 87:0 (extfltruncate 1.0t0) -- unary extflonum
TR opt: extflonums.rkt 88:0 (extflsin 1.0t0) -- unary extflonum
TR opt: extflonums.rkt 89:0 (extflcos 1.0t0) -- unary extflonum
TR opt: extflonums.rkt 90:0 (extfltan 1.0t0) -- unary extflonum
TR opt: extflonums.rkt 91:0 (extflasin 1.0t0) -- unary extflonum
TR opt: extflonums.rkt 92:0 (extflacos 1.0t0) -- unary extflonum
TR opt: extflonums.rkt 93:0 (extflatan 1.0t0) -- unary extflonum
TR opt: extflonums.rkt 94:0 (extfllog 1.0t0) -- unary extflonum
TR opt: extflonums.rkt 95:0 (extflexp 1.0t0) -- unary extflonum
TR opt: extflonums.rkt 96:0 (extflsqrt 1.0t0) -- unary extflonum
TR opt: extflonums.rkt 98:0 (->extfl (bitwise-and 1 2)) -- fixnum to extflonum conversion
TR opt: extflonums.rkt 98:9 (bitwise-and 1 2) -- binary fixnum
TR opt: extflonums.rkt 99:0 (real->extfl (bitwise-and 1 2)) -- fixnum to extflonum conversion
TR opt: extflonums.rkt 99:13 (bitwise-and 1 2) -- binary fixnum
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
