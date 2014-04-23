#;#;
#<<END
TR missed opt: exact-inexact.rkt 8:16 (expt 1.0f0 2.0f0) -- all args float-arg-expr, result not Float -- caused by: 8:22 1.0f0, 8:28 2.0f0
TR missed opt: exact-inexact.rkt 9:21 (expt 1.0f0 2.0f0) -- all args float-arg-expr, result not Float -- caused by: 9:27 1.0f0, 9:33 2.0f0
TR opt: exact-inexact.rkt 2:0 (exact->inexact (expt 10 100)) -- int to float
TR opt: exact-inexact.rkt 3:0 (round (exact->inexact (expt 2.3 3.2))) -- unary float
TR opt: exact-inexact.rkt 3:23 (expt 2.3 3.2) -- binary float
TR opt: exact-inexact.rkt 3:7 (exact->inexact (expt 2.3 3.2)) -- float to float
TR opt: exact-inexact.rkt 4:0 (real->double-flonum (expt 10 100)) -- int to float
TR opt: exact-inexact.rkt 5:0 (round (real->double-flonum (expt 2.3 3.2))) -- unary float
TR opt: exact-inexact.rkt 5:28 (expt 2.3 3.2) -- binary float
TR opt: exact-inexact.rkt 5:7 (real->double-flonum (expt 2.3 3.2)) -- float to float
TR opt: exact-inexact.rkt 8:0 (exact->inexact (expt 1.0f0 2.0f0)) -- single-float to single-float
TR opt: exact-inexact.rkt 9:0 (real->single-flonum (expt 1.0f0 2.0f0)) -- single-float to single-float
END
#<<END
1e+100
14.0
1e+100
14.0
1.0f0
1.0f0

END
#lang typed/scheme
#:optimize
#reader tests/typed-racket/optimizer/reset-port

(exact->inexact (expt 10 100)) ; must not be a fixnum
(round (exact->inexact (expt 2.3 3.2))) ; already a float
(real->double-flonum (expt 10 100)) ; must not be a fixnum
(round (real->double-flonum (expt 2.3 3.2))) ; already a float

;; single floats
(exact->inexact (expt 1.0f0 2.0f0))
(real->single-flonum (expt 1.0f0 2.0f0))
