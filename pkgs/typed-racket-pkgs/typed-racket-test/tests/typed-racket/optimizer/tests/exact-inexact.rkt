#;#;
#<<END
TR opt: exact-inexact.rkt 25:0 (exact->inexact (expt 10 100)) -- int to float
TR opt: exact-inexact.rkt 26:0 (round (exact->inexact (expt 2.3 3.2))) -- unary float
TR opt: exact-inexact.rkt 26:7 (exact->inexact (expt 2.3 3.2)) -- float to float
TR opt: exact-inexact.rkt 27:0 (real->double-flonum (expt 10 100)) -- int to float
TR opt: exact-inexact.rkt 28:0 (round (real->double-flonum (expt 2.3 3.2))) -- unary float
TR opt: exact-inexact.rkt 28:7 (real->double-flonum (expt 2.3 3.2)) -- float to float
TR opt: exact-inexact.rkt 31:0 (exact->inexact (expt 1.0f0 2.0f0)) -- single-float to single-float
TR opt: exact-inexact.rkt 32:0 (real->single-flonum (expt 1.0f0 2.0f0)) -- single-float to single-float
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

(exact->inexact (expt 10 100)) ; must not be a fixnum
(round (exact->inexact (expt 2.3 3.2))) ; already a float
(real->double-flonum (expt 10 100)) ; must not be a fixnum
(round (real->double-flonum (expt 2.3 3.2))) ; already a float

;; single floats
(exact->inexact (expt 1.0f0 2.0f0))
(real->single-flonum (expt 1.0f0 2.0f0))
