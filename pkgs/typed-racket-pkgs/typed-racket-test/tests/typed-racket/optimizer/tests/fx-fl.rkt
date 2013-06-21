#;
#<<END
TR opt: fx-fl.rkt 11:0 (exact->inexact 1) -- fixnum to float
1.0

END

#lang typed/scheme
#:optimize

(exact->inexact 1)
