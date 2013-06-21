#;#;
#<<END
TR opt: fx-fl.rkt 13:0 (exact->inexact 1) -- fixnum to float
END
#<<END
1.0

END

#lang typed/scheme
#:optimize

(exact->inexact 1)
