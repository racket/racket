#;#;
#<<END
TR info: invalid-log-complex.rkt 14:0 (real-part (log (ann 2.0 Float))) -- possible exact real arith
TR missed opt: invalid-log-complex.rkt 14:11 (log (ann 2.0 Float)) -- unexpected complex type
END
#<<END
0.6931471805599453

END

#lang typed/scheme
#:optimize

(real-part (log (ann 2.0 Float)))
