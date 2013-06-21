#;#;
#<<END
TR missed opt: invalid-log-complex.rkt 13:11 (log (ann 2.0 Float)) -- unexpected complex type
END
#<<END
0.6931471805599453

END

#lang typed/scheme
#:optimize

(real-part (log (ann 2.0 Float)))
