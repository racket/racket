#;#;
#<<END
TR missed opt: invalid-log-complex.rkt 2:11 (log (ann 2.0 Float)) -- unexpected complex type
END
#<<END
0.6931471805599453

END
#lang typed/scheme
#:optimize
#reader tests/typed-racket/optimizer/reset-port

(real-part (log (ann 2.0 Float)))
