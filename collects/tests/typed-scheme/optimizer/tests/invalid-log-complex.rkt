#;
(
invalid-log-complex.rkt 10:11 (#%app log (quote 2.0)) -- unexpected complex type
0.6931471805599453
)

#lang typed/scheme
#:optimize

(real-part (log (ann 2.0 Float)))
