#;#;
#<<END
TR opt: flvector-length.rkt 2:0 (flvector-length (flvector 0.0 1.2)) -- flvector-length
END
#<<END
2

END
#lang typed/scheme
#:optimize
#reader tests/typed-racket/optimizer/reset-port
(require racket/flonum)
(flvector-length (flvector 0.0 1.2))
