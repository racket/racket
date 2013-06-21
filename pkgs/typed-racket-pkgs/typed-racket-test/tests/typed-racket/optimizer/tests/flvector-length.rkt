#;#;
#<<END
TR opt: flvector-length.rkt 14:0 (flvector-length (flvector 0.0 1.2)) -- flvector-length

END
#<<END
2

END

#lang typed/scheme
#:optimize
(require racket/flonum)
(flvector-length (flvector 0.0 1.2))
