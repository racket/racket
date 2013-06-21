#;#;
#<<END
TR opt: flvector-length.rkt 13:0 (flvector-length (flvector 0.0 1.2)) -- flvector-length
END
#<<END
2

END

#lang typed/scheme
#:optimize
(require racket/flonum)
(flvector-length (flvector 0.0 1.2))
