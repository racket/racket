#;#;
#<<END
TR missed opt: invalid-float-comp.rkt 2:0 (< 1.0 2) -- generic comparison -- caused by: 2:7 2
END
#<<END
#t

END
#lang typed/scheme
#:optimize
#reader tests/typed-racket/optimizer/reset-port

(< 1.0 2)
