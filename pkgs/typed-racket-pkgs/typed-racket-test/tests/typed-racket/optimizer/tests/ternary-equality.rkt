#;#;
#<<END
TR opt: ternary-equality.rkt 4:0 (= 1 1 1) -- multi fixnum comp
END
#<<END
#t

END
#lang typed/racket/base
#:optimize
#reader tests/typed-racket/optimizer/reset-port

;; PR 12479
;; was incorrectly optimized in the same way as fixnum bitwise-and and co
(= 1 1 1)
