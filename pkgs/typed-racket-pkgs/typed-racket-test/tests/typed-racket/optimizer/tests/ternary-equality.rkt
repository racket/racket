#;#;
#<<END
TR opt: ternary-equality.rkt 15:0 (= 1 1 1) -- multi fixnum comp
END
#<<END
#t

END

#lang typed/racket/base
#:optimize

;; PR 12479
;; was incorrectly optimized in the same way as fixnum bitwise-and and co
(= 1 1 1)
