#;#;
#<<END
TR info: invalid-fxquotient.rkt 5:14 (fxquotient 3 0) -- non-optimized fixnum op
TR missed opt: invalid-fxquotient.rkt 3:21 (quotient fixnum-min -1) -- out of fixnum range
END
#<<END
#t

END
#lang typed/racket/base
(require racket/fixnum)
#reader tests/typed-racket/optimizer/reset-port

(define: fixnum-min : Nonpositive-Fixnum (assert (- (expt 2 30)) fixnum?))
(define: q : Natural (quotient fixnum-min -1)) ; this can't be optimized safely
(= 1073741824 q)
(define (bad) (fxquotient 3 0)) ; can't be optimized
