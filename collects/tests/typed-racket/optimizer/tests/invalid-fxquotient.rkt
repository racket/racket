#;
(
TR missed opt: invalid-fxquotient.rkt 10:21 (quotient fixnum-min -1) -- out of fixnum range
#f
)

#lang typed/racket/base

(define: fixnum-min : Nonpositive-Fixnum (assert (- (expt 2 30)) fixnum?))
(define: q : Natural (quotient fixnum-min -1)) ; this can't be optimized safely
(fixnum? q) ; should return #f
