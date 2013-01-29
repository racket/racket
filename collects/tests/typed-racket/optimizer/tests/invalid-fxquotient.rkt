#;
(
TR missed opt: invalid-fxquotient.rkt 10:21 (quotient fixnum-min -1) -- out of fixnum range
#t
)

#lang typed/racket/base

(define: fixnum-min : Nonpositive-Fixnum (assert (- (expt 2 30)) fixnum?))
(define: q : Natural (quotient fixnum-min -1)) ; this can't be optimized safely
(= 1073741824 q)
