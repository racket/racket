#lang scheme/base
(require "../utils/utils.rkt")

(require (rep type-rep) (utils tc-utils) mzlib/trace)

(define infer-param (make-parameter (lambda e (int-err "infer not initialized"))))
(define (unify X S T) ((infer-param) X null S T (make-Univ)))
;(trace unify)
(provide unify infer-param)
