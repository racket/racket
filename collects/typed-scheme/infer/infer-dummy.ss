#lang scheme/base
(require "../utils/utils.ss")

(require (rep type-rep) (utils tc-utils))

(define infer-param (make-parameter (lambda e (int-err "infer not initialized"))))
(define (unify X S T) ((infer-param) X S T (make-Univ) null))
(provide unify infer-param)
