#lang scheme/base

(require "type-rep.ss")

(define infer-param (make-parameter (lambda e (error 'infer "not initialized"))))
(define (unify X S T) ((infer-param) X S T (make-Univ)))
(provide unify infer-param)