#lang scheme/base

(provide define-record-procedures
	 define-record-procedures-parametric
         define-record-procedures-2
	 define-record-procedures-parametric-2)

(require scheme/include
	 scheme/promise
	 mzlib/struct
         mzlib/pconvert-prop
         mzlib/pretty
	 deinprogramm/contract/contract
	 deinprogramm/contract/contract-syntax)

(require (for-syntax scheme/base)
         (for-syntax deinprogramm/syntax-checkers)
         (for-syntax stepper/private/shared))
(include "define-record-procedures.scm")
