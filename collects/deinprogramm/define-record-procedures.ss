#lang scheme/base

(provide define-record-procedures
	 define-record-procedures-parametric
         define-record-procedures-2
	 define-record-procedures-parametric-2)

(require scheme/include
         mzlib/pconvert-prop
         mzlib/pretty
	 deinprogramm/contract/contract-syntax)

(require deinprogramm/deinprogramm-struct)
(require (for-syntax scheme/base)
         (for-syntax deinprogramm/syntax-checkers)
         (for-syntax stepper/private/shared))
(include "define-record-procedures.scm")
