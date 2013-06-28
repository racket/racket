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
	 deinprogramm/signature/signature
	 deinprogramm/signature/signature-german
	 deinprogramm/signature/signature-syntax
	 (only-in deinprogramm/quickcheck/quickcheck arbitrary-record arbitrary-one-of))

(require (for-syntax scheme/base)
         (for-syntax deinprogramm/syntax-checkers)
         (for-syntax stepper/private/syntax-property))
(include "define-record-procedures.scm")
