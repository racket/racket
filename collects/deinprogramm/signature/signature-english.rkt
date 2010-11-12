#lang scheme/base
(require deinprogramm/signature/signature-unit
	 racket/unit)

(provide-signature-elements signatures^)

(define recursive-signature-message "recursive signature")
(define parameter-count-mismatch-message "wrong number of parameters")
(define argument-count-mismatch-message "wrong number of arguments")

(define-values/invoke-unit signatures@ 
  (import signature-messages^)
  (export signatures^))

