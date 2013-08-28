#lang scheme/base
(require deinprogramm/signature/signature-unit
	 racket/unit)

(provide-signature-elements signatures^)

(define recursive-signature-message "rekursive Signatur")
(define parameter-count-mismatch-message "falsche Anzahl von Parametern")
(define argument-count-mismatch-message "falsche Anzahl von Argumenten")

(define-values/invoke-unit signatures@ 
  (import signature-messages^)
  (export signatures^))

