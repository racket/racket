#lang scheme
(require "xml-sig.ss"
         "xml-unit.ss")

(define-values/invoke-unit/infer xml@)

(provide-signature-elements xml^)