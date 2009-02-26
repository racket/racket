#lang scheme
;; copyright by Paul Graunke June 2000 AD

(require "html-mod.ss" "html-sig.ss" "sgml-reader.ss")

#;(require "html-sig.ss" 
         "html-unit.ss"
         "sgml-reader-sig.ss"
         "sgml-reader-unit.ss"
         xml/private/structures
         xml/private/reader
         xml/private/sig)

#;(define-compound-unit/infer the-html@
  (import)
  (export html^ sgml-reader^)
  (link html@ sgml-reader@ xml-structs@ reader@))

#;(define-values/invoke-unit/infer the-html@)

(provide-signature-elements html^)
(provide read-html-comments)
