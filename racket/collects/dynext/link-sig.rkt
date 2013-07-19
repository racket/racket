#lang racket/base
(require racket/unit)

(provide dynext:link^)

(define-signature dynext:link^
  (link-extension
   current-extension-linker
   current-extension-linker-flags
   current-make-link-input-strings
   current-make-link-output-strings
   current-standard-link-libraries
   current-use-mzdyn
   use-standard-linker
   link-variant
   expand-for-link-variant))
