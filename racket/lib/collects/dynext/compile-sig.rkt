#lang racket/base
(require racket/unit)

(provide dynext:compile^)

(define-signature dynext:compile^
  (compile-extension
   preprocess-extension
   current-extension-compiler 
   current-extension-compiler-flags
   current-extension-preprocess-flags
   current-make-compile-include-strings
   current-make-compile-input-strings
   current-make-compile-output-strings
   use-standard-compiler
   get-standard-compilers
   compile-variant
   expand-for-compile-variant))

