
(module compile-sig mzscheme
  (require (lib "unitsig.ss"))

  (provide dynext:compile^)
  
  (define-signature dynext:compile^
    (compile-extension
     current-extension-compiler 
     current-extension-compiler-flags
     current-make-compile-include-strings
     current-make-compile-input-strings
     current-make-compile-output-strings
     use-standard-compiler
     get-standard-compilers
     compile-variant
     expand-for-compile-variant)))

