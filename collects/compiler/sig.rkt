
#lang mzscheme

(require mzlib/unit)

(provide compiler:option^
         compiler^)

;; Compiler options
(define-signature compiler:option^
  (somewhat-verbose ; default = #f
   verbose ; default = #f
   

   setup-prefix ; string to embed in public names;
                ; used mainly for compiling extensions
                ;  with the collection name so that 
                ;  cross-extension conflicts are less
                ;  likely in architectures that expose
                ;  the public names of loaded extensions
                ; default = ""

   3m ; #t => build for 3m
      ; default = #f

   compile-subcollections   ; #t => compile collection subdirectories
                            ; default = #t

   ))

;; Compiler procedures
(define-signature compiler^
  (compile-zos

   compile-collection-zos
   compile-directory-zos
   compile-directory-srcs

   current-compiler-dynamic-require-wrapper
   compile-notify-handler))
