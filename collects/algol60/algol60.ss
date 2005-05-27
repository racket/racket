#cs(module algol60 mzscheme
     (require-for-syntax "parse.ss" 
			 ;; Parses to generate an AST. Identifiers in the AST
			 ;; are represented as syntax objects with source location.
			 
                         "simplify.ss"
			 ;; Desugars the AST, transforming `for' to `if'+`goto',
			 ;; and flattening `if' statements so they are always
			 ;; of the for `if <exp> then goto <label> else goto <label>'

                         "compile.ss"
			 ;; Compiles a simplified AST to Scheme.

                         (lib "file.ss"))

     ;; By using #'here for the context of identifiers
     ;; introduced by compilation, the identifiers can
     ;; refer to runtime functions and primitives, as
     ;; well as mzscheme:
     (require "runtime.ss" "prims.ss")
              
     
     (provide include-algol)
     
     (define-syntax (include-algol stx)
       (syntax-case stx ()
         [(_ str)
          (string? (syntax-e (syntax str)))
          
	  (compile-simplified
	   (simplify 
	    (parse-a60-file 
	     (normalize-path (syntax-e (syntax str))
			     (or
			      (current-load-relative-directory)
			      (current-directory))))
	    #'here)
	   #'here)])))
