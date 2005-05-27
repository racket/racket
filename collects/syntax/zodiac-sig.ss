
;; Interface for zodiac compatibility layer,
;;  for programs that used to manipulate the
;;  output of zodiac elaboration.

(module zodiac-sig mzscheme
  (require (lib "unitsig.ss"))
  
  (provide zodiac^)

  (define-signature  zodiac^
    (;; Syntax -> zodiac compatibility:
     syntax->zodiac
     ;; Zodiac compatibility -> syntax:
     zodiac->syntax

     structurize-syntax
     zread-object ; = (compose syntax-e zodiac-stx)

     ;; origin struct:
     origin-who  ; 'source or 'macro
     origin-how  ; #f or tree of syntax objects,
                 ;  as repotred by the 'origin
                 ;  property of the syntax object.
     
     ;; location struct:
     location-line    ; = syntax line
     location-column  ; = syntax col
     location-file    ; = syntax src
     ;; Note: there is no location-offset, yet

     ;; EOF
     eof?

     ;; zodiac struct:
     ;;  zodiac (stx) ; used to be (origin start finish)
     (struct zodiac (stx))
     zodiac-origin    ; = identity
     zodiac-start     ; = identity
     zodiac-finish    ; = zodiac-start

     ;; reader structs:
     ;;  zodiac (stx)
     ;;    zread ; used to have (object)
     ;; The sub-tree has been cut off; inspect
     ;;  the stx object, instead.
     (struct zread ())

     ;; elaborator structs:
     (struct parsed (back))

     (struct varref (var))
     (struct top-level-varref (module slot exptime? expdef? position))  ; added module, exptime?, position
     create-top-level-varref
     (struct bound-varref (binding))   create-bound-varref

     (struct binding (var orig-name))  create-binding

     make-lexical-varref
     lexical-varref? create-lexical-varref      ; alias for bound-varref
     make-lexical-binding
     lexical-binding?  create-lexical-binding   ; alias for binding

     (struct app (fun args))           create-app

     (struct if-form (test then else))               create-if-form
     (struct quote-form (expr))                      create-quote-form
     (struct begin-form (bodies))                    create-begin-form
     (struct begin0-form (bodies))                   create-begin0-form
     (struct let-values-form (vars vals body))       create-let-values-form
     (struct letrec-values-form (vars vals body))    create-letrec-values-form
     (struct define-values-form (vars val))          create-define-values-form
     (struct set!-form (var val))                    create-set!-form
     (struct case-lambda-form (args bodies))         create-case-lambda-form
     (struct with-continuation-mark-form (key val body)) create-with-continuation-mark-form

     ;; Thess are new:
     (struct quote-syntax-form (expr))               create-quote-syntax-form
     (struct define-syntaxes-form (names expr))      create-define-syntaxes-form
     (struct define-for-syntax-form (names expr))    create-define-for-syntax-form
     (struct module-form (name requires            ; lstof stx for module paths
			       for-syntax-requires ; lstof stx for module paths
			       for-template-requires ; lstof stx for module paths
			       body                ; begin form
			       syntax-body         ; begin form
			       provides  ; lstof (sym | (def-sym . prvd-sym) | (mod-path def-sym . prvd-sym))
			       syntax-provides  ;  ditto
			       indirect-provides   ; lstof sym
			       kernel-reprovide-hint ; #f | #t | exclude-sym
			       self-path-index))   ; module path index
     create-module-form
     (struct require/provide-form ())                create-require/provide-form

     ;; args:
     (struct arglist (vars))
     (struct sym-arglist ())
     (struct list-arglist ())
     (struct ilist-arglist ())

     make-empty-back-box
     register-client)))
