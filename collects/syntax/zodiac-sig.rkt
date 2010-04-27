
;; Interface for zodiac compatibility layer,
;;  for programs that used to manipulate the
;;  output of zodiac elaboration.

#lang scheme/signature

;; Syntax -> zodiac compatibility:
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
(struct zodiac (stx) #:mutable)
zodiac-origin    ; = identity
zodiac-start     ; = identity
zodiac-finish    ; = zodiac-start

;; reader structs:
;;  zodiac (stx)
;;    zread ; used to have (object)
;; The sub-tree has been cut off; inspect
;;  the stx object, instead.
(struct zread () #:mutable)

;; elaborator structs:
(struct parsed (back) #:mutable)

(struct varref (var) #:mutable)
(struct top-level-varref (module slot exptime? expdef? position) #:mutable)  ; added module, exptime?, position
create-top-level-varref
(struct bound-varref (binding) #:mutable)   create-bound-varref

(struct binding (var orig-name) #:mutable)  create-binding

make-lexical-varref
lexical-varref? create-lexical-varref      ; alias for bound-varref
make-lexical-binding
lexical-binding?  create-lexical-binding   ; alias for binding

(struct app (fun args) #:mutable)           create-app

(struct if-form (test then else) #:mutable)               create-if-form
(struct quote-form (expr) #:mutable)                      create-quote-form
(struct begin-form (bodies) #:mutable)                    create-begin-form
(struct begin0-form (bodies) #:mutable)                   create-begin0-form
(struct let-values-form (vars vals body) #:mutable)       create-let-values-form
(struct letrec-values-form (vars vals body) #:mutable)    create-letrec-values-form
(struct define-values-form (vars val) #:mutable)          create-define-values-form
(struct set!-form (var val) #:mutable)                    create-set!-form
(struct case-lambda-form (args bodies) #:mutable)         create-case-lambda-form
(struct with-continuation-mark-form (key val body) #:mutable) create-with-continuation-mark-form

;; Thess are new:
(struct quote-syntax-form (expr) #:mutable)               create-quote-syntax-form
(struct define-syntaxes-form (names expr) #:mutable)      create-define-syntaxes-form
(struct define-for-syntax-form (names expr) #:mutable)    create-define-for-syntax-form
(struct module-form (name requires            ; lstof stx for module paths
                          for-syntax-requires ; lstof stx for module paths
                          for-template-requires ; lstof stx for module paths
                          body                ; begin form
                          syntax-body         ; begin form
                          provides  ; lstof (sym | (def-sym . prvd-sym) #:mutable | (mod-path def-sym . prvd-sym))
                          syntax-provides  ;  ditto
                          indirect-provides   ; lstof sym
                          kernel-reprovide-hint ; #f | #t | exclude-sym
                          self-path-index))   ; module path index
create-module-form
(struct require/provide-form () #:mutable)                create-require/provide-form

;; These forms are highly mzc-specific. They are recongized
;;  as applications of the corresponding quoted symbols to the
;;  right kinds of arguments.
(struct global-prepare (vec pos) #:mutable)             create-global-prepare
(struct global-lookup (vec pos) #:mutable)              create-global-lookup
(struct global-assign (vec pos expr) #:mutable)         create-global-assign
(struct safe-vector-ref (vec pos) #:mutable)            create-safe-vector-ref
global-prepare-id
global-lookup-id
global-assign-id
safe-vector-ref-id

;; args:
(struct arglist (vars) #:mutable)
(struct sym-arglist () #:mutable)
(struct list-arglist () #:mutable)
(struct ilist-arglist () #:mutable)

make-empty-back-box
register-client
