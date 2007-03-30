
(module deriv-c mzscheme
  (provide (all-defined))

  ;; A Derivation is either
  ;;   - a PRule
  ;;   - (make-mrule syntax syntax Transformation Derivation)
  ;;   - (make-lift-deriv syntax syntax Derivation syntax Derivation)
  ;;   - (make-lift/let-deriv syntax syntax Derivation syntax Derivation)
  (define-struct deriv (e1 e2) #f)
  (define-struct (mrule deriv) (transformation next) #f)
  (define-struct (lift-deriv deriv) (first lift-stx second) #f)
  (define-struct (lift/let-deriv deriv) (first lift-stx second) #f)

  ;; A Transformation is
  ;;   (make-transformation syntax syntax (listof identifier) syntax syntax (listof LocalAction))
  ;; - resolves is the list of identifiers resolved by the macro keyword
  ;; - me1 is the marked version of the input syntax
  ;; - me2 is the marked version of the output syntax
  (define-struct transformation (e1 e2 resolves me1 me2 locals seq) #f)

  ;; A LocalAction is one of
  ;;  - (make-local-expansion Syntax Syntax Syntax Syntax boolean Derivation)
  ;;  - (make-local-expansion/expr Syntax Syntax Syntax Syntax boolean Derivation)
  ;;  - (make-local-lift Syntax Identifier)
  (define-struct local-expansion (e1 e2 me1 me2 for-stx? deriv) #f)
  (define-struct local-expansion/expr (e1 e2 me1 me2 for-stx? opaque deriv) #f)
  (define-struct local-lift (expr id) #f)
  (define-struct local-lift-end (decl) #f)
  (define-struct local-bind (deriv) #f)

  ;; A PRule is one of ...
  (define-struct (prule deriv) (resolves) #f)

  ;; Lexical or Mapped Variable
  (define-struct (p:variable prule) () #f)
  
  ;; Definitions: one subterm each
  (define-struct (p:define-syntaxes prule) (rhs) #f)
  (define-struct (p:define-values prule) (rhs) #f)
  
  ;; Simple expressions
  (define-struct (p:expression prule) (inner) #f)
  (define-struct (p:if prule) (full? test then else) #f)
  (define-struct (p:wcm prule) (key mark body) #f)
  (define-struct (p:set! prule) (id-resolves rhs) #f)
  (define-struct (p:set!-macro prule) (deriv) #f)
  
  ;; Sequence-containing expressions
  (define-struct (p:begin prule) (lderiv) #f)
  (define-struct (p:begin0 prule) (first lderiv) #f)
  (define-struct (p:#%app prule) (tagged-stx lderiv) #f)

  ;; Binding expressions
  (define-struct (p:lambda prule) (renames body) #f)
  (define-struct (p:case-lambda prule) (renames+bodies) #f)
  (define-struct (p:let-values prule) (renames rhss body) #f)
  (define-struct (p:letrec-values prule) (renames rhss body) #f)
  (define-struct (p:letrec-syntaxes+values prule) (srenames srhss vrenames vrhss body) #f)
  
  ;; Atomic primitives: no subterms
  (define-struct (p::STOP prule) () #f)
  (define-struct (p:#%datum p::STOP) (tagged-stx) #f)
  (define-struct (p:#%top p::STOP) (tagged-stx) #f)
  (define-struct (p:quote p::STOP) () #f)
  (define-struct (p:quote-syntax p::STOP) () #f)
  (define-struct (p:require p::STOP) () #f)
  (define-struct (p:require-for-syntax p::STOP) () #f)
  (define-struct (p:require-for-template p::STOP) () #f)
  (define-struct (p:provide p::STOP) () #f)

  ;; for stop expander
  (define-struct (p:stop p::STOP) () #f)
  ;; for early primitive errors
  (define-struct (p:unknown p::STOP) () #f)
  
  ;; Module stuff.... hairy
  (define-struct (p:module prule) (one-body-form? body) #f)
  (define-struct (p:#%module-begin prule) (pass1 pass2) #f)
  ;; where pass1 is a ModPass1
  ;;   and pass2 is a ModPass2

  ;; Artificial Rename
  ;; FIXME: Go back and add more info later, such as rename-identity
  (define-struct (p:rename prule) (renames inner) #f)

  ;; Synthetic primitive
  (define-struct (p:synth prule) (subterms) #f)
  ;; where subterms is list-of-Subterm

  ;; A Subterm is one of
  ;;   - (make-s:subterm Path Derivation)
  ;;   - (make-s:rename Path Syntax Syntax)
  (define-struct s:subterm (path deriv) #f)
  (define-struct s:rename (path before after) #f)

  ;; A ListDerivation is (make-lderiv Syntaxes Syntaxes (listof Derivation))
  (define-struct lderiv (es1 es2 derivs) #f)

  ;; A BlockDerivation is (make-bderiv syntax-list syntax-list BlockPass1 Transition LDeriv)
  ;;   where Transition = (union 'letrec 'list)
  (define-struct bderiv (es1 es2 pass1 trans pass2) #f)

  ;; A BlockPass1 is list-of-BRule
  ;; A BRule is one of
  ;;   - (make-b:defvals BlockRename Derivation/#f)
  ;;   - (make-b:devstx BlockRename Derivation Derivation)
  ;;   - (make-b:splice BlockRename Derivation Syntaxes)
  ;;   - (make-b:expr BlockRename Derivation)
  ;;   - (make-b:begin BlockRename Derivation List-of-BRule)
  ;;     This last only used in macro-hiding
  ;; A BlockRename is (cons syntax syntax)
  ;; It always applies only to the current block element

  (define-struct brule (renames) #f)
  (define-struct (b:defvals brule) (head) #f)
  (define-struct (b:defstx brule) (deriv rhs) #f)
  (define-struct (b:splice brule) (head tail) #f)
  (define-struct (b:expr brule) (head) #f)
  (define-struct (b:begin brule) (head inner) #f)

  ;; A ModPass1 is a list of ModRule1
  ;; A ModRule1 is one of 
  ;;   - (make-mod:prim Derivation ModPrim)
  ;;   - (make-mod:splice Derivation tail)
  ;;   - (make-mod:lift Derivation tail)
  ;;   - (make-mod:begin Derivation (list-of ModRule1))

  ;; A ModPrim is a PRule in:
  ;;   - (make-p:define-values syntax syntax () #f)
  ;;   - (make-p:define-syntaxes syntax syntax () Derivation)
  ;;   - (make-p:require syntax syntax ())
  ;;   - (make-p:require-for-syntax syntax syntax ())
  ;;   - (make-p:require-for-template syntax syntax ())
  ;;   - (make-p:provide syntax syntax ())
  ;;   - #f

  ;; A ModPass2 is a list of ModRule2
  ;; A ModRule2 is one of
  ;;   - (make-mod:skip)
  ;;   - (make-mod:cons Derivation)
  ;;   - (make-mod:lift Derivation syntaxes)

  (define-struct modrule () #f)
  (define-struct (mod:cons modrule) (head) #f)
  (define-struct (mod:prim modrule) (head prim) #f)
  (define-struct (mod:skip modrule) () #f)
  (define-struct (mod:splice modrule) (head tail) #f)
  (define-struct (mod:lift modrule) (head tail) #f)
  (define-struct (mod:lift-end modrule) (tail) #f)
  (define-struct (mod:begin modrule) (head inner) #f)

  ;; Handling Syntax Errors
  ;; ----------------------

  ;; An interrupted node is (make-interrupted-wrap symbol node)
  ;; where node is one of Derivation, ListDerivation, BlockDerivation,
  ;;   PRule, MRule, BRule, or ModRule
  (define-struct interrupted-wrap (tag inner) #f)

  ;; An error-wrapped node is (make-error-wrap exception symbol node)
  ;; where node is one of PRule, MRule, BRule, or ModRule
  (define-struct error-wrap (exn tag inner) #f)
  
  )
