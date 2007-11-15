
(module deriv-c mzscheme
  (provide (all-defined))

  ;; A Node(a) is:
  ;;   (make-node a ?a)
  (define-struct node (z1 z2) #f)
  
  ;; A TopDeriv is one of
  ;;   (make-lift-deriv <Node(Stx)> Deriv Stxs TopDeriv)
  ;;   Deriv
  
  ;; A Deriv is one of
  ;;   (make-mrule <Node(Stx)> Transformation Deriv)
  ;;   PrimDeriv
  (define-struct (deriv node) () #f)
  (define-struct (lift-deriv deriv) (first lift-stx second) #f)
  (define-struct (mrule deriv) (transformation next) #f)

  ;; A DerivLL is one of
  ;;   (make-lift/let-deriv <Node(Stx)> Deriv Stx Deriv)
  ;;   Deriv
  (define-struct (lift/let-deriv deriv) (first lift-stx second) #f)

  ;; A Transformation is
  ;;   (make-transformation <Node(Stx)> Rs ?exn ?Stx (list-of LocalAction) ?exn ?Stx Number)
  (define-struct (transformation node) (resolves ?1 me1 locals ?2 me2 seq) #f)

  ;; A LocalAction is one of
  ;;   (make-local-expansion <Node(Stx)> Stx ?Stx Boolean Deriv)
  ;;   (make-local-expansion/expr <Node(Stx)> Stx ?Stx Boolean ?Opaque Deriv)
  ;;   (make-local-lift Stx Identifier)
  ;;   (make-local-lift-end Stx)
  ;;   (make-local-bind BindSyntaxes)
  (define-struct (local-expansion node) (me1 me2 for-stx? inner) #f)
  (define-struct (local-expansion/expr node) (me1 me2 for-stx? opaque inner) #f)
  (define-struct local-lift (expr id) #f)
  (define-struct local-lift-end (decl) #f)
  (define-struct local-bind (bindrhs) #f)

  ;; Base = << Node(Stx) Rs ?exn >>
  (define-struct (base deriv) (resolves ?1) #f)

  ;; A PrimDeriv is one of
  (define-struct (prule base) () #f)
  (define-struct (p:variable prule) () #f)

  ;;   (make-p:module <Base> Boolean ?Deriv ?exn Deriv)
  ;;   (make-p:#%module-begin <Base> ModulePass1 ModulePass2 ?exn)
  (define-struct (p:module prule) (one-body-form? mb ?2 body) #f)
  (define-struct (p:#%module-begin prule) (pass1 pass2 ?2) #f)

  ;;   (make-p:define-syntaxes <Base> DerivLL)
  ;;   (make-p:define-values <Base> Deriv)
  (define-struct (p:define-syntaxes prule) (rhs ?2) #f)
  (define-struct (p:define-values prule) (rhs) #f)

  ;;   (make-p:#%expression <Base> Deriv)
  ;;   (make-p:if <Base> Boolean Deriv Deriv Deriv)
  ;;   (make-p:wcm <Base> Deriv Deriv Deriv)
  ;;   (make-p:set! <Base> Rs Deriv)
  ;;   (make-p:set!-macro <Base> Rs Deriv)
  (define-struct (p:#%expression prule) (inner) #f)
  (define-struct (p:if prule) (full? test then else) #f)
  (define-struct (p:wcm prule) (key mark body) #f)
  (define-struct (p:set! prule) (id-resolves rhs) #f)
  (define-struct (p:set!-macro prule) (deriv) #f)

  ;;   (make-p:#%app <Base> Stx LDeriv)
  ;;   (make-p:begin <Base> LDeriv)
  ;;   (make-p:begin0 <Base> Deriv LDeriv)
  (define-struct (p:#%app prule) (tagged-stx lderiv) #f)
  (define-struct (p:begin prule) (lderiv) #f)
  (define-struct (p:begin0 prule) (first lderiv) #f)

  ;;   (make-p:lambda <Base> LambdaRenames BDeriv)
  ;;   (make-p:case-lambda <Base> (list-of CaseLambdaClause))
  ;;   (make-p:let-values <Base> LetRenames (list-of Deriv) BDeriv)
  ;;   (make-p:letrec-values <Base> LetRenames (list-of Deriv) BDeriv)
  ;;   (make-p:letrec-syntaxes+values <Base> LSVRenames (list-of BindSyntaxes) (list-of Deriv) BDeriv)
  (define-struct (p:lambda prule) (renames body) #f)
  (define-struct (p:case-lambda prule) (renames+bodies) #f)
  (define-struct (p:let-values prule) (renames rhss body) #f)
  (define-struct (p:letrec-values prule) (renames rhss body) #f)
  (define-struct (p:letrec-syntaxes+values prule) (srenames sbindrhss vrenames vrhss body) #f)

  ;;   (make-p:stop <Base>)
  ;;   (make-p:unknown <Base>)
  ;;   (make-p:#%top <Base> Stx)
  ;;   (make-p:#%datum <Base> Stx)
  ;;   (make-p:quote <Base>)
  ;;   (make-p:quote-syntax <Base>)
  ;;   (make-p:require <Base>)
  ;;   (make-p:require-for-syntax <Base>)
  ;;   (make-p:require-for-template <Base>)
  ;;   (make-p:provide <Base>)
  (define-struct (p::STOP prule) () #f)
  (define-struct (p:stop p::STOP) () #f)
  (define-struct (p:unknown p::STOP) () #f)
  (define-struct (p:#%top p::STOP) (tagged-stx) #f)
  (define-struct (p:#%datum p::STOP) (tagged-stx) #f)
  (define-struct (p:quote p::STOP) () #f)
  (define-struct (p:quote-syntax p::STOP) () #f)
  (define-struct (p:require p::STOP) () #f)
  (define-struct (p:require-for-syntax p::STOP) () #f)
  (define-struct (p:require-for-template p::STOP) () #f)
  (define-struct (p:provide p::STOP) () #f)

  ;;+  (make-p:rename <Base> Renames Deriv)
  ;;+  (make-p:synth <Base> (list-of SynthItem) ?exn)
  (define-struct (p:rename prule) (renames inner) #f)
  (define-struct (p:synth prule) (subterms ?2) #f)


  
  ;; A LDeriv is
  ;;   (make-lderiv <Node(Stxs)> ?exn (list-of Deriv))
  (define-struct (lderiv node) (?1 derivs) #f)

  ;; A BDeriv is
  ;;   (make-bderiv <Node(Stxs)> (list-of BRule) (U 'list 'letrec) LDeriv)
  (define-struct (bderiv node) (pass1 trans pass2) #f)

  ;; A BRule is one of
  ;;   (make-b:error exn)
  ;;   (make-b:expr BlockRenames Deriv)
  ;;   (make-b:splice BlockRenames Deriv ?exn Stxs ?exn)
  ;;   (make-b:defvals BlockRenames Deriv ?exn)
  ;;   (make-b:defstx BlockRenames Deriv ?exn BindSyntaxes)
  ;;i  (make-b:begin BlockRenames Deriv (list-of Deriv))
  (define-struct b:error (?1) #f)
  (define-struct brule (renames) #f)
  (define-struct (b:expr brule) (head) #f)
  (define-struct (b:splice brule) (head ?1 tail ?2) #f)
  (define-struct (b:defvals brule) (head ?1) #f)
  (define-struct (b:defstx brule) (head ?1 bindrhs) #f)
;;(define-struct (b:begin brule) (head inner) #f)

  ;; A BindSyntaxes is
  ;;   (make-bind-syntaxes DerivLL ?exn)
  (define-struct bind-syntaxes (rhs ?1) #f)


  ;; A CaseLambdaClause is
  ;;   (make-clc ?exn CaseLambdaRename BDeriv)
  (define-struct clc (?1 renames body) #f)

  ;; A BlockRename is (cons Stx Stx)

  ;; A ModPass1 is (list-of ModRule1)
  ;; A ModPass2 is (list-of ModRule2)

  ;; A ModRule1 is one of 
  ;;   (make-mod:prim Deriv ModPrim)
  ;;   (make-mod:splice Deriv ?exn Stxs)
  ;;   (make-mod:lift Deriv Stxs)
  ;;   (make-mod:lift-end Stxs)
  ;; A ModRule2 is one of
  ;;   (make-mod:skip)
  ;;   (make-mod:cons Deriv)
  ;;   (make-mod:lift Deriv Stxs)
  (define-struct modrule () #f)
  (define-struct (mod:cons modrule) (head) #f)
  (define-struct (mod:prim modrule) (head prim) #f)
  (define-struct (mod:skip modrule) () #f)
  (define-struct (mod:splice modrule) (head ?1 tail) #f)
  (define-struct (mod:lift modrule) (head tail) #f)
  (define-struct (mod:lift-end modrule) (tail) #f)

  ;; A ModPrim is a PRule in:
  ;;   (make-p:define-values <Base> #f)
  ;;   (make-p:define-syntaxes <Base> Deriv)
  ;;   (make-p:require <Base>)
  ;;   (make-p:require-for-syntax <Base>)
  ;;   (make-p:require-for-template <Base>)
  ;;   (make-p:provide <Base>)
  ;;   #f

  ;; A SynthItem is one of
  ;;   - (make-s:subterm Path Deriv)
  ;;   - (make-s:rename Path Stx Stx)
  (define-struct subitem () #f)
  (define-struct (s:subterm subitem) (path deriv) #f)
  (define-struct (s:rename subitem) (path before after) #f)


  )
