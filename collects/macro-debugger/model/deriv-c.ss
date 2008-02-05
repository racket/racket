
#lang scheme/base
(provide (all-defined-out))

;; A Node(a) is:
;;   (make-node a ?a)
(define-struct node (z1 z2) #:transparent)

;; A TopDeriv is one of
;;   (make-lift-deriv <Node(Stx)> Deriv Stxs TopDeriv)
;;   Deriv

;; A Deriv is one of
;;   (make-mrule <Node(Stx)> Transformation Deriv)
;;   PrimDeriv
(define-struct (deriv node) () #:transparent)
(define-struct (lift-deriv deriv) (first lift-stx second) #:transparent)
(define-struct (mrule deriv) (transformation next) #:transparent)

;; A DerivLL is one of
;;   (make-lift/let-deriv <Node(Stx)> Deriv Stx Deriv)
;;   Deriv
(define-struct (lift/let-deriv deriv) (first lift-stx second) #:transparent)

;; A Transformation is
;;   (make-transformation <Node(Stx)> Rs ?exn ?Stx (list-of LocalAction) ?exn ?Stx Number)
(define-struct (transformation node) (resolves ?1 me1 locals ?2 me2 seq) #:transparent)

;; A LocalAction is one of
;;   (make-local-expansion <Node(Stx)> Stx ?Stx Boolean Deriv)
;;   (make-local-expansion/expr <Node(Stx)> Stx ?Stx Boolean ?Opaque Deriv)
;;   (make-local-lift Stx Identifier)
;;   (make-local-lift-end Stx)
;;   (make-local-bind BindSyntaxes)
(define-struct (local-expansion node) (me1 me2 for-stx? inner) #:transparent)
(define-struct (local-expansion/expr node) (me1 me2 for-stx? opaque inner) #:transparent)
(define-struct local-lift (expr id) #:transparent)
(define-struct local-lift-end (decl) #:transparent)
(define-struct local-bind (bindrhs) #:transparent)

;; Base = << Node(Stx) Rs ?exn >>
(define-struct (base deriv) (resolves ?1) #:transparent)

;; A PrimDeriv is one of
(define-struct (prule base) () #:transparent)
(define-struct (p:variable prule) () #:transparent)

;;   (make-p:module <Base> Boolean ?Deriv ?exn Deriv)
;;   (make-p:#%module-begin <Base> ModulePass1 ModulePass2 ?exn)
(define-struct (p:module prule) (one-body-form? mb ?2 body) #:transparent)
(define-struct (p:#%module-begin prule) (pass1 pass2 ?2) #:transparent)

;;   (make-p:define-syntaxes <Base> DerivLL)
;;   (make-p:define-values <Base> Deriv)
(define-struct (p:define-syntaxes prule) (rhs ?2) #:transparent)
(define-struct (p:define-values prule) (rhs) #:transparent)

;;   (make-p:#%expression <Base> Deriv)
;;   (make-p:if <Base> Boolean Deriv Deriv Deriv)
;;   (make-p:wcm <Base> Deriv Deriv Deriv)
;;   (make-p:set! <Base> Rs Deriv)
;;   (make-p:set!-macro <Base> Rs Deriv)
(define-struct (p:#%expression prule) (inner) #:transparent)
(define-struct (p:if prule) (full? test then else) #:transparent)
(define-struct (p:wcm prule) (key mark body) #:transparent)
(define-struct (p:set! prule) (id-resolves rhs) #:transparent)
(define-struct (p:set!-macro prule) (deriv) #:transparent)

;;   (make-p:#%app <Base> Stx LDeriv)
;;   (make-p:begin <Base> LDeriv)
;;   (make-p:begin0 <Base> Deriv LDeriv)
(define-struct (p:#%app prule) (tagged-stx lderiv) #:transparent)
(define-struct (p:begin prule) (lderiv) #:transparent)
(define-struct (p:begin0 prule) (first lderiv) #:transparent)

;;   (make-p:lambda <Base> LambdaRenames BDeriv)
;;   (make-p:case-lambda <Base> (list-of CaseLambdaClause))
;;   (make-p:let-values <Base> LetRenames (list-of Deriv) BDeriv)
;;   (make-p:letrec-values <Base> LetRenames (list-of Deriv) BDeriv)
;;   (make-p:letrec-syntaxes+values <Base> LSVRenames (list-of BindSyntaxes) (list-of Deriv) BDeriv)
(define-struct (p:lambda prule) (renames body) #:transparent)
(define-struct (p:case-lambda prule) (renames+bodies) #:transparent)
(define-struct (p:let-values prule) (renames rhss body) #:transparent)
(define-struct (p:letrec-values prule) (renames rhss body) #:transparent)
(define-struct (p:letrec-syntaxes+values prule) (srenames sbindrhss vrenames vrhss body) #:transparent)

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
(define-struct (p::STOP prule) () #:transparent)
(define-struct (p:stop p::STOP) () #:transparent)
(define-struct (p:unknown p::STOP) () #:transparent)
(define-struct (p:#%top p::STOP) (tagged-stx) #:transparent)
(define-struct (p:#%datum p::STOP) (tagged-stx) #:transparent)
(define-struct (p:quote p::STOP) () #:transparent)
(define-struct (p:quote-syntax p::STOP) () #:transparent)
(define-struct (p:require p::STOP) () #:transparent)
(define-struct (p:require-for-syntax p::STOP) () #:transparent)
(define-struct (p:require-for-template p::STOP) () #:transparent)
(define-struct (p:provide p::STOP) () #:transparent)

;;+  (make-p:rename <Base> Renames Deriv)
;;+  (make-p:synth <Base> (list-of SynthItem) ?exn)
(define-struct (p:rename prule) (renames inner) #:transparent)
(define-struct (p:synth prule) (subterms ?2) #:transparent)



;; A LDeriv is
;;   (make-lderiv <Node(Stxs)> ?exn (list-of Deriv))
(define-struct (lderiv node) (?1 derivs) #:transparent)

;; A BDeriv is
;;   (make-bderiv <Node(Stxs)> (list-of BRule) (U 'list 'letrec) LDeriv)
(define-struct (bderiv node) (pass1 trans pass2) #:transparent)

;; A BRule is one of
;;   (make-b:error exn)
;;   (make-b:expr BlockRenames Deriv)
;;   (make-b:splice BlockRenames Deriv ?exn Stxs ?exn)
;;   (make-b:defvals BlockRenames Deriv ?exn)
;;   (make-b:defstx BlockRenames Deriv ?exn BindSyntaxes)
;;i  (make-b:begin BlockRenames Deriv (list-of Deriv))
(define-struct b:error (?1) #:transparent)
(define-struct brule (renames) #:transparent)
(define-struct (b:expr brule) (head) #:transparent)
(define-struct (b:splice brule) (head ?1 tail ?2) #:transparent)
(define-struct (b:defvals brule) (head ?1) #:transparent)
(define-struct (b:defstx brule) (head ?1 bindrhs) #:transparent)
;;(define-struct (b:begin brule) (head inner) #:transparent)

;; A BindSyntaxes is
;;   (make-bind-syntaxes DerivLL ?exn)
(define-struct bind-syntaxes (rhs ?1) #:transparent)


;; A CaseLambdaClause is
;;   (make-clc ?exn CaseLambdaRename BDeriv)
(define-struct clc (?1 renames body) #:transparent)

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
(define-struct modrule () #:transparent)
(define-struct (mod:cons modrule) (head) #:transparent)
(define-struct (mod:prim modrule) (head prim) #:transparent)
(define-struct (mod:skip modrule) () #:transparent)
(define-struct (mod:splice modrule) (head ?1 tail) #:transparent)
(define-struct (mod:lift modrule) (head tail) #:transparent)
(define-struct (mod:lift-end modrule) (tail) #:transparent)

;; A ModPrim is a PRule in:
;;   (make-p:define-values <Base> #:transparent)
;;   (make-p:define-syntaxes <Base> Deriv)
;;   (make-p:require <Base>)
;;   (make-p:require-for-syntax <Base>)
;;   (make-p:require-for-template <Base>)
;;   (make-p:provide <Base>)
;;   #f

;; A SynthItem is one of
;;   - (make-s:subterm Path Deriv)
;;   - (make-s:rename Path Stx Stx)
(define-struct subitem () #:transparent)
(define-struct (s:subterm subitem) (path deriv) #:transparent)
(define-struct (s:rename subitem) (path before after) #:transparent)
