
#lang scheme/base
(provide (all-defined-out))

;; A Node(a) is:
;;   (make-node a ?a)
(define-struct node (z1 z2) #:transparent)

;; A TopDeriv is one of
;;   (make-lift-deriv <Node(Stx)> Deriv Stxs TopDeriv)
;;   Deriv

;; A Deriv is one of
;;   MRule
;;   PrimDeriv

;; Base = << Node(Stx) Rs ?exn >>

(define-struct (deriv node) () #:transparent)
(define-struct (base deriv) (resolves ?1) #:transparent)

(define-struct (lift-deriv deriv) (first lift-stx second) #:transparent)
(define-struct (tagrule deriv) (tagged-stx next) #:transparent)

;; A DerivLL is one of
;;   (make-lift/let-deriv <Node(Stx)> Deriv Stx Deriv)
;;   Deriv
(define-struct (lift/let-deriv deriv) (first lift-stx second) #:transparent)

;; A MRule is
;;   (make-mrule <Base(Stx)> ?Stx (listof LocalAction) ?exn ?Stx ?Deriv)
(define-struct (mrule base) (me1 locals me2 ?2 etx next) #:transparent)

;; A LocalAction is one of:
(define-struct local-exn (exn) #:transparent)
(define-struct (local-expansion node) (for-stx? me1 inner lifted me2 opaque)
  #:transparent)
(define-struct local-lift (expr ids) #:transparent)
(define-struct local-lift-end (decl) #:transparent)
(define-struct local-lift-require (req expr mexpr) #:transparent)
(define-struct local-lift-provide (prov) #:transparent)
(define-struct local-bind (names ?1 renames bindrhs) #:transparent)

;; A PrimDeriv is one of
(define-struct (prule base) () #:transparent)
(define-struct (p:variable prule) () #:transparent)

;;   (make-p:module <Base> (listof LocalAction) ?stx stx ?Deriv ?stx ?exn Deriv ?stx)
;;   (make-p:#%module-begin <Base> Stx ModulePass1 ModulePass2 ?exn)
(define-struct (p:module prule) (locals tag rename check tag2 ?3 body shift)
  #:transparent)
(define-struct (p:#%module-begin prule) (me pass1 pass2 ?2) #:transparent)

;;   (make-p:define-syntaxes <Base> DerivLL (listof LocalAction))
;;   (make-p:define-values <Base> Deriv)
(define-struct (p:define-syntaxes prule) (rhs locals) #:transparent)
(define-struct (p:define-values prule) (rhs) #:transparent)

;;   (make-p:#%expression <Base> Deriv ?Stx)
;;   (make-p:if <Base> Boolean Deriv Deriv Deriv)
;;   (make-p:wcm <Base> Deriv Deriv Deriv)
;;   (make-p:set! <Base> Rs ?Exn Deriv)
;;   (make-p:set!-macro <Base> Rs Deriv)
(define-struct (p:#%expression prule) (inner untag) #:transparent)
(define-struct (p:if prule) (test then else) #:transparent)
(define-struct (p:wcm prule) (key mark body) #:transparent)
(define-struct (p:set! prule) (id-resolves ?2 rhs) #:transparent)
(define-struct (p:set!-macro prule) (deriv) #:transparent)

;;   (make-p:#%app <Base> Stx LDeriv)
;;   (make-p:begin <Base> LDeriv)
;;   (make-p:begin0 <Base> Deriv LDeriv)
(define-struct (p:#%app prule) (lderiv) #:transparent)
(define-struct (p:begin prule) (lderiv) #:transparent)
(define-struct (p:begin0 prule) (first lderiv) #:transparent)

;;   (make-p:lambda <Base> LambdaRenames BDeriv)
;;   (make-p:case-lambda <Base> (list-of CaseLambdaClause))
;;   (make-p:let-values <Base> LetRenames (list-of Deriv) BDeriv)
;;   (make-p:letrec-values <Base> LetRenames (list-of Deriv) BDeriv)
;;   (make-p:letrec-syntaxes+values <Base> LSVRenames (list-of BindSyntaxes) (list-of Deriv) BDeriv ?Stx)
(define-struct (p:lambda prule) (renames body) #:transparent)
(define-struct (p:case-lambda prule) (renames+bodies) #:transparent)
(define-struct (p:let-values prule) (renames rhss body) #:transparent)
(define-struct (p:letrec-values prule) (renames rhss body) #:transparent)
(define-struct (p:letrec-syntaxes+values prule)
  (srenames sbindrhss vrenames vrhss body tag)
  #:transparent)

;;   (make-p:provide <Base> (listof Deriv) ?exn)
(define-struct (p:provide prule) (inners ?2) #:transparent)

;;   (make-p:require <Base> (listof LocalAction))
(define-struct (p:require prule) (locals) #:transparent)

;;   (make-p:stop <Base>)
;;   (make-p:unknown <Base>)
;;   (make-p:#%top <Base> Stx)
;;   (make-p:#%datum <Base> Stx)
;;   (make-p:quote <Base>)
;;   (make-p:quote-syntax <Base>)
;;   (make-p:#%variable-reference <Base>)
(define-struct (p::STOP prule) () #:transparent)
(define-struct (p:stop p::STOP) () #:transparent)
(define-struct (p:unknown p::STOP) () #:transparent)
(define-struct (p:#%top p::STOP) () #:transparent)
(define-struct (p:#%datum p::STOP) () #:transparent)
(define-struct (p:quote p::STOP) () #:transparent)
(define-struct (p:quote-syntax p::STOP) () #:transparent)
(define-struct (p:#%variable-reference p::STOP) () #:transparent)

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
;;   (make-b:defvals BlockRenames Deriv ?exn Stx ?exn)
;;   (make-b:defstx BlockRenames Deriv ?exn Stx ?exn BindSyntaxes)
(define-struct b:error (?1) #:transparent)
(define-struct brule (renames) #:transparent)
(define-struct (b:expr brule) (head) #:transparent)
(define-struct (b:splice brule) (head ?1 tail ?2) #:transparent)
(define-struct (b:defvals brule) (head ?1 rename ?2) #:transparent)
(define-struct (b:defstx brule) (head ?1 rename ?2 bindrhs) #:transparent)

;; A BindSyntaxes is
;;   (make-bind-syntaxes DerivLL (listof LocalAction))
(define-struct bind-syntaxes (rhs locals) #:transparent)

;; A CaseLambdaClause is
;;   (make-clc ?exn CaseLambdaRename BDeriv)
(define-struct clc (?1 renames body) #:transparent)

;; A BlockRename is (cons Stx Stx)

;; A ModPass1 is (list-of ModRule1)
;; A ModPass2 is (list-of ModRule2)

;; A ModRule1 is one of 
;;   (make-mod:prim Deriv Stx ModPrim)
;;   (make-mod:splice Deriv Stx ?exn Stxs)
;;   (make-mod:lift Deriv ?Stxs Stxs)
;;   (make-mod:lift-end Stxs)
;; A ModRule2 is one of
;;   (make-mod:skip)
;;   (make-mod:cons Deriv)
;;   (make-mod:lift Deriv Stxs)
(define-struct modrule () #:transparent)
(define-struct (mod:prim modrule) (head rename prim) #:transparent)
(define-struct (mod:splice modrule) (head rename ?1 tail) #:transparent)
(define-struct (mod:lift modrule) (head renames tail) #:transparent)
(define-struct (mod:lift-end modrule) (tail) #:transparent)
(define-struct (mod:cons modrule) (head) #:transparent)
(define-struct (mod:skip modrule) () #:transparent)

;; A ModPrim is a PRule in:
;;   (make-p:define-values <Base> #:transparent)
;;   (make-p:define-syntaxes <Base> Deriv)
;;   (make-p:require <Base> (listof LocalAction))
;;   (make-p:provide <Base>)
;;   #f


;; ECTE represents expand/compile-time-evals
;; (make-ecte stx ?stx (listof LocalAction) Deriv Deriv (listof LocalAction))

(define-struct (ecte deriv) (locals first second locals2) #:transparent)
