#lang racket/base

(require "../utils/utils.rkt"
         (rep type-rep filter-rep object-rep rep-utils)
         (utils tc-utils)
         (only-in (rep free-variance) combine-frees)
         (env index-env tvar-env)
         racket/match
         scheme/contract)

(provide subst-all substitute substitute-dots substitute-dotted subst
         (struct-out t-subst) (struct-out i-subst) (struct-out i-subst/starred) (struct-out i-subst/dotted)
         substitution/c make-simple-substitution)

(define-struct/cond-contract subst-rhs () #:transparent)
(define-struct/cond-contract (t-subst subst-rhs) ([type Type/c]) #:transparent)
(define-struct/cond-contract (i-subst subst-rhs) ([types (listof Type/c)]) #:transparent)
(define-struct/cond-contract (i-subst/starred subst-rhs) ([types (listof Type/c)] [starred Type/c]) #:transparent)
(define-struct/cond-contract (i-subst/dotted subst-rhs) ([types (listof Type/c)] [dty Type/c] [dbound symbol?]) #:transparent)

(define substitution/c (hash/c symbol? subst-rhs? #:immutable #t))

(define (subst v t e) (substitute t v e))

(define/cond-contract (make-simple-substitution vs ts)
  (([vs (listof symbol?)] [ts (listof Type/c)]) ()
   #:pre (vs ts) (= (length vs) (length ts))
    . ->i . [_ substitution/c])
  (for/hash ([v (in-list vs)] [t (in-list ts)])
    (values v (t-subst t))))


;; substitute : Type Name Type -> Type
(define/cond-contract (substitute image name target #:Un [Un (get-union-maker)])
  ((Type/c symbol? Type?) (#:Un procedure?) . ->* . Type?)
  (define (sb t) (substitute image name t))
  (if (hash-ref (free-vars* target) name #f)
      (type-case (#:Type sb #:Filter (sub-f sb) #:Object (sub-o sb))
                 target
                 [#:Union tys (Un (map sb tys))]
                 [#:F name* (if (eq? name* name) image target)]
                 [#:arr dom rng rest drest kws
                        (begin
                          (when (and (pair? drest)
                                     (eq? name (cdr drest))
                                     (not (bound-tvar? name)))
                            (int-err "substitute used on ... variable ~a in type ~a" name target))
                          (make-arr (map sb dom)
                                    (sb rng)
                                    (and rest (sb rest))
                                    (and drest (cons (sb (car drest)) (cdr drest)))
                                    (map sb kws)))]
                 [#:ValuesDots types dty dbound
                               (begin
                                 (when (and (eq? name dbound) (not (bound-tvar? name)))
                                   (int-err "substitute used on ... variable ~a in type ~a" name target))
                                 (make-ValuesDots (map sb types) (sb dty) dbound))]
                 [#:ListDots dty dbound
                             (begin
                               (when (and (eq? name dbound) (not (bound-tvar? name)))
                                 (int-err "substitute used on ... variable ~a in type ~a" name target))
                               (make-ListDots (sb dty) dbound))])
      target))

;; implements angle bracket substitution from the formalism
;; substitute-dots : Listof[Type] Option[type] Name Type -> Type
(define/cond-contract (substitute-dots images rimage name target)
  ((listof Type/c) (or/c #f Type/c) symbol? Type? . -> . Type?)
  (define (sb t) (substitute-dots images rimage name t))
  (if (or (hash-ref (free-idxs* target) name #f) (hash-ref (free-vars* target) name #f))
      (type-case (#:Type sb #:Filter (sub-f sb)) target
                 [#:ListDots dty dbound
                             (if (eq? name dbound)
                                 ;; We need to recur first, just to expand out any dotted usages of this.
                                 (let ([expanded (sb dty)])
                                   (for/fold ([t (make-Value null)])
                                     ([img (reverse images)])
                                     (make-Pair (substitute img name expanded) t)))
                                 (make-ListDots (sb dty) dbound))]
                 [#:ValuesDots types dty dbound
                               (if (eq? name dbound)
                                   (make-Values
                                    (append
                                     (map sb types)
                                     ;; We need to recur first, just to expand out any dotted usages of this.
                                     (let ([expanded (sb dty)])
                                       (for/list ([img images])
                                         (make-Result
                                          (substitute img name expanded)
                                          (make-FilterSet (make-Top) (make-Top))
                                          (make-Empty))))))
                                   (make-ValuesDots (map sb types) (sb dty) dbound))]
                 [#:arr dom rng rest drest kws
                        (if (and (pair? drest)
                                 (eq? name (cdr drest)))
                            (make-arr (append
                                       (map sb dom)
                                       ;; We need to recur first, just to expand out any dotted usages of this.
                                       (let ([expanded (sb (car drest))])
                                         (map (lambda (img) (substitute img name expanded)) images)))
                                      (sb rng)
                                      rimage
                                      #f
                                      (map sb kws))
                            (make-arr (map sb dom)
                                      (sb rng)
                                      (and rest (sb rest))
                                      (and drest (cons (sb (car drest)) (cdr drest)))
                                      (map sb kws)))])
      target))

;; implements curly brace substitution from the formalism
;; substitute-dotted : Type Name Name Type -> Type
(define (substitute-dotted image image-bound name target)
  (define (sb t) (substitute-dotted image image-bound name t))
  (if (hash-ref (free-idxs* target) name #f)
      (type-case (#:Type sb #:Filter (sub-f sb))
                 target
                 [#:ValuesDots types dty dbound
                               (make-ValuesDots (map sb types)
                                                (sb dty)
                                                (if (eq? name dbound) image-bound dbound))]
                 [#:ListDots dty dbound
                             (make-ListDots (sb dty)
                                            (if (eq? name dbound) image-bound dbound))]
                 [#:F name*
                      (if (eq? name* name)
                          image
                          target)]
                 [#:arr dom rng rest drest kws
                        (make-arr (map sb dom)
                                  (sb rng)
                                  (and rest (sb rest))
                                  (and drest
                                       (cons (substitute image (cdr drest) (sb (car drest)))
                                             (if (eq? name (cdr drest)) image-bound (cdr drest))))
                                  (map sb kws))])
       target))

;; substitute many variables
;; substitution = Listof[U List[Name,Type] List[Name,Listof[Type]]]
;; subst-all : substitution Type -> Type
(define/cond-contract (subst-all s t)
  (substitution/c Type? . -> . Type?)
  (for/fold ([t t]) ([(v r) (in-hash s)])
    (match r
      [(t-subst img)
       (substitute img v t)]
      [(i-subst imgs)
       (substitute-dots imgs #f v t)]
      [(i-subst/starred imgs rest)
       (substitute-dots imgs rest v t)]
      [(i-subst/dotted null dty dbound)
       (substitute-dotted dty dbound v t)]
      [(i-subst/dotted imgs dty dbound)
       (int-err "i-subst/dotted nyi")
       #;
       (substitute-dotted imgs rest v t)])))
