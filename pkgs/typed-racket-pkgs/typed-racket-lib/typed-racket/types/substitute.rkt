#lang racket/base

(require "../utils/utils.rkt"
         racket/match racket/set
         racket/lazy-require
         (contract-req)
         (only-in (types base-abbrev) -Tuple* -lst -Null -result ManyUniv)
         (rep type-rep rep-utils)
         (utils tc-utils)
         (rep free-variance)
         (env tvar-env))
(lazy-require ("union.rkt" (Un)))

(provide subst-all substitute substitute-dots substitute-dotted subst
         (struct-out t-subst) (struct-out i-subst)
         (struct-out i-subst/starred) (struct-out i-subst/dotted)
         make-simple-substitution)
(provide-for-cond-contract substitution/c)

(define-struct/cond-contract subst-rhs () #:transparent)
(define-struct/cond-contract (t-subst subst-rhs) ([type Type/c]) #:transparent)
(define-struct/cond-contract (i-subst subst-rhs) ([types (listof Type/c)]) #:transparent)
(define-struct/cond-contract (i-subst/starred subst-rhs) ([types (listof Type/c)] [starred Type/c]) #:transparent)
(define-struct/cond-contract (i-subst/dotted subst-rhs) ([types (listof Type/c)] [dty Type/c] [dbound symbol?]) #:transparent)

(define-for-cond-contract substitution/c (hash/c symbol? subst-rhs? #:immutable #t))
(define-for-cond-contract simple-substitution/c (hash/c symbol? Type/c #:immutable #t))

(define (subst v t e) (substitute t v e))

(define/cond-contract (make-simple-substitution vs ts)
  (([vs (listof symbol?)] [ts (listof Type/c)]) ()
   #:pre (vs ts) (= (length vs) (length ts))
    . ->i . [_ substitution/c])
  (for/hash ([v (in-list vs)] [t (in-list ts)])
    (values v (t-subst t))))

;; TODO: Figure out if free var checking/short circuiting is actually a performance improvement.

;; substitute-many : Hash[Name,Type] Type -> Type
(define/cond-contract (substitute-many subst target)
  (simple-substitution/c Type? . -> .  Type?)
  (define (sb t) (substitute-many subst t))
  (define names (hash-keys subst))
  (define fvs (free-vars* target))
  (if (ormap (lambda (name) (free-vars-has-key? fvs name)) names)
      (type-case (#:Type sb #:Filter (sub-f sb) #:Object (sub-o sb))
                 target
                 [#:Union tys (apply Un (map sb tys))]
                 [#:F name (hash-ref subst name target)]
                 [#:arr dom rng rest drest kws
                        (cond
                          [(and (pair? drest)
                                (ormap (λ (name)
                                          (and (equal? name (cdr drest))
                                               (not (bound-tvar? name))
                                               name))
                                       names))
                           =>
                           (lambda (name)
                             (int-err "substitute used on ... variable ~a in type ~a" name target))]
                          [else
                           (make-arr (map sb dom)
                                     (sb rng)
                                     (and rest (sb rest))
                                     (and drest (cons (sb (car drest)) (cdr drest)))
                                     (map sb kws))])]
                 [#:ValuesDots types dty dbound
                               (cond
                                 [(ormap (lambda (x) (and (equal? dbound x) (not (bound-tvar? x)))) names) =>
                                  (lambda (name)
                                    (int-err "substitute used on ... variable ~a in type ~a" name target))]
                                 [else (make-ValuesDots (map sb types) (sb dty) dbound)])]
                 [#:ListDots dty dbound
                             (cond
                               [(ormap (lambda (x) (and (equal? dbound x) (not (bound-tvar? x)))) names) =>
                                (lambda (name)
                                  (int-err "substitute used on ... variable ~a in type ~a" name target))]
                               [else (make-ListDots (sb dty) dbound)])])
      target))


;; substitute : Type Name Type -> Type
(define/cond-contract (substitute image name target)
  (Type/c symbol? Type? . -> . Type?)
  (substitute-many (hash name image) target))

;; implements angle bracket substitution from the formalism
;; substitute-dots : Listof[Type] Option[type] Name Type -> Type
(define/cond-contract (substitute-dots images rimage name target)
  ((listof Type/c) (or/c #f Type/c) symbol? Type?  . -> . Type?)
  (define (sb t) (substitute-dots images rimage name t))
  (if (or (set-member? (free-vars-names (free-idxs* target)) name)
          (set-member? (free-vars-names (free-vars* target)) name))
      (type-case (#:Type sb #:Filter (sub-f sb)) target
                 [#:ListDots dty dbound
                             (if (eq? name dbound)
                                 ;; We need to recur first, just to expand out any dotted usages of this.
                                 (let ([expanded (sb dty)])
                                   (for/fold ([t (if rimage (-lst rimage) -Null)])
                                     ([img (in-list (reverse images))])
                                     (make-Pair (substitute img name expanded) t)))
                                 (make-ListDots (sb dty) dbound))]
                 [#:ValuesDots types dty dbound
                               (if (eq? name dbound)
                                   (if rimage
                                       ManyUniv
                                       (make-Values
                                        (append
                                         (map sb types)
                                         ;; We need to recur first, just to expand out any dotted usages of this.
                                         (let ([expanded (sb dty)])
                                           (for/list ([img (in-list images)])
                                             (-result (substitute img name expanded)))))))
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

;; implements curly brace substitution from the formalism, with the addition
;; that a substitution can include fixed args in addition to a different dotted arg
;; substitute-dotted : Listof[Type] Type Name Name Type -> Type
(define (substitute-dotted pre-image image image-bound name target)
  (define (sb t) (substitute-dotted pre-image image image-bound name t))
  ;; We do a quick check on the free variables to see if we can short circuit the substitution
  (if (or (set-member? (free-vars-names (free-idxs* target)) name)
          (set-member? (free-vars-names (free-vars* target)) name))
      (type-case (#:Type sb #:Filter (sub-f sb))
                 target
                 [#:ValuesDots types dty dbound
                               (let ([extra-types (if (eq? name dbound) pre-image null)])
                                 (make-ValuesDots (append (map sb types) (map -result extra-types))
                                                  (sb dty)
                                                  (if (eq? name dbound) image-bound dbound)))]
                 [#:ListDots dty dbound
                             (-Tuple*
                               (if (eq? name dbound) pre-image null)
                               (make-ListDots (sb dty)
                                              (if (eq? name dbound) image-bound dbound)))]
                 [#:F name*
                      (if (eq? name* name)
                          image
                          target)]
                 [#:arr dom rng rest drest kws
                        (let ([extra-types (if (and drest (eq? name (cdr drest))) pre-image null)])
                          (make-arr (append (map sb dom) extra-types)
                                    (sb rng)
                                    (and rest (sb rest))
                                    (and drest
                                         (cons (substitute image (cdr drest) (sb (car drest)))
                                               (if (eq? name (cdr drest)) image-bound (cdr drest))))
                                    (map sb kws)))])
       target))

;; substitute many variables
;; subst-all : substitution/c Type -> Type
(define/cond-contract (subst-all s ty)
  (substitution/c Type? . -> . Type?)

  (define t-substs
    (for/fold ([acc (hash)]) ([(v r) (in-hash s)])
      (match r
        [(t-subst img)
         (hash-set acc v img)]
        [_ acc])))
  (define t-substed-ty (substitute-many t-substs ty))

  (for/fold ([t t-substed-ty]) ([(v r) (in-hash s)])
    (match r
      [(t-subst img) t]
      [(i-subst imgs)
       (substitute-dots imgs #f v t)]
      [(i-subst/starred imgs rest)
       (substitute-dots imgs rest v t)]
      [(i-subst/dotted imgs dty dbound)
       (substitute-dotted imgs dty dbound v t)])))
