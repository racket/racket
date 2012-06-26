#lang racket/base

(require "../utils/utils.rkt"
         (rep type-rep filter-rep object-rep rep-utils)
         (utils tc-utils)
         "substitute.rkt" "tc-result.rkt"
         (only-in (rep free-variance) combine-frees)
         (env index-env tvar-env)
         racket/match
         racket/list
         racket/contract)


(provide (all-from-out "tc-result.rkt"))


;; unfold : Type -> Type
;; must be applied to a Mu
(define (unfold t)
  (match t
    [(Mu: name b)
     (substitute t name b #:Un (lambda (tys)
                                 (make-Union (sort tys < #:key Type-seq))))]
    [_ (int-err "unfold: requires Mu type, got ~a" t)]))

(define (instantiate-poly t types)
  (match t
    [(Poly: ns body)
     (unless (= (length types) (length ns))
       (int-err "instantiate-poly: wrong number of types: expected ~a, got ~a"
                (length ns) (length types)))
     (subst-all (make-simple-substitution ns types) body)]
    [(PolyDots: (list fixed ... dotted) body)
     (unless (>= (length types) (length fixed))
       (int-err
        "instantiate-poly: wrong number of types: expected at least ~a, got ~a"
        (length fixed) (length types)))
     (let* ([fixed-tys (take types (length fixed))]
            [rest-tys (drop types (length fixed))]
            [body* (subst-all (make-simple-substitution fixed fixed-tys)
                              body)])
       (substitute-dots rest-tys #f dotted body*))]
    [_ (int-err "instantiate-poly: requires Poly type, got ~a" t)]))

(define (instantiate-poly-dotted t types image var)
  (match t
    [(PolyDots: (list fixed ... dotted) body)
     (unless (= (length fixed) (length types))
       (int-err (string-append "instantiate-poly-dotted: wrong number of"
                               " types: expected ~a, got ~a, types were ~a")
                (length fixed) (length types) types))
     (let ([body* (subst-all (make-simple-substitution fixed types) body)])
       (substitute-dotted image var dotted body*))]
    [_ (int-err "instantiate-poly-dotted: requires PolyDots type, got ~a" t)]))


;; fv : Type -> Listof[Symbol]
(define (fv t) (hash-map (free-vars* t) (lambda (k v) k)))
(define (fi t) (for/list ([(k v) (in-hash (free-idxs* t))]) k))

;; fv/list : Listof[Type] -> Listof[Symbol]
(define (fv/list ts)
  (hash-map (combine-frees (map free-vars* ts)) (lambda (k v) k)))

(define (tc-error/expr msg
                       #:return [return (make-Union null)]
                       #:stx [stx (current-orig-stx)]
                       . rest)
  (apply tc-error/delayed #:stx stx msg rest)
  return)

;; error for unbound variables
(define (lookup-fail e)
  (match (identifier-binding e)
    ['lexical (tc-error/expr "untyped identifier ~a" (syntax-e e))]
    [#f (tc-error/expr "untyped top-level identifier ~a" (syntax-e e))]
    [(list _ _ nominal-source-mod nominal-source-id _ _ _)
     (let-values ([(x y) (module-path-index-split nominal-source-mod)])
       (cond [(and (not x) (not y))
              (tc-error/expr "untyped identifier ~a" (syntax-e e))]
             [else
              (tc-error/expr "untyped identifier ~a imported from module <~a>"
                             (syntax-e e) x)]))]))

(define (lookup-type-fail i)
  (tc-error/expr "~a is not bound as a type" (syntax-e i)))

;; a parameter for the current polymorphic structure being defined
;; to allow us to prevent non-regular datatypes
(define-struct poly (name vars) #:prefab)
(define current-poly-struct (make-parameter #f))

;; UNUSED
;; a table indicating what variables should be abstracted away before using
;; this expected type keyed on the numeric Rep sequence
(define to-be-abstr
  (make-weak-hash))

(provide to-be-abstr)


(provide/cond-contract
 [unfold (Mu? . -> . Type/c)]
 [instantiate-poly ((or/c Poly? PolyDots?) (listof Type/c) . -> . Type/c)]
 [instantiate-poly-dotted
  (PolyDots? (listof Type/c) Type/c symbol? . -> . Type/c)] 
 [tc-error/expr ((string?) (#:return any/c #:stx syntax?) #:rest (listof any/c)
                 . ->* . any/c)]
 [fv (Rep? . -> . (listof symbol?))]
 [fi (Rep? . -> . (listof symbol?))]
 [fv/list ((listof Type/c) . -> . (listof symbol?))]
 [lookup-fail (identifier? . -> . Type/c)]
 [lookup-type-fail (identifier? . -> . Type/c)] 
 [current-poly-struct (parameter/c (or/c #f poly?))]
 )

