#lang racket/base

(require "../utils/utils.rkt"
         (rep type-rep filter-rep object-rep rep-utils)
         (utils tc-utils)
         "substitute.rkt" "tc-result.rkt"
         (rep free-variance) 
         (env index-env tvar-env)
         racket/match
         racket/set
         racket/list
         (contract-req)
         "tc-error.rkt")


;; Don't provide things that may be exported with a contract
(provide (except-out (all-from-out "tc-result.rkt" "tc-error.rkt")
                     tc-error/expr
                     lookup-fail
                     lookup-type-fail))


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
(define (fv t) (set->list (free-vars-names (free-vars* t))))
(define (fi t) (set->list (free-vars-names (free-idxs* t))))

;; fv/list : Listof[Type] -> Setof[Symbol]
(define (fv/list ts)
  (apply set-union (seteq) (map (compose free-vars-names free-vars*) ts)))

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
 [fv/list ((listof Type/c) . -> . (set/c symbol?))]
 [lookup-fail (identifier? . -> . Type/c)]
 [lookup-type-fail (identifier? . -> . Type/c)] 
 [current-poly-struct (parameter/c (or/c #f poly?))]
 )

