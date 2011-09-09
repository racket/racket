#lang racket/base

(require "../utils/utils.rkt"
         (rep type-rep filter-rep object-rep rep-utils)
         (utils tc-utils)
         "substitute.rkt"
         (only-in (rep free-variance) combine-frees)
         (env index-env tvar-env)
         racket/match
         racket/list
         racket/contract
         (for-syntax racket/base syntax/parse))


(provide effects-equal?) ;;Never Used


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


;; this structure represents the result of typechecking an expression
(define-struct/cond-contract tc-result
  ([t Type/c] [f FilterSet/c] [o Object?])
  #:transparent)
(define-struct/cond-contract tc-results
  ([ts (listof tc-result?)] [drest (or/c (cons/c Type/c symbol?) #f)])
  #:transparent)

(define-match-expander tc-result:
  (syntax-parser
   [(_ tp fp op) #'(struct tc-result (tp fp op))]
   [(_ tp) #'(struct tc-result (tp _ _))]))

(define-match-expander tc-results:
  (syntax-parser
   [(_ tp fp op)
    #'(struct tc-results ((list (struct tc-result (tp fp op)) (... ...))
                          #f))]
   [(_ tp fp op dty dbound)
    #'(struct tc-results ((list (struct tc-result (tp fp op)) (... ...))
                          (cons dty dbound)))]
   [(_ tp)
    #'(struct tc-results ((list (struct tc-result (tp _ _)) (... ...))
                          #f))]))

(define-match-expander tc-result1:
  (syntax-parser
   [(_ tp fp op) #'(struct tc-results ((list (struct tc-result (tp fp op)))
                                       #f))]
   [(_ tp) #'(struct tc-results ((list (struct tc-result (tp _ _)))
                                 #f))]))

(define (tc-results-t tc)
  (match tc
    [(tc-results: t) t]))

(provide tc-result: tc-results: tc-result1: Result1: Results:)

(define-match-expander Result1:
  (syntax-parser
   [(_ tp) #'(Values: (list (Result: tp _ _)))]
   [(_ tp fp op) #'(Values: (list (Result: tp fp op)))]))

(define-match-expander Results:
  (syntax-parser
   [(_ tp) #'(Values: (list (Result: tp _ _) (... ...)))]
   [(_ tp fp op) #'(Values: (list (Result: tp fp op) (... ...)))]))

;; convenience function for returning the result of typechecking an expression
(define ret
  (case-lambda [(t)
                (let ([mk (lambda (t) (make-FilterSet (make-Top) (make-Top)))])
                  (make-tc-results
                   (cond [(Type? t)
                          (list (make-tc-result t (mk t) (make-Empty)))]
                         [else
                          (for/list ([i t])
                            (make-tc-result i (mk t) (make-Empty)))])
                   #f))]
               [(t f)
                (make-tc-results
                 (if (Type? t)
                     (list (make-tc-result t f (make-Empty)))
                     (for/list ([i t] [f f])
                               (make-tc-result i f (make-Empty))))
                 #f)]
               [(t f o)
                (make-tc-results
                 (if (and (list? t) (list? f) (list? o))
                     (map make-tc-result t f o)
                     (list (make-tc-result t f o)))
                 #f)]
               [(t f o dty)
                (int-err "ret used with dty without dbound")]
               [(t f o dty dbound)
                (make-tc-results
                 (if (and (list? t) (list? f) (list? o))
                     (map make-tc-result t f o)
                     (list (make-tc-result t f o)))
                 (cons dty dbound))]))

;(trace ret)

(provide/cond-contract
 [ret
  (->i ([t (or/c Type/c (listof Type/c))])
       ([f (t) (if (list? t)
                   (listof FilterSet/c)
                   FilterSet/c)]
        [o (t) (if (list? t)
                   (listof Object?)
                   Object?)]
        [dty Type/c]
        [dbound symbol?])
       [res tc-results?])])

(define (combine-results tcs)
  (match tcs
    [(list (tc-result1: t f o) ...)
     (ret t f o)]))


;; type comparison

;; equality - good

(define tc-result-equal? equal?)
(define (effects-equal? fs1 fs2)
  (and
   (= (length fs1) (length fs2))
   (andmap eq? fs1 fs2)))


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
 [tc-result? (any/c . -> . boolean?)]
 [tc-result-t (tc-result? . -> . Type/c)]
 [tc-result-equal? (tc-result? tc-result? . -> . boolean?)]
 [tc-results? (any/c . -> . boolean?)]
 [tc-error/expr ((string?) (#:return any/c #:stx syntax?) #:rest (listof any/c)
                 . ->* . any/c)]

 [fv (Rep? . -> . (listof symbol?))]
 [fi (Rep? . -> . (listof symbol?))]
 [fv/list ((listof Type/c) . -> . (listof symbol?))]
 [lookup-fail (identifier? . -> . Type/c)]
 [lookup-type-fail (identifier? . -> . Type/c)]
 [combine-results ((listof tc-results?) . -> . tc-results?)]
 [current-poly-struct (parameter/c (or/c #f poly?))]

 )

