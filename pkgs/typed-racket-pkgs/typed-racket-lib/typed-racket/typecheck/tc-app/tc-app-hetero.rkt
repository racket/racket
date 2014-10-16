#lang racket/unit

(require "../../utils/utils.rkt"
         syntax/parse syntax/stx racket/match unstable/sequence unstable/syntax
         "signatures.rkt"
         "utils.rkt"
         (utils tc-utils)
         (types utils abbrev numeric-tower union resolve type-table generalize)
         (typecheck signatures check-below)
         (rep type-rep rep-utils)
         (for-label racket/unsafe/ops racket/base))

(import tc-expr^ tc-app^ tc-literal^)
(export tc-app-hetero^)

(define-literal-set hetero-literals
  #:for-label
  (vector-ref unsafe-vector-ref unsafe-vector*-ref
   vector-set! unsafe-vector-set! unsafe-vector*-set!
   unsafe-struct-ref unsafe-struct*-ref
   unsafe-struct-set! unsafe-struct*-set!
   vector-immutable vector))

(define (tc/index expr)
  (syntax-parse expr
   #:literal-sets (kernel-literals)
   [(quote i:number)
    (let ((type (tc-literal #'i)))
      (add-typeof-expr expr (ret type))
      (syntax-e #'i))]
   [_
    (match (tc-expr expr)
     [(tc-result1: (Value: (? number? i))) i]
     [tc-results
       (check-below tc-results (ret -Integer))
       #f])]))

(define (index-error i-val i-bound expr type name)
  (cond 
    [(not (and (integer? i-val) (exact? i-val)))
     (tc-error/expr #:stx expr "expected exact integer for ~a index, but got ~a" name i-val)]
    [(< i-val 0)
     (tc-error/expr #:stx expr "index ~a too small for ~a ~a" i-val name type)]
    [(not (< i-val i-bound))
     (tc-error/expr #:stx expr "index ~a too large for ~a ~a" i-val name type)]))

(define (valid-index? i bound)
 (and (integer? i) (exact? i) (<= 0 i (sub1 bound))))


;; FIXME - Do something with paths in the case that a structure/vector is not mutable
(define (tc/hetero-ref i-e es-t vec-t name)
  (define i-val (tc/index i-e))
  (define i-bound (length es-t))
  (cond
    [(valid-index? i-val i-bound)
     (ret (list-ref es-t i-val))]
    [(not i-val)
     (ret (apply Un es-t))]
    [else
     (index-error i-val i-bound i-e vec-t name)]))

(define (tc/hetero-set! i-e es-t val-e vec-t name)
  (define i-val (tc/index i-e))
  (define i-bound (length es-t))
  (cond 
    [(valid-index? i-val i-bound)
     (tc-expr/check val-e (ret (list-ref es-t i-val)))
     (ret -Void)]
    [(not i-val)
     (define val-t (single-value val-e))
     (for ((es-type (in-list es-t)))
       (check-below val-t (ret es-type)))
     (ret -Void)]
    [else
     (single-value val-e)
     (index-error i-val i-bound i-e vec-t name)]))

(define-tc/app-syntax-class (tc/app-hetero expected)
  #:literal-sets (hetero-literals)
  (pattern (~and form ((~or unsafe-struct-ref unsafe-struct*-ref) struct:expr index:expr))
    (match (single-value #'struct)
      [(tc-result1: (and struct-t (app resolve (Struct: _ _ (list (fld: flds _ _) ...) _ _ _))))
       (tc/hetero-ref #'index flds struct-t "struct")]
      [s-ty (tc/app-regular #'form expected)]))
  ;; vector-ref on het vectors
  (pattern (~and form ((~or vector-ref unsafe-vector-ref unsafe-vector*-ref) vec:expr index:expr))
    (match (single-value #'vec)
      [(tc-result1: (and vec-t (app resolve (HeterogeneousVector: es))))
       (tc/hetero-ref #'index es vec-t "vector")]
      [v-ty (tc/app-regular #'form expected)]))
  ;; unsafe struct-set! 
  (pattern (~and form ((~or unsafe-struct-set! unsafe-struct*-set!) s:expr index:expr val:expr))
    (match (single-value #'s)
      [(tc-result1: (and struct-t (app resolve (Struct: _ _ (list (fld: flds _ _) ...) _ _ _))))
       (tc/hetero-set! #'index flds #'val struct-t "struct")]
      [s-ty (tc/app-regular #'form expected)]))
  ;; vector-set! on het vectors
  (pattern (~and form ((~or vector-set! unsafe-vector-set! unsafe-vector*-set!) v:expr index:expr val:expr))
    (match (single-value #'v)
      [(tc-result1: (and vec-t (app resolve (HeterogeneousVector: es))))
       (tc/hetero-set! #'index es #'val vec-t "vector")]
      [v-ty (tc/app-regular #'form expected)]))
  (pattern (~and form ((~or vector-immutable vector) args:expr ...))
    (match expected
      [(tc-result1: (app resolve (Vector: t)))
       (ret (make-HeterogeneousVector 
              (for/list ([e (in-syntax #'(args ...))])
                (tc-expr/check e (ret t))
                t)))]
      [(tc-result1: (app resolve (HeterogeneousVector: ts)))
       (cond
         [(= (length ts) (syntax-length #'(args ...)))
          (ret
            (make-HeterogeneousVector
              (for/list ([e (in-syntax #'(args ...))]
                         [t (in-list ts)])
                (tc-expr/check/t e (ret t))))
            -true-filter)]
         [else
          (tc-error/expr
            "expected vector with ~a elements, but got ~a"
            (length ts) (make-HeterogeneousVector (stx-map tc-expr/t #'(args ...))))])]
      ;; If the expected type is a union, then we examine just the parts
      ;; of the union that are vectors.  If there's only one of those,
      ;; we re-run this whole algorithm with that.  Otherwise, we treat
      ;; it like any other expected type.
      [(tc-result1: (app resolve (Union: ts))) (=> continue)
       (define u-ts (for/list ([t (in-list ts)]
                               #:when (eq? 'vector (Type-key t)))
                      t))
       (match u-ts
         [(list t0) (tc/app #'(#%plain-app . form) (ret t0))]
         [_ (continue)])]
      ;; since vectors are mutable, if there is no expected type, we want to generalize the element type
      [(or #f (tc-any-results: _) (tc-result1: _))
       (ret (make-HeterogeneousVector
              (for/list ((e (in-syntax #'(args ...))))
                (generalize (tc-expr/t e)))))]
      [_ (ret Err)])))
