#lang typed/racket/base

(require (for-syntax racket/base syntax/parse)
         "../unsafe.rkt"
         "array-struct.rkt"
         "mutable-array.rkt"
         "utils.rkt")

(provide for/array: for*/array:)

(define-syntax (base-for/array: stx)
  (syntax-case stx ()
    [(_ name for A ds-expr (clauses ...) defs+exprs ...)
     (syntax/loc stx
       (let*: ([ds : User-Indexes  ds-expr]
               [ds : Indexes  (check-array-shape
                               ds (λ () (raise-type-error 'name "Indexes" ds)))]
               [n : Natural  (array-shape-size ds)])
         (cond [(index? n)
                (define: vs : (Vectorof A) (vector))
                (define: i : Nonnegative-Fixnum 0)
                (let/ec: break : Void
                  (for (clauses ...)
                    (define: v : A (let () defs+exprs ...))
                    (cond [(= i 0)  (set! vs (make-vector n v))]
                          [else  (unsafe-vector-set! vs i v)])
                    (set! i (unsafe-fx+ i 1))
                    (when (i . >= . n) (break (void)))))
                (define len (vector-length vs))
                (cond [(= len n)  (unsafe-mutable-array ds vs)]
                      [else  (error 'name "expected ~e elements; produced ~e" n len)])]
               [else
                (error 'name "array size ~e (for shape ~e) is too large (is not an Index)" n ds)])))]
    [(_ for A (clauses ...) defs+exprs ...)
     (syntax/loc stx
       (let ()
         (define: lst : (Listof A) null)
         (for (clauses ...)
           (define: v : A (let () defs+exprs ...))
           (set! lst (cons v lst)))
         (define vs (list->vector (reverse lst)))
         (unsafe-mutable-array ((inst vector Index) (vector-length vs)) vs)))]))

(define-syntax (for/array: stx)
  (syntax-parse stx
    [(_ A:expr #:shape ds-expr:expr ([bnd val] ...) defs+exprs:expr ...+)
     (syntax/loc stx
       (base-for/array: for/array: for: A ds-expr ([bnd val] ...) defs+exprs ...))]
    [(_ A:expr ([bnd val] ...) defs+exprs:expr ...+)
     (syntax/loc stx
       (base-for/array: for: A ([bnd val] ...) defs+exprs ...))]))

(define-syntax (for*/array: stx)
  (syntax-parse stx
    [(_ A:expr #:shape ds-expr:expr ([bnd val] ...) defs+exprs:expr ...+)
     (syntax/loc stx
       (base-for/array: for*/array: for*: A ds-expr ([bnd val] ...) defs+exprs ...))]
    [(_ A:expr ([bnd val] ...) defs+exprs:expr ...+)
     (syntax/loc stx
       (base-for/array: for*: A ([bnd val] ...) defs+exprs ...))]))
