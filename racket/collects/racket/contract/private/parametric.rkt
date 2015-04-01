#lang racket/base
(require "prop.rkt"
         "blame.rkt"
         "misc.rkt"
         "guts.rkt"
         (for-syntax "arr-util.rkt" racket/base))
(provide parametric->/c)

(define-syntax (parametric->/c stx)
  (syntax-case stx ()
    [(_ [x ...] c)
     (begin
       (for ([x (in-list (syntax->list #'(x ...)))])
         (unless (identifier? x)
           (raise-syntax-error 'parametric->/c 
                               "expected an identifier"
                               stx
                               x)))
       (define dup (check-duplicate-identifier (syntax->list #'(x ...))))
       (when dup (raise-syntax-error
                  'parametric->/c 
                  "duplicate identifier"
                  stx
                  dup))
       #`(make-polymorphic-contract opaque/c
                                    '(x ...)
                                    (lambda (x ...) c)
                                    '#,(compute-quoted-src-expression #'c)))]))



(define-struct polymorphic-contract [barrier vars body body-src-exp]
  #:property prop:custom-write custom-write-property-proc
  #:property prop:contract
  (build-contract-property
   #:name
   (lambda (c)
     `(parametric->/c ,(polymorphic-contract-vars c) ,(polymorphic-contract-body-src-exp c)))
   #:stronger
   (λ (this that)
     (cond
       [(polymorphic-contract? that)
        (define this-vars (polymorphic-contract-vars this))
        (define that-vars (polymorphic-contract-vars that))
        (define this-barrier/c (polymorphic-contract-barrier this))
        (define that-barrier/c (polymorphic-contract-barrier that))
        (cond
          [(and (eq? this-barrier/c that-barrier/c)
                (= (length this-vars) (length that-vars)))
           (define instances
             (for/list ([var (in-list this-vars)])
               (this-barrier/c #t var)))
           (contract-stronger? (apply (polymorphic-contract-body this) instances)
                               (apply (polymorphic-contract-body that) instances))]
          [else #f])]
       [else #f]))
   #:projection
   (lambda (c)
     (lambda (orig-blame)
       (define blame (blame-add-context orig-blame #f))
       (define (wrap p)
         ;; values in polymorphic types come in from negative position,
         ;; relative to the poly/c contract
         (define negative? (blame-swapped? blame))
         (define barrier/c (polymorphic-contract-barrier c))
         (define instances
           (for/list ([var (in-list (polymorphic-contract-vars c))])
             (barrier/c negative? var)))
         (define protector
           (apply (polymorphic-contract-body c) instances))
         (((contract-projection protector) blame) p))

       (lambda (p)
         (unless (procedure? p)
           (raise-blame-error blame p '(expected "a procedure" given: "~e") p))
         (make-keyword-procedure
          (lambda (keys vals . args) (keyword-apply (wrap p) keys vals args))
          (case-lambda
            [() ((wrap p))]
            [(a) ((wrap p) a)]
            [(a b) ((wrap p) a b)]
            [(a b c) ((wrap p) a b c)]
            [(a b c d) ((wrap p) a b c d)]
            [(a b c d e) ((wrap p) a b c d e)]
            [(a b c d e f) ((wrap p) a b c d e f)]
            [(a b c d e f g) ((wrap p) a b c d e f g)]
            [(a b c d e f g h) ((wrap p) a b c d e f g h)]
            [args (apply (wrap p) args)])))))))

(define (opaque/c positive? name)
  (define-values [ type make pred getter setter ]
    (make-struct-type name #f 1 0))
  (define get (make-struct-field-accessor getter 0))
  (make-barrier-contract name positive? make pred get))

(define-struct barrier-contract [name positive? make pred get]
  #:property prop:custom-write custom-write-property-proc
  #:property prop:contract
  (build-contract-property
   #:name (lambda (c) (barrier-contract-name c))
   #:first-order (λ (c) (barrier-contract-pred c))
   #:stronger (λ (this that) (eq? this that))
   #:projection
   (lambda (c)
     (define mk (barrier-contract-make c))
     (define pred (barrier-contract-pred c))
     (define get (barrier-contract-get c))
     (lambda (blame)
       (if (equal? (blame-original? blame) (barrier-contract-positive? c))
           mk
           (lambda (x)
             (if (pred x)
                 (get x)
                 (raise-blame-error blame x '(expected: "~a" given: "~e")
                                    (barrier-contract-name c)
                                    x))))))))
