#lang racket/base

(require unstable/match racket/match
         racket/dict syntax/id-table racket/syntax unstable/syntax
         "../utils/utils.rkt"
         (for-template racket/base)
         (types type-table utils subtype)
         (rep type-rep))

(provide *show-optimized-code*
         subtypeof? isoftype?
         mk-unsafe-tbl
         n-ary->binary n-ary-comp->binary
         unboxed-gensym reset-unboxed-gensym
         optimize
         print-res
         syntax/loc/origin quasisyntax/loc/origin)

;; for tracking both origin and source location information
(define-syntax-rule (syntax/loc/origin loc op body)
  (syntax-track-origin (syntax/loc loc body) loc op))
(define-syntax-rule (quasisyntax/loc/origin loc op body)
  (syntax-track-origin (quasisyntax/loc loc body) loc op))

;; if set to #t, the optimizer will dump its result to stdout before compilation
(define *show-optimized-code* #f)

;; is the syntax object s's type a subtype of t?
(define (subtypeof? s t)
  (match (type-of s)
    [(tc-result1: (== t (lambda (x y) (subtype y x)))) #t] [_ #f]))
;; similar, but with type equality
(define (isoftype? s t)
  (match (type-of s)
         [(tc-result1: (== t type-equal?)) #t] [_ #f]))

;; generates a table matching safe to unsafe promitives
(define (mk-unsafe-tbl generic safe-pattern unsafe-pattern)
  (for/fold ([h (make-immutable-free-id-table)]) ([g generic])
    (let ([f (format-id g safe-pattern g)] [u (format-id g unsafe-pattern g)])
      (dict-set (dict-set h g u) f u))))

;; unlike their safe counterparts, unsafe binary operators can only take 2 arguments
;; this works on operations that are (A A -> A)
(define (n-ary->binary op arg1 arg2 rest)
  (for/fold ([o arg1])
      ([e (syntax->list #`(#,arg2 #,@rest))])
    #`(#,op #,o #,e)))
;; this works on operations that are (A A -> B)
(define (n-ary-comp->binary op arg1 arg2 rest)
  ;; First, generate temps to bind the result of each arg2 args ...
  ;; to avoid computing them multiple times.
  (define lifted (map (lambda (x) (unboxed-gensym))
                      (syntax->list #`(#,arg2 #,@rest))))
  ;; Second, build the list ((op arg1 tmp2) (op tmp2 tmp3) ...)
  (define tests
    (let loop ([res  (list #`(#,op #,arg1 #,(car lifted)))]
               [prev (car lifted)]
               [l    (cdr lifted)])
      (cond [(null? l) (reverse res)]
            [else (loop (cons #`(#,op #,prev #,(car l)) res)
                        (car l)
                        (cdr l))])))
  ;; Finally, build the whole thing.
  #`(let #,(for/list ([lhs (in-list lifted)]
                      [rhs (in-list (syntax->list #`(#,arg2 #,@rest)))])
             #`(#,lhs #,rhs))
      (and #,@tests)))

;; to generate temporary symbols in a predictable manner
;; these identifiers are unique within a sequence of unboxed operations
(define *unboxed-gensym-counter* 0)
(define (unboxed-gensym [name 'unboxed-gensym-])
  (set! *unboxed-gensym-counter* (add1 *unboxed-gensym-counter*))
  (format-unique-id #'here "~a~a" name *unboxed-gensym-counter*))
(define (reset-unboxed-gensym)
  (set! *unboxed-gensym-counter* 0))

;; to avoid mutually recursive syntax classes
;; will be set to the actual optimization function at the entry point
;; of the optimizer
(define optimize (make-parameter #f))

(define (print-res t)
  (match t
    [(tc-result1: t f o)
     (format "~a" t)]))
