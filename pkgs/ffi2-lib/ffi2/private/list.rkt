#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre)
         "core.rkt"
         (only-in "type.rkt"
                  :malloc-kind))

(provide (rename-out [list_t* list_t]
                     [vector_t* vector_t]))

(define-ffi2-type-syntax (list_t* stx)
  (syntax-parse stx
    [(who (~optional kind::malloc-kind) elem_t)
     (build-list_t #'who (attribute kind) #'list_t #'elem_t #'0)]
    [(who (~optional kind::malloc-kind) elem_t #:length length-expr)
     (build-list_t #'who (attribute kind) #'list_t #'elem_t #'length-expr)]))

(define-ffi2-type-syntax (vector_t* stx)
  (syntax-parse stx
    [(who (~optional kind::malloc-kind) elem_t)
     (build-list_t #'who (attribute kind) #'vector_t #'elem_t #'0)]
    [(who (~optional kind::malloc-kind) elem_t #:length length-expr)
     (build-list_t #'who (attribute kind) #'vector_t #'elem_t #'length-expr)]))

(define-struct elem-t (is-a? malloc ref set!))

(define-for-syntax (build-list_t who kind list_t elem_t length-expr)
  #`(#,list_t
     (let ()
       (define-ffi2-type elem_t #,elem_t
         #:racket->c values)
       (elem-t
        (lambda (e) (ffi2-is-a? e elem_t))
        (lambda (n) (ffi2-malloc #,(or kind '#:gcable) elem_t n))
        (lambda (p i) (ffi2-ref p elem_t i))
        (lambda (p i e) (ffi2-set! p elem_t i e))))
     (let ([len #,length-expr])
       (unless (variable-reference-from-unsafe? (#%variable-reference))
         (unless (exact-nonnegative-integer? len)
           (raise-argument-error '#,who "exact-nonnegative-integer?" len)))
       len)))

(define-ffi2-type (list_t procs out-length) ptr_t
  #:predicate (lambda (v) (and (list? v)
                               (for/and ([e (in-list v)])
                                 ((elem-t-is-a? procs) e))))
  #:racket->c (lambda (v)
                (define n (length v))
                (define arr ((elem-t-malloc procs) n))
                (for ([e (in-list v)]
                      [i (in-naturals)])
                  ((elem-t-set! procs) arr i e))
                arr)
  #:c->racket (lambda (p)
                (for/list ([i (in-range out-length)])
                  ((elem-t-ref procs) p i))))

(define-ffi2-type (vector_t procs out-length) ptr_t
  #:predicate (lambda (v) (and (vector? v)
                               (for/and ([e (in-vector v)])
                                 ((elem-t-is-a? procs) e))))
  #:racket->c (lambda (v)
                (define n (vector-length v))
                (define arr ((elem-t-malloc procs) n))
                (for ([e (in-vector v)]
                      [i (in-naturals)])
                  ((elem-t-set! procs) arr i e))
                arr)
  #:c->racket (lambda (p)
                (for/vector #:length out-length ([i (in-range out-length)])
                  ((elem-t-ref procs) p i))))
