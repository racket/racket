#lang racket/base

(provide make-struct-type-property/generic
         make-generic-struct-type-property)

(require racket/function
         "generic-methods.rkt"
         (for-syntax racket/base
                     syntax/stx))

(begin-for-syntax
  ;; stx-expr? : Stx -> Bool
  (define (stx-expr? v)
    (not (keyword? (if (syntax? v) (syntax-e v) v))))

  ;; super-pairs : Stx Stx [Listof Stx] -> [Listof Stx]
  (define (option-super-pairs ctx stx acc)
    (cond
      [(stx-null? stx) (reverse acc)]
      [(stx-pair? stx)
       (define kw (stx-car stx))
       (define kwe (syntax-e kw))
       (cond
         [(eq? kwe '#:property)
          (define r1 (stx-cdr stx))
          (unless (stx-pair? r1)
            (raise-syntax-error #f
              "expected prop-expr and val-expr after #:property"
              ctx
              r1
              (list kw)))
          (define p (stx-car r1))
          (unless (stx-expr? p)
            (raise-syntax-error #f
              "expected prop-expr and val-expr after #:property"
              ctx
              p))
          (define r2 (stx-cdr r1))
          (unless (stx-pair? r2)
            (raise-syntax-error #f
              "expected val-expr after #:property prop-expr"
              ctx
              r2
              (list kw)))
          (define v (stx-car r2))
          (unless (stx-expr? v)
            (raise-syntax-error #f
              "expected val-expr after #:property prop-expr"
              ctx
              v))
          (option-super-pairs ctx (stx-cdr r2) (cons #`(cons #,p (const #,v)) acc))]
         [(eq? kwe '#:methods)
          (define r1 (stx-cdr stx))
          (unless (stx-pair? r1)
            (raise-syntax-error #f
              "expected gen:name-id and method-defs after #:methods"
              ctx
              r1
              (list kw)))
          (define g (stx-car r1))
          (unless (identifier? g)
            (raise-syntax-error #f
              "expected gen:name-id and method-defs after #:methods"
              ctx
              g))
          (define r2 (stx-cdr r1))
          (unless (stx-pair? r2)
            (raise-syntax-error #f
              "expected method-defs after #:methods gen:name-id"
              ctx
              r2
              (list kw)))
          (define m (stx-car r2))
          (unless (syntax->list m)
            (raise-syntax-error #f
              "expected method-defs after #:methods gen:name-id"
              ctx
              m))
          (option-super-pairs ctx (stx-cdr r2)
                              (cons #`(cons (generic-property #,g)
                                            (const (generic-method-table #,g #,@m)))
                                    acc))]
         [else
          (raise-syntax-error #f
            "expected one of these literals: #:property or #:methods"
            ctx
            kw)])]
      [else
       (raise-syntax-error #f "bad syntax" ctx stx)])))

(define-syntax make-struct-type-property/generic
  (lambda (stx)
    (unless (stx-pair? stx) (raise-syntax-error #f "bad syntax" stx))
    (define r1 (stx-cdr stx))
    (unless (stx-pair? r1) (raise-syntax-error #f "expected name-expr" stx r1))
    (define name (stx-car r1))
    (unless (stx-expr? name) (raise-syntax-error #f "expected name-expr" stx name))
    (define r2 (stx-cdr r1))
    (define-values [guard r3]
      (if (and (stx-pair? r2) (stx-expr? (stx-car r2)))
          (values (stx-car r2) (stx-cdr r2))
          (values #'#f r2)))
    (define-values [supers r4]
      (if (and (stx-pair? r3) (stx-expr? (stx-car r3)))
          (values (stx-car r3) (stx-cdr r3))
          (values #''() r3)))
    (define-values [can-impersonate? r5]
      (if (and (stx-pair? r4) (stx-expr? (stx-car r4)))
          (values (stx-car r4) (stx-cdr r4))
          (values #'#f r4)))
    (syntax-protect
     #`(make-struct-type-property #,name
                                  #,guard
                                  (list* #,@(option-super-pairs stx r5 '()) #,supers)
                                  #,can-impersonate?))))

(define-syntax make-generic-struct-type-property
  (lambda (stx)
    (unless (stx-pair? stx) (raise-syntax-error #f "bad syntax" stx))
    (define r1 (stx-cdr stx))
    (unless (stx-pair? r1) (raise-syntax-error #f "expected gen:name-id" stx r1))
    (define g (stx-car r1))
    (unless (identifier? g) (raise-syntax-error #f "expected gen:name-id" stx g))
    (define m (stx-cdr r1))
    (unless (stx->list m)
      (raise-syntax-error #f "expected method-defs after gen:name-id" stx m))
    (syntax-protect
     #`(let-values [((prop _pred _ref)
                     (make-struct-type-property/generic '#,g #:methods #,g #,m))]
         prop))))

