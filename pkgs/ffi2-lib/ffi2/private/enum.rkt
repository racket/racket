#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre)
         "core.rkt")

(provide enum)

(begin-for-syntax
  (define-splicing-syntax-class :enum-case
    #:description "enum case"
    #:attributes (int sym)
    #:literals (=)
    (pattern (~seq sym:identifier = int:exact-integer))
    (pattern (~seq sym:identifier)
             #:attr int #f)))

(define-syntax (enum stx)
  (syntax-parse stx
    [(_ name:identifier parent_t
        c::enum-case
        ...)
     (with-syntax ([(int ...)
                    (let loop ([syms (attribute c.sym)]
                               [ints (attribute c.int)]
                               [counter 0]
                               [keys #hasheq()])
                      (cond
                        [(null? syms) null]
                        [else
                         (define sym (car syms))
                         (define int (or (car ints) counter))
                         (when (hash-ref keys (syntax-e sym) #f)
                           (raise-syntax-error #f "duplicate symbol" stx sym))
                         (cons int
                               (loop (cdr syms) (cdr ints)
                                     (add1 (if (syntax? int) (syntax-e int) int))
                                     (hash-set keys (syntax-e sym) #t)))]))])
       #'(begin
           (define syms->int (hasheq (~@ 'c.sym int) ...))
           (define int->syms (hasheqv (~@ int 'c.sym) ...))
           (define-ffi2-type name parent_t
             #:predicate (lambda (v) (hash-ref syms->int v #f))
             #:racket->c (lambda (sym) (hash-ref syms->int sym 0))
             #:c->racket (lambda (i) (hash-ref int->syms i #false)))))]))

