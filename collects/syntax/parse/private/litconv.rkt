#lang racket/base
(require (for-syntax racket/base
                     "sc.rkt"
                     "lib.rkt"
                     unstable/syntax
                     "rep-data.rkt"
                     "rep.rkt"
                     "kws.rkt")
         "runtime.rkt")
(provide define-conventions
         define-literal-set
         kernel-literals)

(define-syntax (define-conventions stx)

  (define-syntax-class header
    #:description "name or name with formal parameters"
    (pattern name:id
             #:with formals #'()
             #:attr arity (arity 0 0 null null))
    (pattern (name:id . formals)
             #:attr arity (parse-kw-formals #'formals #:context stx)))

  (syntax-parse stx
    [(define-conventions h:header rule ...)
     (let ()
       (define rules (check-conventions-rules #'(rule ...) stx))
       (define rxs (map car rules))
       (define dens0 (map cadr rules))
       (define den+defs-list
         (for/list ([den0 (in-list dens0)])
           (let-values ([(den defs) (create-aux-def den0)])
             (cons den defs))))
       (define dens (map car den+defs-list))
       (define defs (apply append (map cdr den+defs-list)))

       (define/with-syntax (rx ...) rxs)
       (define/with-syntax (def ...) defs)
       (define/with-syntax (parser ...)
         (map den:delayed-parser dens))
       (define/with-syntax (class-name ...)
         (map den:delayed-class dens))

       #'(begin
           (define-syntax h.name
             (make-conventions
              (quote-syntax get-parsers)
              (lambda ()
                (let ([class-names (list (quote-syntax class-name) ...)])
                  (map list
                       (list 'rx ...)
                       (map make-den:delayed
                            (generate-temporaries class-names)
                            class-names))))))
           (define get-parsers
             (lambda formals
               def ...
               (list parser ...)))))]))

(define-syntax (define-literal-set stx)
  (syntax-case stx ()
    [(define-literal-set name (lit ...))
     (let ([phase-of-definition (syntax-local-phase-level)])
       (unless (identifier? #'name)
         (raise-syntax-error #f "expected identifier" stx #'name))
       (let ([lits (check-literals-list/litset #'(lit ...) stx)])
         (with-syntax ([((internal external) ...) lits])
           #`(begin
               (define phase-of-literals
                 (phase-of-enclosing-module))
               (define-syntax name
                 (make-literalset
                  (list (list 'internal (quote-syntax external)) ...)
                  (quote-syntax phase-of-literals)))
               (begin-for-syntax/once
                (for ([x (in-list (syntax->list #'(external ...)))])
                  (unless (identifier-binding x 0)
                    (raise-syntax-error #f "literal is unbound in phase 0"
                                        (quote-syntax #,stx) x))))))))]))

(define-syntax (phase-of-enclosing-module stx)
  (syntax-case stx ()
    [(poem)
     (let ([phase-within-module (syntax-local-phase-level)])
       #`(let ([phase-of-this-expression
                (variable-reference->phase (#%variable-reference))])
           (- phase-of-this-expression
              #,(if (zero? phase-within-module) 0 1))))]))

#|
Literal sets: The goal is for literals to refer to their bindings at

  phase 0 relative to the enclosing module

Use cases, explained:
1) module X with def-lit-set is required-for-syntax
     phase-of-mod-inst = 1
     phase-of-def = 0
     literals looked up at abs phase 1
       which is phase 0 rel to module X
2) module X with local def-lit-set within define-syntax
     phase-of-mod-inst = 1 (mod at 0, but +1 within define-syntax)
     phase-of-def = 1
     literals looked up at abs phase 0
       which is phase 0 rel to module X
3) module X with def-lit-set in phase-2 position (really uncommon case!)
     phase-of-mod-inst = 1 (not 2, apparently)
     phase-of-def = 2
     literals looked up at abs phase 0
       (that's why the weird (if (z?) 0 1) term)
|#


;; Literal sets

(define-literal-set kernel-literals
  (begin
   begin0
   define-values
   define-syntaxes
   define-values-for-syntax
   set!
   let-values
   letrec-values
   #%plain-lambda
   case-lambda
   if
   quote
   quote-syntax
   letrec-syntaxes+values
   with-continuation-mark
   #%expression
   #%plain-app
   #%top
   #%datum
   #%variable-reference
   module #%provide #%require
   #%plain-module-begin))
