#lang scheme

(define-struct block-context (return?))
(define-struct (top-block-context block-context) ())
(define-struct (expression-block-context block-context) ())

(define-struct definition-context ())
(define-struct (function-definition-context definition-context) ())
(define-struct (value-definition-context definition-context) ())
(define-struct (constant-definition-context value-definition-context) ())
(define-struct (variable-definition-context value-definition-context) ())

(define-struct expression-context ())
(define-struct type-context ())
(define-struct type-or-expression-context ())
(define-struct prototype-context ())

(define the-block-context (make-block-context #f))
(define the-top-block-context (make-top-block-context #f))
(define the-expression-block-context (make-expression-block-context #f))
(define the-return-block-context (make-block-context #t))
(define the-expression-return-block-context (make-expression-block-context #t))

(define the-variable-definition-context (make-variable-definition-context))
(define the-constant-definition-context (make-constant-definition-context))
(define the-function-definition-context (make-function-definition-context))

(define the-expression-context (make-expression-context))
(define the-type-context (make-type-context))
(define the-type-or-expression-context (make-type-or-expression-context))
(define the-prototype-context (make-prototype-context))

(define (context->name ctx)
  (cond
    [(type-context? ctx) "a type"]
    [(type-or-expression-context? ctx) "a type or expression"]
    [(expression-context? ctx) "an expression"]
    [(expression-block-context? ctx) "a statement"]
    [(block-context? ctx) "a block"]
    [(variable-definition-context? ctx) "a variable-definition"]
    [(constant-definition-context? ctx) "a constant-definition"]
    [(function-definition-context? ctx) "a function-definition"]
    [(prototype-context? ctx) "a function-definition"]
    [else "an expression"]))

(provide block-context?
         expression-block-context?
         top-block-context?

         definition-context?
         function-definition-context?
         value-definition-context?
         variable-definition-context?
         constant-definition-context?

         expression-context?
         type-context?
         type-or-expression-context?
         prototype-context?

         block-context-return?

         the-block-context
         the-top-block-context
         the-return-block-context
         the-expression-block-context
         the-expression-return-block-context

         make-function-definition-context
         the-variable-definition-context
         the-constant-definition-context
         the-function-definition-context

         the-expression-context
         the-type-context
         the-type-or-expression-context
         the-prototype-context

         context->name)
