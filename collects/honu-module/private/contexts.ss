(module contexts mzscheme

  (define-struct block-context ())
  (define-struct (top-block-context block-context) ())
  (define-struct (return-block-context block-context) ())

  (define-struct definition-context ())
  (define-struct (function-definition-context definition-context) ())
  (define-struct (value-definition-context definition-context) ())
  (define-struct (constant-definition-context value-definition-context) ())
  (define-struct (variable-definition-context value-definition-context) ())

  (define-struct expression-context ())
  (define-struct type-context ())

  (define the-block-context (make-block-context))
  (define the-top-block-context (make-top-block-context))
  (define the-return-block-context (make-return-block-context))

  (define the-function-definition-context (make-function-definition-context))
  (define the-variable-definition-context (make-variable-definition-context))
  (define the-constant-definition-context (make-constant-definition-context))

  (define the-expression-context (make-expression-context))
  (define the-type-context (make-type-context))

   (define (context->name ctx)
     (cond
      [(type-context? ctx) "a type"]
      [(block-context? ctx) "a block"]
      [(variable-definition-context? ctx) "a variable-definition"]
      [(constant-definition-context? ctx) "a constant-definition"]
      [(function-definition-context? ctx) "a function-definition"]
      [else "an expression"]))
   
  (provide block-context?
	   top-block-context?
	   return-block-context?

	   definition-context?
	   function-definition-context?
	   value-definition-context?
	   variable-definition-context?
	   constant-definition-context?

	   expression-context?
	   type-context?

	   the-block-context
	   the-top-block-context
	   the-return-block-context

	   the-function-definition-context
	   the-variable-definition-context
	   the-constant-definition-context

	   the-expression-context
	   the-type-context

	   context->name))
