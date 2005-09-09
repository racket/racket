
(module datatype mzscheme

  (require "datatype-core.ss")
  
  (define-for-syntax (do-define-type stx name type-params variants)
    (unless (identifier? name)
      (raise-syntax-error
       #f
       "expected an identifier for the type name"
       stx
       name))
    (for-each (lambda (type-param)
		(unless (identifier? type-param)
		  (raise-syntax-error
		   #f
		   "expected an identifier for a type parameter"
		   stx
		   type-param)))
	      type-params)
    (with-syntax ([orig-stx stx]
		  [name name]
		  [name? (datum->syntax-object name
					       (string->symbol
						(format "~a?" (syntax-e name))))]
		  [(type-param ...) type-params]
		  [variants variants])
      #'(define-datatype-core orig-stx
	  (define-selectors define-predicates define-polymorphic define-contracts (kind "type"))
	  define-values
	  name (type-param ...) name?
	  . variants)))

  (define-syntax (define-type stx)
    (syntax-case stx (represented-as)
      [(_ (name-stx type-param-stx ...) . variants)
       (do-define-type stx #'name-stx (syntax->list #'(type-param-stx ...)) #'variants)]
      [(_ name-stx . variants)
       (do-define-type stx #'name-stx null #'variants)]
      [(_ . __)
       (raise-syntax-error
	#f
	"expected an identifier for the type name"
	stx)]))
  
  (define-syntax (case-begin stx)
    (syntax-case stx ()
      [(_ orig-stx orig-clause expr) #'expr]
      [(_ orig-stx orig-clause expr0 expr ...)
       (raise-syntax-error
	#f
	(format "expected only one result expression, found ~a"
		(add1 (length (syntax->list #'(expr ...)))))
	#'orig-stx
	#'orig-clause)]))

  (define-syntax define-type-case
    (syntax-rules ()
      [(_ type-case else)
       (define-syntax (type-case stx)
	 (syntax-case stx ()
	   [(_ . rest) #`(cases-core #,stx "type" case-begin else . rest)]))]))

  (define-syntax (provide-type stx)
    (syntax-case stx ()
      [(provide-type . rest)
       #`(provide-datatype-core #,stx . rest)]))
  
  (provide provide-type
           define-type
	   define-type-case))
