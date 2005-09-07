(module plai-beginner mzscheme
  (require (rename (lib "htdp-beginner.ss" "lang") plai-else else)
	   (lib "prim.ss" "lang")
	   "private/datatype.ss"
	   "private/datatype-core.ss"
           "test-harness.ss")

  ;; This macro requires & provides bindings without
  ;;  making them locally visible:
  (define-syntax (provide-beginner stx)
    #'(begin
	(require (lib "htdp-beginner.ss" "lang"))
	(provide (all-from-except (lib "htdp-beginner.ss" "lang")
				  plai-else))))
  (provide-beginner)
		   
  (provide (rename beginner-type-case type-case)
	   (rename beginner-define-type define-type)
	   require provide provide-type
           (all-from "test-harness.ss"))

  (define-syntax (name-it stx)
    (syntax-case stx ()
      [(_ id expr)
       (identifier? #'id)
       #'(let ([id expr]) id)]
      [(_ non-id expr)
       #'expr]))

  ;; For beginner, `define-type' requires predicates for
  ;;  contracts, and it doesn't define contracts
  (define-syntax (beginner-define-type stx)
    (syntax-case stx ()
      [(_ name (variant (field predicate) ...) ...)
       (let ([name #'name])
	 (unless (identifier? name)
	   (raise-syntax-error
	    #f
	    "expected an identifier for the type name"
	    stx
	    name))
	 (with-syntax ([orig-stx stx]
		       [name name]
		       [name? (datum->syntax-object name
						    (string->symbol
						     (format "~a?" (syntax-e name))))])
	   #'(define-datatype-core orig-stx
	       (define-selectors define-predicates (kind "type"))
	       define-proc-values
	       name () name?
	       (variant (field (name-it predicate (lambda (x) (predicate x)))) ...)
	       ...)))]
      ;; If the above pattern doesn't match, let `define-type' handle the syntax errors:
      [(_ name-stx . variants)
       (identifier? #'name-stx)
       #'(define-type name-stx . variants)]
      [(_ . __)
       (raise-syntax-error
	#f
	"expected an identifier for the type name"
	stx)]))

  (define-syntax (define-proc-values stx)
    (syntax-case stx ()
      [(_ (id ...) expr)
       (with-syntax ([(alt-id ...) (generate-temporaries #'(id ...))])
	 (with-syntax ([top-level-hack (if (eq? 'top-level (syntax-local-context))
					   #'(define-syntaxes (alt-id ...) (values))
					   #'(begin))])
	   #'(begin
	       top-level-hack
	       (define-primitive id alt-id) ...
	       (define-values (alt-id ...) expr))))]))

  (define-type-case beginner-type-case plai-else))
