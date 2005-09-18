
;; Like PLAI advanced, but with dynamic scope.
;; No `let', `let*', or `letrec'.
;;`local' expects ;;  all definitions to have the
;;  form `(define id expr)'.

(module plai-dynamic mzscheme
  (require (rename (lib "htdp-advanced.ss" "lang") plai-else else)
	   (rename (lib "htdp-advanced.ss" "lang") advanced-define define)
	   "private/datatype.ss"
           "test-harness.ss")

  ;; This macro requires & provides bindings without
  ;;  making them locally visible:
  (define-syntax (provide-advanced stx)
    #'(begin
	(require (all-except (lib "htdp-advanced.ss" "lang")
			     #%top define local let let* letrec lambda))
	(provide (all-from-except (lib "htdp-advanced.ss" "lang")
				  plai-else advanced-define))))
  (provide-advanced)

  (define-for-syntax (make-dynamic k)
    (make-set!-transformer
     (lambda (stx)
       (syntax-case stx (set!)
	 [id
	  (identifier? #'id)
	  #'(lookup-dynamic 'id)]
	 [(set! id val)	    
	  #'(set-dynamic! 'id val)]
	 [(id expr ...)
	  #'((lookup-dynamic 'id) expr ...)]))))

  (define-syntax to-dynamic
    (syntax-rules ()
      [(_ (id ...) expr)
       (with-continuation-mark*
	('id ...) ((box id) ...)
	(let-syntax ([id (make-dynamic (quote-syntax id))]
		     ...)
	  expr))]))
  
  (define-syntax (dynamic-type-case stx)
    (syntax-case stx ()
      [(_ type expr
	  [id (param ...) body-expr]
	  ...)
       #'(advanced-type-case 
	  type expr
	  [id (param ...) 
	      (to-dynamic (param ...)
			  body-expr)] ...)]
      [(_ . rest)
       #'(advanced-type-case . rest)]))

  (define-syntax (dynamic-define stx)
    (syntax-case stx ()
      [(_ (id arg ...) body-expr)
       #'(advanced-define (id arg ...)
			  (to-dynamic
			   (arg ...)
			   body-expr))]
      [(_ . rest)
       #'(advanced-define . rest)]))

  (define-syntax (dynamic-lambda stx)
    (syntax-case stx ()
      [(_ (id ...) expr)
       #'(lambda (id ...)
	   (to-dynamic (id ...)
		       expr))]))

  (define-syntax (dynamic-local stx)
    (syntax-case stx (dynamic-define)
      [(_ [(dynamic-define id val) ...] body-expr)
       (andmap identifier? (syntax->list #'(id ...)))
       #'(let [(id val) ...]
	   (to-dynamic (id ...)
		       body-expr))]))
  
  (define-syntax (dynamic-top stx)
    (syntax-case stx ()
      [(_ . id)
       (identifier? #'id)
       #'(lookup-dynamic 'id)]))

  (define (lookup-dynamic id)
    (let ([v (continuation-mark-set-first #f id)])
      (if v
	  (unbox v)
	  (namespace-variable-value id #f (lambda ()
					    (error 'eval
						   "no dynamic value for identifier: ~a"
						   id))))))

  (define (set-dynamic! id val)
    (let ([v (continuation-mark-set-first #f id)])
      (if v
	  (set-box! v val)
	  (namespace-set-variable-value! id val))))

  (define-syntax with-continuation-mark*
    (syntax-rules ()
      [(_ () () expr) expr]
      [(_ (key . krest) (val . vrest) expr)
       (with-continuation-mark key val
	 (with-continuation-mark* krest vrest expr))]))

  (provide (rename dynamic-type-case type-case)
	   (rename dynamic-define define)
	   (rename dynamic-lambda lambda)
	   (rename dynamic-local local)
	   (rename dynamic-top #%top)
	   define-type
	   require provide provide-type
           (all-from "test-harness.ss")

	   ;; Hack to avoid certification bug :(
	   lookup-dynamic)

  (define-type-case advanced-type-case plai-else))
