
(module spidey mzscheme

  (provide define-constructor
	  define-type
	  :
	  mrspidey:control
	  polymorphic
	  type:)

  (define-syntax define-constructor
    (lambda (stx) (syntax (void))))

  (define-syntax define-type
    (lambda (stx) (syntax (void))))

  (define-syntax :
    (lambda (stx)
      (syntax-case stx ()
	[(_ v t) (syntax v)])))

  (define-syntax mrspidey:control
    (lambda (stx) (syntax (void))))  

  (define-syntax polymorphic
    (lambda (stx)
      (syntax-case stx ()
	[(_ e) (syntax e)])))

  (define-syntax type:
    (lambda (stx) (syntax (void)))))
