(module debug mzscheme
  (provide debug-printf debug-when)

  (define-syntax debug-when
    (lambda (stx)

      ;; all of the steps in the tcp connection
      (define mz-tcp? #f)
      (define mr-tcp? mz-tcp?)
      
      ;; administrative messages about preferences files and
      ;; command line flags
      (define admin? #f)
      
      ;; tests that passed and those that failed
      (define schedule? #t)
      
      ;; all of the sexpression transactions between mz and mred
      (define messages? #t)

      (syntax-case stx (mr-tcp mz-tcp admin schedule messages)
	[(_ mr-tcp rest ...)
	 (if mr-tcp?
	     (syntax (begin rest ...))
	     (syntax (void)))]
	[(_ mz-tcp rest ...)
	 (if mz-tcp?
	     (syntax (begin rest ...))
	     (syntax (void)))]
	[(_ admin rest ...)
	 (if admin?
	     (syntax (begin rest ...))
	     (syntax (void)))]
	[(_ schedule rest ...)
	 (if schedule?
	     (syntax (begin rest ...))
	     (syntax (void)))]
	[(_ messages rest ...)
	 (if messages?
	     (syntax (begin rest ...))
	     (syntax (void)))]
	[(_ unk rest ...)
	 (raise-syntax-error 'debug-when "unknown flag" stx (syntax unk))])))

  (define-syntax debug-printf
    (lambda (stx)
      (syntax-case stx ()
	[(_ flag fmt-string rest ...)
	 (with-syntax ([flag-name (format ">> ~a: " (syntax-object->datum (syntax flag)))])
	   (syntax (debug-when flag (printf (string-append flag-name fmt-string) rest ...))))]))))
