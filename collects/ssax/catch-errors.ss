(module catch-errors mzscheme

  (provide failed?)

  (define-syntax failed?
    (syntax-rules ()
      ((failed? stmts ...)
       (thunk-failed? (lambda () stmts ...)))))
  (define (thunk-failed? thunk)
    (call-with-current-continuation
     (lambda (return)
       (with-handlers
	   (((lambda (x) #t) (lambda (exn) #t)))
	 (thunk)
	 #f)))))
