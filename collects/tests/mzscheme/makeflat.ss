
(with-handlers ([exn:fail:contract:variable?
		 (lambda (exn)
		   (namespace-set-variable-value!
		    'flat-load
		    "mz-tests.ss"))])
  (namespace-variable-value 'flat-load))

(with-handlers ([exn:fail:contract:variable?
		 (lambda (exn)
		   (namespace-set-variable-value!
		    'lines-per-file
		    +inf.0))])
  (namespace-variable-value 'lines-per-file))

(with-handlers ([exn:fail:contract:variable?
		 (lambda (exn)
		   (namespace-set-variable-value!
		    'flat-number
		    ""))])
  (namespace-variable-value 'flat-number))

(require mzlib/pretty)

(define line-count 0)
(define file-count 0)

(define flatp (open-output-file (format "flat~a.ss" flat-number) #:exists 'replace))
(define old-eval (current-eval))
(define old-namespace (current-namespace))

(pretty-print '(define error-test void) flatp)
(pretty-print '(define building-flat-tests? #t) flatp)
(pretty-print '(define section #f) flatp)

(define (flat-pp v)
  (parameterize ([print-hash-table #t])
    (pretty-print (if (syntax? v) (syntax->datum v) v) flatp))
  (set! line-count (add1 line-count))
  (when (>= line-count lines-per-file)
    (set! line-count 0)
    (set! file-count (add1 file-count))
    (close-output-port flatp)
    (set! flatp
	  (open-output-file
	   (format "flat~a.ss" file-count)
	   #:exists 'replace))))

(define error-test
  (case-lambda
   [(expr) (error-test expr #f)]
   [(expr exn?)
    (unless (or (eq? exn? exn:fail:syntax?)
		(syntax-case expr (define define-values define-syntax define-syntaxes)
		  [(define . _) #t]
		  [(define-values . _) #t]
		  [(define-syntax . _) #t]
		  [(define-syntaxes . _) #t]
		  [_else #f]))
      (let ([dexpr (syntax->datum expr)])
	(flat-pp 
	 `(thunk-error-test (lambda () ,dexpr)
			    (quote-syntax ,dexpr)
			    ,@(if exn?
				  (list (object-name exn?))
				  null)))))]))

(define building-flat-tests? #t)

(dynamic-wind
 (lambda () 
   (current-eval
    (lambda (e)
      (unless (or (syntax-case* e (load load-relative error-test unless) (lambda (a b)
									   (eq? (syntax-e a) (syntax-e b)))
		    [(load . _) #t]
		    [(load-relative . _) #t]
		    [(error-test . _) #t]
		    [(unless _ (load-relative s)) (string? (syntax-e (syntax s))) #t]
		    [else #f])
		  (compiled-expression? e)
		  (and (syntax? e) (compiled-expression? (syntax-e e)))
		  (not (eq? (current-namespace) old-namespace))
		  ;; Skip test use of `eval' on unprintable value:
		  (and (pair? e) (pair? (cdr e))
		       (eq? void (cadr e))))
	(flat-pp e))
      (if (syntax-case* e (quote hygiene) (lambda (a b)
					    (eq? (syntax-e a) (syntax-e b)))
	    [(_ __ 'hygiene . ___) #t]
	    [_else #f])
	  ;; Don't save the evaluated:
	  (parameterize ([current-eval old-eval])
	    (old-eval e))
	  ;; Normal:
	  (old-eval e)))))
 (lambda ()
   (load-relative flat-load))
 (lambda ()
   (current-eval old-eval)))
