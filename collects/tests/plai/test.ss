
(define (run-one-test lang src mod-only?)
  (printf "Trying ~a ~a\n" lang src)
  (let ([prog (with-input-from-file src
		(lambda ()
		  (let loop ()
		    (let ([v (read)])
		      (if (eof-object? v)
			  null
			  (cons v (loop)))))))])
    (parameterize ([current-namespace (make-namespace)])
      (eval `(module m (lib ,lang "plai")
	       ,@prog))
      (eval `(require m)))
    (unless mod-only?
      (let ([n (current-namespace)])
	(parameterize ([current-namespace (make-namespace 'empty)])
	  (namespace-attach-module n 'mzscheme)
	  (namespace-require `(lib ,lang "plai"))
	  (for-each (lambda (v) (printf "~e\n" (eval v))) prog))))))

(run-one-test "plai-beginner.ss" "arith-interp.scm" #t)
(run-one-test "plai-intermediate.ss" "arith-interp.scm" #f)
(for-each (lambda (src)
	    (run-one-test "plai-advanced.ss" src #f))
	  '("all.scm"
	    "arith-interp.scm"
	    "hof-env-buggy.scm"
	    "hof-subst.scm"
	    "subst.scm"))
