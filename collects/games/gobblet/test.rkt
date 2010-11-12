
(module test mzscheme
  (provide test report-test-results)
  
  (define failed? #f)
  (define (set-failed!) (set! failed? #t))

  (define-syntax test
    (syntax-rules ()
      [(_ expect expr)
       (begin
	 (printf "~s =>" 'expr)
	 (flush-output)
	 (let ([v expr]
	       [ex expect])
	   (printf " ~s" v)
	   (unless (equal? v ex)
	     (set-failed!)
	     (printf " EXPECTED ~s" ex)
	     (exit))
	   (printf "\n")))]))

  (define (report-test-results)
    (printf (if failed?
		"\nTESTS FAILED\n"
		"\nAll tests passed.\n"))))

