(module main mzscheme 
  (require (lib "launcher.ss" "launcher")
	   (lib "cmdline.ss")
	   (lib "unitsig.ss")
	   "debug.ss"
	   "test-suite-utils.ss")

  (define argv (current-command-line-arguments))

  (define preferences-file (find-system-path 'pref-file))

  (define old-preferences-file (let-values ([(base name _2) (split-path preferences-file)])
				 (build-path base (string-append (path->string name) ".save"))))
  

  (define all-files
    (map symbol->string
	 (load
	  (build-path
	   (collection-path "tests" "framework")
	   "README"))))

  (define all? #f)
  (define 3m? #f)
  (define files-to-process null)
  (define command-line-flags
    `((once-each
       [("--3m")
        ,(lambda (flag) (use-3m #t))
        ("Run the tests using a 3m mred")]
       [("-a" "--all")
	,(lambda (flag)
	   (set! all? #t))
	("Run all of the tests")])
      (multi
       [("-o" "--only")
	,(lambda (flag _only-these-tests)
	   (set-only-these-tests! (cons (string->symbol _only-these-tests)
					(or (get-only-these-tests) null))))
	("Only run test named <test-name>" "test-name")])))
      
  (parse-command-line "framework-test" argv command-line-flags
		      (lambda (collected . files)
			(set! files-to-process (if (or all? (null? files)) all-files files)))
		      `("Names of the tests; defaults to all tests"))

  (when (file-exists? preferences-file)
    (debug-printf admin "  saving preferences file ~s to ~s~n" preferences-file old-preferences-file)
    (if (file-exists? old-preferences-file)
	(debug-printf admin "  backup preferences file exists, using that one~n")
	(begin (copy-file preferences-file old-preferences-file)
	       (debug-printf admin "  saved preferences file~n"))))
      
  (with-handlers ([(lambda (x) #f)
		   (lambda (x) (display (exn-message x)) (newline))])
    (for-each
     (lambda (x)
       (when (member x all-files)
	 (shutdown-mred)
         (load-framework-automatically #t)
	 (let/ec k
	   (dynamic-wind
	    (lambda ()
	      (set-section-name! x)
	      (set-section-jump! k))
	    (lambda ()
	      (with-handlers ([(lambda (x) #t)
			       (lambda (exn)
				 (debug-printf schedule "~a~n" (if (exn? exn) (exn-message exn) exn)))])

		(debug-printf schedule "beginning ~a test suite~n" x)
		(dynamic-require `(lib ,x "tests" "framework") #f)
		(debug-printf schedule "PASSED ~a test suite~n" x)))
	    (lambda ()
	      (reset-section-name!)
	      (reset-section-jump!))))))
     files-to-process))

  (debug-printf admin "  restoring preferences file ~s to ~s~n" old-preferences-file preferences-file)
  (when (file-exists? preferences-file)
    (unless (file-exists? old-preferences-file)
      (error 'framework-test "lost preferences file backup!"))
    (delete-file preferences-file)
    (copy-file old-preferences-file preferences-file)
    (delete-file old-preferences-file))
  (debug-printf admin "  restored preferences file~n")

  (shutdown-listener)

  (cond
   [(null? failed-tests)
    (printf "All tests passed.~n")]
   [else
    (debug-printf schedule "FAILED tests:~n")
    (for-each (lambda (failed-test)
		(debug-printf schedule "  ~a // ~a~n" (car failed-test) (cdr failed-test)))
	      failed-tests)]))
