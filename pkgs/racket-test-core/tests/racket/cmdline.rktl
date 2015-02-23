
(load-relative "loadtest.rktl")

(Section 'command-line)

(require racket/cmdline)

(define (r-append opt . rest)
  (append opt (list (list->vector rest))))

(test '("-bye" #())
      parse-command-line
      "test"
      #("--hi" "-bye")
      (list 
       (list
	'multi
	(list (list "--hi")
	      (lambda (flag v) v)
	      (list "Hello" "x"))))
      r-append
      '("arg"))

;; test that args can be a list instead of a vector
(test '("-bye" #())
      parse-command-line
      "test"
      '("--hi" "-bye")
      (list 
       (list
	'multi
	(list (list "--hi")
	      (lambda (flag v) v)
	      (list "Hello" "x"))))
      r-append
      '("arg"))

(test '("1" "2" #("3"))
      parse-command-line
      "test"
      #("-xi" "1" "2" "3")
      (list 
       (list
       'multi
	(list (list "-x" "-i")
	      (lambda (flag v) v)
	      (list "x or i" "x"))))
      r-append
      '("arg"))

(test '(("-x" "a" "b") ("-i") #())
      parse-command-line
      "test"
      #("-xi" "a" "b")
      (list 
       (list
	'multi
	(list (list "-x" "-i")
	      list
	      (list "xi"))))
      r-append
      '("arg"))

(test '("--simple" ("-x" . "a") ("-i" . "b") #())
      parse-command-line
      "test"
      #("--simple" "-xi" "a" "b")
      (list 
       (list
	'multi
	(list (list "--simple") (lambda (v) v) (list "S"))
	(list (list "-x" "-i")
	      cons
	      (list "xi" "v"))))
      r-append
      '("arg"))

(test '(("-x" "a" "c") ("-i" . "b") #("d"))
      parse-command-line
      "test"
      #("-xi" "a" "c" "b" "d")
      (list 
       (list
	'multi
	(list (list "-x")
	      (lambda (x y z) (list x y z))
	      (list "X" "y" "z"))
	(list (list "-i")
	      cons
	      (list "i" "v"))))
      r-append
      '("arg"))

(define (test-end-flags v include?)
  (test (list
	 (list->vector
	  (let ([l '("-xi" "--bad" "--")])
	    (if include?
		(cons v l)
		l))))
	parse-command-line
	"test"
	(vector v "-xi" "--bad" "--")
	(list 
	 (list
	  'multi
	  (list (list "-x" "-i")
		list
		(list "xi"))))
	r-append
	'("arg")))

(test-end-flags "1" #t)
(test-end-flags "+" #t)
(test-end-flags "-" #t)
(test-end-flags "--" #f)
(test-end-flags "-1" #t)
(test-end-flags "+1" #t)
(test-end-flags "-1.4" #t)
(test-end-flags "+1999.0" #t)

(define (test-bad-flag v name) ; -h and -i defined
  (test 'yes-it-worked
	(lambda (x-ignored y-ignored)
	  (with-handlers ([void
			   (lambda (exn)
			     (if (regexp-match
				  (format "unknown switch: ~a" name)
				  (exn-message exn))
				 'yes-it-worked
				 exn))])
	    (parse-command-line
	     "test"
	     (vector name "--")
	     (list 
	      (list
	       'multi
	       (list (list "-x" "-i")
		     list
		     (list "x i"))))
	     r-append
	     '("arg"))))
	v name))

(test-bad-flag "--ok" "--ok")
(test-bad-flag "-xbi" "-b")

(test (void) parse-command-line "test" #() null void '("arg"))
(test (void) parse-command-line "test" #() (list (list 'once-each (list null void '("")))) void '("arg"))
(test (void) parse-command-line "test" #() (list (list 'once-any (list null void '("")))) void '("arg"))
(test (void) parse-command-line "test" #() (list (list 'multi (list null void '("")))) void '("arg"))
(test (void) parse-command-line "test" #() (list (list 'multi)) void '("arg"))

(test "2" parse-command-line "test" #("1" "2") null (lambda (a b c) c) '("b" "c"))

(err/rt-test (parse-command-line 'test #() null void '("arg")))
(err/rt-test (parse-command-line "test" 9 null void '("arg")))
(err/rt-test (parse-command-line "test" #() (list 0) void '("arg")))
(err/rt-test (parse-command-line "test" #() (list (list 'malti)) void '("arg")))
(err/rt-test (parse-command-line "test" #() (list (list 'multi (list 0 void '("")))) void '("arg")))
(err/rt-test (parse-command-line "test" #() (list (list 'multi (list (list 0) void '("")))) void '("arg")))
(err/rt-test (parse-command-line "test" #() (list (list 'multi (list (list "hi") void '("")))) void '("arg")))
(err/rt-test (parse-command-line "test" #() (list (list 'multi (list (list "--") void '("")))) void '("arg")))
(err/rt-test (parse-command-line "test" #() (list (list 'multi (list (list "-xi") void '("")))) void '("arg")))
(err/rt-test (parse-command-line "test" #() (list (list 'multi (list (list "-x") (lambda () null) '("")))) void '("arg")))
(err/rt-test (parse-command-line "test" #() (list (list 'multi (list (list "--xi") void ""))) void '("arg")))
(err/rt-test (parse-command-line "test" #() (list (list 'multi (list (list "--xi") void '("" a)))) void '("arg")))
(err/rt-test (parse-command-line "test" #() null (lambda () null) null))

(err/rt-test (parse-command-line "test" #() null (lambda (x y) null) null) exn:fail?)

(test (void) 'cmdline
      (command-line 
       #:program "something"
       #:argv #("-ab")
       #:once-each
       [("-a") "ok" 5]
       [("-b" "--more") "Help" 7]))

;; test that keywords are compared for the literal symbol
(test "foo" 'cmdline
      (let ([once-each 3] [args "args"])
        (command-line 
         #:program "something" 
         #:argv #("-ab" "foo")
         #:once-each
         [("-a") "ok" 5]
         [("-b" "--more") "Help" 7]
         #:args
         (x)
         x)))

(syntax-test #'(command-line . x))
(syntax-test #'(command-line "hello"))
(err/rt-test (command-line #:program 'hello #:argv #("ok")))
(syntax-test #'(command-line #:program "hello" #:argv #("ok") #:bad))
(syntax-test #'(command-line #:program "hello" #:argv #("ok") (bad)))
(syntax-test #'(command-line #:program "hello" #:argv #("ok") #:once-any ()))
(syntax-test #'(command-line #:program "hello" #:argv #("ok") #:once-any [("-ok")]))
(syntax-test #'(command-line #:program "hello" #:argv #("ok") #:once-any ["-ok" "the ok flag"]))
(syntax-test #'(command-line #:program "hello" #:argv #("ok") #:once-any ["-ok" a "the ok flag"]))
(syntax-test #'(command-line #:program "hello" #:argv #("ok") #:once-any [("-ok") #() "the ok flag"]))
(syntax-test #'(command-line #:program "hello" #:argv #("ok") #:once-any [("-ok") a "the ok flag"] ()))
(syntax-test #'(command-line #:program "hello" #:argv #("ok") #:args 'done #:once-any [("-ok") a 7]))
(syntax-test #'(command-line #:program "hello" #:argv #("ok") #:args (ok) 'done #:once-any [("-ok") a "the ok flag" 7]))

(err/rt-test (parse-command-line "test" #("x") null (lambda () 'too-few) '("arg")))
(err/rt-test (parse-command-line "test" #("x") null (lambda (x) 'still-too-few) '("arg")))
(err/rt-test (parse-command-line "test" #("x") null (lambda (x y z) 'too-many) '("arg")))
(err/rt-test (parse-command-line "test" #("x") null (lambda (x y z . w) 'too-many) '("arg")))
(test 'ok parse-command-line "test" #("x") null (lambda (x y) 'ok) '("arg"))
(test 'ok parse-command-line "test" #("x") null (lambda (x . y) 'ok) '("arg"))
(test 'ok parse-command-line "test" #("x") null (lambda (x y . z) 'ok) '("arg"))
(test 'ok parse-command-line "test" #("x") null (case-lambda [(x) 'none] [(x y) 'ok]) '("arg"))
(test 'ok parse-command-line "test" #("x") null (case-lambda [(x) 'none] [(x . ys) 'ok]) '("arg"))
(test 'ok parse-command-line "test" #("x") null (case-lambda [(x) 'none] [(x y . z) 'ok]) '("arg"))

(report-errs)
