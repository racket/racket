#|

add this test:


There shouldn't be any error (but add in a bug that triggers one to be sure!)

|#
(module repl-test mzscheme
  (require "drscheme-test-util.ss"
           (lib "class.ss")
           (lib "file.ss")
           (lib "mred.ss" "mred")
           (lib "framework.ss" "framework"))
  
  (provide run-test)
  
  (define-struct loc (line col offset))
  ;; loc = (make-loc number number number)
  ;; all numbers in loc structs start at zero.
  
  (define-struct test (program
                       ;; : (union 
                       ;;    string 
                       ;;    'fraction-sum
                       ;;    (listof
                       ;;      (union string?  // typed in
                       ;;             'left    // left arrow key
                       ;;             (list string? string?)))) // menu item select
                       
                       raw-execute-answer  ;; answer when executing without debugging
                       raw-load-answer     ;; answer when loading after executing (w/out debugging)
                       err-execute-answer  ;; answer when executing with debugging
                       err-load-answer     ;; answer when loading after executing (with debugging)
                       
                       source-location ;; : (union 'definitions
                       ;;                          'interactions
                       ;;                          (cons loc loc))
                       ;; if cons, the car and cdr are the start and end positions resp.
                       ;; if 'interactions, no source location and
                       ;;    the focus must be in the interactions window
                       ;; if 'definitions, no source location and
                       ;;     the focus must be in the definitions window
                       
                       breaking-test?              ;; : boolean
                       
                       ;; setup is called before the test case is run.
                       setup ;; : -> void
                       ;; teardown is called after the test case is complete.
                       teardown ;; : -> void
                       ))
  
  (define (to-strings . args)
    (apply string-append (map (λ (x) (format "~s\n" x)) args)))
  
  (define test-data
    (list

     ;; basic tests
     (make-test "1"
                "1"
                "1"
                "1"
                "1"
                'interactions
		#f
                void
                void)
     
     (make-test "\"a\""
                "\"a\""
                "\"a\""
                "\"a\""
                "\"a\""
                'interactions
		#f
                void
                void)
     
     (make-test "1 2"
                "1\n2"
                "2"
                "1\n2"
                "2"
                'interactions
		#f
                void
                void)
     
     (make-test "\"a\" \"b\""
                "\"a\"\n\"b\""
                "\"b\""
                "\"a\"\n\"b\""
                "\"b\""
                'interactions
		#f
                void
                void)
     
     (make-test "("
                "{bug09.gif} read: expected a `)'"
                "{bug09.gif} {file.gif} repl-test-tmp.ss:1:0: read: expected a `)'"
                "read: expected a `)'"
                "{bug09.gif} {file.gif} repl-test-tmp.ss:1:0: read: expected a `)'"
                (cons (make-loc 0 0 0) (make-loc 0 1 1))
		#f
                void
                void)
     
     (make-test "."
                "{bug09.gif} read: illegal use of \".\""
                "{bug09.gif} {file.gif} repl-test-tmp.ss:1:0: read: illegal use of \".\""
                "read: illegal use of \".\""
                "{bug09.gif} {file.gif} repl-test-tmp.ss:1:0: read: illegal use of \".\""
                (cons (make-loc 0 0 0) (make-loc 0 1 1))
                #f
		void
                void)

     (make-test "(lambda ())"
                "lambda: bad syntax in: (lambda ())"
                "{file.gif} repl-test-tmp.ss:1:0: lambda: bad syntax in: (lambda ())"
                "lambda: bad syntax in: (lambda ())"
                "{bug09.gif} {file.gif} repl-test-tmp.ss:1:0: lambda: bad syntax in: (lambda ())"
                (cons (make-loc 0 0 0) (make-loc 0 11 11))
		#f
                void
                void)
     (make-test "xx"
                "reference to undefined identifier: xx"
                "reference to undefined identifier: xx"
                "{bug09.gif} reference to undefined identifier: xx"
                "{bug09.gif} {file.gif} repl-test-tmp.ss::1: reference to undefined identifier: xx"
                (cons (make-loc 0 0 0) (make-loc 0 2 2))
		#f
                void
                void)
     (make-test "(raise 1)"
                "uncaught exception: 1"
                "uncaught exception: 1"
                "uncaught exception: 1"
                "uncaught exception: 1"
                'interactions
                #f
                void
                void)
     (make-test "(raise #f)"
                "uncaught exception: #f"
                "uncaught exception: #f"
                "uncaught exception: #f"
                "uncaught exception: #f"
		'interactions
		#f
                void
                void)
     
     (make-test "(values 1 2)"
                "1\n2"
                "1\n2"
                "1\n2"
                "1\n2"
		'interactions
                #f
                void
                void)
     (make-test "(list 1 2)"
                "(1 2)"
                "(1 2)"
                "(1 2)"
                "(1 2)"
		'interactions
		#f
                void
                void)
     
     (make-test "(parameterize ([print-struct #t])(define-struct s (x) (make-inspector))(printf \"~s\\n\" (make-s 1)))"
                "#(struct:s 1)"
                "#(struct:s 1)"
                "#(struct:s 1)"
                "#(struct:s 1)"
                'interactions
		#f
                void
                void)
     
     ;; top-level semantics test
     (make-test "(define (f) (+ 1 1)) (define + -) (f)"
                "0"
                "0"
                "0"
                "0"
		'interactions
                #f
                void
                void)

     (make-test "(begin (define-struct a ()) (define-struct (b a) ()))"
		""
		""
                ""
                ""
		'interactions
		#f
                void
                void)
     
     (make-test "(begin (values) 1)"
		"1"
		"1"
                "1"
                "1"
		'interactions
		#f
                void
                void)

#|
     ;; syntax error template
     "{bug09.gif} "
     "{bug09.gif} {file.gif} repl-test-tmp.ss:1:0: "
     ""
     "{bug09.gif} {file.gif} repl-test-tmp.ss:1:0: "
     |#           
     
     (make-test (string-append
                 "(module m mzscheme (provide e) (define e #'1))\n"
                 "(module n mzscheme (require-for-syntax m) (provide s) (define-syntax (s stx) e))\n"
                 "(require n)\n"
                 "s")
                "compile: bad syntax; literal data is not allowed, because no #%datum syntax transformer is bound in: 1"
                "{file.gif} repl-test-tmp.ss:1:43: compile: bad syntax; literal data is not allowed, because no #%datum syntax transformer is bound in: 1"
                "compile: bad syntax; literal data is not allowed, because no #%datum syntax transformer is bound in: 1"
                "{bug09.gif} {file.gif} repl-test-tmp.ss:1:43: compile: bad syntax; literal data is not allowed, because no #%datum syntax transformer is bound in: 1"
                (cons (make-loc 0 43 43) (make-loc 0 44 44))
                #f
                void
                void)

     
     ;; leading comment test
     (make-test "#!\n1"
                "1"
                "1"
                "1"
                "1"
		'interactions
		#f
                void
                void)

     (make-test "#!/bin/sh\nxx"
                "reference to undefined identifier: xx"
                "reference to undefined identifier: xx"
                "{bug09.gif} reference to undefined identifier: xx"
                "{bug09.gif} {file.gif} repl-test-tmp.ss::11: reference to undefined identifier: xx"
                (cons (make-loc 1 0 10) (make-loc 1 2 12))
		#f
                void
                void)
     
     (make-test (list "#!\n" 
                      '("Special" "Insert XML Box")
                      "<a>")
                "(a ())"
                "(a ())"
                "(a ())"
                "(a ())"
		'interactions
                #f
                void
                void)

  #|
   ;; XML tests
     (make-test
      '(("Special" "Insert XML Box")
        "<a>")
      "(a ())"
      "(a ())"
      #f
      'interactions
      #f
     #f
      void
      void)
     
     (make-test
      '(("Special" "Insert XML Box")
        "<a>"
        ("Special" "Insert Scheme Box")
        "1")
      "(a () 1)"
      "(a () 1)"
      #f
      'interactions
      #f
     #f
      void
      void)
     
     (make-test
      '(("Special" "Insert XML Box")
        "<a>"
        ("Special" "Insert Scheme Splice Box")
        "'(1)")
      "(a () 1)"
      "(a () 1)"
      #f
      'interactions
      #f
     #f
      void
      void)
     
     (make-test
      '(("Special" "Insert XML Box")
        "<a>"
        ("Special" "Insert Scheme Splice Box")
        "1")
      "scheme-splice-box: expected a list, found: 1"
      "scheme-splice-box: expected a list, found: 1"
      #t
      'definitions
      #f
     #f
      void
      void)
|#     
     ;; eval tests

     (make-test "    (eval '(values 1 2))"
                "1\n2"
                "1\n2"
                "1\n2"
                "1\n2"
		'interactions
                #f
                void
                void)

     (make-test "    (eval '(list 1 2))"
                "(1 2)"
                "(1 2)"
                "(1 2)"
                "(1 2)"
		'interactions
                #f
                void
                void)
     
     (make-test "    (eval '(lambda ()))"
                "lambda: bad syntax in: (lambda ())"
                "lambda: bad syntax in: (lambda ())"
                "{bug09.gif} lambda: bad syntax in: (lambda ())"
                "{bug09.gif} {file.gif} repl-test-tmp.ss::5: lambda: bad syntax in: (lambda ())"
                (cons (make-loc 0 4 4) (make-loc 0 23 23))
                #f
                void
                void)
     (make-test "    (eval 'x)"
                "reference to undefined identifier: x"
                "reference to undefined identifier: x"
                "{bug09.gif} reference to undefined identifier: x"
                "{bug09.gif} {file.gif} repl-test-tmp.ss::5: reference to undefined identifier: x"
		(cons (make-loc 0 4 4) (make-loc 0 13 13))
		#f
                void
                void)
     
     (make-test "(eval (box 1))"
		"#&1"
		"#&1"
                "#&1"
                "#&1"
		'interactions
		#f
                void
                void)
     
     (make-test "(eval '(box 1))"
		"#&1"
		"#&1"
		"#&1"
		"#&1"
		'interactions
		#f
                void
                void)
     ; printer setup test
     (make-test "(car (void))"
                "car: expects argument of type <pair>; given #<void>"
                "car: expects argument of type <pair>; given #<void>"
                "{bug09.gif} car: expects argument of type <pair>; given #<void>"
                "{bug09.gif} {file.gif} repl-test-tmp.ss::1: car: expects argument of type <pair>; given #<void>"
                (cons (make-loc 0 0 0) (make-loc 0 12 12))
                #f
                void
                void)
     
     ;; error in the middle
     (make-test "1 2 ( 3 4"
                "1\n2\n{bug09.gif} read: expected a `)'"
                "{bug09.gif} {file.gif} repl-test-tmp.ss:1:4: read: expected a `)'"
                "1\n2\nread: expected a `)'"
                "{bug09.gif} {file.gif} repl-test-tmp.ss:1:4: read: expected a `)'"
		(cons (make-loc 0 4 4) (make-loc 0 9 9))
		#f
                void
                void)
     (make-test "1 2 . 3 4"
                "1\n2\n{bug09.gif} read: illegal use of \".\""
                "{bug09.gif} {file.gif} repl-test-tmp.ss:1:4: read: illegal use of \".\""
                "1\n2\nread: illegal use of \".\""
                "{bug09.gif} {file.gif} repl-test-tmp.ss:1:4: read: illegal use of \".\""
		(cons (make-loc 0 4 4) (make-loc 0 5 5))
		#f
                void
                void)
     (make-test "1 2 (lambda ()) 3 4"
                "1\n2\nlambda: bad syntax in: (lambda ())"
                "{file.gif} repl-test-tmp.ss:1:4: lambda: bad syntax in: (lambda ())"
                "1\n2\nlambda: bad syntax in: (lambda ())"
                "{bug09.gif} {file.gif} repl-test-tmp.ss:1:4: lambda: bad syntax in: (lambda ())"
		(cons (make-loc 0 4 4) (make-loc 0 15 15))
                #f
                void
                void)
     (make-test "1 2 x 3 4"
                "1\n2\nreference to undefined identifier: x"
                "reference to undefined identifier: x"
                "1\n2\n{bug09.gif} reference to undefined identifier: x"
                "{bug09.gif} {file.gif} repl-test-tmp.ss::5: reference to undefined identifier: x"
		(cons (make-loc 0 4 4) (make-loc 0 5 5))
                #f
                void
                void)
     (make-test "1 2 (raise 1) 3 4"
                "1\n2\nuncaught exception: 1"
                "uncaught exception: 1"
                "1\n2\nuncaught exception: 1"
                "uncaught exception: 1"
		'interactions
		#f
                void
                void)
     (make-test "1 2 (raise #f) 3 4"
                "1\n2\nuncaught exception: #f"
                "uncaught exception: #f"
                "1\n2\nuncaught exception: #f"
                "uncaught exception: #f"
		'interactions
		#f
                void
                void)

     ;; error across separate files
     (make-test
      "(load \"repl-test-tmp2.ss\") (define (g) (+ 1 (car 1))) (f g)"
      "{bug09.gif} car: expects argument of type <pair>; given 1"
      "{bug09.gif} {file.gif} repl-test-tmp.ss:1:27: car: expects argument of type <pair>; given 1"
      "{bug09.gif} car: expects argument of type <pair>; given 1"
      "{bug09.gif} {file.gif} repl-test-tmp.ss::45: car: expects argument of type <pair>; given 1"
      (cons (make-loc -1 -1 44)
            (make-loc -1 -1 51))
      #f
      (λ ()
        (call-with-output-file (build-path tmp-load-directory "repl-test-tmp2.ss")
          (lambda (port)
            (write '(define (f t) (+ 1 (t)))
                   port))
          'truncate))
      (λ () (delete-file (build-path tmp-load-directory "repl-test-tmp2.ss"))))
     
     ;; new namespace test
     (make-test "(current-namespace (make-namespace))\nif"
                "if: bad syntax in: if"
                "{file.gif} repl-test-tmp.ss:2:0: if: bad syntax in: if"
                "if: bad syntax in: if"
                "{bug09.gif} {file.gif} repl-test-tmp.ss:2:0: if: bad syntax in: if"
		(cons (make-loc 1 0 37) (make-loc 1 2 39))
		#f
                void
                void)
     
     (make-test "(current-namespace (make-namespace 'empty))\nif"
                "compile: bad syntax; reference to top-level identifier is not allowed, because no #%top syntax transformer is bound in: if"
                "{file.gif} repl-test-tmp.ss:2:0: compile: bad syntax; reference to top-level identifier is not allowed, because no #%top syntax transformer is bound in: if"
                "compile: bad syntax; reference to top-level identifier is not allowed, because no #%top syntax transformer is bound in: if"
                "{bug09.gif} {file.gif} repl-test-tmp.ss:2:0: compile: bad syntax; reference to top-level identifier is not allowed, because no #%top syntax transformer is bound in: if"
		(cons (make-loc 1 0 44) (make-loc 1 0 46))
		#f
                void
                void)
     
     ;; macro tests
     (make-test "(define-syntax (c stx) (syntax-case stx () [(_ p q r) (syntax (+ p q r))]))"
		""
                ""
                ""
		""
		'interactions
		#f
                void
                void)
     
     ;; error escape handler test
     (make-test
      "(let ([old (error-escape-handler)])\n(+ (let/ec k\n(dynamic-wind\n(lambda () (error-escape-handler (lambda () (k 5))))\n(lambda () (car))\n(lambda () (error-escape-handler old))))\n10))"
      "car: expects 1 argument, given 0\n15"
      "car: expects 1 argument, given 0\n15"
      "{bug09.gif} car: expects 1 argument, given 0\n15"
      "{bug09.gif} {file.gif} repl-test-tmp.ss::153: car: expects 1 argument, given 0\n15"
      'definitions
      #f
      void
      void)
     
     ; fraction snip test
     ;; this test depends on the state of the 'framework:fraction-snip-style preference
     ;; make sure this preference is set to the default when running this test.
     (make-test 'fraction-sum
                #rx"{number 5/6 \"5/6\" (improper|mixed)}"
                #rx"{number 5/6 \"5/6\" (improper|mixed)}"
                #rx"{number 5/6 \"5/6\" (improper|mixed)}"
                #rx"{number 5/6 \"5/6\" (improper|mixed)}"
                'interactions
		#f
                void
                void)
     
     ;; should produce a syntax object with a turn-down triangle.
     (make-test "(write (list (syntax x)))" 
                "({embedded \".#<syntax:1:21>\"})"
                "({embedded \".#<syntax:/Users/robby/svn/plt/collects/tests/drscheme/repl-test-tmp.ss:1:21>\"})"
                "({embedded \".#<syntax:1:21>\"})"
                "({embedded \".#<syntax:/Users/robby/svn/plt/collects/tests/drscheme/repl-test-tmp.ss:1:21>\"})"
                'interactions
		#f
                void
                void)
     
     ;; make sure syntax objects only go into good ports
     (make-test "(define-syntax (foo stx) (with-handlers ([exn:fail? (lambda (x) #'10)]) (syntax-local-value #'foot))) (foo)"
                "10"
                "10"
                "10"
                "10"
                'interactions
		#f
                void
                void)
     
     ;; make sure syntax objects don't go into bad ports
     (make-test "(parameterize ([current-output-port (open-output-string)]) (write #'1))"
                ""
                ""
                ""
                ""
                'interactions
		#f
                void
                void)
     
     (make-test "(parameterize ([current-output-port (open-output-string)]) (fprintf (current-error-port) \"~e\" #'foot))"
                "#<syntax:1:96>"
                "#<syntax:/Users/robby/svn/plt/collects/tests/drscheme/repl-test-tmp.ss:1:96>"
                "#<syntax:1:96>"
                "#<syntax:/Users/robby/svn/plt/collects/tests/drscheme/repl-test-tmp.ss:1:96>"
                'interactions
		#f
                void
                void)
     
     (make-test "(write-special 1)"
                "1#t"
                "1#t"
                "1#t"
                "1#t"
                'interactions
		#f
                void
                void)

     (make-test
      ;; the begin/void combo is to make sure that no value printout
      ;; comes and messes up the source location for the error.
      "(define s (make-semaphore 0))\n(queue-callback\n(lambda ()\n(dynamic-wind\nvoid\n(lambda () (car))\n(lambda () (semaphore-post s)))))\n(begin (yield s) (void))"
      "car: expects 1 argument, given 0"
      "car: expects 1 argument, given 0"
      "{bug09.gif} car: expects 1 argument, given 0"
      "{bug09.gif} {file.gif} repl-test-tmp.ss::100: car: expects 1 argument, given 0"
      (cons (make-loc 0 99 99) (make-loc 0 104 104))
      #f
      void
      void)


     ;; breaking tests
     (make-test "(semaphore-wait (make-semaphore 0))"
                #rx"user break$"
                #rx"user break$"
                #rx"user break$"
                #rx"user break$"
                (cons (make-loc 0 0 0) (make-loc 0 35 35))
                #t
                void
                void)
     
     (make-test "(let l()(l))"
                #rx"user break$"
                #rx"user break$"
                #rx"user break$"
                #rx"user break$"
		(cons (make-loc 0 8 8) (make-loc 0 11 11))
                #t
                void
                void)
     
     ;; continuation tests
     (make-test "(define k (call/cc (lambda (x) x)))\n(k 17)\nk"
		"17"
                "17"
                "17"
                "17"
		'interactions
		#f
                void
                void)
     (make-test "(define v (vector (call/cc (lambda (x) x))))\n((vector-ref v 0) 2)\nv"
		"#1(2)"
                "#1(2)"
                "#1(2)"
                "#1(2)"
		'interactions
		#f
                void
                void)
     (make-test "(define v (vector (eval '(call/cc (lambda (x) x)))))\n((vector-ref v 0) 2)\nv"
		"#1(2)"
                "#1(2)"
                "#1(2)"
                "#1(2)"
		'interactions
		#f
                void
                void)

     (make-test "(define x 1)\n((λ (x y) y) (set! x (call/cc (lambda (x) x)))\n(x 3))"
		"procedure application: expected procedure, given: 3; arguments were: 3"
                "procedure application: expected procedure, given: 3; arguments were: 3"
                "{bug09.gif} procedure application: expected procedure, given: 3; arguments were: 3"
                "{bug09.gif} {file.gif} repl-test-tmp.ss::74: procedure application: expected procedure, given: 3; arguments were: 3"
		(cons (make-loc 3 19 73) (make-loc 3 24 78))
		#f
                void
                void)
     
     ;; top-level & continuation interaction test
     (make-test "(begin (define k (call/cc (λ (x) x)))\n(define x 'wrong))\n(set! x 'right)\n(k 1)\nx"
                "right"
                "right"
                "right"
                "right"
                'interactions
                #f
                void
                void)
     
     ;; graphical lambda tests
     (make-test (list "((" '("Special" "Insert λ") "(x) x) 1)")
                "1"
                "1"
                "1"
                "1"
                'interactions
		#f
                void
                void)
     
     (make-test (list "(" '("Special" "Insert λ") "())")
                "λ: bad syntax in: (λ ())"
                "{file.gif} repl-test-tmp.ss:1:0: λ: bad syntax in: (λ ())"
                "λ: bad syntax in: (λ ())"
                "{bug09.gif} {file.gif} repl-test-tmp.ss:1:0: λ: bad syntax in: (λ ())"
                (cons (make-loc 0 0 0) (make-loc 0 5 5))
		#f
                void
                void)
     
     ;; thread tests
     (make-test "(begin (thread (lambda () x)) (sleep 1/10))"
                "reference to undefined identifier: x"
                "reference to undefined identifier: x"
                "{bug09.gif} reference to undefined identifier: x"
                "{bug09.gif} {file.gif} repl-test-tmp.ss::27: reference to undefined identifier: x"
                (cons (make-loc 0 26 26) (make-loc 0 27 27))
                #f
                void
                void)

     
     ;; setup of the namespaces for pict printing (from slideshow)
     
     (make-test "(require (lib \"utils.ss\" \"texpict\"))(disk 3)"
                "{unknown snip: #<struct:object:pict-value-snip%>}\n"
                "{unknown snip: #<struct:object:pict-value-snip%>}\n"
                "{unknown snip: #<struct:object:pict-value-snip%>}\n"
                "{unknown snip: #<struct:object:pict-value-snip%>}\n"
                'interactions
                #f
                void
                void)
     
     (make-test (to-strings
                 '(require (lib "utils.ss" "texpict"))
                 '(let ()
                    (current-namespace (make-namespace))
                    (namespace-set-variable-value! 'd (disk 3)))
                 'd)
                "{unknown snip: #<struct:object:pict-value-snip%>}\n"
                "{unknown snip: #<struct:object:pict-value-snip%>}\n"
                "{unknown snip: #<struct:object:pict-value-snip%>}\n"
                "{unknown snip: #<struct:object:pict-value-snip%>}\n"
                'interactions
                #f
                void
                void)
     (make-test (to-strings
                 '(let ([on (current-namespace)]
                        [n ((current-module-name-resolver) '(lib "mred.ss" "mred") #f #f)])
                    (current-namespace (make-namespace))
                    (namespace-attach-module on n))
                 '(require (lib "utils.ss" "texpict"))
                 '(disk 3))
                "#<struct:pict>"
                "#<struct:pict>"
                "#<struct:pict>"
                "#<struct:pict>"
                'interactions
                #f
                void
                void)
     
     (make-test (string-append
                 "(require (lib \"pretty.ss\"))"
                 "(pretty-print-print-hook (lambda x (car)))"
                 "(list 1 2 3)")
                "(1 2 3)"
                "(1 2 3)"
                "(1 2 3)"
                "(1 2 3)"
                'interactions
                #f
                void
                void)
     ))
  
  (define backtrace-image-string "{bug09.gif}")
  (define file-image-string "{file.gif}")

  (define tmp-load-directory
    (normal-case-path
     (normalize-path 
      (collection-path "tests" "drscheme"))))
  
  (define (run-test)
    
    (define drscheme-frame (wait-for-drscheme-frame))
    
    (define interactions-text (send drscheme-frame get-interactions-text))
    (define interactions-canvas (send drscheme-frame get-interactions-canvas))
    (define definitions-text (send drscheme-frame get-definitions-text))
    (define definitions-canvas (send drscheme-frame get-definitions-canvas))
    (define execute-button (send drscheme-frame get-execute-button))
    
    (define (insert-string string)
      (let loop ([n 0])
        (unless (= n (string-length string))
          (let ([c (string-ref string n)])
            (if (char=? c #\newline)
                (test:keystroke #\return)
                (test:keystroke c)))
          (loop (+ n 1)))))
    
    (define wait-for-execute (lambda () (wait-for-button execute-button)))
    (define get-int-pos (lambda () (get-text-pos interactions-text)))
    
    (define tmp-load-short-filename "repl-test-tmp.ss")
    (define tmp-load-filename (build-path tmp-load-directory tmp-load-short-filename))
    
    (define short-tmp-load-filename
      (let-values ([(base name dir?) (split-path tmp-load-filename)])
        (path->string name)))
    
    ;; setup-fraction-sum-interactions : -> void 
    ;; clears the definitions window, and executes `1/2' to
    ;; get a fraction snip in the interactions window.
    ;; Then, copies that and uses it to construct the sum
    ;; of the 1/2 image and 1/3.
    (define (setup-fraction-sum-interactions)
      (clear-definitions drscheme-frame)
      (type-in-definitions drscheme-frame "1/2")
      (do-execute drscheme-frame)
      (let ([s (make-semaphore 0)])
        (queue-callback
         (lambda ()
           (let* ([start (send interactions-text paragraph-start-position 2)]
                  ;; since the fraction is supposed to be one char wide, we just
                  ;; select one char, so that, if the regular number prints out,
                  ;; this test will fail.
                  [end (+ start 1)])
             (send interactions-text set-position start end)
             (semaphore-post s))))
        (semaphore-wait s))
      (test:menu-select "Edit" "Copy")
      (clear-definitions drscheme-frame)
      (type-in-definitions drscheme-frame "(+ ")
      (test:menu-select "Edit" "Paste")
      (type-in-definitions drscheme-frame " 1/3)"))
    
    ; given a filename "foo", we perform two operations on the contents 
    ; of the file "foo.ss".  First, we insert its contents into the REPL
    ; directly, and second, we use the load command.  We compare the
    ; the results of these operations against expected results.
    (define ((run-single-test execute-text-start escape raw?) in-vector)
      (let* ([program (test-program in-vector)]
             [execute-answer (if raw?
                                 (test-raw-execute-answer in-vector)
                                 (test-err-execute-answer in-vector))]
             [load-answer (if raw?
                              (test-raw-load-answer in-vector)
                              (test-err-load-answer in-vector))]
             [source-location (test-source-location in-vector)]
             [setup (test-setup in-vector)]
             [teardown (test-teardown in-vector)]
             [start-line (and (pair? source-location)
                              (number->string (+ 1 (loc-line (car source-location)))))]
             [start-col (and (pair? source-location)
                             (number->string (loc-col (car source-location))))]
             [start-pos (and (pair? source-location)
                             (number->string (+ 1 (loc-offset (car source-location)))))]
             [breaking-test? (test-breaking-test? in-vector)])
        
        (setup)
        
        (clear-definitions drscheme-frame)
        ; load contents of test-file into the REPL, recording
        ; the start and end positions of the text
        
        (cond
          [(string? program)
           (insert-string program)]
          [(eq? program 'fraction-sum)
           (setup-fraction-sum-interactions)]
          [(list? program)
           (for-each
            (lambda (item)
              (cond
                [(string? item) (insert-string item)]
                [(eq? item 'left)
                 (send definitions-text 
                       set-position
                       (- (send definitions-text get-start-position) 1)
                       (- (send definitions-text get-start-position) 1))]
                [(pair? item) (apply test:menu-select item)]))
            program)])
        
        (do-execute drscheme-frame #f)
        (when breaking-test?
          (test:button-push (send drscheme-frame get-break-button)))
        (wait-for-execute)
        
        (let* ([execute-text-end (- (get-int-pos) 1)] ;; subtract one to skip last newline
               [received-execute
                (fetch-output drscheme-frame execute-text-start execute-text-end)])
          
          ; check focus and selection for execute test
          (unless raw?
            (cond
              [(eq? source-location 'definitions)
               (unless (send definitions-canvas has-focus?)
                 (printf "FAILED execute test for ~s\n  expected definitions to have the focus\n"
                         program))]
              [(eq? source-location 'interactions)
               (unless (send interactions-canvas has-focus?)
                 (printf "FAILED execute test for ~s\n  expected interactions to have the focus\n"
                         program))]
              [(send definitions-canvas has-focus?)
               (let ([start (car source-location)]
                     [finish (cdr source-location)])
                 (let* ([error-ranges (send interactions-text get-error-ranges)]
                        [error-range (and error-ranges
                                          (not (null? error-ranges))
                                          (car error-ranges))])
                   (unless (and error-range
                                (= (+ (srcloc-position error-range) -1) (loc-offset start))
                                (= (+ (srcloc-position error-range) -1 (srcloc-span error-range)) (loc-offset finish)))
                     (printf "FAILED execute test for ~s\n  error-range is ~s\n  expected ~s\n"
                             program
                             (list (+ (srcloc-position error-range) -1)
                                   (+ (srcloc-position error-range) -1 (srcloc-span error-range)))
                             (list (loc-offset start)
                                   (loc-offset finish))))))]))
          
          ; check text for execute test
          (next-test)
          (unless (cond
                    [(string? execute-answer)
                     (string=? execute-answer received-execute)]
                    [(regexp? execute-answer)
                     (regexp-match execute-answer received-execute)]
                    [else #f])
            (failure)
            (printf "FAILED execute test for ~s (~a)\n  expected: ~s\n       got: ~s\n"
                    program
                    raw?
                    execute-answer received-execute))
          
          (test:new-window interactions-canvas)
          
          ; save the file so that load is in sync
          (test:menu-select "File" "Save Definitions")
          
          ; make sure that a prompt is available at end of the REPL
          (unless (and (char=? #\>
                               (send interactions-text get-character
                                     (- (send interactions-text last-position) 2)))
                       (char=? #\space
                               (send interactions-text get-character
                                     (- (send interactions-text last-position) 1))))
            (test:keystroke #\return))
          
          ; in order to erase the state in the namespace already, we clear (but don't save!)
          ; the definitions and click execute with the empty buffer
          (test:new-window definitions-canvas)
          (test:menu-select "Edit" "Select All")
          (test:menu-select "Edit" "Delete")
          (do-execute drscheme-frame #f)
          (wait-for-execute)
          
          ; stuff the load command into the REPL 
          (for-each test:keystroke
                    (string->list (format "(load ~s)" tmp-load-short-filename)))
          
          ; record current text position, then stuff a CR into the REPL
          (let ([load-text-start (+ 1 (send interactions-text last-position))])
            
            (test:keystroke #\return)
            
            (when breaking-test?
              (test:button-push (send drscheme-frame get-break-button)))
            (wait-for-execute)
            
            (let* ([load-text-end (- (get-int-pos) 1)] ;; subtract one to eliminate newline
                   [received-load 
                    (fetch-output drscheme-frame load-text-start load-text-end)])
              
              ; check load text 
              (next-test)
              (unless (cond
                        [(string? load-answer)
                         (string=? load-answer received-load)]
                        [(regexp? load-answer)
                         (regexp-match load-answer received-load)]
                        [else #f])
                (failure)
                (printf "FAILED load test for ~s\n  expected: ~s\n       got: ~s\n"
                        program load-answer received-load))))
          
          (teardown)
          
          ; check for edit-sequence
          (when (repl-in-edit-sequence?)
            (printf "FAILED: repl in edit-sequence")
            (escape)))))

    (define tests 0)
    (define failures 0)
    (define (next-test) (set! tests (+ tests 1)))
    (define (failure) (set! failures (+ failures 1)))
    (define (reset) (set! tests 0) (set! failures 0))
    (define (final-report)
      (if (= 0 failures)
          (printf "tests finished: ALL ~a TESTS PASSED\n" tests)
          (printf "tests finished: ~a failed out of ~a total\n" failures tests)))
    
    (define (run-test-in-language-level raw?)
      (let ([level (list "PLT" (regexp "Graphical"))])
        (printf "running tests ~a debugging\n" (if raw? "without" "with"))
        (if raw?
            (begin
              (set-language-level! level #f)
              (test:set-radio-box-item! "No debugging or profiling")
              (let ([f (get-top-level-focus-window)])
                (test:button-push "OK")
                (wait-for-new-frame f)))
            (set-language-level! level))
        
        (test:new-window definitions-canvas)
        (clear-definitions drscheme-frame)
        (do-execute drscheme-frame)
        (let/ec escape 
          (for-each (run-single-test (get-int-pos) escape raw?) test-data))))
    
    (define (kill-tests)
      (clear-definitions drscheme-frame)
      (do-execute drscheme-frame)
      
      (test:menu-select "Scheme" "Kill")
      
      (let ([win (wait-for-new-frame drscheme-frame)])
        (test:button-push "OK")
        (let ([drs2 (wait-for-new-frame win)])
          (unless (eq? drs2 drscheme-frame)
            (error 'kill-test1 "expected original drscheme frame to come back to the front"))))
      
      (type-in-definitions drscheme-frame "(kill-thread (current-thread))")
      (do-execute drscheme-frame #f)
      (let ([win (wait-for-new-frame drscheme-frame)])
        (test:button-push "OK")
        (let ([drs2 (wait-for-new-frame win)])
          (unless (eq? drs2 drscheme-frame)
            (error 'kill-test2 "expected original drscheme frame to come back to the front"))))
      
      (clear-definitions drscheme-frame)
      (do-execute drscheme-frame)
      (type-in-definitions
       drscheme-frame
       "(define (f) (queue-callback f) (error 'ouch)) (f)")
      (do-execute drscheme-frame #f)
      (sleep 1/2)
      (test:menu-select "Scheme" "Kill")
      (let ([win (wait-for-new-frame drscheme-frame null 360)])
        (test:button-push "OK")
        (let ([drs2 (wait-for-new-frame win)])
          (unless (eq? drs2 drscheme-frame)
            (error
             'kill-test3
             "expected original drscheme frame to come back to the front"))))
      (when (send (send drscheme-frame get-interactions-text) local-edit-sequence?)
        (error 'kill-test3 "in edit-sequence")))
    
    (define (callcc-test)
      (clear-definitions drscheme-frame)
      (type-in-definitions drscheme-frame "(define kont #f) (let/cc empty (set! kont empty))")
      (do-execute drscheme-frame)
      (wait-for-execute)
      
      (for-each test:keystroke (string->list "(kont)"))
      (test:keystroke #\return)
      (wait-for-execute)
      
        
      (for-each test:keystroke (string->list "x"))
      (let ([start (+ 1 (send interactions-text last-position))])
        (test:keystroke #\return)
        (wait-for-execute)
        
        (let* ([end (- (get-int-pos) 1)]
               [output (fetch-output drscheme-frame start end)]
               [expected "{bug09.gif} reference to undefined identifier: x"])
          (unless (equal? output expected)
            (error 'callcc-test "expected ~s, got ~s" expected output)))))

    
    (when (file-exists? tmp-load-filename)
      (delete-file tmp-load-filename))
    (save-drscheme-window-as tmp-load-filename)
    
    (run-test-in-language-level #t)
    (run-test-in-language-level #f)
    (kill-tests)
    (callcc-test)
    (final-report)))
