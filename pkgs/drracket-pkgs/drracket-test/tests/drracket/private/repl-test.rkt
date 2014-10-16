#lang racket

;; NOTES: 
;;  - (expt 3 #f) is likely to never be optimized, 
;;    making the stack traces produced from it predictable

#|

This produces an ACK message

#lang scheme
(require scheme/sandbox)
(make-evaluator '(file "/tmp/foo.rkt"))

|#

(require "drracket-test-util.rkt"
         mred
         framework)

(provide/contract [run-test (-> (listof (or/c 'raw 'debug 'debug/profile 'misc)) any)])

(define-struct loc (line col offset))
;; loc = (make-loc number number number)
;; all numbers in loc structs start at zero.

(define-struct test 
  (program
   ;; : (union 
   ;;    string 
   ;;    'fraction-sum
   ;;    (listof
   ;;      (union string?  // typed in
   ;;             'left    // left arrow key
   ;;             (list string? string?)))) // menu item select
   
   
   answer ;; : answer
   ;; the answers for the various modes of the test, specifically:
   ;;   with debugging enabled: execute, load with different filename, load with same filename
   ;;   as in ordinary racket: execute, load with different filename, load with same filename
   
   source-location ;; (or/c 'interactions 'definitions (cons number number))
   
   breaking-test? ;; : boolean
   
   ;; setup is called before the test case is run.
   setup ;; : -> void
   ;; teardown is called after the test case is complete.
   teardown ;; : -> void
   ))

(define-struct answer (debug-execute debug-load-fn debug-load raw-execute raw-load-fn raw-load))

(define-syntax (mktest stx)
  (syntax-case stx ()
    [(_ program (a b c d e f) di breaking-test?)
     #'(make-test program (make-answer a b c d e f) di breaking-test? void void)]
    [(_ program (a b c d e f) di breaking-test? setup teardown)
     #'(make-test program (make-answer a b c d e f) di breaking-test? setup teardown)]))

(define syntax-regexp-prefix
  (string-append
   (regexp-quote "#<syntax:")
   ".*"
   (regexp-quote "tests/drracket/repl-test-tmp")
   "3?"
   (regexp-quote ".rkt")))


(define (to-strings . args)
  (apply string-append (map (λ (x) (format "~s\n" x)) args)))

(define (add-load-handler-context str)
  (regexp
   (string-append (regexp-quote (string-append 
                                 backtrace-image-string
                                 " "
                                 file-image-string
                                 " .*rkt:"))
                  "[0-9]+:[0-9]+: "
                  (regexp-quote str))))

(define test-data
  (list
   ;; basic tests
   (mktest "1"
           ("1"
            "1"
            "1"
            "1"
            "1"
            "1")
           'interactions
           #f)
   
   (mktest "\"a\""
           ("\"a\""
            "\"a\""
            "\"a\""
            "\"a\""
            "\"a\""
            "\"a\"")
           'interactions
           #f)
   
   (mktest "1 2"
           ("1\n2"
            "2"
            "2"
            "1\n2"
            "2"
            "2")
           'interactions
           #f)
   
   (mktest "\"a\" \"b\""
           ("\"a\"\n\"b\""
            "\"b\""
            "\"b\""
            "\"a\"\n\"b\""
            "\"b\""
            "\"b\"")
           'interactions
           #f)
   
   (mktest 
    "("
    ("{stop-22x22.png} read: expected a `)' to close `('"
     "{stop-multi.png} {stop-22x22.png} read: expected a `)' to close `('"
     "{stop-multi.png} {stop-22x22.png} repl-test-tmp3.rkt:1:0: read: expected a `)' to close `('"
     "{stop-22x22.png} read: expected a `)' to close `('"
     "{stop-22x22.png} read: expected a `)' to close `('"
     "{stop-22x22.png} repl-test-tmp3.rkt:1:0: read: expected a `)' to close `('")
    'definitions
    #f)
   
   (mktest "."
           ("{stop-22x22.png} read: illegal use of `.'"
            "{stop-multi.png} {stop-22x22.png} read: illegal use of `.'"
            "{stop-multi.png} {stop-22x22.png} repl-test-tmp3.rkt:1:0: read: illegal use of `.'"
            "{stop-22x22.png} read: illegal use of `.'"
            "{stop-22x22.png} read: illegal use of `.'"
            "{stop-22x22.png} repl-test-tmp3.rkt:1:0: read: illegal use of `.'")
           'definitions
           #f)
   
   (mktest "(lambda ())"
           ("{stop-22x22.png} lambda: bad syntax in: (lambda ())"
            "{stop-22x22.png} lambda: bad syntax in: (lambda ())"
            "{stop-22x22.png} repl-test-tmp3.rkt:1:0: lambda: bad syntax in: (lambda ())"
            "{stop-22x22.png} lambda: bad syntax in: (lambda ())"
            "{stop-22x22.png} lambda: bad syntax in: (lambda ())"
            "{stop-22x22.png} repl-test-tmp3.rkt:1:0: lambda: bad syntax in: (lambda ())")
           'definitions
           #f)
   
   ;; make sure only a single syntax error occurs when in nested begin situation
   (mktest "(begin (lambda ()) (lambda ()))"
           ("{stop-22x22.png} lambda: bad syntax in: (lambda ())"
            "{stop-22x22.png} lambda: bad syntax in: (lambda ())"
            "{stop-22x22.png} repl-test-tmp3.rkt:1:7: lambda: bad syntax in: (lambda ())"
            "{stop-22x22.png} lambda: bad syntax in: (lambda ())"
            "{stop-22x22.png} lambda: bad syntax in: (lambda ())"
            "{stop-22x22.png} repl-test-tmp3.rkt:1:7: lambda: bad syntax in: (lambda ())")
           'definitions
           #f)
   
   (mktest "xx"
           (#rx"{stop-multi.png} {stop-22x22.png} xx:.*cannot reference undefined identifier"
            #rx"{stop-multi.png} {stop-22x22.png} xx:.*cannot reference undefined identifier"
            #rx"{stop-multi.png} {stop-22x22.png} repl-test-tmp3.rkt:1:0: xx:.*cannot reference undefined identifier"
            #rx"xx:.*cannot reference undefined identifier"
            #rx"xx:.*cannot reference undefined identifier"
            #rx"xx:.*cannot reference undefined identifier")
           'definitions
           #f)
   
   (mktest "(raise 1)"
           ("uncaught exception: 1"
            "uncaught exception: 1"
            "uncaught exception: 1"
            "uncaught exception: 1"
            "uncaught exception: 1"
            "uncaught exception: 1")
           'interactions
           #f)
   
   (mktest "(raise #f)"
           ("uncaught exception: #f"
            "uncaught exception: #f"
            "uncaught exception: #f"
            "uncaught exception: #f"
            "uncaught exception: #f"
            "uncaught exception: #f")
           'interactions
           #f)
   
   (mktest "(values 1 2)"
           ("1\n2"
            "1\n2"
            "1\n2"
            "1\n2"
            "1\n2"
            "1\n2")
           'interactions
           #f)
   
   (mktest "(list 1 2)"
           ("(1 2)"
            "(1 2)"
            "(1 2)"
            "(1 2)"
            "(1 2)"
            "(1 2)")
           'interactions
           #f)
   
   (mktest "(parameterize ([print-struct #t])(define-struct s (x) (make-inspector))(printf \"~s\\n\" (make-s 1)))"
           ("#(struct:s 1)"
            "#(struct:s 1)"
            "#(struct:s 1)"
            "#(struct:s 1)"
            "#(struct:s 1)"
            "#(struct:s 1)")
           'interactions
           #f)
   
   ;; top-level semantics test
   (mktest "(define (f) (+ 1 1)) (define + -) (f)"
           (#rx"define-values:.*cannot change constant.*: \\+"
            #rx"define-values:.*cannot change constant.*: \\+"
            #rx"define-values:.*cannot change constant.*: \\+"
            #rx"define-values:.*cannot change constant.*: \\+"
            #rx"define-values:.*cannot change constant.*: \\+"
            #rx"define-values:.*cannot change constant.*: \\+")
           'interactions
           #f)
   
   (mktest "(begin (define-struct a ()) (define-struct (b a) ()))"
           (""
            ""
            ""
            ""
            ""
            "")
           'interactions
           #f)
   
   (mktest "(begin (values) 1)"
           ("1"
            "1"
            "1"
            "1"
            "1"
            "1")
           'interactions
           #f)
   
   (mktest "(begin xx (printf \"hi\\n\"))"
           (#rx"{stop-multi.png} {stop-22x22.png} xx:.*cannot reference undefined identifier"
            #rx"{stop-multi.png} {stop-22x22.png} xx:.*cannot reference undefined identifier"
            #rx"{stop-multi.png} {stop-22x22.png} repl-test-tmp3.rkt:1:7: xx:.*cannot reference undefined identifier"
            #rx"xx:.*cannot reference undefined identifier"
            #rx"xx:.*cannot reference undefined identifier"
            #rx"xx:.*cannot reference undefined identifier")
           'definitions
           #f)
   
   (mktest (string-append
            "(module m mzscheme (provide e) (define e #'1))\n"
            "(module n mzscheme (require-for-syntax 'm) (provide s) (define-syntax (s stx) e))\n"
            "(require 'n)\n"
            "s")
           
           (#rx"{stop-22x22.png} [?]: literal data is not allowed.*no #%datum syntax transformer is bound in: 1"
            #rx"{stop-22x22.png} [?]: literal data is not allowed.*no #%datum syntax transformer is bound in: 1"
            #rx"{stop-22x22.png} repl-test-tmp3.rkt:1:43: [?]: literal data is not allowed.*no #%datum syntax transformer is bound in: 1"
            #rx"{stop-22x22.png} [?]: literal data is not allowed.*no #%datum syntax transformer is bound in: 1"
            #rx"{stop-22x22.png} [?]: literal data is not allowed.*no #%datum syntax transformer is bound in: 1"
            #rx"{stop-22x22.png} repl-test-tmp3.rkt:1:43: [?]: literal data is not allowed.*no #%datum syntax transformer is bound in: 1")
           'definitions
           #f)
   
   
   ;; leading comment test
   (mktest "#!/bin/sh\n1"
           ("1"
            "1"
            "1"
            "1"
            "1"
            "1")
           'interactions
           #f)
   
   (mktest "#!/bin/sh\nxx"
           (#rx"{stop-multi.png} {stop-22x22.png} xx:.*cannot reference undefined identifier"
            #rx"{stop-multi.png} {stop-22x22.png} xx:.*cannot reference undefined identifier"
            #rx"{stop-multi.png} {stop-22x22.png} repl-test-tmp3.rkt:2:0: xx:.*cannot reference undefined identifier"
            #rx"xx:.*cannot reference undefined identifier"
            #rx"xx:.*cannot reference undefined identifier"
            #rx"xx:.*cannot reference undefined identifier")
           'definitions
           #f)
   
   ;; eval tests
   
   (mktest "    (eval '(values 1 2))"
           ("1\n2"
            "1\n2"
            "1\n2"
            "1\n2"
            "1\n2"
            "1\n2")
           'interactions
           #f)
   
   (mktest "    (eval '(list 1 2))"
           ("(1 2)"
            "(1 2)"
            "(1 2)"
            "(1 2)"
            "(1 2)"
            "(1 2)")
           'interactions
           #f)
   
   (mktest "    (eval '(lambda ()))"
           ("lambda: bad syntax in: (lambda ())"
            "lambda: bad syntax in: (lambda ())"
            "lambda: bad syntax in: (lambda ())"
            "lambda: bad syntax in: (lambda ())"
            "lambda: bad syntax in: (lambda ())"
            "lambda: bad syntax in: (lambda ())")
           'interactions
           #f)
   
   (mktest "    (read (open-input-string \".\"))"
           ("{stop-multi.png} read: illegal use of `.'"
            "{stop-multi.png} read: illegal use of `.'"
            "{stop-multi.png} read: illegal use of `.'"
            "read: illegal use of `.'"
            "read: illegal use of `.'"
            "read: illegal use of `.'")
           'interactions
           #f)
   
   (mktest "    (eval 'x)"
           (#rx"{stop-multi.png} {stop-22x22.png} x:.*cannot reference undefined identifier"
            #rx"{stop-multi.png} {stop-22x22.png} x:.*cannot reference undefined identifier"
            #rx"{stop-multi.png} {stop-22x22.png} repl-test-tmp3.rkt:1:4: x:.*cannot reference undefined identifier"
            #rx"x:.*cannot reference undefined identifier"
            #rx"x:.*cannot reference undefined identifier"
            #rx"x:.*cannot reference undefined identifier")
           'definitions
           #f)
   
   (mktest "(eval (box 1))"
           ("#&1"
            "#&1"
            "#&1"
            "#&1"
            "#&1"
            "#&1")
           'interactions
           #f)
   
   (mktest "(eval '(box 1))"
           ("#&1"
            "#&1"
            "#&1"
            "#&1"
            "#&1"
            "#&1")
           'interactions
           #f)
   
   ; printer setup test
   (mktest "(expt 3 (void))"
           (#rx"{stop-multi.png} {stop-22x22.png} expt: contract violation.*given: #<void>"
            #rx"{stop-multi.png} {stop-22x22.png} expt: contract violation.*given: #<void>"
            #rx"{stop-multi.png} {stop-22x22.png} repl-test-tmp3.rkt:1:0: expt: contract violation.*given: #<void>"
            #rx"expt: contract violation.*given: #<void>"
            #rx"expt: contract violation.*given: #<void>"
            #rx"expt: contract violation.*given: #<void>")
           'definitions
           #f)
   
   ;; error in the middle
   (mktest "1 2 ( 3 4"
           ("1\n2\n{stop-22x22.png} read: expected a `)' to close `('"
            "{stop-multi.png} {stop-22x22.png} read: expected a `)' to close `('"
            "{stop-multi.png} {stop-22x22.png} repl-test-tmp3.rkt:1:4: read: expected a `)' to close `('"
            "1\n2\n{stop-22x22.png} read: expected a `)' to close `('"
            "{stop-22x22.png} read: expected a `)' to close `('"
            "{stop-22x22.png} repl-test-tmp3.rkt:1:4: read: expected a `)' to close `('")
           'definitions
           #f)
   
   (mktest "1 2 . 3 4"
           ("1\n2\n{stop-22x22.png} read: illegal use of `.'"
            "{stop-multi.png} {stop-22x22.png} read: illegal use of `.'"
            "{stop-multi.png} {stop-22x22.png} repl-test-tmp3.rkt:1:4: read: illegal use of `.'"
            "1\n2\n{stop-22x22.png} read: illegal use of `.'"
            "{stop-22x22.png} read: illegal use of `.'"
            "{stop-22x22.png} repl-test-tmp3.rkt:1:4: read: illegal use of `.'")
           'definitions
           #f)
   
   (mktest "1 2 (lambda ()) 3 4"
           ("1\n2\n{stop-22x22.png} lambda: bad syntax in: (lambda ())"
            "{stop-22x22.png} lambda: bad syntax in: (lambda ())"
            "{stop-22x22.png} repl-test-tmp3.rkt:1:4: lambda: bad syntax in: (lambda ())"
            "1\n2\n{stop-22x22.png} lambda: bad syntax in: (lambda ())"
            "{stop-22x22.png} lambda: bad syntax in: (lambda ())"
            "{stop-22x22.png} repl-test-tmp3.rkt:1:4: lambda: bad syntax in: (lambda ())")
           'definitions
           #f)
   
   (mktest "1 2 x 3 4"
           (#rx"1\n2\n{stop-multi.png} {stop-22x22.png} x:.*cannot reference undefined identifier"
            #rx"{stop-multi.png} {stop-22x22.png} x:.*cannot reference undefined identifier"
            #rx"{stop-multi.png} {stop-22x22.png} repl-test-tmp3.rkt:1:4: x:.*cannot reference undefined identifier"
            #rx"1\n2\nx:.*cannot reference undefined identifier"
            #rx".*cannot reference undefined identifier"
            #rx".*cannot reference undefined identifier")
           'definitions
           #f)
   
   (mktest "1 2 (raise 1) 3 4"
           ("1\n2\nuncaught exception: 1"
            "uncaught exception: 1"
            "uncaught exception: 1"
            "1\n2\nuncaught exception: 1"
            "uncaught exception: 1"
            "uncaught exception: 1")
           'interactions
           #f)
   
   (mktest "1 2 (raise #f) 3 4"
           ("1\n2\nuncaught exception: #f"
            "uncaught exception: #f"
            "uncaught exception: #f"
            "1\n2\nuncaught exception: #f"
            "uncaught exception: #f"
            "uncaught exception: #f")
           'interactions
           #f)
   
   (mktest "(require lang/htdp-beginner)\n(cond [1 2 3 4])"
           ("{stop-22x22.png} cond: expected a clause with a question and an answer, but found a clause with 4 parts in:\n  1\n  2\n  3\n  4"
            "{stop-22x22.png} cond: expected a clause with a question and an answer, but found a clause with 4 parts in:\n  1\n  2\n  3\n  4"
            "{stop-22x22.png} repl-test-tmp3.rkt:2:7: cond: expected a clause with a question and an answer, but found a clause with 4 parts in:\n  1\n  2\n  3\n  4"
            "{stop-22x22.png} cond: expected a clause with a question and an answer, but found a clause with 4 parts in:\n  1\n  2\n  3\n  4"
            "{stop-22x22.png} cond: expected a clause with a question and an answer, but found a clause with 4 parts in:\n  1\n  2\n  3\n  4"
            "{stop-22x22.png} repl-test-tmp3.rkt:2:7: cond: expected a clause with a question and an answer, but found a clause with 4 parts in:\n  1\n  2\n  3\n  4")
           'definitions
           #f)
   
   ;; error across separate files
   (mktest
    "(load \"repl-test-tmp2.rkt\") (define (g) (+ 1 (expt 3 #f))) (f g)"
    (#rx"{stop-multi.png} {stop-22x22.png} expt: contract violation.*given: #f"
     #rx"{stop-multi.png} {stop-22x22.png} expt: contract violation.*given: #f"
     #rx"{stop-multi.png} {stop-22x22.png} repl-test-tmp3.rkt:1:45: expt: contract violation.*given: #f"
     #rx"{stop-multi.png} {stop-22x22.png} expt: contract violation.*given: #f"
     #rx"{stop-multi.png} {stop-22x22.png} expt: contract violation.*given: #f"
     #rx"{stop-multi.png} {stop-22x22.png} repl-test-tmp3.rkt:1:28: expt: contract violation.*given: #f")
    'definitions
    #f
    (λ ()
      (call-with-output-file (build-path tmp-load-directory "repl-test-tmp2.rkt")
        (lambda (port)
          (write '(define (f t) (+ 1 (t)))
                 port))
        #:exists 'truncate))
    (λ () (delete-file (build-path tmp-load-directory "repl-test-tmp2.rkt"))))
   
   ;; new namespace test
   (mktest "(current-namespace (make-namespace))\nif"
           ("{stop-22x22.png} if: bad syntax in: if"
            "{stop-22x22.png} if: bad syntax in: if"
            "{stop-22x22.png} repl-test-tmp3.rkt:2:0: if: bad syntax in: if"
            "{stop-22x22.png} if: bad syntax in: if"
            "{stop-22x22.png} if: bad syntax in: if"
            "{stop-22x22.png} repl-test-tmp3.rkt:2:0: if: bad syntax in: if")
           'definitions
           #f)
   
   (mktest "(current-namespace (make-namespace 'empty))\nif"
           ("{stop-22x22.png} #%top-interaction: unbound identifier;\n also, no #%app syntax transformer is bound in: #%top-interaction"
            "{stop-22x22.png} #%top-interaction: unbound identifier;\n also, no #%app syntax transformer is bound in: #%top-interaction"
            "{stop-22x22.png} repl-test-tmp3.rkt:2:0: #%top-interaction: unbound identifier;\n also, no #%app syntax transformer is bound in: #%top-interaction"
            "{stop-22x22.png} #%top-interaction: unbound identifier;\n also, no #%app syntax transformer is bound in: #%top-interaction"
            "{stop-22x22.png} #%top-interaction: unbound identifier;\n also, no #%app syntax transformer is bound in: #%top-interaction"
            "{stop-22x22.png} repl-test-tmp3.rkt:2:0: #%top-interaction: unbound identifier;\n also, no #%app syntax transformer is bound in: #%top-interaction")
           'definitions
           #f)
   
   ;; macro tests
   (mktest "(define-syntax (c stx) (syntax-case stx () [(_ p q r) (syntax (+ p q r))]))"
           (""
            ""
            ""
            ""
            ""
            "")
           'interactions
           #f)

   ;; error escape handler test
   (mktest
    (string-append "(let ([old (error-escape-handler)])\n"
                   "(+ (let/ec k\n"
                   "(dynamic-wind\n"
                   "(lambda () (error-escape-handler (lambda () (k 5))))\n"
                   "(lambda () (expt 3 #f))\n"
                   "(lambda () (error-escape-handler old))))\n"
                   "10))")
    (#rx"{stop-multi.png} {stop-22x22.png} expt: contract violation.*given: #f\n15"
     #rx"{stop-multi.png} {stop-22x22.png} expt: contract violation.*given: #f\n15"
     #rx"{stop-multi.png} {stop-22x22.png} repl-test-tmp3.rkt:5:19: expt: contract violation.*given: #f\n15"
     #rx"expt: contract violation.*given: #f\n15"
     #rx"expt: contract violation.*given: #f\n15"
     #rx"expt: contract violation.*given: #f\n15")
    'definitions
    #f)
   
   ; fraction snip test
   ;; this test depends on the state of the 'framework:fraction-snip-style preference
   ;; make sure this preference is set to the default when running this test.
   (mktest 'fraction-sum
           ("{number 5/6 \"5/6\" mixed}"
            "{number 5/6 \"5/6\" mixed}"
            "{number 5/6 \"5/6\" mixed}"
            "{number 5/6 \"5/6\" mixed}"
            "{number 5/6 \"5/6\" mixed}"
            "{number 5/6 \"5/6\" mixed}")
           'interactions
           #f)
   
   ;; should produce a syntax object with a turn-down triangle.
   (mktest "(write (list (syntax x)))" 
           (#rx"({embedded \".#<syntax:.*repl-test-tmp.rkt:1:21.*>\"})"
            #rx"({embedded \".#<syntax:.*repl-test-tmp.rkt:1:21.*>\"})"
            #rx"({embedded \".#<syntax:.*repl-test-tmp3.rkt:1:21.*>\"})"
            #rx"({embedded \".#<syntax:.*repl-test-tmp.rkt:1:21.*>\"})"
            #rx"({embedded \".#<syntax:.*repl-test-tmp.rkt:1:21.*>\"})"
            #rx"({embedded \".#<syntax:.*repl-test-tmp3.rkt:1:21.*>\"})")
           'interactions
           #f)

   ;; make sure syntax objects only go into good ports
   (mktest (string-append
            "(define-syntax (foo stx)"
            " (with-handlers ([exn:fail? (lambda (x) #'10)])"
            " (syntax-local-value #'foot))) (foo)")
           ("10"
            "10"
            "10"
            "10"
            "10"
            "10")
           'interactions
           #f)
   
   ;; make sure syntax objects don't go into bad ports
   (mktest "(parameterize ([current-output-port (open-output-string)]) (write #'1))"
           (""
            ""
            ""
            ""
            ""
            "")
           'interactions
           #f)
   
   (mktest (string-append
            "(parameterize ([current-output-port (open-output-string)])"
            " (fprintf (current-error-port) \"~e\" #'foot))")
           (#rx"#<syntax:.*repl-test-tmp.rkt:1:96.*>"
            #rx"#<syntax:.*repl-test-tmp.rkt:1:96.*>"
            #rx"#<syntax:.*repl-test-tmp3.rkt:1:96.*>"
            #rx"#<syntax:.*repl-test-tmp.rkt:1:96.*>"
            #rx"#<syntax:.*repl-test-tmp.rkt:1:96.*>"
            #rx"#<syntax:.*repl-test-tmp3.rkt:1:96.*>")
           'interactions
           #f)
   
   
   (mktest "(write-special 1)"
           ("1#t"
            "1#t"
            "1#t"
            "1#t"
            "1#t"
            "1#t")
           'interactions
           #f)
   
   (mktest
    ;; the begin/void combo is to make sure that no value printout
    ;; comes and messes up the source location for the error.
    (string-append
     "(define s (make-semaphore 0))\n(queue-callback\n"
     "(lambda ()\n"
     "(dynamic-wind\n"
     "void\n"
     "(lambda () (expt 3 #f))\n"
     "(lambda () (semaphore-post s)))))\n"
     "(begin (yield s) (void))")
    (#rx"expt: contract violation.*given: #f"
     #rx"expt: contract violation.*given: #f"
     #rx"expt: contract violation.*given: #f"
     #rx"expt: contract violation.*given: #f"
     #rx"expt: contract violation.*given: #f"
     #rx"expt: contract violation.*given: #f")
    'definitions
    #f)
   
   
   ;; breaking tests
   (mktest "(semaphore-wait (make-semaphore 0))"
           (#rx"user break"
            #rx"user break"
            #rx"user break"
            #rx"user break"
            #rx"user break"
            #rx"user break")
           'dont-care
           #t)
   
   (mktest "(let l()(l))"
           (#rx"user break"
            #rx"user break"
            #rx"user break"
            #rx"user break"
            #rx"user break"
            #rx"user break")
           'dont-care
           #t)
   
   ;; continuation tests
   (mktest "(define k (call/cc (lambda (x) x)))\n(k 17)\nk"
           ("17"
            "17"
            "17"
            "17"
            "17"
            "17")
           'interactions
           #f)
   
   (mktest "(define v (vector (call/cc (lambda (x) x))))\n((vector-ref v 0) 2)\nv"
           ("#(2)"
            "#(2)"
            "#(2)"
            "#(2)"
            "#(2)"
            "#(2)")
           'interactions
           #f)
   
   (mktest "(define v (vector (eval '(call/cc (lambda (x) x)))))\n((vector-ref v 0) 2)\nv"
           ("#(2)"
            "#(2)"
            "#(2)"
            "#(2)"
            "#(2)"
            "#(2)")
           'interactions
           #f)
   
   (mktest "(define x 1)\n((λ (x y) y) (set! x (call/cc (lambda (x) x)))\n(x 3))"
           (#rx"{stop-multi.png} {stop-22x22.png} application:.*given: 3"
            #rx"{stop-multi.png} {stop-22x22.png} application:.*given: 3"
            #rx"{stop-multi.png} {stop-22x22.png} repl-test-tmp3.rkt:3:13: application:.*given: 3"
            #rx"application:.*given: 3"
            #rx"application:.*given: 3"
            #rx"application:.*given: 3")
           'definitions
           #f)
   
   ;; top-level & continuation interaction test
   (mktest "(begin (define k (call/cc (λ (x) x)))\n(define x 'wrong))\n(set! x 'right)\n(k 1)\nx"
           ("right"
            "right"
            "right"
            "right"
            "right"
            "right")
           'interactions
           #f)
   
   (mktest (format "~s"
                   '(call-with-continuation-prompt
                     (lambda ()
                       (eval '(begin (abort-current-continuation
                                      (default-continuation-prompt-tag)
                                      1 2 3)
                                     10)))
                     (default-continuation-prompt-tag)
                     list))
           ("(1 2 3)"
            "(1 2 3)"
            "(1 2 3)"
            "(1 2 3)"
            "(1 2 3)"
            "(1 2 3)")
           'interactions
           #f)
   
   (mktest "(new snip%)"
           ("{unknown snip: (object:snip% ...)}\n"
            "{unknown snip: (object:snip% ...)}\n"
            "{unknown snip: (object:snip% ...)}\n"
            "{unknown snip: (object:snip% ...)}\n"
            "{unknown snip: (object:snip% ...)}\n"
            "{unknown snip: (object:snip% ...)}\n")
           'interactions
           #f)

   ;; graphical lambda tests
   (mktest (list "((" '("Insert" "Insert λ") "(x) x) 1)")
           ("1"
            "1"
            "1"
            "1"
            "1"
            "1")
           'interactions
           #f)
   
   (mktest (list "(" '("Insert" "Insert λ") "())")
           
           ("{stop-22x22.png} λ: bad syntax in: (λ ())"
            "{stop-22x22.png} λ: bad syntax in: (λ ())"
            "{stop-22x22.png} repl-test-tmp3.rkt:1:0: λ: bad syntax in: (λ ())"
            "{stop-22x22.png} λ: bad syntax in: (λ ())"
            "{stop-22x22.png} λ: bad syntax in: (λ ())"
            "{stop-22x22.png} repl-test-tmp3.rkt:1:0: λ: bad syntax in: (λ ())")
           'definitions
           #f)
   
   ;; thread tests
   (mktest "(begin (thread (lambda () x)) (sleep 1/10))"
           (#rx"{stop-multi.png} {stop-22x22.png} x:.*cannot reference undefined identifier"
            #rx"{stop-multi.png} {stop-22x22.png} x:.*cannot reference undefined identifier"
            (regexp
             (string-append "{stop-multi.png} {stop-22x22.png} repl-test-tmp3.rkt:1:26:"
                            " x:.*cannot reference undefined identifier"))
            #rx"x:.*cannot reference undefined identifier"
            #rx"x:.*cannot reference undefined identifier"
            #rx"x:.*cannot reference undefined identifier")
           'definitions
           #f)
   
   ;; brought down from above for comparison
   (mktest
    "xx"
    (#rx"{stop-multi.png} {stop-22x22.png} xx:.*cannot reference undefined identifier"
     #rx"{stop-multi.png} {stop-22x22.png} xx:.*cannot reference undefined identifier"
     (regexp (string-append "{stop-multi.png} {stop-22x22.png} repl-test-tmp3.rkt:1:0:"
                            " xx:.*cannot reference undefined identifier"))
     #rx"xx:.*cannot reference undefined identifier"
     #rx"xx:.*cannot reference undefined identifier"
     #rx"xx:.*cannot reference undefined identifier")
    'definitions
    #f)

   ;; setup of the namespaces for pict printing (from slideshow)

   (mktest "(require texpict/utils)(disk 3)"
           ("{pict-snip}"
            "{pict-snip}"
            "{pict-snip}"
            "{pict-snip}"
            "{pict-snip}"
            "{pict-snip}")
           'interactions
           #f)
   
   ;; this test is kind of hokey in our current, module-based world
   ;; we get images instead of snips because the image conversion code
   ;; is in a library that's shared with the user, whereas the pict
   ;; library isn't (I believe)
   (mktest (to-strings
            '(require texpict/utils)
            '(let ()
               (current-namespace (make-namespace))
               (namespace-set-variable-value! 'd (disk 3)))
            'd)
           ("{image}"
            "{image}"
            "{image}"
            "{image}"
            "{image}"
            "{image}")
           'interactions
           #f)
   
   (mktest (to-strings
            '(let ([on (current-namespace)]
                   [n ((current-module-name-resolver) 'mred #f #f #t)])
               (current-namespace (make-namespace))
               (namespace-attach-module on n))
            '(require texpict/utils)
            '(disk 3))
           ("{pict-snip}"
            "{pict-snip}"
            "{pict-snip}"
            "{pict-snip}"
            "{pict-snip}"
            "{pict-snip}")
           'interactions
           #f)

   (mktest (string-append
            "(require mzlib/pretty)"
            "(pretty-print-print-hook (lambda x (expt 3 #f)))"
            "(list 1 2 3)")
           ("(1 2 3)"
            "(1 2 3)"
            "(1 2 3)"
            "(1 2 3)"
            "(1 2 3)"
            "(1 2 3)")
           'interactions
           #f)
   
   (mktest (format "~s\n~s"
                   `(require scheme/pretty)
                   `(parameterize ((pretty-print-exact-as-decimal #t)) (display 1/4)))
           ("1/4"
            "1/4"
            "1/4"
            "1/4"
            "1/4"
            "1/4")
           'interactions
           #f)
   
   (mktest
    (string-append
     "(define p (open-output-string))\n"
     "(parameterize ([current-error-port p])\n"
     "(dynamic-wind\n"
     "void\n"
     "(lambda ()\n"
     "((error-display-handler)\n"
     "\"x\"\n"
     "(with-handlers ((void values)) (eval '(lambda ())))))\n"
     "(lambda ()\n"
     "(display (get-output-string p)))))\n")
    ("x in: (lambda ())"
     "x in: (lambda ())"
     "x in: (lambda ())"
     "x in: (lambda ())"
     "x in: (lambda ())"
     "x in: (lambda ())")
    'interactions
    #f)
  
   (mktest
    (format "~s" '(+ 1 (+ 1 (abort-current-continuation
                             (default-continuation-prompt-tag)
                             (lambda () 
                               (abort-current-continuation
                                (default-continuation-prompt-tag)
                                (λ () 0)))))))
    ("0" "0" "0" "0" "0" "0")
    'interactions
    #f)
   
   (mktest
    (format "~s ~s ~s" 
            '1
            '(+ 1 (+ 1 (abort-current-continuation
                        (default-continuation-prompt-tag)
                        (lambda () 
                          (abort-current-continuation
                           (default-continuation-prompt-tag)
                           (λ () 0))))))
            '2)
    ("1\n0" "0" "0" "1\n0" "0" "0")
    'interactions
    #f)
   
   (mktest
    (format "~s" 
            '(begin
               1
               (+ 1 (+ 1 (abort-current-continuation
                          (default-continuation-prompt-tag)
                          (lambda () 
                            (abort-current-continuation
                             (default-continuation-prompt-tag)
                             (λ () 0))))))
               2))
    ("0" "0" "0" "0" "0" "0")
    'interactions
    #f)
   
   ;; this test case used to fail, but not by printing the wrong
   ;; thing; instead the REPL just didn't return and Run didn't
   ;; light up, etc.
   (mktest
    "(abort-current-continuation (default-continuation-prompt-tag))) 2"
    (#rx"call-with-continuation-prompt: result arity mismatch"
     #rx"call-with-continuation-prompt: result arity mismatch"
     #rx"call-with-continuation-prompt: result arity mismatch"
     #rx"call-with-continuation-prompt: result arity mismatch"
     #rx"call-with-continuation-prompt: result arity mismatch"
     #rx"call-with-continuation-prompt: result arity mismatch")
    'interactions
    #f)
   
   (mktest
    (format "~s"
            '(abort-current-continuation
              (default-continuation-prompt-tag)
              (λ ()
                (abort-current-continuation
                 (default-continuation-prompt-tag)))))
    (#rx"call-with-continuation-prompt: result arity mismatch"
     #rx"call-with-continuation-prompt: result arity mismatch"
     #rx"call-with-continuation-prompt: result arity mismatch"
     #rx"call-with-continuation-prompt: result arity mismatch"
     #rx"call-with-continuation-prompt: result arity mismatch"
     #rx"call-with-continuation-prompt: result arity mismatch")
    'interactions
    #f)

   
   ))

;; these tests aren't used at the moment.
#;
(define xml-tests
  (list
   ;; XML tests
   (mktest (list "#!/bin/sh\n" 
                 '("Insert" "Insert XML Box")
                 "<a>")
           "(a ())"
           "(a ())"
           #f #f #f
           'interactions
           #f)
   
   (mktest
    '(("Insert" "Insert XML Box")
      "<a>")
    "(a ())"
    "(a ())"
    #f
    'interactions
    #f
    #f)
   
   (mktest
    '(("Insert" "Insert XML Box")
      "<a>"
      ("Insert" "Insert Scheme Box")
      "1")
    "(a () 1)"
    "(a () 1)"
    #f
    'interactions
    #f
    #f)
   
   (mktest
    '(("Insert" "Insert XML Box")
      "<a>"
      ("Insert" "Insert Scheme Splice Box")
      "'(1)")
    "(a () 1)"
    "(a () 1)"
    #f
    'interactions
    #f
    #f)
   
   (mktest
    '(("Insert" "Insert XML Box")
      "<a>"
      ("Insert" "Insert Scheme Splice Box")
      "1")
    "scheme-splice-box: expected a list, found: 1"
    "scheme-splice-box: expected a list, found: 1"
    #t
    'definitions
    #f
    #f)))

(define backtrace-image-string "{stop-multi.png}")
(define file-image-string "{stop-22x22.png}")

(define tmp-load-directory (find-system-path 'temp-dir))

(define tmp-load-short-filename "repl-test-tmp.rkt")
(define tmp-load-filename (build-path tmp-load-directory tmp-load-short-filename))

(define tmp-load3-short-filename "repl-test-tmp3.rkt")
(define tmp-load3-filename (build-path tmp-load-directory tmp-load3-short-filename))

(define (cleanup-tmp-files)
  (when (file-exists? tmp-load-filename) (delete-file tmp-load-filename))
  (when (file-exists? tmp-load3-filename) (delete-file tmp-load3-filename)))

(define (run-test which-tests)
    
  (define drscheme-frame (wait-for-drracket-frame))
  
  (define ints-text (queue-callback/res (λ () (send drscheme-frame get-interactions-text))))
  (define ints-canvas (queue-callback/res (λ () (send drscheme-frame get-interactions-canvas))))
  (define defs-text (queue-callback/res (λ () (send drscheme-frame get-definitions-text))))
  (define defs-canvas (queue-callback/res (λ () (send drscheme-frame get-definitions-canvas))))
  (define execute-button (queue-callback/res (λ () (send drscheme-frame get-execute-button))))
  
  (define wait-for-execute (lambda () (wait-for-button execute-button)))
  (define get-int-pos (lambda () (queue-callback/res (λ () (get-text-pos ints-text)))))
  
  
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
    (queue-callback/res
     (lambda ()
       (let* ([start (send ints-text paragraph-start-position 2)]
              ;; since the fraction is supposed to be one char wide, we just
              ;; select one char, so that, if the regular number prints out,
              ;; this test will fail.
              [end (+ start 1)])
         (send ints-text set-position start end))))
    (test:menu-select "Edit" "Copy")
    (clear-definitions drscheme-frame)
    (type-in-definitions drscheme-frame "(+ ")
    (test:menu-select "Edit" "Paste")
    (type-in-definitions drscheme-frame " 1/3)"))

  ; given a filename "foo", we perform two operations on the contents
  ; of the file "foo.rkt".  First, we insert its contents into the REPL
  ; directly, and second, we use the load command.  We compare the
  ; results of these operations against expected results.
  (define ((run-single-test execute-text-start escape language-cust) in-vector)
    ;(printf "\n>> testing ~s\n" (test-program in-vector))
    (let* ([program (test-program in-vector)]
           [execute-answer (make-execute-answer in-vector language-cust)]
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
      
      (wait-for-drracket-frame)
      
      (cond
        [(string? program)
         (insert-in-definitions/newlines drscheme-frame program)]
        [(eq? program 'fraction-sum)
         (setup-fraction-sum-interactions)]
        [(list? program)
         (for-each
          (lambda (item)
            (cond
              [(string? item) (insert-in-definitions/newlines drscheme-frame item)]
              [(eq? item 'left)
               (queue-callback/res
                (λ ()
                  (send defs-text 
                        set-position
                        (- (send defs-text get-start-position) 1)
                        (- (send defs-text get-start-position) 1))))]
              [(pair? item) (apply test:menu-select item)]))
          program)])
      
      (do-execute drscheme-frame #f)
      
      ;; make sure that the execute callback has really completed 
      ;; (is this necc w/ test:run-one below?)
      (queue-callback/res void)
      
      (when breaking-test?
        (test:run-one (λ () (send (send drscheme-frame get-break-button) command))))
      (wait-for-execute)
      
      (let* ([execute-text-end (max 0 (- (get-int-pos) 1))] ;; subtract one to skip last newline
             [received-execute
              (fetch-output drscheme-frame execute-text-start execute-text-end)])
        
        ; check focus and selection for execute test
        (case language-cust
          [(raw) (void)]
          [else
           (define edit-target 
             (queue-callback/res (λ () (send drscheme-frame get-edit-target-window))))
           (define defs-focus? (eq? edit-target defs-canvas))
           (define ints-focus? (eq? edit-target ints-canvas))
           (cond
             [(eq? source-location 'dont-care)
              (void)]
             [(eq? source-location 'definitions)
              (unless defs-focus?
                (eprintf "FAILED execute test for ~s\n  expected definitions to have the focus\n"
                         program))]
             [(eq? source-location 'interactions)
              (unless ints-focus?
                (eprintf "FAILED execute test for ~s\n  expected interactions to have the focus\n"
                         program))]
             [defs-focus?
               (let ([start (car source-location)]
                     [finish (cdr source-location)])
                 (let* ([error-ranges (queue-callback/res (λ () (send ints-text get-error-ranges)))]
                        [error-range (and error-ranges
                                          (not (null? error-ranges))
                                          (car error-ranges))])
                   (unless (and error-range
                                (= (+ (srcloc-position error-range) -1) (loc-offset start))
                                (= (+ (srcloc-position error-range) -1 (srcloc-span error-range)) 
                                   (loc-offset finish)))
                     (eprintf "FAILED execute test for ~s\n  error-range is ~s\n  expected ~s\n"
                              program
                              (and error-range
                                   (list (+ (srcloc-position error-range) -1)
                                         (+ (srcloc-position error-range) -1 
                                            (srcloc-span error-range))))
                              (list (loc-offset start)
                                    (loc-offset finish))))))])])
        
        ; check text for execute test
        (next-test)
        (unless (cond
                  [(string? execute-answer)
                   (string=? execute-answer received-execute)]
                  [(regexp? execute-answer)
                   (regexp-match execute-answer received-execute)]
                  [else #f])
          (failure)
          (eprintf "FAILED execute test for ~s (~a)\n  expected: ~s\n       got: ~s\n"
                   program
                   language-cust
                   execute-answer received-execute))
        
        (test:new-window ints-canvas)
        
        ; save the file so that load is in sync
        (test:menu-select "File" "Save Definitions")
        
        ; make sure that a prompt is available at end of the REPL
        (unless (queue-callback/res
                 (λ () 
                   (and (char=? #\>
                                (send ints-text get-character
                                      (- (send ints-text last-position) 2)))
                        (char=? #\space
                                (send ints-text get-character
                                      (- (send ints-text last-position) 1))))))
          (test:keystroke #\return))
        
        (define (load-test short-filename load-answer)
          ;; in order to erase the state in the namespace already, we clear (but don't save!)
          ;; the definitions and click execute with the empty buffer
          (test:new-window defs-canvas)
          (test:menu-select "Edit" "Select All")
          (test:menu-select "Edit" "Delete")
          (do-execute drscheme-frame #f)
          (wait-for-execute)
          
          ;; stuff the load command into the REPL 
          (insert-in-interactions drscheme-frame (format "(load ~s)" short-filename))
          
          ;; record current text position, then stuff a CR into the REPL
          (define load-text-start 
            (+ 1 (queue-callback/res (λ () (send ints-text last-position)))))
          
          (test:keystroke #\return)
          
          (when breaking-test?
            (test:run-one (λ () (send (send drscheme-frame get-break-button) command))))
          (wait-for-execute)
          
          (let* ([load-text-end (- (get-int-pos) 1)] ;; subtract one to eliminate newline
                 [received-load 
                  (fetch-output drscheme-frame load-text-start load-text-end)])
            
            ;; check load text 
            (next-test)
            (unless (cond
                      [(string? load-answer)
                       (string=? load-answer received-load)]
                      [(regexp? load-answer)
                       (regexp-match load-answer received-load)]
                      [else #f])
              (failure)
              (eprintf "FAILED load test ~a for ~s\n  expected: ~s\n       got: ~s\n"
                       short-filename
                       program load-answer received-load))))
        (load-test tmp-load-short-filename (make-load-answer in-vector language-cust #f))
        (when (file-exists? tmp-load3-filename)
          (delete-file tmp-load3-filename))
        (copy-file tmp-load-filename tmp-load3-filename)
        (load-test tmp-load3-short-filename 
                   (make-load-answer in-vector language-cust tmp-load3-short-filename))
        
        (teardown)
        
        ; check for edit-sequence
        (when (repl-in-edit-sequence?)
          (eprintf "FAILED: repl in edit-sequence")
          (escape)))))
  
  (define tests 0)
  (define failures 0)
  (define (next-test) (set! tests (+ tests 1)))
  (define (failure) (set! failures (+ failures 1)))
  (define (reset) (set! tests 0) (set! failures 0))
  (define (final-report)
    (if (= 0 failures)
        (printf "tests finished: all ~a tests passed\n" tests)
        (eprintf "tests finished: ~a failed out of ~a total\n" failures tests)))
  
  (define (run-main-tests language-cust)
    (random-seed-test)
    
    (test:new-window defs-canvas)
    (clear-definitions drscheme-frame)
    (do-execute drscheme-frame)
    (let/ec escape 
      (for-each (run-single-test (get-int-pos) escape language-cust) test-data)))
  
  (define kill-menu-item "Force the Program to Quit")
  
  (define (kill-tests)
    
    (next-test)
    (clear-definitions drscheme-frame)
    (do-execute drscheme-frame)
    (test:menu-select "Racket" kill-menu-item)
    (let ([win (wait-for-new-frame drscheme-frame)])
      (test:button-push "OK")
      (let ([drs2 (wait-for-new-frame win)])
        (unless (eq? drs2 drscheme-frame)
          (error 'kill-test1 "expected original drscheme frame to come back to the front"))))
    
    (next-test)
    (type-in-definitions drscheme-frame "(kill-thread (current-thread))")
    (do-execute drscheme-frame #f)
    (let ([win (wait-for-new-frame drscheme-frame)])
      (test:button-push "OK")
      (let ([drs2 (wait-for-new-frame win)])
        (unless (eq? drs2 drscheme-frame)
          (error 'kill-test2 "expected original drscheme frame to come back to the front"))))
    
    (next-test)
    (clear-definitions drscheme-frame)
    (do-execute drscheme-frame)
    (type-in-definitions
     drscheme-frame
     "(define (f) (queue-callback f) (error 'ouch)) (f)")
    (do-execute drscheme-frame #f)
    (sleep 1/2)
    (test:menu-select "Racket" kill-menu-item)
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
    (next-test)
    (clear-definitions drscheme-frame)
    (type-in-definitions drscheme-frame "(define kont #f) (let/cc empty (set! kont empty))")
    (do-execute drscheme-frame)
    (wait-for-execute)
    
    (for-each test:keystroke (string->list "(kont)"))
    (test:keystroke #\return)
    (wait-for-execute)
    
    (for-each test:keystroke (string->list "x"))
    (let ([start (+ 1 (queue-callback/res (λ () (send ints-text last-position))))])
      (test:keystroke #\return)
      (wait-for-execute)
      
      (let* ([end (- (get-int-pos) 1)]
             [output (fetch-output drscheme-frame start end)]
             [expected #rx"x:.*cannot reference undefined identifier"])
        (unless (regexp-match expected output)
          (failure)
          (eprintf "callcc-test: expected something matching ~s, got ~s\n" expected output)))))
  
  (define (random-seed-test)
    (define expression 
      (format "~s" '(pseudo-random-generator->vector (current-pseudo-random-generator))))
    (next-test)
    (clear-definitions drscheme-frame)
    (do-execute drscheme-frame)
    (wait-for-execute)
    
    (insert-in-interactions drscheme-frame expression)
    (let ([start1 (+ 1 (queue-callback/res (λ () (send ints-text last-position))))])
      (test:keystroke #\return)
      (wait-for-execute)
      (let ([output1 (fetch-output drscheme-frame start1 (- (get-int-pos) 1))])
        (insert-in-interactions drscheme-frame expression)
        (let ([start2 (+ 1 (queue-callback/res (λ () (send ints-text last-position))))])
          (test:keystroke #\return)
          (wait-for-execute)
          (let ([output2 (fetch-output drscheme-frame start2 (- (get-int-pos) 1))])
            (unless (equal? output1 output2)
              (failure)
              (eprintf "random-seed-test: expected\n  ~s\nand\n  ~s\nto be the same"
                       output1
                       output2)))))))
  
  (define (top-interaction-test)
    (clear-definitions drscheme-frame)
    (do-execute drscheme-frame)
    (wait-for-execute)
    (let ([ints-just-after-welcome (queue-callback/res (λ () (+ 1 (send ints-text last-position))))])
      
      (type-in-definitions 
       drscheme-frame
       "(define-syntax #%top-interaction (syntax-rules () [(_ . e) 'e]))\n(+ 1 2)\n")
      (test:menu-select "File" "Save Definitions")
      
      (clear-definitions drscheme-frame)
      (do-execute drscheme-frame)
      (wait-for-execute)
      
      (for-each test:keystroke (string->list (format "(load ~s)" tmp-load-short-filename)))
      (let ([start (+ 1 (queue-callback/res (λ () (send ints-text last-position))))])
        (test:keystroke #\return)
        (wait-for-execute)
        (let* ([end (- (get-int-pos) 1)]
               [output (fetch-output drscheme-frame start end)]
               [expected "(+ 1 2)"])
          (unless (equal? output expected)
            (error 'top-interaction-test "expected.1 ~s, got ~s" expected output))
          (next-test)))
      
      (for-each test:keystroke (string->list "(+ 4 5)"))
      (let ([start (+ 1 (queue-callback/res (λ () (send ints-text last-position))))])
        (test:keystroke #\return)
        (wait-for-execute)
        (let* ([end (- (get-int-pos) 1)]
               [output (fetch-output drscheme-frame start end)]
               [expected "(+ 4 5)"])
          (unless (equal? output expected)
            (error 'top-interaction-test "expected.2 ~s, got ~s" expected output))
          (next-test)))))
  
  (when (file-exists? tmp-load-filename)
    (delete-file tmp-load-filename))
  (save-drracket-window-as tmp-load-filename)
  
  ;; the debug and debug/profile tests should not differ in their output
  ;; they are both run here because debug uses the automatic-compilation 
  ;; stuff and debug/profile does not (so they use different instantiations
  ;; of the stacktrace module.
  
  (define level (list #rx"Pretty Big"))
  (when (memq 'raw which-tests)
    (set-language-level! level #f)
    (test:set-radio-box-item! "No debugging or profiling")
    (let ([f (test:get-active-top-level-window)])
      (test:button-push "OK")
      (wait-for-new-frame f))
    (run-main-tests 'raw))
  (when (memq 'debug which-tests)
    (set-language-level! level)
    (run-main-tests 'debug))
  (when (memq 'debug/profile which-tests)
    (set-language-level! level #f)
    (test:set-radio-box-item! "Debugging and profiling")
    (let ([f (test:get-active-top-level-window)])
      (test:button-push "OK")
      (wait-for-new-frame f))
    (run-main-tests 'debug/profile))
  (when (memq 'misc which-tests)
    (set-language-level! level #t)
    (kill-tests)
    (callcc-test)
    (top-interaction-test))
  
  (final-report))

(define (insert-in-definitions/newlines drs str)
  (let loop ([strs (regexp-split #rx"\n" str)])
    (insert-in-definitions drs (car strs))
    (unless (null? (cdr strs))
      (test:keystroke #\return)
      (loop (cdr strs)))))

(define (make-execute-answer test language-cust)
  ((case language-cust
     [(debug debug/profile)
      answer-debug-execute]
     [(raw)
      answer-raw-execute]) 
   (test-answer test)))

(define (make-load-answer test language-cust src-file)
  ((case language-cust
     [(debug debug/profile)
      (if src-file
          answer-debug-load
          answer-debug-load-fn)]
     [(raw)
      (if src-file
          answer-raw-load
          answer-raw-load-fn)])
   (test-answer test)))

(define (string/rx-append a b)
  (if (regexp? b)
      (regexp (string-append (regexp-quote a) (object-name b)))
      (string-append a b)))


(exit-handler
 (let ([eh (exit-handler)])
   (λ (val)
      (cleanup-tmp-files)
      (eh val))))
