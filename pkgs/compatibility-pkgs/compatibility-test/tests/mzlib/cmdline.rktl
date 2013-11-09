
(load-relative "loadtest.rktl")

(Section 'command-line)

(require mzlib/cmdline)

(test (void) 'cmdline
      (command-line "something" #("-ab")
        (once-each
         [("-a") "ok" 5]
         [("-b" "--more") "Help" 7])))

;; test that keywords are compared for the literal symbol
(test "foo" 'cmdline
      (let ([once-each 3] [args "args"])
        (command-line "something" #("-ab" "foo")
          (once-each
           [("-a") "ok" 5]
           [("-b" "--more") "Help" 7])
          (args (x) x))))

(syntax-test #'(command-line))
(syntax-test #'(command-line "hello"))
(err/rt-test (command-line 'hello #("ok")))
(syntax-test #'(command-line "hello" #("ok") (bad)))
(syntax-test #'(command-line "hello" #("ok") (once-any ())))
(syntax-test #'(command-line "hello" #("ok") (once-any ("-ok"))))
(syntax-test #'(command-line "hello" #("ok") (once-any ("-ok" "the ok flag"))))
(syntax-test #'(command-line "hello" #("ok") (once-any ("-ok" a "the ok flag"))))
(syntax-test #'(command-line "hello" #("ok") (once-any ("-ok" (a) "the ok flag"))))
(syntax-test #'(command-line "hello" #("ok") (once-any ("-ok" a "the ok flag") ())))
(syntax-test #'(command-line "hello" #("ok") (args 'done) (once-any ("-ok" a "the ok flag" 7))))
(syntax-test #'(command-line "hello" #("ok") (args (ok) 'done) (once-any ("-ok" a "the ok flag" 7))))
(syntax-test #'(command-line "hello" #("ok") (=> 'done) (once-any ("-ok" a "the ok flag" 7))))
(syntax-test #'(command-line "hello" #("ok") (=> 1 2 3 4) (once-any ("-ok" a "the ok flag" 7))))

(report-errs)
