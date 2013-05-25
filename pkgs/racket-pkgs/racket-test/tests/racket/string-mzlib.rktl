
(load-relative "loadtest.rktl")

(Section 'mzlib-string)

(require mzlib/string)

(let ([s1 (string-copy "Hello!")])
  (string-lowercase! s1)
  (test "hello!" 'lowercase s1)
  (string-uppercase! s1)
  (test "HELLO!" 'uppercase s1))

(test 1 read-from-string "1")
(test #f read-from-string "#f (2 3) (")
(test #f read-from-string #"#f (2 3) (")
(test 1 read-from-string "(" (lambda () 1))
(test 1 read-from-string "(" (lambda (_) 1))
(test '(1) read-from-string-all "1")
(test '(#f (2 3)) read-from-string-all "#f (2 3)")
(test '(#f (2 3)) read-from-string-all #"#f (2 3)")
(test 1 read-from-string-all "(" (lambda () 1))
(test 1 read-from-string-all "(" (lambda (_) 1))

(test '1 eval-string "1")
(test-values '(1 2 3) (lambda () (eval-string "1 2 3")))
(test-values '(1 2 3) (lambda () (eval-string #"1 2 3")))
(test-values '(1 2 3) (lambda () (eval-string "(values 1 2 3)")))
(test-values '()      (lambda () (eval-string "(values)")))
(test-values '(1 2 3) (lambda () (eval-string "1 (values 2 3)")))
(test-values '(1 2 3) (lambda () (eval-string "(values 1 2) 3")))
(test-values '(1 2 3 4 5)
             (lambda ()
               (eval-string "(values 1 2) 3 (values) (values 4 5)")))

(let ([s (open-input-string "hello there")])
  (test #f regexp-match/fail-without-reading #rx"not there" s)
  (test #f regexp-match/fail-without-reading #rx"t$" s)
  (test #f regexp-match/fail-without-reading #rx"hello there!!!" s)
  (test "hello there" read-string 50 s))

;; Check remaining `regexp-match/fail-without-reading' arguments
(let ([s (open-input-string "hello there")]
      [o (open-output-bytes)])
  (test #f regexp-match/fail-without-reading #rx"not there" s 0 5 o)
  (test #"" get-output-bytes o)
  (test #f regexp-match/fail-without-reading #rx"^hello" s 1 #f o)
  (test #"" get-output-bytes o)
  (test #f regexp-match/fail-without-reading #rx"^hello" s 0 #f o #"_")
  (test #"" get-output-bytes o)
  (test '(#"ello") regexp-match/fail-without-reading #rx"ello" s 0 #f o)
  (test #"h" get-output-bytes o))

(let ([g->re-test
       (lambda (glob . more)
         (let ([re (apply glob->regexp glob more)])
           (lambda xs
             (let loop ([xs xs] [res 'unspecified])
               (when (pair? xs)
                 (loop (cdr xs)
                       (if (boolean? (car xs))
                         (car xs)
                         (begin (test res regexp-match? re (car xs))
                                res))))))))])
  ((g->re-test #"foo*bar" #t #t)
   #t #"foobar" #"foo-bar" #"foo--bar"
   #f #"fobar" #"foo-barr" #"ffoo-bar" #".foobar")
  ((g->re-test "foo*bar" #t #t)
   #t "foobar" "foo-bar" "foo--bar"
   #f "fobar" "foo-barr" "ffoo-bar" ".foobar")
  ((g->re-test "*foo-bar" #t #t)
   #f "foobar" "foo-barr"
   #t "foo-bar" "-foo-bar" "foo-foo-bar" "foo-bar-foo-bar" "f.foo-bar"
   #f ".foo-bar" ".foo-foo-bar")
  ((g->re-test "[ab]*foo-bar" #t #t)
   #f "foobar" "foo-barr" "foo-bar"
   #t "afoo-bar" "b-foo-bar" "a-foo-foo-bar" "b.foo-bar"
   #f ".afoo-bar" ".b-foo-bar")
  ((g->re-test "[.]foo-bar" #t #t)
   #f "foobar" "foo-barr" "foo-bar" "-foo-bar" ".foo-bar")
  ((g->re-test "foo*bar" #t #f)
   #t "fOobAr" "Foo-Bar" "foO--baR"
   #f "FoBar" "fOO-baRR" "FFoo-bar" ".foobar")
  ((g->re-test "foo*bar" #f #t)
   #t "foobar" "foo-bar" "foo--bar"
   #f "fobar" "foo-barr" "ffoo-bar" ".foobar")
  ((g->re-test "*foo-bar" #f #t)
   #f "foobar" "foo-barr"
   #t "foo-bar" "-foo-bar" "foo-foo-bar" "foo-bar-foo-bar" "f.foo-bar"
   #t ".foo-bar" ".foo-foo-bar")
  ((g->re-test "[ab]*foo-bar" #f #t)
   #f "foobar" "foo-barr" "foo-bar"
   #t "afoo-bar" "b-foo-bar" "a-foo-foo-bar" "b.foo-bar"
   #f ".afoo-bar" ".b-foo-bar")
  ((g->re-test "[.]foo-bar" #f #t)
   #f "foobar" "foo-barr" "foo-bar" "-foo-bar"
   #t ".foo-bar")
  ((g->re-test "foo{*}bar" #t #t)
   #f "foo{bar" "foo{-{bar" "foo{}barr" ".foo-bar"
   #t "foo{}bar" "foo{-}bar" "foo{{}}bar"  "foo{}{}bar")
  ((g->re-test "^foo{[*]}bar$" #t #t #t)
   #f "^foo{[}bar$" "^foo{[-]{bar$" "^foo{[]}barr$" ".^foo{[]}bar$"
   #t "^foo{[]}bar$" "^foo{[-]}bar$" "^foo{[{}]}bar$"  "^foo{[]}{[]}bar$")
  ((g->re-test "$[.]^" #t #t #t) #f "$[,]^" #t "$[.]^"))

(report-errs)
