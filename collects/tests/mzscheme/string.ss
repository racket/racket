
(load-relative "loadtest.ss")

(Section 'mzlib-string)

(require (lib "string.ss"))

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

(let ([s (list->string
	  (let loop ([i 0])
	    (if (= i 256)
		null
		(cons (integer->char i) (loop (add1 i))))))])
  (test (list s) regexp-match (regexp-quote s) s)
  (test (string-append "!" s "!")
	regexp-replace
        (regexp-quote s) s (regexp-replace-quote (string-append "!" s "!"))))

(let ([s (open-input-string "hello there")])
  (test #f regexp-match/fail-without-reading #rx"not there" s)
  (test #f regexp-match/fail-without-reading #rx"t$" s)
  (test #f regexp-match/fail-without-reading #rx"hello there!!!" s)
  (test "hello there" read-string 50 s))


(test '("a" "b" "c") regexp-match* "[abc]" "here's a buck")
(test '("b" "c") regexp-match* "[abc]" "here's a buck" 8)
(test '("a") regexp-match* "[abc]" "here's a buck" 0 8)
(test '("a" "b" "c") regexp-match* "[abc]" "here's a buck" 0 #f)

(test '(#"a" #"b" #"c") regexp-match* "[abc]" #"here's a buck")
(test '(#"b" #"c") regexp-match* "[abc]" #"here's a buck" 8)
(test '(#"a") regexp-match* "[abc]" #"here's a buck" 0 8)
(test '(#"a" #"b" #"c") regexp-match* "[abc]" #"here's a buck" 0 #f)

(test '(#"a" #"b" #"c") regexp-match* #"[abc]" "here's a buck")
(test '(#"b" #"c") regexp-match* #"[abc]" "here's a buck" 8)
(test '(#"a") regexp-match* #"[abc]" "here's a buck" 0 8)
(test '(#"a" #"b" #"c") regexp-match* #"[abc]" "here's a buck" 0 #f)

(test '(#"a" #"b" #"c")
      regexp-match* "[abc]" (open-input-string "here's a buck"))
(test '(#"b" #"c") regexp-match* "[abc]" (open-input-string "here's a buck") 8)
(let ([s (open-input-string "here's a buck")])
  (test '(#"a") regexp-match* "[abc]" s 0 8)
  (test " buck" read-string 50 s))
(let ([s (open-input-string "here's a buck")])
  (test '(#"a" #"b" #"c") regexp-match* "[abc]" s 0 #f)
  (test eof read-char s))

(test '((7 . 8) (9 . 10) (11 . 12))
      regexp-match-positions* "[abc]" "here's a buck")
(test '((9 . 10) (11 . 12))
      regexp-match-positions* "[abc]" "here's a buck" 8)
(test '((7 . 8))
      regexp-match-positions* "[abc]" "here's a buck" 0 8)
(test '((7 . 8) (9 . 10) (11 . 12))
      regexp-match-positions* "[abc]" "here's a buck" 0 #f)

(test '((7 . 8) (9 . 10) (11 . 12))
      regexp-match-positions* "[abc]" (open-input-string "here's a buck"))
(test '((9 . 10) (11 . 12))
      regexp-match-positions* "[abc]" (open-input-string "here's a buck") 8)
(let ([s (open-input-string "here's a buck")])
  (test '((7 . 8)) regexp-match-positions* "[abc]" s 0 8)
  (test " buck" read-string 50 s))
(let ([s (open-input-string "here's a buck")])
  (test '((7 . 8) (9 . 10) (11 . 12)) regexp-match-positions* "[abc]" s 0 #f)
  (test eof read-char s))

(let ([s (open-input-string "here's a buck")])
  (test '((7 . 8) (9 . 10) (11 . 12))
        regexp-match-peek-positions*
        "[abc]" (open-input-string "here's a buck"))
  (test '((9 . 10) (11 . 12))
        regexp-match-peek-positions*
        "[abc]" (open-input-string "here's a buck") 8)
  (test '((7 . 8))
        regexp-match-peek-positions* "[abc]" s 0 8)
  (test '((7 . 8) (9 . 10) (11 . 12))
        regexp-match-peek-positions* "[abc]" s 0 #f)
  (test "here's a buck"
        read-string 50 s))

(test '("here's " " " "u" "k") regexp-split "[abc]" "here's a buck")
(test '(" " "u" "k")           regexp-split "[abc]" "here's a buck" 8)
(test '("" "u" "k")            regexp-split "[abc]" "here's a buck" 9)
(test '("" "u" "")             regexp-split "[abc]" "here's a buck" 9 12)
(test '("here's " "")          regexp-split "[abc]" "here's a buck" 0 8)
(test '("here's " " ")         regexp-split "[abc]" "here's a buck" 0 9)
(test '("here's " " " "u" "k") regexp-split "[abc]" "here's a buck" 0 #f)

(test '(#"here's " #" " #"u" #"k")
      regexp-split "[abc]" (open-input-string "here's a buck"))
(test '(#" " #"u" #"k")
      regexp-split "[abc]" (open-input-string "here's a buck") 8)
(test '(#"" #"u" #"k")
      regexp-split "[abc]" (open-input-string "here's a buck") 9)
(let ([s (open-input-string "here's a buck")])
  (test '(#"" #"u" #"") regexp-split "[abc]" s 9 12)
  (test "k" read-string 50 s))
(let ([s (open-input-string "here's a buck")])
  (test '(#"here's " #"") regexp-split "[abc]" s 0 8)
  (test " buck" read-string 50 s))
(let ([s (open-input-string "here's a buck")])
  (test '(#"here's " #" ") regexp-split "[abc]" s 0 9)
  (test "buck" read-string 50 s))
(let ([s (open-input-string "here's a buck")])
  (test '(#"here's " #" " #"u" #"k") regexp-split "[abc]" s 0 #f)
  (test eof read-char s))

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
