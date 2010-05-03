
(load-relative "loadtest.rktl")

(Section 'string)

(require scheme/string)

;; ---------- real->decimal-string ----------
(test "0." real->decimal-string 0 0)
(test "0." real->decimal-string 0.0 0)
(test "1." real->decimal-string 0.6 0)
(test "1." real->decimal-string 3/4 0)
(test "1." real->decimal-string 1.2 0)
(test "-0." real->decimal-string -0.0 0) ; note this!
(test "-0.00" real->decimal-string -0.0) ; same here...
(test "-1." real->decimal-string -0.6 0)
(test "-1." real->decimal-string -3/4 0)
(test "-1." real->decimal-string -1.2 0)
(test "1.20" real->decimal-string 1.2)
(test "-1.20" real->decimal-string -1.2)
(test "1.00" real->decimal-string 0.99999999999)
(test "-1.00" real->decimal-string -0.99999999999)
(test "1999999999999999859514578049071102439861518336.00"
      real->decimal-string 2e45)

;; ---------- regexp-quote ----------
(let ([s (list->string
          (let loop ([i 0])
            (if (= i 256)
              null
              (cons (integer->char i) (loop (add1 i))))))])
  (test (list s) regexp-match (regexp-quote s) s)
  (test (string-append "!" s "!")
        regexp-replace
        (regexp-quote s) s (regexp-replace-quote (string-append "!" s "!"))))

;; ---------- regexp-<?>* functions ----------
(let ()
  (define (->b x)
    (cond [(list? x) (map ->b x)]
          [(string? x) (string->bytes/utf-8 x)]
          [(pregexp? x) (byte-pregexp (->b (object-name x)))]
          [else x]))
  (define fun* #f)
  (define t
    (case-lambda
      [(fun) (set! fun* fun)]
      [(res left rx str . args)
       (unless (eq? fun* regexp-match-peek-positions*)
         ;; test with a string
         (apply test res fun* rx str args)
         ;; test with a byte-regexp and/or a byte string
         (apply test (->b res) fun* (->b rx) str args)
         (apply test (->b res) fun* rx (->b str) args)
         (apply test (->b res) fun* (->b rx) (->b str) args))
       ;; test with a port, and test leftovers
       (when left
         (let ([p (open-input-string str)])
           (apply test (->b res) fun* rx p args)
           (test left read-string 50 p)))]))
  ;; --------------------
  (t regexp-match*)
  (t '("a" "b" "c") eof "[abc]" " a b c ")
  (t '("b" "c") eof     "[abc]" " a b c " 2)
  (t '("b" "c") eof     "[abc]" " a b c " 3)
  (t '("a") " b c "     "[abc]" " a b c " 0 2)
  (t '("a") "b c "      "[abc]" " a b c " 0 3)
  (t '("a" "b" "c") eof "[abc]" " a b c " 0 #f)
  (t '("a" "b" "c") eof "[abc]" " a b c " 0 7)
  (t '("a" "b" "c") eof "[abc]" "a b c")
  (t '("b" "c") eof     "[abc]" "a b c" 1)
  (t '("b" "c") eof     "[abc]" "a b c" 2)
  (t '("a") " b c"      "[abc]" "a b c" 0 1)
  (t '("a") "b c"       "[abc]" "a b c" 0 2)
  (t '("a" "b" "c") eof "[abc]" "a b c" 0)
  (t '("a" "b" "c") eof "[abc]" "a b c" 0 #f)
  (t '("a" "b" "c") eof "[abc]" "a b c" 0 5)
  (t '("a")         eof "^."    "a b c" 0 5 #"")
  (t '()            eof "^."    "a b c" 0 5 #"x")
  (t '("a\n" "b\n" "c\n") eof "(?m:^.\n)" "a\nb\nc\n" 0 6)
  (t '("b\n" "c\n") eof "(?m:^.\n)" "a\nb\nc\n" 0 6 #"x")
  (for-each (lambda (cvt)
              (test '(#"\x80" #"\x80") regexp-match* (cvt #"\x80") #"a\x80z\x80q"))
            (list values byte-regexp byte-pregexp))
  ;; --------------------
  (t regexp-match-positions*)
  (t '((1 . 2) (3 . 4) (5 . 6)) eof "[abc]" " a b c ")
  (t '((3 . 4) (5 . 6)) eof         "[abc]" " a b c " 2)
  (t '((3 . 4) (5 . 6)) eof         "[abc]" " a b c " 3)
  (t '((1 . 2)) " b c "             "[abc]" " a b c " 0 2)
  (t '((1 . 2)) "b c "              "[abc]" " a b c " 0 3)
  (t '((1 . 2) (3 . 4) (5 . 6)) eof "[abc]" " a b c " 0 #f)
  (t '((1 . 2) (3 . 4) (5 . 6)) eof "[abc]" " a b c " 0 7)
  (t '((0 . 1) (2 . 3) (4 . 5)) eof "[abc]" "a b c")
  (t '((2 . 3) (4 . 5)) eof     "[abc]" "a b c" 1)
  (t '((2 . 3) (4 . 5)) eof     "[abc]" "a b c" 2)
  (t '((0 . 1)) " b c"      "[abc]" "a b c" 0 1)
  (t '((0 . 1)) "b c"       "[abc]" "a b c" 0 2)
  (t '((0 . 1) (2 . 3) (4 . 5)) eof "[abc]" "a b c" 0)
  (t '((0 . 1) (2 . 3) (4 . 5)) eof "[abc]" "a b c" 0 #f)
  (t '((0 . 1) (2 . 3) (4 . 5)) eof "[abc]" "a b c" 0 5)
  (t '((0 . 1))                 eof "^."    "a b c" 0 5 #"")
  (t '()                        eof "^."    "a b c" 0 5 #"x")
  (t '((0 . 2) (2 . 4) (4 . 6)) eof "(?m:^.\n)" "a\nb\nc\n" 0 6)
  (t '((2 . 4) (4 . 6))         eof "(?m:^.\n)" "a\nb\nc\n" 0 6 #"x")
  (for-each (lambda (cvt)
              (test '((1 . 2) (3 . 4)) regexp-match-positions* (cvt #"\x80") #"a\x80z\x80q"))
            (list values byte-regexp byte-pregexp))
  ;; --------------------
  (t regexp-split)
  (t '("1" "2" "3" "4") eof "[abc]" "1a2b3c4")
  (t '("2" "3" "4") eof     "[abc]" "1a2b3c4" 2)
  (t '("" "3" "4") eof      "[abc]" "1a2b3c4" 3)
  (t '("1" "") "2b3c4"      "[abc]" "1a2b3c4" 0 2)
  (t '("1" "2") "b3c4"      "[abc]" "1a2b3c4" 0 3)
  (t '("1" "2" "3" "4") eof "[abc]" "1a2b3c4" 0 #f)
  (t '("1" "2" "3" "4") eof "[abc]" "1a2b3c4" 0 7)
  (t '("" "1" "2" "") eof   "[abc]" "a1b2c")
  (t '("1" "2" "") eof      "[abc]" "a1b2c" 1)
  (t '("" "2" "") eof       "[abc]" "a1b2c" 2)
  (t '("" "") "1b2c"        "[abc]" "a1b2c" 0 1)
  (t '("" "1") "b2c"        "[abc]" "a1b2c" 0 2)
  (t '("" "1" "2" "") eof   "[abc]" "a1b2c" 0)
  (t '("" "1" "2" "") eof   "[abc]" "a1b2c" 0 #f)
  (t '("" "1" "2" "") eof   "[abc]" "a1b2c" 0 5)
  (t '("" " b c")     eof   "^."    "a b c" 0 5 #"")
  (t '("a b c")       eof   "^."    "a b c" 0 5 #"x")
  (t '("" "" "" "")   eof "(?m:^.\n)" "a\nb\nc\n" 0 6)
  (t '("a\n" "" "")   eof "(?m:^.\n)" "a\nb\nc\n" 0 6 #"x")
  (for-each (lambda (cvt)
              (test '(#"" #"a" #"z" #"q" #"") regexp-split (cvt #"\x80") #"\x80a\x80z\x80q\x80"))
            (list values byte-regexp byte-pregexp))
  ;; --------------------
  (t regexp-match-peek-positions*)
  (err/rt-test (regexp-match-peek-positions* "[abc]" "a b c"))
  (t '((1 . 2) (3 . 4) (5 . 6)) " a b c " "[abc]" " a b c ")
  (t '((3 . 4) (5 . 6))         " a b c " "[abc]" " a b c " 2)
  (t '((3 . 4) (5 . 6))         " a b c " "[abc]" " a b c " 3)
  (t '((1 . 2))                 " a b c " "[abc]" " a b c " 0 2)
  (t '((1 . 2))                 " a b c " "[abc]" " a b c " 0 3)
  (t '((1 . 2) (3 . 4) (5 . 6)) " a b c " "[abc]" " a b c " 0 #f)
  (t '((1 . 2) (3 . 4) (5 . 6)) " a b c " "[abc]" " a b c " 0 7)
  (t '((0 . 1) (2 . 3) (4 . 5)) "a b c" "[abc]" "a b c")
  (t '((2 . 3) (4 . 5))         "a b c" "[abc]" "a b c" 1)
  (t '((2 . 3) (4 . 5))         "a b c" "[abc]" "a b c" 2)
  (t '((0 . 1))                 "a b c" "[abc]" "a b c" 0 1)
  (t '((0 . 1))                 "a b c" "[abc]" "a b c" 0 2)
  (t '((0 . 1) (2 . 3) (4 . 5)) "a b c" "[abc]" "a b c" 0 #f)
  (t '((0 . 1) (2 . 3) (4 . 5)) "a b c" "[abc]" "a b c" 0 5)
  (t '((0 . 1))                 "a b c" "^."    "a b c" 0 5 #"")
  (t '()                        "a b c" "^."    "a b c" 0 5 #"x")
  (t '((0 . 2) (2 . 4) (4 . 6)) "a\nb\nc\n" "(?m:^.\n)" "a\nb\nc\n" 0 6)
  (t '((2 . 4) (4 . 6))         "a\nb\nc\n" "(?m:^.\n)" "a\nb\nc\n" 0 6 #"x")
  ;; ---------- tests with zero-length matches ----------
  ;; Many of these tests can be repeated with Perl.  To try something in Perl,
  ;; put this code in a file:
  ;;   #!/usr/bin/perl
  ;;   sub test {
  ;;     my ($rx,$str) = @_; @words = split /$rx/, $str;
  ;;     print "(t '(";
  ;;     while (@words) {print '"'.shift(@words).'"'.(@words?" ":"");}
  ;;     print ") eof \"$rx\" \"$str\")\n";
  ;;   };
  ;;   test("[abc]","1a2b3");
  ;; and it will print a test that does what perl is doing.  Tests that differ
  ;; from Perl have explanations.
  ;;
  (t regexp-split)
  ;; test("a","a");
  ;; (t '() eof "a" "a")
  ;;   perl returns an empty list, we return '("" ""), and this is a difference
  ;;   that is unrelated to dealing with empty matches, just the way that
  ;;   perl's split throws out some empty matches (it throws empty matches at
  ;;   the end (but not at the beginning for some reason...))
  (t '("" "") eof "a" "a")
  ;; test("3","123");
  ;; (t '("12") eof "3" "123")
  ;;   a more straightforward demonstration of this
  (t '("12" "") eof "3" "123")
  ;; test("a","1a2a3");
  (t '("1" "2" "3") eof "a" "1a2a3")
  ;; test("","123");
  (t '("" "1" "2" "3" "") eof "" "123")
  ;; test("","12 34");
  (t '("" "1" "2" " " "3" "4" "") eof "" "12 34")
  ;; test(" *","123");
  (t '("" "1" "2" "3" "") eof " *" "123")
  ;; test(" *","12 34");
  (t '("" "1" "2" "" "3" "4" "") eof " *" "12 34")
  ;; test(" *"," 12 34 ");
  (t '("" "" "1" "2" "" "3" "4" "" "") eof " *" " 12 34 ")
  ;; test("2|", "1234");
  (t '("" "1" "" "3" "4" "") eof "2|" "1234")
  ;; test("1|", "1234");
  (t '("" "" "2" "3" "4" "") eof "1|" "1234")
  ;; test("4|", "1234");
  ;; (t '("1" "2" "3") eof "4|" "1234")
  ;;   perl perl drops the last empty string again
  (t '("" "1" "2" "3" "" "") eof "4|" "1234")
  ;; test("|2", "1234");
  (t '("" "1" "" "" "3" "4" "") eof "|2" "1234")
  ;; test("2|3", "1234");
  (t '("1" "" "4") eof "2|3" "1234")
  ;; test("2|3|4", "12345");
  (t '("1" "" "" "5") eof "2|3|4" "12345")
  ;; test("1|2", "1234");
  (t '("" "" "34") eof "1|2" "1234")
  ;; test("3|4", "1234");
  ;; (t '("12") eof "3|4" "1234")
  ;;   perl perl drops the last empty string again -- even two here
  (t '("12" "" "") eof "3|4" "1234")
  ;; test("2|3|4", "1234");
  ;; (t '("1") eof "2|3|4" "1234")
  ;;   ...and even three in this example
  (t '("1" "" "" "") eof "2|3|4" "1234")
  ;; test('$',"123");
  (t '("123" "") eof "$" "123")
  ;; test('^',"123");
  (t '("" "123") eof "^" "123")
  ;; test('^',"123\n456");
  (t '("" "123\n" "456") eof "(?m:^)" "123\n456")
  ;; test("\\b", "123 456");
  (t '("" "123" " " "456" "") eof #px"\\b" "123 456")
  ;; test("^|a", "abc");
  (t '("" "" "bc") eof "^|a" "abc")
  ;; some tests with bounds (these have no perl equivalences)
  (t '("" "1" "2" " " "3" "4" "") eof "" "12 34" 0)
  (t '("" "1" "2" " " "3" "4" "") eof "" "12 34" 0 #f)
  (t '("" "1" "2" " " "3" "4" "") eof "" "12 34" 0 5)
  (t '("" "" "1" "2" "" "3" "4" "" "") eof " *" " 12 34 " 0)
  (t '("" "" "1" "2" "" "3" "4" "" "") eof " *" " 12 34 " 0 #f)
  (t '("" "" "1" "2" "" "3" "4" "" "") eof " *" " 12 34 " 0 7)
  (t '("" "2" "3" "") eof " *" "123" 1)
  (t '("" "2" "" "3" "4" "") eof " *" "12 34" 1)
  (t '("" "2" "" "3" "4" "" "") eof " *" " 12 34 " 2)
  (t '("" "1" "") "23" "" "123" 0 1)
  (t '("" "2" "") "3" "" "123" 1 2)
  (t '("" "3" "") eof "" "123" 2 3)
  (t '("" "1" "2" "") "3" " *" "123" 0 2)
  (t '("" "2" "3" "") eof " *" "123" 1 3)
  ;; more tests with match*
  (t regexp-match*)
  (t '("" "" "" "") eof "" "123")
  (t '("" "" " " "" "" "") eof " *" "12 34")
  (t '(" " "" "" " " "" "" " " "") eof " *" " 12 34 ")
  (t '("" "" " " "" "" "") " " " *" " 12 34 " 1 6)
  (t regexp-match-positions*)
  (t '((0 . 0) (1 . 1) (2 . 2) (3 . 3)) eof "" "123")
  (t '((0 . 0) (1 . 1) (2 . 3) (3 . 3) (4 . 4) (5 . 5)) eof " *" "12 34")
  (t '((0 . 1) (1 . 1) (2 . 2) (3 . 4) (4 . 4) (5 . 5) (6 . 7) (7 . 7)) eof " *" " 12 34 ")
  (t '((1 . 1) (2 . 2) (3 . 4) (4 . 4) (5 . 5) (6 . 6)) " " " *" " 12 34 " 1 6)
  (t regexp-match-peek-positions*)
  (t '((0 . 0) (1 . 1) (2 . 2) (3 . 3)) "123" "" "123")
  (t '((0 . 0) (1 . 1) (2 . 3) (3 . 3) (4 . 4) (5 . 5)) "12 34" " *" "12 34")
  (t '((0 . 1) (1 . 1) (2 . 2) (3 . 4) (4 . 4) (5 . 5) (6 . 7) (7 . 7)) " 12 34 " " *" " 12 34 ")
  (t '((1 . 1) (2 . 2) (3 . 4) (4 . 4) (5 . 5) (6 . 6)) " 12 34 " " *" " 12 34 " 1 6)
  ;; finally, some tests for the match* + split property
  (t (lambda (rx str)
       (let ([s (regexp-split rx str)]
             [m (regexp-match* rx str)])
         (and (pair? s) (= (length s) (add1 (length m)))
              (apply (if (string? (car s)) string-append bytes-append)
                     (car s)
                     (append-map list m (cdr s)))))))
  (t "12 34"   #f " " "12 34")
  (t " 12 34 " #f " " " 12 34 ")
  (t "12 34"   #f " *" "12 34")
  (t " 12 34 " #f " *" " 12 34 ")
  (t "12 34"   #f "" "12 34")
  (t " 12 34 " #f "" " 12 34 ")
  )

;; ---------- string-append* ----------
(let ()
  (test ""           string-append* '())
  (test ""           string-append* '(""))
  (test ""           string-append* '("" ""))
  (test "0123456789" string-append* '("0123456789"))
  (test "0123456789" string-append* "0123456789" '())
  (test "0123456789" string-append* "0123456789" '(""))
  (test "0123456789" string-append* "0123456789" '("" ""))
  (test "0123456789" string-append* "01234567" '("8" "9")))

;; ---------- string-join ----------
(let ()
  (test ""    string-join '() " ")
  (test ""    string-join '("") " ")
  (test " "   string-join '("" "") " ")
  (test "x y" string-join '("x" "y") " ")
  (test "x"   string-join '("x") " "))

(report-errs)
