
(load-relative "loadtest.ss")

(SECTION 'mzlib-string)

(require (lib "string.ss"))

(let ([s1 (string-copy "Hello!")])
  (string-lowercase! s1)
  (test "hello!" 'lowercase s1)
  (string-uppercase! s1)
  (test "HELLO!" 'uppercase s1))

(let ([s (list->string
	  (let loop ([i 0])
	    (if (= i 256)
		null
		(cons (integer->char i) (loop (add1 i))))))])
  (test (list s) regexp-match (regexp-quote s) s)
  (test (string-append "!" s "!")
	regexp-replace (regexp-quote s) s (regexp-replace-quote (string-append "!" s "!"))))

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

(test '(#"a" #"b" #"c") regexp-match* "[abc]" (open-input-string "here's a buck"))
(test '(#"b" #"c") regexp-match* "[abc]" (open-input-string "here's a buck") 8)
(let ([s (open-input-string "here's a buck")])
  (test '(#"a") regexp-match* "[abc]" s 0 8)
  (test " buck" read-string 50 s))
(let ([s (open-input-string "here's a buck")])
  (test '(#"a" #"b" #"c") regexp-match* "[abc]" s 0 #f)
  (test eof read-char s))

(test '((7 . 8) (9 . 10) (11 . 12)) regexp-match-positions* "[abc]" "here's a buck")
(test '((9 . 10) (11 . 12)) regexp-match-positions* "[abc]" "here's a buck" 8)
(test '((7 . 8)) regexp-match-positions* "[abc]" "here's a buck" 0 8)
(test '((7 . 8) (9 . 10) (11 . 12)) regexp-match-positions* "[abc]" "here's a buck" 0 #f)

(test '((7 . 8) (9 . 10) (11 . 12)) regexp-match-positions* "[abc]" (open-input-string "here's a buck"))
(test '((9 . 10) (11 . 12)) regexp-match-positions* "[abc]" (open-input-string "here's a buck") 8)
(let ([s (open-input-string "here's a buck")])
  (test '((7 . 8)) regexp-match-positions* "[abc]" s 0 8)
  (test " buck" read-string 50 s))
(let ([s (open-input-string "here's a buck")])
  (test '((7 . 8) (9 . 10) (11 . 12)) regexp-match-positions* "[abc]" s 0 #f)
  (test eof read-char s))

(let ([s (open-input-string "here's a buck")])
  (test '((7 . 8) (9 . 10) (11 . 12)) regexp-match-peek-positions* "[abc]" (open-input-string "here's a buck"))
  (test '((9 . 10) (11 . 12)) regexp-match-peek-positions* "[abc]" (open-input-string "here's a buck") 8)
  (test '((7 . 8)) regexp-match-peek-positions* "[abc]" s 0 8)
  (test '((7 . 8) (9 . 10) (11 . 12)) regexp-match-peek-positions* "[abc]" s 0 #f)
  (test "here's a buck" read-string 50 s))

(test '("here's " " " "u" "k") regexp-split "[abc]" "here's a buck")
(test '(" " "u" "k") regexp-split "[abc]" "here's a buck" 8)
(test '("" "u" "k") regexp-split "[abc]" "here's a buck" 9)
(test '("" "u" "") regexp-split "[abc]" "here's a buck" 9 12)
(test '("here's " "") regexp-split "[abc]" "here's a buck" 0 8)
(test '("here's " " ") regexp-split "[abc]" "here's a buck" 0 9)
(test '("here's " " " "u" "k") regexp-split "[abc]" "here's a buck" 0 #f)

(test '(#"here's " #" " #"u" #"k") regexp-split "[abc]" (open-input-string "here's a buck"))
(test '(#" " #"u" #"k") regexp-split "[abc]" (open-input-string "here's a buck") 8)
(test '(#"" #"u" #"k") regexp-split "[abc]" (open-input-string "here's a buck") 9)
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

(report-errs)
