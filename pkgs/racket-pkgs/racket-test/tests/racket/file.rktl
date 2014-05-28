
(load-relative "loadtest.rktl")

(Section 'file)

(define testing.rktl (build-path (current-load-relative-directory) "testing.rktl"))

(test #t input-port? (current-input-port))
(test #t output-port? (current-output-port))
(test #t output-port? (current-error-port))
(test (void) current-input-port (current-input-port))
(test (void) current-output-port (current-output-port))
(test (void) current-error-port (current-error-port))
(test #t call-with-input-file testing.rktl input-port?)
(define this-file (open-input-file testing.rktl))
(test #t input-port? this-file)
(close-input-port this-file)
(define this-file (open-input-file testing.rktl #:mode 'binary))
(test #t input-port? this-file)
(close-input-port this-file)
(define this-file (open-input-file testing.rktl #:mode 'text))
(test #t input-port? this-file)
(arity-test input-port? 1 1)
(arity-test output-port? 1 1)
(arity-test current-input-port 0 1)
(arity-test current-output-port 0 1)
(arity-test current-error-port 0 1)
(err/rt-test (current-input-port 8))
(err/rt-test (current-output-port 8))
(err/rt-test (current-error-port 8))
(err/rt-test (current-input-port (current-output-port)))
(err/rt-test (current-output-port (current-input-port)))
(err/rt-test (current-error-port (current-input-port)))
(test #\; peek-char this-file)
(arity-test peek-char 0 2)
(arity-test peek-char-or-special 0 2)
(test #\; read-char this-file)
(arity-test read-char 0 1)
(arity-test read-char-or-special 0 1)
(test '(define cur-section '()) read this-file)
(arity-test read 0 1)
(test #\( peek-char this-file)
(test '(define errs '()) read this-file)
(close-input-port this-file)
(close-input-port this-file)
(arity-test close-input-port 1 1)
(arity-test close-output-port 1 1)
(err/rt-test (peek-char 5))
(err/rt-test (peek-char (current-output-port)))
(err/rt-test (read-char 5))
(err/rt-test (read-char (current-output-port)))
(err/rt-test (read 5))
(err/rt-test (read (current-output-port)))
(err/rt-test (close-input-port 5))
(err/rt-test (close-output-port 5))
(err/rt-test (close-input-port (current-output-port)))
(err/rt-test (close-output-port (current-input-port)))
(define (check-test-file name)
  (define test-file (open-input-file name))
  (test #t 'input-port?
	(call-with-input-file
	    name
	  (lambda (test-file)
	    (test load-test-obj read test-file)
	    (test #t eof-object? (peek-char test-file))
	    (test #t eof-object? (read-char test-file))
	    (input-port? test-file))))
  (test #\; read-char test-file)
  (test display-test-obj read test-file)
  (test load-test-obj read test-file)
  (close-input-port test-file))
(define write-test-obj
  '(#t #f #\a () 9739 -3 . #((test) "te \" \" st" "" test #() b c)))
(define display-test-obj
  '(#t #f a () 9739 -3 . #((test) te " " st test #() b c)))
(define load-test-obj
  (list 'define 'foo (list 'quote write-test-obj)))
(let ([f (lambda (test-file)
	   (write-char #\; test-file)
	   (display write-test-obj test-file)
	   (newline test-file)
	   (write load-test-obj test-file)
	   (output-port? test-file))])
  (test #t 'cwo (call-with-output-file
                    "tmp1" f #:exists 'truncate)))
(check-test-file "tmp1")

(test (string #\null #\null #\" #\\ #\u #\0 #\0 #\0 #\0 #\")
      'write-null
      (let ([p (open-output-string)])
	(write-char #\null p)
	(display (string #\null) p)
	(write (string #\null) p)
	(let ([s (get-output-string p)])
	  s)))

;; Test escapes (input):
(test (apply 
       string 
       (map 
	integer->char 
	'(7 8 9 10 12 13 27 11 92 34 65 32 5 65 15 80 15 80 221 68 255 55 1 49)))
      values "\a\b\t\n\f\r\e\v\\\"\101\40\5A\xFP\xfP\xdDD\3777\0011")
(err/rt-test (read (open-input-string "\"\\z\"")) exn:fail:read?)
(err/rt-test (read (open-input-string "\"\\xX\"")) exn:fail:read?)
(err/rt-test (read (open-input-string "\"\\x\"")) exn:fail:read?)
(err/rt-test (read (open-input-string "\"\\x")) exn:fail:read:eof?)
(err/rt-test (read (open-input-string "\"\\400\"")) exn:fail:read?)
(err/rt-test (read (open-input-string "\"\\8\"")) exn:fail:read?)

;; Test escape printing:
(parameterize ([current-locale #f])
  (test "\"\\a\\b\\t\\n\\f\\r\\e\\v\\\\\\\"A \\u0005A\\u000FP\\u000FP\u00DDD\u00FF7\\u00011\\U00054321\""
	'output-escapes
	(let ([p (open-output-string)])
	  (write "\a\b\t\n\f\r\e\v\\\"\101\40\5A\xFP\xfP\xdDD\3777\0011\U54321" p)
	  (get-output-string p))))

(parameterize ([current-locale #f])
  (test "#\"\\a\\b\\t\\n\\f\\r\\e\\v\\\\\\\"A \\5A\\17P\\17P\\335D\\3777\\0011\""
	'output-escapes
	(let ([p (open-output-string)])
	  (write #"\a\b\t\n\f\r\e\v\\\"\101\40\5A\xFP\xfP\xdDD\3777\0011" p)
	  (get-output-string p))))

;; Test return, linefeed, and return--linefeed escapes:
(test "12" values "1\
2")
(test "123" read (open-input-string (string #\" #\1 #\\ #\newline #\2 #\\ #\return #\3 #\")))
(test "123" read (open-input-string (string #\" #\1 #\\ #\return #\2 #\\ #\newline #\3 #\")))
(test "12\r3" read (open-input-string (string #\" #\1 #\\ #\return #\newline #\2 #\\ #\newline #\return #\3 #\")))
(test "1\r23" read (open-input-string (string #\" #\1 #\\ #\newline #\return #\2 #\\ #\return #\newline #\3 #\")))

;; test names of file handling procedures (see racket/private/kw-file)
(test 'open-input-file object-name open-input-file)
(test 'with-input-from-file object-name with-input-from-file)
(test 'call-with-output-file object-name call-with-output-file)

; Test string ports with file-position:
(let ([s (open-output-string)])
  (test (string) get-output-string s)
  (test 0 file-position s)
  (display "a" s)
  (test (string #\a) get-output-string s)
  (test 1 file-position s)
  (test (void) file-position s 10)
  (test 10 file-position s)
  (test (string #\a #\nul #\nul #\nul #\nul #\nul #\nul #\nul #\nul #\nul) get-output-string s)
  (display "z" s)
  (test (string #\a #\nul #\nul #\nul #\nul #\nul #\nul #\nul #\nul #\nul #\z) get-output-string s)
  (test 11 file-position s)
  (test (void) file-position s 3)
  (display "mmm" s)
  (test (string #\a #\nul #\nul #\m #\m #\m #\nul #\nul #\nul #\nul #\z) get-output-string s)
  (test 6 file-position s)
  (display "banana" s)
  (test (string #\a #\nul #\nul #\m #\m #\m #\b #\a #\n #\a #\n #\a) get-output-string s)
  (test 12 file-position s))
(let ([s (open-input-string "hello")])
  (test 0 file-position s)
  (test #\h read-char s)
  (test 1 file-position s)
  (test #\e read-char s)
  (test (void) file-position s 0)
  (test 0 file-position s)
  (test #\h read-char s)
  (test (void) file-position s 4)
  (test 4 file-position s)
  (test #\o read-char s)
  (test 5 file-position s)
  (test eof read-char s)
  (test 5 file-position s)
  (test (void) file-position s 502)
  (test eof read-char s)
  (test eof read-char s)
  (test 502 file-position s)
  (test (void) file-position s 2)
  (test #\l read-char s)
  (test 3 file-position s))

(let ([os (open-output-string)])
  (write '((0 54609) (1 32874234)) os)
  (file-position os 2)
  (file-position os eof)
  (test #"((0 54609) (1 32874234))" get-output-bytes os)
  (test #"((0 54" get-output-bytes os #f 0 6)
  (test #"0 54609) (1 32874234))" get-output-bytes os #f 2)
  (test #"0 54609) (1 32874234))" get-output-bytes os #f 2 #f)
  (test #"0 546" get-output-bytes os #f 2 7)
  (test #"((0 54609) (1 32874234))" get-output-bytes os #t)
  (test #"" get-output-bytes os))

(let ([os (open-output-string)])
  (write '1234 os)
  (file-position os 10)
  (write 'z os)
  (test #"1234\0\0\0\0\0\0z" get-output-bytes os)
  (file-position os 5)
  (test #"1234\0\0\0\0\0\0z" get-output-bytes os)
  (test #"1234\0" get-output-bytes os #f 0 (file-position os))
  (test #"34\0\0\0\0\0\0z" get-output-bytes os #f 2 #f)
  (test #"34\0\0\0\0\0\0z" get-output-bytes os #t 2 #f)
  (test #"" get-output-bytes os #t 0 #f))

(let ([os (open-output-string)])
  (write '1234 os)
  (file-position os 10)
  (file-position os eof)
  (write 'z os)
  (test #"1234\0\0\0\0\0\0z" get-output-bytes os)
  (test #"23" get-output-bytes os #t 1 3)
  (test #"" get-output-bytes os #f 0 #f))

(err/rt-test (get-output-bytes (open-output-bytes) #t 1))
(err/rt-test (get-output-bytes (open-output-bytes) #t 0 1))
(err/rt-test (get-output-bytes (open-output-string) #t 1))
(err/rt-test (get-output-bytes (open-output-string) #t 0 1))

(define s (open-output-string))
(err/rt-test (file-position 's 1))
(err/rt-test (file-position s 'one))
(err/rt-test (file-position s -1))
(err/rt-test (file-position s (expt 2 100)) exn:application:mismatch?)
(err/rt-test (file-position (make-input-port 'name void #f void) 100) exn:application:mismatch?)
(err/rt-test (file-position (make-output-port 'name always-evt void void) 100) exn:application:mismatch?)
(arity-test file-position 1 2)

(define (test-read-line r1 r2 s1 s2 flags sep)
  (let ([p (open-input-string (string-append s1
					     (apply string sep)
					     s2))])
    (test r1 apply read-line p flags)
    (test r2 apply read-line p flags)))
(define (add-return s t) (string-append s (string #\return) t))
(define (add-linefeed s t) (string-append s (string #\linefeed) t))

(test-read-line "ab" "cd" "ab" "cd" null '(#\linefeed))
(test-read-line (add-return "ab" "cd") eof "ab" "cd" null '(#\return))
(test-read-line (add-return "ab" "") "cd" "ab" "cd" null '(#\return #\linefeed))
(test-read-line "ab" "cd" "ab" "cd" '(return) '(#\return))
(test-read-line (add-linefeed "ab" "cd") eof "ab" "cd" '(return) '(#\linefeed))
(test-read-line "ab" (add-linefeed "" "cd") "ab" "cd" '(return) '(#\return #\linefeed))
(test-read-line (add-return "ab" "cd") eof "ab" "cd" '(return-linefeed) '(#\return))
(test-read-line (add-linefeed "ab" "cd") eof "ab" "cd" '(return-linefeed) '(#\linefeed))
(test-read-line "ab" "cd" "ab" "cd" '(return-linefeed) '(#\return #\linefeed))
(test-read-line (add-return "ab" "") "cd" "ab" "cd" '(return-linefeed) '(#\return #\return #\linefeed))
(test-read-line "ab" (add-linefeed "" "cd") "ab" "cd" '(return-linefeed) '(#\return #\linefeed #\linefeed))
(test-read-line "ab" "cd" "ab" "cd" '(any) '(#\return))
(test-read-line "ab" "cd" "ab" "cd" '(any) '(#\linefeed))
(test-read-line "ab" "cd" "ab" "cd" '(any) '(#\return #\linefeed))
(test-read-line "ab" "" "ab" "cd" '(any) '(#\linefeed #\return))
(test-read-line "ab" "cd" "ab" "cd" '(any-one) '(#\return))
(test-read-line "ab" "cd" "ab" "cd" '(any-one) '(#\linefeed))
(test-read-line "ab" "" "ab" "cd" '(any-one) '(#\return #\linefeed))
(test-read-line "ab" "" "ab" "cd" '(any-one) '(#\linefeed #\return))

(arity-test read-line 0 2)
(err/rt-test (read-line 8))
(err/rt-test (read-line 'any))
(err/rt-test (read-line (current-input-port) 8))
(err/rt-test (read-line (current-input-port) 'anyx))

(arity-test open-input-file 1 1)
(err/rt-test (open-input-file 8))
(err/rt-test (open-input-file "x" 8))
(err/rt-test (open-input-file "x" 'something-else))
(err/rt-test (open-input-file "badfile") exn:fail:filesystem:errno?)
(err/rt-test (open-input-file "badfile" #:for-module? #t) exn:fail:filesystem:errno?)
(err/rt-test (parameterize ([current-module-path-for-load 'badfile])
               (open-input-file "badfile" #:for-module? #t))
             exn:fail:filesystem:missing-module?)
(err/rt-test (parameterize ([current-module-path-for-load #'badfile])
               (open-input-file "badfile" #:for-module? #t))
             exn:fail:syntax:missing-module?)

(arity-test open-output-file 1 1)
(err/rt-test (open-output-file 8))
(err/rt-test (open-output-file "x" 8))
(err/rt-test (open-output-file "x" 'something-else))
(let ([conflict? exn:application:mismatch?]
      [modes '(binary text)]
      [replacement '(error replace truncate append truncate/replace update)])
  (for-each
   (lambda (ones)
     (for-each
      (lambda (one)
	(err/rt-test (open-output-file "x" one 'bad))
	(err/rt-test (open-output-file "x" one 8))
	(err/rt-test (open-output-file "x" 'bad one))
	(err/rt-test (open-output-file "x" 8 one))
	(err/rt-test (call-with-output-file "x" void one 'bad))
	(err/rt-test (call-with-output-file "x" void one 8))
	(err/rt-test (call-with-output-file "x" void 'bad one))
	(err/rt-test (call-with-output-file "x" void 8 one))
	(err/rt-test (with-output-to-file "x" void one 8))
	(err/rt-test (with-output-to-file "x" void one 'bad))
	(err/rt-test (with-output-to-file "x" void 8 one))
	(err/rt-test (with-output-to-file "x" void 'bad one))
	(for-each
	 (lambda (two)
	   (err/rt-test (open-output-file "x" one two) conflict?)
	   (err/rt-test (call-with-output-file "x" void one two) conflict?)
	   (err/rt-test (with-output-to-file "x" void one two) conflict?))
	 ones))
      ones))
   `(,modes ,replacement)))
(err/rt-test (open-output-file (build-path (current-directory) "baddir" "x"))
	    exn:fail:filesystem?)

(let ([tf (make-temporary-file)])
  (let-values ([(base name dir?) (split-path tf)])
    (test #t 'make-temporary-file-uses-srcloc (and (regexp-match #rx"file.rktl" (path->bytes name)) #t)))
  (delete-file tf))

(let ([tf ((λ (t) (t)) make-temporary-file)])
  (test #t 'make-temporary-file-in-ho-position (file-exists? tf))
  (delete-file tf))


(define tempfilename (make-temporary-file))
(when (file-exists? tempfilename)
  (delete-file tempfilename))
(let ([p (open-output-file tempfilename)])
  (err/rt-test (write-special 'foo p) exn:application:mismatch?)
  (test #t integer? (port-file-identity p))
  (let ([q (open-input-file tempfilename)])
    (test (port-file-identity p) port-file-identity q)
    (close-input-port q)
    (err/rt-test (file-position q) exn:fail?)
    (err/rt-test (port-file-identity q) exn:fail?))
  (close-output-port p)
  (err/rt-test (file-position p) exn:fail?)
  (err/rt-test (port-file-identity p) exn:fail?))
(err/rt-test (let ([c (make-custodian)])
	       (let ([p (parameterize ([current-custodian c])
				      (open-output-file tempfilename #:exists 'replace))])
		 (custodian-shutdown-all c)
		 (display 'hi p)))
	    exn:fail?)
(err/rt-test (open-output-file tempfilename #:exists 'error) exn:fail:filesystem?)
(define p (open-output-file tempfilename #:exists 'replace))
(display 7 p)
(display "" p)
(close-output-port p)
(close-output-port (open-output-file tempfilename #:exists 'truncate))
(define p (open-input-file tempfilename))
(test eof read p)
(close-input-port p)
(define p (open-output-file tempfilename #:exists 'replace))
(display 7 p)
(close-output-port p)
(define p (open-output-file tempfilename #:exists 'append))
(display 7 p)
(close-output-port p)
(err/rt-test (display 9 p) exn:fail?)
(err/rt-test (write 9 p) exn:fail?)
(err/rt-test (write-char #\a p) exn:fail?)

(err/rt-test (let ([c (make-custodian)])
	       (let ([p (parameterize ([current-custodian c])
				      (open-input-file tempfilename))])
		 (custodian-shutdown-all c)
		 (read p)))
	    exn:fail?)
(define p (open-input-file tempfilename))
(test 77 read p)
(close-input-port p)
(err/rt-test (read p) exn:fail?)
(err/rt-test (read-char p) exn:fail?)
(err/rt-test (char-ready? p) exn:fail?)

(define-values (in-p out-p) (open-input-output-file tempfilename #:exists 'update))
(test #\7 read-char in-p)
(close-output-port out-p)
(test #\7 read-char in-p)
(test eof read-char in-p)
(close-input-port in-p)

(define p (open-output-file tempfilename #:exists 'update))
(display 6 p)
(close-output-port p)
(test 2 file-size tempfilename)
(define p (open-input-file tempfilename))
(test 67 read p)
(test eof read p)
(close-input-port p)

(define p (open-output-file tempfilename #:exists 'update))
(file-position p 1)
(display 68 p)
(close-output-port p)
(test 3 file-size tempfilename)
(define p (open-input-file tempfilename))
(test 0 file-position p)
(test 668 read p)
(test 3 file-position p)
(test eof read p)
(test 3 file-position p)
(file-position p 1)
(test 1 file-position p)
(test #\6 read-char p)
(test #\8 read-char p)
(file-position p 0)
(test 0 file-position p)
(test #\6 read-char p)
(test 1 file-position p)
(file-position p 2)
(test #\8 read-char p)
(test 3 file-position p)
(close-input-port p)

(close-output-port (open-output-file tempfilename #:exists 'truncate/replace))
(define p (open-input-file tempfilename))
(test eof read p)
(close-input-port p)

(define-values (in-p out-p) (open-input-output-file tempfilename #:exists 'update))
(fprintf out-p "hi\n")
(flush-output out-p)
(test eof read-char in-p)
(test 3 file-position out-p)
(test 3 file-position in-p)
(file-position out-p 0)
(test 0 file-position out-p)
(test 0 file-position in-p)
(test #\h read-char in-p) ; might read more characters into a buffer!
(file-position out-p 1)
(close-input-port in-p)
(test 1 file-position out-p)
(write-char #\x out-p)
(close-output-port out-p)
(test 'hx with-input-from-file tempfilename read)

(let ([o (open-output-file tempfilename #:exists 'truncate)])
  (close-output-port o))
(test 0 file-size tempfilename)
(let ([o (open-output-file tempfilename #:exists 'update)])
  (file-position o 899)
  (write-byte 0 o)
  (close-output-port o))
(test 900 file-size tempfilename)
(let ([o (open-output-file tempfilename #:exists 'update)])
  (file-truncate o 399)
  (close-output-port o))
(test 399 file-size tempfilename)

(delete-file tempfilename)

(arity-test call-with-input-file 2 2)
(arity-test call-with-output-file 2 2)
(arity-test with-input-from-file 2 2)
(arity-test with-output-to-file 2 2)

(err/rt-test (call-with-input-file "x" 8))
(err/rt-test (call-with-input-file  8 (lambda (x) x)))
(err/rt-test (call-with-input-file  8 (lambda () 9)))
(err/rt-test (call-with-input-file  "x" (lambda (x) x) 8))
(err/rt-test (call-with-input-file  "x" (lambda (x) x) 'bad))

(err/rt-test (call-with-output-file "x" 8))
(err/rt-test (call-with-output-file  8 (lambda (x) x)))
(err/rt-test (call-with-output-file  8 (lambda () 9)))
(err/rt-test (call-with-output-file  "x" (lambda (x) x) 8))
(err/rt-test (call-with-output-file  "x" (lambda (x) x) 'bad))

(err/rt-test (with-input-from-file "x" 8))
(err/rt-test (with-input-from-file  8 (lambda () 9)))
(err/rt-test (with-input-from-file  8 (lambda (x) x)))
(err/rt-test (with-input-from-file  "x" (lambda () 9) 8))
(err/rt-test (with-input-from-file  "x" (lambda () 9) 'bad))

(err/rt-test (with-output-to-file "x" 8))
(err/rt-test (with-output-to-file  8 (lambda () 9)))
(err/rt-test (with-output-to-file  8 (lambda (x) x)))
(err/rt-test (with-output-to-file  "x" (lambda () 9) 8))
(err/rt-test (with-output-to-file  "x" (lambda () 9) 'bad))

(define s (open-output-string))
(test #f input-port? s)
(test #t output-port? s)
(let ([c (current-output-port)])
  (current-output-port s) 
  (display 8)
  (current-output-port c))
(test "8" get-output-string s)
(let ([c (current-error-port)])
  (current-error-port s) 
  (display 9 (current-error-port))
  (current-error-port c))
(test "89" get-output-string s)
(define s2 (open-input-string (get-output-string s)))
(test #t input-port? s2)
(test #f output-port? s2)
(test 89 + 0
      (let ([c (current-input-port)])
	(current-input-port s2) 
	(begin0
	 (read)
	 (current-input-port c))))
(test eof read s2)

(arity-test open-output-string 0 1)
(arity-test open-input-string 1 2)
(arity-test get-output-string 1 1)

(arity-test open-output-bytes 0 1)
(arity-test open-input-bytes 1 2)
(arity-test get-output-bytes 1 4)

(test 75 object-name (open-input-string "x" 75))
(test 76 object-name (open-input-bytes #"x" 76))
(test 175 object-name (open-output-string 175))
(test 176 object-name (open-output-bytes 176))

(err/rt-test (get-output-string 9))
(err/rt-test (get-output-string (current-output-port)))

(let ([p (open-input-string "a\nb")])
  (test '(#f #f 1) call-with-values (lambda () (port-next-location p)) list)
  (port-count-lines! p)
  (test '(1 0 1) call-with-values (lambda () (port-next-location p)) list)
  (test #\a read-char p)
  (test '(1 1 2) call-with-values (lambda () (port-next-location p)) list)
  (test '(1 1 2) call-with-values (lambda () (port-next-location p)) list)
  (test #\newline peek-char p)
  (test '(1 1 2) call-with-values (lambda () (port-next-location p)) list)
  (test #\newline read-char p)
  (test '(2 0 3) call-with-values (lambda () (port-next-location p)) list)
  (test #\b peek-char p)
  (test '(2 0 3) call-with-values (lambda () (port-next-location p)) list)
  (test #\b read-char p)
  (test '(2 1 4) call-with-values (lambda () (port-next-location p)) list)
  (test eof read-char p)
  (test '(2 1 4) call-with-values (lambda () (port-next-location p)) list)
  (test eof read-char p)
  (test '(2 1 4) call-with-values (lambda () (port-next-location p)) list))

(define-values (out in) (make-pipe))
(test #t input-port? out)
(test #t output-port? in)
(let loop ([n 1000])
  (unless (zero? n)
	  (display n in)
	  (newline in)
	  (loop (sub1 n))))
(let loop ([n 999])
  (unless (zero? n)
	  (read out)
	  (loop (sub1 n))))
(test 1 read out)
(close-output-port in)
(test eof read out)
(close-input-port out)

(define-values (in out) (make-pipe 3))
(test 3 write-bytes-avail #"12345" out)
(let ([s (make-bytes 5 (char->integer #\-))])
  (test 3 read-bytes-avail! s in)
  (test #"123--" values s))
(display 1 out)
(test 2 write-bytes-avail #"2345" out)
(let ([th1 (thread (lambda ()
		     (display "a" out)))]
      [th2 (thread (lambda ()
		      (display "a" out)))]
      [th3 (thread (lambda ()
		      (display "a" out)))])
  (test #t thread-running? th1)
  (test #t thread-running? th2)
  (test #t thread-running? th3)

  (test 49 read-byte in)
  
  (sleep 0.1)

  (test 2 + 
	(if (thread-running? th1) 1 0)
	(if (thread-running? th2) 1 0)
	(if (thread-running? th3) 1 0))

  (test 50 read-byte in)

  (sleep 0.1)

  (test 1 + 
	(if (thread-running? th1) 1 0)
	(if (thread-running? th2) 1 0)
	(if (thread-running? th3) 1 0))
  
  (test 51 read-byte in)
  
  (sleep 0.1)

  (test #f thread-running? th1)
  (test #f thread-running? th2)
  (test #f thread-running? th3)

  (close-output-port out)

  (test #"aaa" read-bytes 10 in))
(close-input-port in)

(arity-test write-bytes-avail 1 4)
(arity-test write-bytes-avail* 1 4)
(arity-test write-bytes-avail/enable-break 1 4)

(arity-test make-pipe 0 3)
(err/rt-test (make-pipe 0))
(err/rt-test (make-pipe -1))
(err/rt-test (make-pipe (- (expt 2 40))))
(err/rt-test (make-pipe "hello"))

(let-values ([(r w) (make-pipe #f 'in)])
  (test 'in object-name r)
  (test 'pipe object-name w))
(let-values ([(r w) (make-pipe #f 'in 'out)])
  (test 'in object-name r)
  (test 'out object-name w))

(test #t input-port? (make-input-port void void void void))
(test #t input-port? (make-input-port void void #f void))
(test #t input-port? (make-input-port 1000 void #f void))
(test 1000 object-name (make-input-port 1000 void #f void))
(err/rt-test (read (make-input-port #f void void void)))
(err/rt-test (read-char (make-input-port #f void void void)))
(err/rt-test (peek-char (make-input-port #f void void void)))
(arity-test make-input-port 4 10)
(err/rt-test (make-custom-input-port #f 8 void void))
(err/rt-test (make-custom-input-port #f void 8 void))
(err/rt-test (make-custom-input-port #f void void 8))
(err/rt-test (make-custom-input-port #f cons void void))
(err/rt-test (make-custom-input-port #f void add1 void))
(err/rt-test (make-custom-input-port #f void void add1))

(test #t output-port? (make-output-port #f always-evt void void))
(test #t output-port? (make-output-port #f always-evt void void))
(test 7786 object-name (make-output-port 7786 always-evt void void))
(arity-test make-output-port 4 11)
(err/rt-test (make-output-port #f 8 void void void))
(err/rt-test (make-output-port #f always-evt 8 void))
(err/rt-test (make-output-port #f always-evt void 8))
(err/rt-test (make-output-port #f always-evt add1 void))
(err/rt-test (make-output-port #f always-evt void add1))
(err/rt-test (write-special 'foo (make-custom-output-port void always-evt void void)) exn:application:mismatch?)

(let ([p (make-input-port 
	  'name
	  (lambda (s) (bytes-set! s 0 97) 1)
	  (lambda (s skip progress-evt)
	    (test 0 'skip-is-0 skip)
	    (bytes-set! s 0 98) 1)
	  void)])
  (test #\a read-char p)
  (test #\b peek-char p)
  (test #\a read-char p)
  (test #\b peek-char p)
  (test #\b peek-char p)
  (test #\a read-char p)
  (test 3 file-position p))

(let* ([s (open-input-string "(apple \"banana\" [coconut])")]
       [p (make-input-port 
	   'name
	   (lambda (str)
	     (if (or (byte-ready? s)
		     (zero? (random 2)))
		 (begin
		   (bytes-set! str 0 (read-byte s))
		   1)
		 s))
	   (lambda (str skip progress-evt)
	     (if (or (byte-ready? s)
		     (zero? (random 2)))
		 (begin
		   (bytes-set! str 0 (peek-byte s))
		   1)
		 s))
	   void)])
  (test '(apple "banana" [coconut]) read p))

(let ([test-file (open-output-file "tmp2" #:exists 'truncate)])
  (test 7 write-string (make-string 7 #\a) test-file)
  (test 4095 write-string (make-string 4095 #\b) test-file)
  (test 4096 write-string (make-string 4096 #\c) test-file)
  (test 4097 write-string (make-string 4097 #\d) test-file)
  (test (+ 7 4095 4096 4097) file-position test-file)
  (close-output-port test-file)
  (test (+ 7 4095 4096 4097) file-size "tmp2"))

(let ([go
       (lambda (write-bytes)
	 (let ([test-file (open-output-file "tmp2" #:exists 'truncate)])
	   (test 7 write-bytes-avail (make-bytes 7 97) test-file)
	   (test 4095 write-bytes (make-bytes 4095 98) test-file)
	   (test 4096 write-bytes (make-bytes 4096 99) test-file)
	   (test 4097 write-bytes (make-bytes 4097 100) test-file)
	   (test (+ 7 4095 4096 4097) file-position test-file)
	   (close-output-port test-file)
	   (test (+ 7 4095 4096 4097) file-size "tmp2")))])
  (go write-bytes)
  (go write-bytes-avail)
  (go write-bytes-avail*))

(let ([p (open-input-file "tmp1")]
      [q (open-input-file "tmp2")])
  (test #f = (port-file-identity p) (port-file-identity q))
  (close-input-port p)
  (close-input-port q))

;; We should be able to install the current permissions:
(test (void) file-or-directory-permissions "tmp1" (file-or-directory-permissions "tmp1" 'bits))

(define test-file 
  (open-output-file "tmp2" #:exists 'truncate))
(write-char #\; test-file)
(display write-test-obj test-file)
(newline test-file)
(write load-test-obj test-file)
(test #t output-port? test-file)
(close-output-port test-file)
(check-test-file "tmp2")

(let-values ([(p p-out) (open-input-output-file "tmp2" #:exists 'update)])
  (test #t port-try-file-lock? p 'shared)
  (test #t port-try-file-lock? p 'shared)
  (let ([p2 (open-input-file "tmp2")])
    (test #t port-try-file-lock? p2 'shared)
    (test #t port-try-file-lock? p2 'shared)
    (test (void) port-file-unlock p2)
    (close-input-port p2))
  (let ([p3 (open-output-file "tmp2" #:exists 'update)])
    (test #f port-try-file-lock? p3 'exclusive)
    (test (void) port-file-unlock p)
    (when (eq? (system-type) 'windows)
      ;; need another unlock, since we got a 'shared lock twice
      (test #f port-try-file-lock? p3 'exclusive)
      (test (void) port-file-unlock p))
    (test #t port-try-file-lock? p3 'exclusive)
    (test (not (eq? 'windows (system-type))) port-try-file-lock? p3 'exclusive)
    (test #f port-try-file-lock? p 'shared)
    (close-output-port p3))
  (err/rt-test (port-try-file-lock? p 'exclusive))
  (err/rt-test (port-try-file-lock? p-out 'shared))
  (test #t port-try-file-lock? p-out 'exclusive)
  (close-input-port p)
  (close-output-port p-out))

(define ui (make-input-port 'name (lambda (s) (bytes-set! s 0 (char->integer #\")) 1) #f void))
(test "" read ui)
(arity-test (port-read-handler ui) 1 2)
(err/rt-test ((port-read-handler ui) 8))
(let ([old (port-read-handler ui)])
  (port-read-handler ui (case-lambda [(x) "hello"][(x y) #'"goodbye"]))
  (test "hello" read ui)
  (test "goodbye" syntax-e (read-syntax 'x ui))
  (port-read-handler ui old)
  (test "" read ui))
(let ([old (port-read-handler ui)])
  (port-read-handler ui (case-lambda [(x) eof][(x y) eof]))
  (test eof read ui)
  (test eof read-syntax 'x ui)
  (port-read-handler ui old)
  (test "" read ui))
(arity-test port-read-handler 1 2)
(err/rt-test (port-read-handler 1))
(err/rt-test (port-read-handler ui 8))
(err/rt-test (port-read-handler (current-output-port) 8))
(err/rt-test (port-read-handler ui (lambda () 9)))
(err/rt-test (port-read-handler ui (lambda (x) 9)))
(err/rt-test (port-read-handler ui (lambda (x y) 9)))
(err/rt-test (port-read-handler ui (lambda (x y z) 9)))

(define sp (open-output-string))
(test (void) display "hello" sp)
(test "hello" get-output-string sp)
(test (void) write "hello" sp)
(test "hello\"hello\"" get-output-string sp)
(arity-test (port-display-handler sp) 2 2)
(arity-test (port-write-handler sp) 2 2)
(arity-test (port-print-handler sp) 2 3)
(err/rt-test ((port-display-handler sp) 8 8))
(err/rt-test ((port-write-handler sp) 8 8))
(err/rt-test ((port-print-handler sp) 8 8))
(let ([oldd (port-display-handler sp)]
      [oldw (port-write-handler sp)]
      [oldp (port-print-handler sp)]
      [adding (let ([s "hello\"hello\""])
		(lambda (a)
		  (set! s (string-append s a))
		  s))])
  (port-display-handler sp (lambda (v p) (oldd "X" p) (values 1 2)))
  (test (void) display "hello" sp)
  (test (adding "X") get-output-string sp)
  (test (void) write "hello" sp)
  (test (adding "\"hello\"") get-output-string sp)
  (test (void) print "hello" sp)
  (test (adding "\"hello\"") get-output-string sp)

  (port-write-handler sp (lambda (v p) (oldd "Y" p) 5))
  (test (void) display "hello" sp)
  (test (adding "X") get-output-string sp)
  (test (void) write "hello" sp)
  (test (adding "Y") get-output-string sp)
  (test (void) print "hello" sp)
  (test (adding "\"hello\"") get-output-string sp)
  (parameterize ([global-port-print-handler display])
     (test (void) print "hello" sp)
     (test (adding "X") get-output-string sp))
  (parameterize ([global-port-print-handler oldd])
     (test (void) print "hello" sp)
     (test (adding "hello") get-output-string sp))
  (test (void) print "hello" sp)
  (test (adding "\"hello\"") get-output-string sp)
		

  (port-print-handler sp (lambda (v p) (oldd "Z" p) 5))
  (test (void) display "hello" sp)
  (test (adding "X") get-output-string sp)
  (test (void) write "hello" sp)
  (test (adding "Y") get-output-string sp)
  (test (void) print "hello" sp)
  (test (adding "Z") get-output-string sp)
  (parameterize ([global-port-print-handler display])
     (test (void) print "hello" sp)
     (test (adding "Z") get-output-string sp))
  (test (void) print "hello" sp)
  (test (adding "Z") get-output-string sp)

  (port-display-handler sp oldd)
  (test (void) display "hello" sp)
  (test (adding "hello") get-output-string sp)
  (test (void) write "hello" sp)
  (test (adding "Y") get-output-string sp)

  (port-write-handler sp oldw)
  (test (void) display "hello" sp)
  (test (adding "hello") get-output-string sp)
  (test (void) write "hello" sp)
  (test (adding "\"hello\"") get-output-string sp)

  (port-display-handler sp oldw)
  (port-write-handler sp oldd)
  (port-print-handler sp oldp)
  (test (void) display "hello" sp)
  (test (adding "\"hello\"") get-output-string sp)
  (test (void) write "hello" sp)
  (test (adding "hello") get-output-string sp)
  (test (void) print "goodbye" sp)
  (test (adding "\"goodbye\"") get-output-string sp)
  (port-display-handler sp oldd)
  (port-write-handler sp oldw))
(err/rt-test (port-display-handler 1))
(err/rt-test (port-display-handler sp 8))
(err/rt-test (port-display-handler (current-input-port) 8))
(err/rt-test (port-display-handler sp (lambda (x) 9)))
(err/rt-test (port-display-handler sp (lambda (x y z) 9)))
(err/rt-test (port-write-handler 1))
(err/rt-test (port-write-handler sp 8))
(err/rt-test (port-write-handler (current-input-port) 8))
(err/rt-test (port-write-handler sp (lambda (x) 9)))
(err/rt-test (port-write-handler sp (lambda (x y z) 9)))

;;------------------------------------------------------------
;; peek-string and variants:

(let ([p (open-input-string "Hello World!")])
  (test "World!" peek-string 6 6 p)
  (test "World!" peek-string 7 6 p)
  (test "He" read-string 2 p)
  (test "rld!" peek-string 7 6 p)
  (test eof peek-string 7 600 p))

(define (test-a-port p go sync)
  (let* ([s (bytes 49 50 51)]
	 [reset-s! (lambda ()
		     (bytes-set! s 0 49)
		     (bytes-set! s 1 50)
		     (bytes-set! s 2 51))]
	 [test-empty (lambda()
		       (test #f byte-ready? p)
		       (test 0 peek-bytes-avail!* s 0 #f p)
		       (test 0 peek-bytes-avail!* s 1 #f p)
		       (test 0 read-bytes-avail!* s p))])
    (test-empty)

    (test 0 peek-bytes-avail!* s 500 #f p)
    (test 0 read-bytes-avail!* s p)

    (let ([test-basic
	   (lambda (str sync?)
	     (go) 
	     (when sync?
	       (sync p)
	       (test #t byte-ready? p)
	       (test 1 peek-bytes-avail!* s 0 #f p)
	       (test str values s)
	       (reset-s!))
	     (test 1 peek-bytes-avail! s 0 #f p)
	     (test str values s)
	     (reset-s!)
	     (test 1 peek-bytes-avail!* s 0 #f p)
	     (test str values s)
	     (reset-s!)
	     (test 1 read-bytes-avail!* s p)
	     (test str values s)
	     (reset-s!))])
      (test-basic #"A23" #t)
      (test-basic #"B23" #f))

    (test-empty)

    (go) (go)

    (let ([peek0
	   (lambda ()
	     (let ([avail (peek-bytes-avail! s 0 #f p)])
	       (cond
		[(= avail 1) (test #"C23" values s)]
		[(= avail 2) (test #"CD3" values s)]
		[else (test 1-or-2 values avail)])))])
      (peek0)
      (reset-s!)  
      (test 1 peek-bytes-avail! s 1 #f p)
      (test #"D23" values s)
      (reset-s!)
      (peek0)
      (reset-s!)
      (test 0 peek-bytes-avail!* s 2 #f p)
      (test 2 read-bytes-avail! s p)
      (test #"CD3" values s)

      (test-empty)

      (go) (go) (go)
      (test #"E" peek-bytes 1 0 p)
      (test #"F" peek-bytes 1 1 p)
      (test #"G" peek-bytes 1 2 p)
      (test 0 peek-bytes-avail!* s 3 #f p)
      (test #"EFG" read-bytes 3 p)

      (test-empty)

      (go) (go) (go)
      (test #"HI" peek-bytes 2 0 p)
      (test #"IJ" peek-bytes 2 1 p)
      (test #"J" peek-bytes 1 2 p)
      (test 0 peek-bytes-avail!* s 3 #f p)
      (test #"HI" read-bytes 2 p)
      (test #"J" read-bytes 1 p)

      (test-empty)

      (go) (go) (go)
      (test #"KLM" peek-bytes 3 0 p)
      (test #"LM" peek-bytes 2 1 p)
      (test #"M" peek-bytes 1 2 p)
      (test #"K" read-bytes 1 p)
      (test #"L" read-bytes 1 p)
      (test #"M" read-bytes 1 p)
      
      (test-empty))))

(define (gdelay go)
  (lambda ()
    (thread go)))

(define (gsync port)
  (sync port))

;; Test custom port with test-a-port
(define (test-a-custom-port supply-peek? gdelay gsync)
  (let* ([counter 0]
	 [lock (make-semaphore 1)]
	 [ready-sema (make-semaphore)]
	 [extras null]
	 [go (lambda ()
	       (semaphore-wait lock)
	       (semaphore-post ready-sema)
	       (for-each semaphore-post extras)
	       (set! extras null)
	       (semaphore-post lock))]
	 [p (make-input-port
	     'name
	     ;; read-bytes:
	     (lambda (s)
	       (if (semaphore-try-wait? lock)
		   (begin0
		    (let loop ([got 0])
		      (if (and (got . < . (bytes-length s))
			       (semaphore-try-wait? ready-sema))
			  (begin
			    (bytes-set! s got (+ 65 counter))
			    (set! counter (add1 counter))
			    (loop (add1 got)))
			  (if (zero? got)
			      (wrap-evt
			       (semaphore-peek-evt ready-sema)
			       (lambda (x) 0))
			      got)))
		    (semaphore-post lock))
		   (wrap-evt
		    (semaphore-peek-evt lock)
		    (lambda (x) 0))))
	     (and supply-peek?
		  (lambda (s d progress-evt)
		    (if (semaphore-try-wait? lock)
			(begin0
			 (let loop ([d d][counter counter])
			   (if (semaphore-try-wait? ready-sema)
			       (begin0
				(cond
				 [(zero? d)
				  (bytes-set! s 0 (+ 65 counter))
				  1]
				 [else
				  (loop (sub1 d) (add1 counter))])
				(semaphore-post ready-sema))
			       ;; Provide a new semaphore to be posted
			       ;; when new things appear:
			       (let ([s (make-semaphore)])
				 (set! extras (cons s extras))
				 (wrap-evt s (lambda (x) 0)))))
			 (semaphore-post lock))
			(wrap-evt
			 (semaphore-peek-evt lock)
			 (lambda (v) 0)))))
	     void)])
    (test-a-port p (gdelay go) gsync)))

(test-a-custom-port #f values void)
(test-a-custom-port #t values void)
(test-a-custom-port #f gdelay gsync)
(test-a-custom-port #t gdelay gsync)

;; Pipe
(define (test-a-pipe gdelay gsync)
  (let-values ([(r w) (make-pipe)])
    (let* ([counter 0]
	   [go (lambda ()
		 (write-byte (+ 65 counter) w)
		 (set! counter (add1 counter)))])
      (test-a-port r (gdelay go) gsync))))

(test-a-pipe values void)
(test-a-pipe gdelay gsync)

(arity-test read-bytes 1 2)
(arity-test peek-bytes 2 3)
(arity-test read-string 1 2)
(arity-test peek-string 2 3)
(arity-test read-bytes! 1 4)
(arity-test peek-bytes! 2 5)
(arity-test read-string! 1 4)
(arity-test peek-string! 2 5)
(arity-test read-bytes-avail! 1 4)
(arity-test peek-bytes-avail! 2 6)
(arity-test read-bytes-avail!* 1 4)
(arity-test peek-bytes-avail!* 2 6)
(arity-test read-bytes-avail!/enable-break 1 4)
(arity-test peek-bytes-avail!/enable-break 2 6)

(let ([fill-a
       (lambda (s pos)
	 (let ([l (bytes-length s)])
	   (let loop ([i 0])
	     (unless (= i l)
	       (bytes-set! s i (+ 48 (modulo (+ i pos) 10)))
	       (loop (add1 i))))
	   l))]
      [pos 0])
  (let ([p (make-input-port 
	    'name
	    (lambda (s) (let ([n (fill-a s pos)])
			  (set! pos (+ pos n))
			  n))
	    (lambda (s skip progress-evt) (fill-a s (+ pos skip)))
	    void)])
    (test 48 read-byte p)
    (test 49 peek-byte p)
    (test 49 read-byte p)
    (test #"234" read-bytes 3 p)
    (test #"567" peek-bytes 3 0 p)
    (test #"567890" read-bytes 6 p)
    (test #"123" peek-bytes 3 500 p)
    (test #"123" peek-bytes 3 (expt 10 100) p)
    (test #"567" peek-bytes 3 (+ 4 (expt 10 100)) p)
    (test #f regexp-match #"11" p 0 10000)
    (test #"123" read-bytes 3 p)
    (test '(#"0") regexp-match #"0" p 0)
    (let ([x (+ 9 (expt 10 100))])
      (test (list (cons x (add1 x))) regexp-match-peek-positions #"0" p (expt 10 100)))))

;;------------------------------------------------------------

;; Test custom output port
(let ([l null]
      [s (make-semaphore)]
      [flushed? #f]
      [spec #f])
  (let ([p (make-output-port
	    'name
	    (semaphore-peek-evt s)
	    (lambda (bytes start end non-block? breakable?)
	      (define can-block? (not non-block?))
	      (let loop ()
		(if (= start end)
		    (begin
		      (set! flushed? #t)
		      0)
		    (if (if can-block?
			    (semaphore-wait s)
			    (semaphore-try-wait? s))
			(let ([len (if can-block?
				       (- end start)
				       1)])
			  (set! l (append l
					  (list (subbytes bytes start (+ start len)))))
			  len)
			(wrap-evt (semaphore-peek-evt s)
				  (lambda (x) (loop)))))))
	    (lambda ()
	      (set! l #f))
	    (lambda (s non-block? breakable?)
	      (set! spec s)
	      (not non-block?)))])
    (test 0 write-bytes-avail* #"abc" p)
    (semaphore-post s)
    (test 3 write-bytes #"abc" p)
    (test '(#"abc") values l)
    (semaphore-post s)
    (let ([n (write-bytes-avail #"abc" p)])
      (test #t <= n 1 3)
      (test (add1 n) length l))
    (flush-output p)
    (test #t values flushed?)
    (test #f write-special-avail* 'thing p)
    (test 'thing values spec)
    (test #t write-special 'thung p)
    (test 'thung values spec)
    (test (void) close-output-port p)
    (test #f values l)
    (set! l 10)
    (test (void) close-output-port p)
    (test 10 values l)))

; --------------------------------------------------

(load "tmp1")
(test write-test-obj 'load foo)

(define wto write-test-obj)
(define dto display-test-obj)
(define lto load-test-obj)
(define f-3.25 (string->number "-3.25"))
(define f.25 (string->number ".25"))
(set! write-test-obj (list f.25 f-3.25));.25 inexact errors less likely.
(set! display-test-obj (list f.25 f-3.25));.3 often has such errors (~10^-13)
(set! load-test-obj (list 'define 'foo (list 'quote write-test-obj)))
(let ([f (lambda (test-file)
	   (write-char #\; test-file)
	   (display write-test-obj test-file)
	   (newline test-file)
	   (write load-test-obj test-file)
	   (output-port? test-file))])
  (test #t 'cwo (call-with-output-file
                    "tmp3" f #:exists 'truncate)))
(check-test-file "tmp3")
(set! write-test-obj wto)
(set! display-test-obj dto)
(set! load-test-obj lto)

(define badc-range-start 0)
(define badc-range-end 255)

(define (test-format format)
  (test "~" format "~~")
  (test "hello---~---there" format "~a---~~---~a" "hello" 'there)
  (test "\"hello\"---~---there" format "~s---~~---~s" "hello" 'there)
  (test "\"hello\"---~---'there" format "~v---~~---~v" "hello" 'there)
  (test "hello---~---there" format "~.a---~~---~a" "hello" 'there)
  (test "\"hello\"---~---there" format "~.s---~~---~s" "hello" 'there)
  (test "\"hello\"---~---'there" format "~.v---~~---~v" "hello" 'there)
  (test (string #\a #\newline #\b #\newline #\c) format "a\nb~%c")
  (let ([try-newline-stuff
	 (lambda (newlines)
	   (test "12" format (apply string `(#\1 #\~ #\space ,@newlines #\space #\2)))
	   (test "12" format (apply string `(#\1 #\~ ,@newlines #\space #\2)))
	   (test "12" format (apply string `(#\1 #\~ ,@newlines #\2)))
	   (test (apply string `(#\1 ,@newlines #\2)) 
		 format (apply string `(#\1 #\~ ,@newlines #\space ,@newlines #\2))))])
    (for-each try-newline-stuff '((#\return) (#\newline) (#\return #\newline))))
  (test "twenty=20..." format "twenty=~s..." 20)
  (test "twenty=20..." format "twenty=~v..." 20)
  (test "twenty=20..." format "twenty=~e..." 20)
  (test "twenty=20..." format "twenty=~.s..." 20)
  (test "twenty=14..." format "twenty=~x..." 20)
  (test "twenty=24..." format "twenty=~o..." 20)
  (test "twenty=10100..." format "twenty=~b..." 20)
  (test "zee=z..." format "zee=~c..." #\z)

  (test #\. 
	(lambda (s) (string-ref s (sub1 (string-length s))))
	(parameterize ([error-print-width 40])
	  (format "~e" (make-string 200 #\v))))
  (test "(vvvvvv..."
        '.a
	(parameterize ([error-print-width 10])
	  (format "~.a" (list (make-string 200 #\v)))))
  (test "(\"vvvvv..."
        '.v
	(parameterize ([error-print-width 10])
	  (format "~.s" (list (make-string 200 #\v)))))
  (test "'(\"vvvv..."
        '.v
	(parameterize ([error-print-width 10])
	  (format "~.v" (list (make-string 200 #\v)))))
  
  (let()
    (define bads
      (let loop ([i badc-range-end])
	(cond
	 [(eq? i badc-range-start) (list (integer->char i))]
	 [else (let ([c (integer->char i)]
		     [rest (loop (sub1 i))])
		 (case c
		   [(#\~ #\% #\n #\a #\s #\c #\o #\x #\b #\v #\e
			 #\N #\A #\S #\C #\O #\X #\B #\V #\E)
		    rest]
		   [else (if (char-whitespace? c)
			     rest
			     (cons c rest))]))])))

    (define with-censor (load-relative "censor.rktl"))

    ; test for all bad tags; the string we generate shouldn't
    ;  be printed to a terminal directly because it can contain contain
    ;  control characters; censor it
    (unless building-flat-tests?
      (with-censor
       (lambda ()
	 (for-each (lambda (c)
		     (err/rt-test (format (format "a~~~cb" c) 0)))
		   bads)))))
  
  (err/rt-test (format 9))
  (err/rt-test (format "apple~"))
  (err/rt-test (format "~"))
  (err/rt-test (format "~~~"))
  (err/rt-test (format "~o") exn:application:mismatch?)
  (err/rt-test (format "~o" 1 2) exn:application:mismatch?)
  (err/rt-test (format "~c" 1) exn:application:mismatch?)
  (err/rt-test (format "~x" 'a) exn:application:mismatch?)
  (err/rt-test (format "~x" 4.0) exn:application:mismatch?)
  (err/rt-test (format "~x" 5+4.0i) exn:application:mismatch?))

(test-format format)
(test-format
 (lambda args
   (let ([p (open-output-string)])
     (apply fprintf p args)
     (get-output-string p))))
(test-format
 (lambda args
   (let ([p (open-output-string)])
     (parameterize ([current-output-port p])
		   (apply printf args))
     (get-output-string p))))

(arity-test format 1 -1)
(arity-test printf 1 -1)
(arity-test fprintf 2 -1)

(test #t environment-variables? (current-environment-variables))
(test #f environment-variables? 10)
(test #t environment-variables? (environment-variables-copy (current-environment-variables)))
(test #t environment-variables? (make-environment-variables))
(test #t environment-variables? (make-environment-variables #"A" #"1"))
(test #t list? (environment-variables-names (current-environment-variables)))
(test #t andmap bytes? (environment-variables-names (current-environment-variables)))
(test #t = 
      (length (environment-variables-names (current-environment-variables)))
      (length (environment-variables-names (environment-variables-copy (current-environment-variables)))))
(test #f bytes-environment-variable-name? #"x=")
(test #f bytes-environment-variable-name? #"x\0")
(test (not (eq? 'windows (system-type))) bytes-environment-variable-name? #"")

(test #f string-environment-variable-name? "x=")
(test #f string-environment-variable-name? "x\0")
(test (not (eq? 'windows (system-type))) string-environment-variable-name? "")

(test #"1" environment-variables-ref (make-environment-variables #"a" #"1" #"b" #"two") #"a")
(test #"two" environment-variables-ref (make-environment-variables #"a" #"1" #"b" #"two") #"b")
(test #f environment-variables-ref (make-environment-variables #"a" #"1" #"b" #"two") #"c")
(test #f environment-variables-ref (make-environment-variables) #"a")

(define (env-var-tests)
  (define success-1? (putenv "APPLE" "AnApple"))
  (define success-2? (putenv "BANANA" "AnotherApple"))
  (err/rt-test (getenv 7))
  (err/rt-test (getenv (string #\a #\nul #\b)))
  (err/rt-test (putenv 7 "hi"))
  (err/rt-test (putenv "hi" 7))
  (err/rt-test (putenv (string #\a #\nul #\b) "hi"))
  (err/rt-test (putenv "hi" (string #\a #\nul #\b)))
  (collect-garbage)
  (test #t 'success-1 success-1?)
  (test #t 'success-2 success-2?)
  (test "AnApple" getenv "APPLE")
  (test "AnotherApple" getenv "BANANA")
  (test #f getenv "AnUndefinedEnvironmentVariable")

  (define env (current-environment-variables))
  (test #"AnApple" environment-variables-ref env #"APPLE")
  (err/rt-test (environment-variables-ref env #"=AP=PLE="))
  (test (void) environment-variables-set! env #"APPLE" #"=x=")
  (test #"=x=" environment-variables-ref env #"APPLE")
  (test #"AnotherApple" environment-variables-ref env #"BANANA")
  (test (void) environment-variables-set! env #"BANANA" #f)
  (test #f environment-variables-ref env #"BANANA")
  (test #f getenv "BANANA")

  (let ([apple (if (eq? 'windows (system-type))
                   #"apple"
                   #"APPLE")])
    (test apple car (member apple (environment-variables-names env))))
  (test #f member #"BANANA" (environment-variables-names env))
  (test #f member #"banana" (environment-variables-names env)))

(parameterize ([current-environment-variables
                (environment-variables-copy
                 (current-environment-variables))])
  (env-var-tests))
(env-var-tests)

(arity-test getenv 1 1)
(arity-test putenv 2 2)

(arity-test read-eval-print-loop 0 0)
(test (void) 'r-e-p-l-return 
      (parameterize ([current-input-port (make-input-port
					  'name
					  (lambda (s) eof)
					  #f
					  void)])
	   (read-eval-print-loop)))

(define (listen-port x)
  (let-values ([(la lp pa pp) (tcp-addresses x #t)])
    lp))

(define (cust-test open)
  (let ([try
	 (lambda ()
	   (let ([c (make-custodian)])
	     (parameterize ([current-custodian c])
	       (let ([n
		      (let loop ([n 0])
			(with-handlers ([exn:fail:filesystem?
					 (lambda (exn) 
					   (printf "expected open failure: ~a\n"
						   (exn-message exn))
					   n)])
			  ;; leave the port open:
			  (open)
			  (if (= n 5000)
			      n
			      (loop (add1 n)))))])
		 ;; should close all the ports
		 (custodian-shutdown-all c)
		 (printf "got ~a ports\n" n)
		 n))))])
    (let ([n (try)])
      (test n try))))

(unless (eq? 'macos (system-type)) ;; no limit in Mac OS Classic!
  (cust-test (lambda ()
	       (open-input-file 
		(build-path (current-load-relative-directory)
			    "file.rktl")))))

;; Too time-consuming, does bad things to the network:
'(let* ( [l (tcp-listen 0)]
         [pn (listen-port l)])
  (cust-test (lambda ()
	       (let-values ([(r1 w1) (tcp-connect "localhost" pn)]
			    [(r2 w2) (tcp-accept l)])
		 '(close-input-port r1)
		 '(close-output-port w1)
		 '(close-output-port w2)
		 '(close-input-port r2))))
  (tcp-close l))

;;----------------------------------------------------------------------
;; Filesystem-change events

(test #f filesystem-change-evt? 'evt)

(let ([dir (make-temporary-file "change~a" 'directory)])
  (define known-supported? (vector-ref (system-type 'fs-change) 0))
  (define known-file-supported? (vector-ref (system-type 'fs-change) 3))
  (define (check-supported evt file?)
    (when (if file?
              known-file-supported?
              known-supported?)
      (test #t filesystem-change-evt? evt)))

  (define (check f1-name f2-name as-file? known-x-supported?)
    (printf "checking ~s, ~s as ~a\n" f1-name f2-name (if as-file? "file" "dir"))
    (define f1 (build-path dir f1-name))
    (define f2 (build-path dir f2-name))

    (define dir-e (filesystem-change-evt dir (lambda () #f)))
    (check-supported dir-e #f)
    (if as-file?
        (call-with-output-file* f1 (lambda (o) (fprintf o "1\n")))
        (make-directory f1))
    (when dir-e
      (test dir-e sync dir-e)
      (test dir-e sync dir-e))
    (if as-file?
        (call-with-output-file* f2 (lambda (o) (fprintf o "2\n")))
        (make-directory f2))

    (define f1-e (filesystem-change-evt f1 (lambda () #f)))
    (define f2-e (filesystem-change-evt f2 (lambda () #f)))
    (check-supported f1-e #t)
    (check-supported f2-e #t)
    
    (when f1-e
      (test #f sync/timeout 0 f1-e)
      (test #f sync/timeout 0 f2-e)
      
      (call-with-output-file (if as-file?
                                 f1
                                 (build-path f1 "x"))
        #:exists 'append 
        (lambda (o) (newline o)))
      (test f1-e sync f1-e)
      (when known-x-supported?
        (test #f sync/timeout 0 f2-e))

      (call-with-output-file (if as-file?
                                 f2
                                 (build-path f2 "y"))
        #:exists 'append 
        (lambda (o) (newline o)))
      (test f2-e sync/timeout 0 f2-e)
      (test f2-e sync f2-e)
      (test f1-e sync f1-e)

      (define f1-e2 (filesystem-change-evt f1 (lambda () #f)))
      (when known-x-supported?
        (test #f sync/timeout 0 f1-e2))
      (test f1-e sync/timeout 0 f1-e)
      (test f1-e sync f1-e)

      (filesystem-change-evt-cancel f1-e2)
      (test f1-e2 sync/timeout 0 f1-e2)

      (define cust (make-custodian))
      (define f1-e3 (parameterize ([current-custodian cust])
                      (filesystem-change-evt f2 (lambda () #f))))
      (when known-x-supported?
        (test #f sync/timeout 0 f1-e3))
      (custodian-shutdown-all cust)
      (test f1-e3 sync/timeout 0 f1-e3)))

  (check "f1" "f2" #t known-file-supported?)
  (check "f1d" "f2d" #f known-supported?)

  (delete-directory/files dir))

;;----------------------------------------------------------------------
;; TCP

(let ([do-once
       (lambda (evt? localhost)
	 (let* (
	  [l (tcp-listen 0 5 #t)]
    [pn (listen-port l)])
	   (let-values ([(r1 w1) (tcp-connect localhost pn)]
			[(r2 w2) (if evt?
				     (apply values (sync (tcp-accept-evt l)))
				     (tcp-accept l))])
	     (test #t tcp-port? r1)
	     (test #t tcp-port? r2)
	     (test #t tcp-port? w1)
	     (test #t tcp-port? w2)
	     (fprintf w1 "Hello\n")
	     (flush-output w1)
	     (test "Hello" read-line r2)
	     (tcp-abandon-port r1)
	     (close-output-port w1)
	     (close-output-port w2)
	     (close-input-port r2))
	   (when evt?
	     (test #f sync/timeout 0 (tcp-accept-evt l)))
	   (tcp-close l)))])
  (do-once #f "localhost")
  (do-once #t "localhost")
  (with-handlers ([exn:fail:network:errno? (lambda (e)
                                             ;; catch EAFNOSUPPORT Address family not supported by protocol
                                             (unless (regexp-match? #rx"Address family not supported by protocol" (exn-message e))
                                               (raise e)))])
    (do-once #f "::1")
    (do-once #t "::1")))

(test #f tcp-port? (current-input-port))
(test #f tcp-port? (current-output-port))

(arity-test tcp-port? 1 1)

;; Check that `tcp-accept-evt' uses the right custodian
(let ()
  (define l (tcp-listen 0 5 #t))
  (define port (listen-port l))
  (define c (make-custodian))
  
  (define-values (i o) (values #f #f))
  
  (define t
    (thread
     (lambda ()
       (parameterize ([current-custodian c])
         (set!-values (i o) (apply values (sync (tcp-accept-evt l))))))))
  
  (define-values (ci co) (tcp-connect "localhost" port))
  (sync t)
  
  (custodian-shutdown-all c)
  (port-closed? i))

;;----------------------------------------------------------------------
;; UDP

(unless (eq? 'macos (system-type))
  (load-relative "udp.rktl"))

(when (eq? 'macos (system-type))
  (err/rt-test (udp-open-socket) exn:misc:unsupported?)
  ;; All others fail b/c can't supply a UDP.
  )

(test #f udp? 5)

;; more type tests in udp.rktl, where we have UDP socket values
(err/rt-test (udp-close 5))
(err/rt-test (udp-bound? 5))
(err/rt-test (udp-connected? 5))
(err/rt-test (udp-bind! 5 #f 40000))
(err/rt-test (udp-connect! 5 "localhost" 40000))
(err/rt-test (udp-send-to 5 "localhost" 40000 #"hello"))
(err/rt-test (udp-send-to* 5 "localhost" 40000 #"hello"))
(err/rt-test (udp-send-to/enable-break 5 "localhost" 40000 #"hello"))
(err/rt-test (udp-send 5 #"hello"))
(err/rt-test (udp-send* 5 #"hello"))
(err/rt-test (udp-send/enable-break 5 #"hello"))
(err/rt-test (udp-receive! 5 (make-bytes 10)))
(err/rt-test (udp-receive!* 5 (make-bytes 10)))
(err/rt-test (udp-receive!/enable-break 5 (make-bytes 10)))
(err/rt-test (udp-receive!-evt 5 (make-bytes 10)))
(err/rt-test (udp-send-ready-evt 5))
(err/rt-test (udp-receive-ready-evt 5))

(arity-test udp-open-socket 0 2)
(arity-test udp-close 1 1)
(arity-test udp? 1 1)
(arity-test udp-bound? 1 1)
(arity-test udp-connected? 1 1)
(arity-test udp-bind! 3 4)
(arity-test udp-connect! 3 3)
(arity-test udp-send-to 4 6)
(arity-test udp-send-to* 4 6)
(arity-test udp-send-to/enable-break 4 6)
(arity-test udp-send 2 4)
(arity-test udp-send* 2 4)
(arity-test udp-send/enable-break 2 4)
(arity-test udp-send-to-evt 4 6)
(arity-test udp-send-evt 2 4)
(arity-test udp-receive! 2 4)
(arity-test udp-receive!* 2 4)
(arity-test udp-receive!/enable-break 2 4)
(arity-test udp-receive!-evt 2 4)
(arity-test udp-send-ready-evt 1 1)
(arity-test udp-receive-ready-evt 1 1)

(Section 'file-after-udp)

;;----------------------------------------------------------------------
;; Security guards:

(arity-test make-security-guard 3 4)

;; Files - - - - - - - - - - - - - - - - - - - - - -

(define (make-file-sg ok-modes)
  (make-security-guard (current-security-guard)
		       (lambda (who path modes)
			 (unless (andmap (lambda (m) (memq m ok-modes))
					 modes)
			   (raise (cons 'fs-reject who))))
		       void
		       (lambda (who path path2)
			 (unless (memq 'link ok-modes)
			   (raise (cons 'fs-reject (list 'link path2 who)))))))
(define (fs-reject? who)
  (lambda (x) 
    (and (pair? x)
	 (eq? (car x) 'fs-reject)
	 (equal? (cdr x) who))))

(parameterize ([current-security-guard (make-file-sg '(exists read))])
  (test #t path? (cleanse-path "tmp1"))
  (test #t file-exists? "tmp1")
  (test #f directory-exists? "tmp1")
  (test #f link-exists? "tmp1")

  (err/rt-test (open-output-file "tmp1") (fs-reject? 'open-output-file))
  (err/rt-test (delete-file "tmp1") (fs-reject? 'delete-file))
  (err/rt-test (rename-file-or-directory "tmp1" "tmp11") (fs-reject? 'rename-file-or-directory))
  (err/rt-test (copy-file "tmp1" "tmp11") (fs-reject? 'copy-file))
  (err/rt-test (make-file-or-directory-link "tmp1" "tmp11") (fs-reject? 'make-file-or-directory-link))
  (err/rt-test (file-or-directory-permissions "tmp1" 7) (fs-reject? 'file-or-directory-permissions))
  (err/rt-test (file-or-directory-permissions "tmp1" 0) (fs-reject? 'file-or-directory-permissions))
  (test #t exact-integer? (file-or-directory-permissions "tmp1" 'bits))
  (let ([p (open-input-file "tmp1")])
    (test #t input-port? p)
    (close-input-port p))
  (test #t list? (directory-list)))

(parameterize ([current-security-guard (make-file-sg '(exists write))])
  (test #t path? (cleanse-path "tmp1"))
  (err/rt-test (open-input-file "tmp1") (fs-reject? 'open-input-file))
  (err/rt-test (open-output-file "tmp1" #:exists 'append) (fs-reject? 'open-output-file))
  (err/rt-test (open-output-file "tmp1" #:exists 'update) (fs-reject? 'open-output-file))
  (err/rt-test (directory-list) (fs-reject? 'directory-list))
  (err/rt-test (directory-list (current-directory)) (fs-reject? 'directory-list))
  (err/rt-test (delete-directory (current-directory)) (fs-reject? 'delete-directory))
  (err/rt-test (rename-file-or-directory "tmp1" "tmp11") (fs-reject? 'rename-file-or-directory))
  (err/rt-test (copy-file "tmp1" "tmp11") (fs-reject? 'copy-file))
  (err/rt-test (file-or-directory-modify-seconds "tmp1") (fs-reject? 'file-or-directory-modify-seconds))
  (err/rt-test (file-or-directory-permissions "tmp1") (fs-reject? 'file-or-directory-permissions))
  (err/rt-test (file-size "tmp1") (fs-reject? 'file-size))
  (err/rt-test (make-file-or-directory-link "tmp1" "tmp11") (fs-reject? (list 'link  
									      (string->path "tmp1")
									      'make-file-or-directory-link))))

(parameterize ([current-security-guard (make-file-sg '(read write))])
  (err/rt-test (current-directory) (fs-reject? 'current-directory))
  (err/rt-test (current-directory "tmp1") (fs-reject? 'current-directory))
  (err/rt-test (current-drive) (lambda (x)
				 (or (exn:unsupported? x) ((fs-reject? 'current-drive) x))))
  (test #t path? (cleanse-path "tmp1")) ;; no security guard for cleanse
  (err/rt-test (resolve-path "tmp1") (fs-reject? 'resolve-path))
  (err/rt-test (simplify-path "../tmp1") (fs-reject? 'simplify-path))
  (err/rt-test (file-exists? "tmp1") (fs-reject? 'file-exists?))
  (err/rt-test (directory-exists? "tmp1") (fs-reject? 'directory-exists?))
  (err/rt-test (link-exists? "tmp1") (fs-reject? 'link-exists?))
  (err/rt-test (path->complete-path "tmp1") (fs-reject? 'path->complete-path))
  (err/rt-test (filesystem-root-list) (fs-reject? 'filesystem-root-list))
  (err/rt-test (find-system-path 'temp-dir) (fs-reject? 'find-system-path)))

;; Cleanup files created above
(for ([f '("tmp1" "tmp2" "tmp3")] #:when (file-exists? f)) (delete-file f))

;; Network - - - - - - - - - - - - - - - - - - - - - -

(define (net-reject? who host port what)
  (lambda (x) (and (pair? x)
		   (eq? (car x) 'net-reject)
		   (eq? (cadr x) who)
		   (equal? (caddr x) host)
		   (equal? (cadddr x) port)
		   (eq? (cddddr x) what))))

(define early-udp (and (not (eq? 'macos (system-type)))
		       (udp-open-socket)))

(parameterize ([current-security-guard 
		(make-security-guard (current-security-guard)
				     void
				     (lambda (who host port mode)
				       (raise (list* 'net-reject who host port mode))))])
  (err/rt-test (tcp-connect "other" 123)  (net-reject? 'tcp-connect "other" 123 'client))
  (err/rt-test (tcp-listen 123)  (net-reject? 'tcp-listen #f 123 'server))
  (unless (eq? 'macos (system-type)) ; no UDP in Mac OS Classic
    (err/rt-test (udp-open-socket)  (net-reject? 'udp-open-socket #f #f 'server))
    (err/rt-test (udp-bind! early-udp "localhost" 40000)  (net-reject? 'udp-bind! "localhost" 40000 'server))
    (err/rt-test (udp-connect! early-udp "localhost" 40000)  (net-reject? 'udp-connect! "localhost" 40000 'client))
    (err/rt-test (udp-send-to early-udp "localhost" 40000 #"hi")  (net-reject? 'udp-send-to "localhost" 40000 'client))))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Check `in-directory'

(let ([tmp-dir (make-temporary-file "rktindir~a" 'directory)])
  (define (touch p) (call-with-output-file* p #:exists 'can-update void))
  (make-directory (build-path tmp-dir "a"))
  (touch (build-path tmp-dir "a" "alpha"))
  (touch (build-path tmp-dir "a" "apple"))
  (make-directory (build-path tmp-dir "b"))
  (touch (build-path tmp-dir "b" "beta"))
  (make-directory (build-path tmp-dir "c"))
  (define paths (list (build-path "a")
                      (build-path "a" "alpha")
                      (build-path "a" "apple")
                      (build-path "b")
                      (build-path "b" "beta")
                      (build-path "c")))
  (let ([ht (for/hash ([p (in-list paths)]) (values p #t))])
    (test ht
          'in-dir/no-arg
          (parameterize ([current-directory tmp-dir])
            (for/hash ([f (in-directory)])
              (values f #t))))
    (define (mk) (in-directory))
    (test ht
          'in-dir/no-arg/outline
          (parameterize ([current-directory tmp-dir])
            (for/hash ([f (mk)])
              (values f #t)))))
  (let ([ht (for/hash ([p (in-list paths)]) (values (build-path tmp-dir p) #t))])
    (test ht
          'in-dir/full
          (for/hash ([f (in-directory tmp-dir)])
            (values f #t)))
    (define (mk) (in-directory tmp-dir))
    (test ht
          'in-dir/full/outline
          (for/hash ([f (mk)])
            (values f #t))))
  (define-values (dir-parent dir-name dir?) (split-path tmp-dir))
  (let ([ht (for/hash ([p (in-list paths)]) (values (build-path dir-name p) #t))])
    (test ht
          'in-dir/relative
          (parameterize ([current-directory dir-parent])
            (for/hash ([f (in-directory dir-name)])
              (values f #t))))
    (define (mk) (in-directory dir-name))
    (test ht
          'in-dir/relative/outline
          (parameterize ([current-directory dir-parent])
            (for/hash ([f (mk)])
              (values f #t)))))
  (let ([ht (hash-remove (for/hash ([p (in-list paths)]) (values p #t))
                         (build-path "b" "beta"))])
    (define (not-b? p)
      (not (equal? p (build-path tmp-dir "b"))))
    (test ht
          'in-dir/skip-b
          (parameterize ([current-directory tmp-dir])
            (for/hash ([f (in-directory #f not-b?)])
              (values f #t))))
    (define (mk) (in-directory #f not-b?))
    (test ht
          'in-dir/skip-b/outline
          (parameterize ([current-directory tmp-dir])
            (for/hash ([f (mk)])
              (values f #t)))))
  (delete-directory/files tmp-dir))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Check that `in-directory' fails properly on filesystem errors

(unless (eq? 'windows (system-type))
  (define tmp (build-path (build-path (find-system-path 'temp-dir))
                          (format "in-dir-tmp-dir~a" (random 1000))))
  (define sub (build-path tmp "sub"))
  (make-directory* tmp)
  (make-directory* sub)
  (file-or-directory-permissions sub #o000)
  (file-or-directory-permissions sub)
  (err/rt-test (for ([v (in-directory sub)]) v) exn:fail:filesystem?)
  (delete-directory sub)
  (delete-directory/files tmp))

(let ()
  (define tmp (build-path (build-path (find-system-path 'temp-dir))
                          (format "in-dir-tmp-dir~a" (random 1000))))
  (define sub (build-path tmp "sub"))
  (make-directory* tmp)
  (make-directory* sub)
  (test (list sub) 'in-directory (for/list ([v (in-directory tmp)]) v))
  (delete-directory sub)
  (delete-directory/files tmp))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(test #t exn? (exn:fail:filesystem:errno "a" (current-continuation-marks) '(10 . posix)))
(err/rt-test (exn:fail:filesystem:errno "a" (current-continuation-marks) 10))
(err/rt-test (exn:fail:filesystem:errno "a" (current-continuation-marks) '(10 posix)))
(err/rt-test (exn:fail:filesystem:errno "a" (current-continuation-marks) '(10)))
(err/rt-test (exn:fail:filesystem:errno "a" (current-continuation-marks) '#(10)))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(report-errs)
