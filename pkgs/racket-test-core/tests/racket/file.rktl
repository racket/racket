
(load-relative "loadtest.rktl")
(require ffi/file
         ffi/unsafe
         compiler/find-exe
         racket/file)

(Section 'file)

(define testing.rktl (build-path (current-load-relative-directory) "testing.rktl"))

(define original-dir (current-directory))
(define work-dir (make-temporary-file "path~a" 'directory))
(current-directory work-dir)

(test #t port? (current-input-port))
(test #t port? (current-output-port))
(test #t input-port? (current-input-port))
(test #t output-port? (current-output-port))
(test #t output-port? (current-error-port))
(test (void) current-input-port (current-input-port))
(test (void) current-output-port (current-output-port))
(test (void) current-error-port (current-error-port))
(test #t call-with-input-file testing.rktl input-port?)

(test #f port? 7)
(test #f input-port? 7)
(test #f output-port? 7)
(test #f terminal-port? 7)
(test #f file-stream-port? 7)

(define this-file (open-input-file testing.rktl))
(test #t port? this-file)
(test #t input-port? this-file)
(test #f output-port? this-file)
(test #f terminal-port? this-file)
(test #t file-stream-port? this-file)
(close-input-port this-file)

(define this-file (open-input-file testing.rktl #:mode 'binary))
(test #t port? this-file)
(test #t input-port? this-file)
(test #f output-port? this-file)
(test #f terminal-port? this-file)
(test #t file-stream-port? this-file)
(close-input-port this-file)

(define this-file (open-input-file testing.rktl #:mode 'text))
(test #t port? this-file)
(test #t input-port? this-file)
(test #f output-port? this-file)
(test #f terminal-port? this-file)
(test #t file-stream-port? this-file)
(arity-test port? 1 1)
(arity-test input-port? 1 1)
(arity-test output-port? 1 1)
(arity-test terminal-port? 1 1)
(arity-test file-stream-port? 1 1)
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
(arity-test peek-char-or-special 0 4)
(test #\; read-char this-file)
(arity-test read-char 0 1)
(arity-test read-char-or-special 0 3)
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

(test #t string? (system-language+country))

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

(when (file-exists? "/dev/zero")
  ;; Make sure read-line is interruptable on a primitive port that
  ;; has no line ending:
  (define t (thread (lambda () (call-with-input-file* "/dev/zero" read-line))))
  (sleep 0.1)
  (kill-thread t)
  (test #t thread-dead? t))

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

;; Tests for make-temporary-{file,directory}{,*}

;; arities
(arity-test make-temporary-file 0 3)
(test 'make-temporary-file object-name make-temporary-file)
(let-values ([(required accepted) (procedure-keywords make-temporary-file)])
  (test '() 'make-temporary-file-no-required-keywords required)
  (test '(#:base-dir #:copy-from) 'make-temporary-file-accepts-keywords accepted))
(arity-test make-temporary-file* 2 2)
(test 'make-temporary-file* object-name make-temporary-file*)
(let-values ([(required accepted) (procedure-keywords make-temporary-file*)])
  (test '() 'make-temporary-file*-no-required-keywords required)
  (test '(#:base-dir #:copy-from) 'make-temporary-file*-accepts-keywords accepted))
(arity-test make-temporary-directory 0 1)
(test 'make-temporary-directory object-name make-temporary-directory)
(let-values ([(required accepted) (procedure-keywords make-temporary-directory)])
  (test '() 'make-temporary-directory-no-required-keywords required)
  (test '(#:base-dir) 'make-temporary-directory-accepts-keywords accepted))
(arity-test make-temporary-directory* 2 2)
(test 'make-temporary-directory* object-name make-temporary-directory*)
(let-values ([(required accepted) (procedure-keywords make-temporary-directory*)])
  (test '() 'make-temporary-directory*-no-required-keywords required)
  (test '(#:base-dir) 'make-temporary-directory*-accepts-keywords accepted))

;; tests for using srcloc in templates
(let ([tf (make-temporary-file)])
  (let-values ([(base name dir?) (split-path tf)])
    (test #t 'make-temporary-file-uses-srcloc (regexp-match? #rx"file.rktl" name)))
  (delete-file tf))
(let ([tf (make-temporary-file #:copy-from 'directory)])
  (let-values ([(base name dir?) (split-path tf)])
    (test #t 'make-temporary-file-with-kw-uses-srcloc (regexp-match? #rx"file.rktl" name)))
  (delete-directory tf))
(let ([tf (make-temporary-directory)])
  (let-values ([(base name dir?) (split-path tf)])
    (test #t 'make-temporary-directory-uses-srcloc (regexp-match? #rx"file.rktl" name)))
  (delete-directory tf))
(let ([tf (make-temporary-directory #:base-dir (find-system-path 'temp-dir))])
  (let-values ([(base name dir?) (split-path tf)])
    (test #t 'make-temporary-directory-with-kw-uses-srcloc (regexp-match? #rx"file.rktl" name)))
  (delete-directory tf))
(let ([tf (make-temporary-file* #"abc" #"xyz")])
  (let-values ([(base name dir?) (split-path tf)])
    (test #t 'make-temporary-file*-uses-srcloc (regexp-match? #rx"file.rktl" name)))
  (delete-file tf))
(let ([tf (make-temporary-file* #"abc" #"xyz" #:base-dir (find-system-path 'temp-dir))])
  (let-values ([(base name dir?) (split-path tf)])
    (test #t 'make-temporary-file*-with-kw-uses-srcloc (regexp-match? #rx"file.rktl" name)))
  (delete-file tf))
(let ([tf (make-temporary-directory* #"abc" #"xyz")])
  (let-values ([(base name dir?) (split-path tf)])
    (test #t 'make-temporary-directory*-uses-srcloc (regexp-match? #rx"file.rktl" name)))
  (delete-directory tf))
(let ([tf (make-temporary-directory* #"abc" #"xyz" #:base-dir (find-system-path 'temp-dir))])
  (let-values ([(base name dir?) (split-path tf)])
    (test #t 'make-temporary-directory*-with-kw-uses-srcloc (regexp-match? #rx"file.rktl" name)))
  (delete-directory tf))

;; tests for actually using template, prefix, and suffix
(let ([tf (make-temporary-file "abc~a.xyz")])
  (let-values ([(base name dir?) (split-path tf)])
    (test #t 'make-temporary-file-respects-template
          (regexp-match? #rx"^abc.*\\.xyz$" name)))
  (delete-file tf))
(let ([tf (make-temporary-file* #"abc" #".xyz")])
  (let-values ([(base name dir?) (split-path tf)])
    (test #t 'make-temporary-file*-respects-prefix/suffix
          (regexp-match? #rx"^abc.*\\.xyz$" name)))
  (delete-file tf))
(let ([td (make-temporary-directory "abc~a.xyz")])
  (let-values ([(base name dir?) (split-path td)])
    (test #t 'make-temporary-directory-respects-template
          (regexp-match? #rx"^abc.*\\.xyz$" name)))
  (delete-directory td))
(let ([td (make-temporary-directory* #"abc" #".xyz")])
  (let-values ([(base name dir?) (split-path td)])
    (test #t 'make-temporary-directory*-respects-prefix/suffix
          (regexp-match? #rx"^abc.*\\.xyz$" name)))
  (delete-directory td))

;; tests for higher-order use
(let ([make-temporary-file ((λ (x) x) make-temporary-file)])
  (define dir (make-temporary-file "mtf-ho-dir~a" 'directory))
  (test #t 'make-temporary-file-in-ho-position (directory-exists? dir))
  (define tf (make-temporary-file #:base-dir dir))
  (test #t 'make-temporary-file-in-ho-position-with-kw (file-exists? tf))
  (delete-file tf)
  (delete-directory dir))
(let ([make-temporary-directory ((λ (x) x) make-temporary-directory)])
  (define dir (make-temporary-directory "mtd-ho-dir~a"))
  (test #t 'make-temporary-directory-in-ho-position (directory-exists? dir))
  (define td (make-temporary-directory #:base-dir dir))
  (test #t 'make-temporary-directory-in-ho-position-with-kw (directory-exists? td))
  (delete-directory td)
  (delete-directory dir))
(let ([make-temporary-file* ((λ (x) x) make-temporary-file*)])
  (define tf1 (make-temporary-file* #"mtf-star-ho-dir" #""))
  (test #t 'make-temporary-file*-in-ho-position (file-exists? tf1))
  (delete-file tf1)
  (define tf2 (make-temporary-file* #"abc" #"xyz" #:base-dir (find-system-path 'temp-dir)))
  (test #t 'make-temporary-file*-in-ho-position-with-kw (file-exists? tf2))
  (delete-file tf2))
(let ([make-temporary-directory* ((λ (x) x) make-temporary-directory*)])
  (define dir (make-temporary-directory* #"mtd-star-ho-dir" #""))
  (test #t 'make-temporary-directory*-in-ho-position (directory-exists? dir))
  (define td (make-temporary-directory* #"abc" #"xyz" #:base-dir dir))
  (test #t 'make-temporary-directory*-in-ho-position-with-kw (directory-exists? td))
  (delete-directory td)
  (delete-directory dir))

;; tests for #:copy-from
(let ()
  (define a (make-temporary-file))
  (call-with-output-file a
    #:exists 'truncate
    (λ (out) (write 'copy-from out)))
  (define b (make-temporary-file #:copy-from a))
  (delete-file a)
  (test 'copy-from 'make-temporary-file-copy-from (call-with-input-file b read))
  (delete-file b))

;; tests for function names in error messages
(define rx:tmp-file #rx"make-temporary-file")
(define rx:tmp-dir #rx"make-temporary-directory")
(define rx:tmp-file* #rx"make-temporary-file*")
(define rx:tmp-dir* #rx"make-temporary-directory*")
(err/rt-test (make-temporary-file "bad\0~a") exn:fail:contract? rx:tmp-file)
(err/rt-test (make-temporary-directory "bad\0~a") exn:fail:contract? rx:tmp-dir)
(err/rt-test (make-temporary-file "bad~x") exn:fail:contract? rx:tmp-file)
(err/rt-test (make-temporary-directory "bad~x") exn:fail:contract? rx:tmp-dir)
(err/rt-test (make-temporary-file* #"bad\0" #"xyz") exn:fail:contract? rx:tmp-file*)
(err/rt-test (make-temporary-directory* #"abc" #"bad\0") exn:fail:contract? rx:tmp-dir*)
(err/rt-test (make-temporary-file* "bad" #"xyz") exn:fail:contract? rx:tmp-file*)
(err/rt-test (make-temporary-directory* #"abc" "bad~x") exn:fail:contract? rx:tmp-dir*)
(err/rt-test (make-temporary-file* #"abc" #"xyz" #:copy-from 'directory)
             exn:fail:contract?
             rx:tmp-file*)

;; tests for absolute paths
(let* ([temp-dir (find-system-path 'temp-dir)]
       [absolute-prefix
        (path->bytes (build-path temp-dir "absolute"))])
  (define tf (make-temporary-file* absolute-prefix #""))
  (test #t 'make-temporary-file-absolute (file-exists? tf))
  (delete-file tf)
  (define td (make-temporary-directory* absolute-prefix #""))
  (test #t 'make-temporary-directory-absolute (directory-exists? td))
  (delete-directory td)
  (err/rt-test (make-temporary-file* absolute-prefix #"" #:base-dir temp-dir)
               exn:fail:contract?
               rx:tmp-file)
  (err/rt-test (make-temporary-directory* absolute-prefix #"" #:base-dir temp-dir)
               exn:fail:contract?
               rx:tmp-dir))

;; tests for a template that produces a syntactic directory path
(let ([dir-template
       (path->string
        ;; path-element->string would drop directory separator
        (path->directory-path "syntactically-dir~atmp"))])
  (define td (make-temporary-directory dir-template))
  (test #t 'make-temporary-dir-syntactic-dir-template (directory-exists? td))
  (delete-directory td)
  (err/rt-test (make-temporary-file dir-template) exn:fail:contract? rx:tmp-file))

;; tests for Windows paths that are absolute but not complete
(when (eq? 'windows (system-path-convention-type))
  (define complete-temp-dir
    (path->complete-path
     (find-system-path 'temp-dir)))
  (define elems
    (explode-path complete-temp-dir))
  (define drive
    (car elems))
  (test #t 'make-temporary-file-w32-drive-spec (complete-path? drive))
  (define temp-dir-no-drive
    (apply build-path (cdr elems)))
  (define abs-not-complete
    (let* ([pth (build-path temp-dir-no-drive "rkttmp")]
           [bs (path->bytes pth)])
      (if (eqv? (char->integer #\\) (bytes-ref bs 0))
          pth
          (bytes->path (bytes-append #"\\" bs)))))
  (test #t 'make-temporary-file-w32-abs-template (absolute-path? abs-not-complete))
  (test #f 'make-temporary-file-w32-abs-not-complete (complete-path? abs-not-complete))
  (define abs-not-complete/bytes
    (path->bytes abs-not-complete))
  (let ([tf (make-temporary-file* abs-not-complete/bytes #"")])
    (test #t 'make-temporary-file-w32-abs-no-base-dir (file-exists? tf))
    (delete-file tf))
  (let ([tf (make-temporary-file* abs-not-complete/bytes #"" #:base-dir drive)])
    (test #t 'make-temporary-file-w32-abs-base-drive (file-exists? tf))
    (delete-file tf))
  ;; no absolute template w/ relative base-dir
  (err/rt-test (make-temporary-file* abs-not-complete/bytes #""
                                     #:base-dir "relative")
               exn:fail:contract?)
  ;; no absolute template w/ driveless absolute base-dir
  (err/rt-test (make-temporary-file* abs-not-complete/bytes #""
                                     #:base-dir temp-dir-no-drive)
               exn:fail:contract?)
  ;; no absolute template w/ complete base-dir that is not just drive spec
  (err/rt-test (make-temporary-file* abs-not-complete/bytes #""
                                     #:base-dir (build-path drive (cadr elems)))
               exn:fail:contract?))


(define tempfilename (make-temporary-file))
(when (file-exists? tempfilename)
  (delete-file tempfilename))

(err/rt-test (open-output-file tempfilename #:exists 'update) exn:fail:filesystem?)
(let ([p (open-output-file tempfilename #:exists 'can-update)])
  (test #t output-port? p)
  (close-output-port p))
(delete-file tempfilename)

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
  (file-position o 10)
  (file-truncate o 399)
  (test 10 file-position o)
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
  
  (sync (system-idle-evt))

  (test 2 + 
	(if (thread-running? th1) 1 0)
	(if (thread-running? th2) 1 0)
	(if (thread-running? th3) 1 0))

  (test 50 read-byte in)

  (sync (system-idle-evt))

  (test 1 + 
	(if (thread-running? th1) 1 0)
	(if (thread-running? th2) 1 0)
	(if (thread-running? th3) 1 0))
  
  (test 51 read-byte in)
  
  (sync (system-idle-evt))

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
(let-values ([(pin pout) (make-pipe 4 'name)])
  (write-bytes (make-bytes 4) pout)
  (test #f sync/timeout 0 pout))

(test #t input-port? (make-input-port void void void void))
(test #t input-port? (make-input-port void void #f void))
(test #t input-port? (make-input-port 1000 void #f void))
(test 1000 object-name (make-input-port 1000 void #f void))
(err/rt-test (read (make-input-port #f void void void)))
(err/rt-test (read-char (make-input-port #f void void void)))
(err/rt-test (peek-char (make-input-port #f void void void)))
(arity-test make-input-port 4 10)
(err/rt-test (make-input-port #f 8 void void))
(err/rt-test (make-input-port #f void 8 void))
(err/rt-test (make-input-port #f void void 8))
(err/rt-test (make-input-port #f cons void void))
(err/rt-test (make-input-port #f void add1 void))
(err/rt-test (make-input-port #f void void add1))

(test #t output-port? (make-output-port #f always-evt void void))
(test #t output-port? (make-output-port #f always-evt void void))
(test 7786 object-name (make-output-port 7786 always-evt void void))
(arity-test make-output-port 4 11)
(err/rt-test (make-output-port #f 8 void void void))
(err/rt-test (make-output-port #f always-evt 8 void))
(err/rt-test (make-output-port #f always-evt void 8))
(err/rt-test (make-output-port #f always-evt add1 void))
(err/rt-test (make-output-port #f always-evt void add1))
(err/rt-test (write-special 'foo (make-output-port void always-evt void void)) exn:application:mismatch?)

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
  (parameterize ([global-port-print-handler (lambda (v p [depth 0])
                                              (test #t pair? (member depth '(0 1)))
                                              (write 'changes-to-Y p))])
    (test (void) print "hello" sp)
    (parameterize ([print-as-expression #f])
      (test (void) print "hello" sp))
    (test (adding "YY") get-output-string sp))
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

;; Check use of handlers by `printf`
(let ()
  (define p (open-output-bytes))
  (port-display-handler p (lambda (x p)
                            (write-bytes #"D" p)))
  (port-write-handler p (lambda (x p)
                          (write-bytes #"W" p)))
  (port-print-handler p (lambda (x p [d 0])
                          (test #t pair? (memq d '(0 1)))
                          (write-bytes #"P" p)))

  (display 'x p)
  (fprintf p "~a" 'y)
  (fprintf p "~.a" 'z) ; does not use handler

  (write 'x p)
  (fprintf p "~s" 'y)
  (fprintf p "~.s" 'z) ; does not use handler

  (print 'x p)
  (fprintf p "~v" 'y)
  (fprintf p "~.v" 'z) ; does not use handler

  (test #"DDzWWzPP'z" get-output-bytes p))

;; Make sure `printf` works with wrapped ports
(let ()
  (struct w (p) #:property prop:output-port (struct-field-index p))
  (define o (open-output-bytes))
  (define p (w o))

  (fprintf p "0~a~a~s~v~.a~a~.s~.v" 1 #"1" 2 3 4 #"4" 5 6)
  (test #"011234456" get-output-bytes o))

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

;; Make sure that peeking past the end of a
;; file as a first action gives EOF right away
(let ([tempfilename (make-temporary-file)])
  (call-with-output-file*
   tempfilename
   #:exists 'truncate
   (lambda (o) (write-bytes (make-bytes 50 65) o)))
  (for ([end '(60 50)])
    (test (list eof eof)
          call-with-input-file
          tempfilename
          (lambda (i)
            (list (peek-byte i end)
                  (peek-byte i end))))
    (test (list eof eof)
          call-with-input-file
          tempfilename
          (lambda (i)
            (list (peek-bytes 1 end i)
                  (peek-bytes 1 end i)))))
  (delete-file tempfilename))

;;------------------------------------------------------------
;; File-stream ports and blocking behavior

(let ()
  (define-values (s i o e) (subprocess #f #f #f (find-exe) "-e" "(read)"))

  (thread (lambda ()
            (sync (system-idle-evt))
            (close-input-port i)))

  (err/rt-test
   (peek-bytes-avail! (make-bytes 10) 0 #f i)
   exn:fail?)

  (close-output-port o)
  (close-input-port e)
  (subprocess-wait s))

(let ()
  (define-values (s i o e) (subprocess #f #f #f (find-exe) "-e" "(read)"))

  (thread (lambda ()
            (sync (system-idle-evt))
            (close-input-port i)))

  (test 0 peek-bytes-avail! (make-bytes 10) 0 (port-progress-evt i) i)

  (close-output-port o)
  (close-input-port e)
  (subprocess-wait s))

(let ()
  (define-values (s i o e) (subprocess #f #f #f (find-exe) "-e" "(read)"))

  (thread (lambda ()
            (sync (system-idle-evt))
            (close-input-port i)))

  ;; Short not get stuck:
  (sync (port-progress-evt i))

  (close-output-port o)
  (close-input-port e)
  (subprocess-wait s))

(for ([force-close? '(#t #f)])
  (define c (make-custodian))

  (define-values (s i o e)
    (parameterize ([current-custodian c])
      (subprocess #f #f #f (find-exe) "-e" "(let loop () (write-bytes (make-bytes 1024)) (loop))")))

  (thread (lambda ()
            (sync (system-idle-evt))
            (if (or force-close?
                    ;; For really old Windows, we need a close that doesn't try
                    ;; to flush, because there's no way to avoid
                    ;; buffering at the rktio level:
                    (and (eq? 'windows (system-type))
                         (not (regexp-match? "Windows NT" (system-type 'machine)))))
                (custodian-shutdown-all c)
                (close-output-port o))))

  (err/rt-test
   (let loop ()
     (write-bytes-avail #"hello" o)
     (loop))
   exn:fail?)

  (close-input-port i)
  (close-input-port e)
  (subprocess-wait s))

;;-----------------------------------------------------

(parameterize ([current-directory work-dir])
  (define fn (make-temporary-file "subproc~a"))
  (define i (open-input-file fn))
  (define o (open-output-file fn #:exists 'update))
  (close-input-port i)
  (close-output-port o)
  (err/rt-test (subprocess o #f #f (find-exe)) exn:fail? #rx"closed")
  (err/rt-test (subprocess #f i #f (find-exe)) exn:fail? #rx"closed")
  (err/rt-test (subprocess #f #f o (find-exe)) exn:fail? #rx"closed"))

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

(let ()
  (define-struct myport (port)
    #:property prop:output-port 0)
  (define o (open-output-string))

  (define out (make-myport o))
  (test (void) newline out)
  (test "\n" get-output-string o)

  (err/rt-test (newline 9)))

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
  (err/rt-test (format "~s") (lambda (e) (regexp-match "requires one")))
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

  (let ([apple #"APPLE"])
    (test apple car (member apple (environment-variables-names env))))
  (test #f member #"BANANA" (environment-variables-names env))
  (test #f member #"banana" (environment-variables-names env)))

(parameterize ([current-environment-variables
                (environment-variables-copy
                 (current-environment-variables))])
  (env-var-tests))
(env-var-tests)

;; Partly a test of case-normalization:
(let* ([e (current-environment-variables)]
       [e2 (environment-variables-copy e)]
       [names2 (environment-variables-names e2)])
  (test (length (environment-variables-names e)) length names2)
  (for ([k (in-list (environment-variables-names e))])
    (test #t 'name (and (member k names2) #t))
    (test (environment-variables-ref e k)
          environment-variables-ref e2 k)))

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
    (test #t exact-integer? lp)
    (test #t equal? 0 pp)
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
;; Directory and permissions

(unless (eq? 'windows (system-type))
  (let ([dir (make-temporary-file "perms~a" 'directory)])
    (define d2 (build-path dir "created"))
    (make-directory d2 #o500)
    (test #o500 file-or-directory-permissions d2 'bits)
    (delete-directory/files dir)))
(err/rt-test (make-directory "would-be-new" 'a))
(err/rt-test (make-directory "would-be-new" -1))

;;----------------------------------------------------------------------
;; Filesystem-change events

(test #f filesystem-change-evt? 'evt)

(let ([dir (make-temporary-file "change~a" 'directory)])
  (define known-supported? (vector-ref (system-type 'fs-change) 0))
  (define known-file-supported? (vector-ref (system-type 'fs-change) 3))
  (define (filesystem-change-evt* dir file?)
    (if (if file?
            known-file-supported?
            known-supported?)
        (let ([e (filesystem-change-evt dir)])
          (test #t filesystem-change-evt? e)
          e)
        (filesystem-change-evt dir (lambda () #f))))

  (define (check f1-name f2-name as-file? known-x-supported?)
    (printf "checking ~s, ~s as ~a\n" f1-name f2-name (if as-file? "file" "dir"))
    (define f1 (build-path dir f1-name))
    (define f2 (build-path dir f2-name))

    (define dir-e (filesystem-change-evt* dir #f))
    (if as-file?
        (call-with-output-file* f1 (lambda (o) (fprintf o "1\n")))
        (make-directory f1))
    (when dir-e
      (test dir-e sync dir-e)
      (test dir-e sync dir-e))
    (if as-file?
        (call-with-output-file* f2 (lambda (o) (fprintf o "2\n")))
        (make-directory f2))

    (define f1-e (filesystem-change-evt* f1 #t))
    (define f2-e (filesystem-change-evt* f2 #t))
    
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

  (let ([no-file (build-path dir "no-such-file-here")])
    (test 'no filesystem-change-evt no-file (lambda () 'no))
    (err/rt-test (filesystem-change-evt no-file) (lambda (x)
                                                   (or (exn:fail:filesystem? x)
                                                       (exn:fail:unsupported? x)))))

  (delete-directory/files dir))

;;----------------------------------------------------------------------
;; TCP

(let ([do-once
       (lambda (evt? localhost [serve-localhost #f])
	 (let* ([l (tcp-listen 0 5 #t serve-localhost)]
                [pn (listen-port l)])
           (test #f tcp-accept-ready? l)
           (test #f sync/timeout 0 l)
	   (let-values ([(r1 w1) (tcp-connect localhost pn)]
			[(r2 w2) (if evt?
				     (apply values (sync (tcp-accept-evt l)))
				     (tcp-accept l))])
             (let-values ([(la1 lp1 pa1 pp1) (tcp-addresses r1 #t)]
                          [(la2 lp2 pa2 pp2) (tcp-addresses r2 #t)])
               (test #t equal? lp1 pp2)
               (test #t equal? pp1 lp2))
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
                                             ;; Catch forms of non-support for IPv6:
                                             ;;       EAFNOSUPPORT "Address family not supported by protocol"
                                             ;;    or getaddrinfo failure "no address associated with name"
                                             ;; In case IPv6 is supported by the OS but not for the loopback
                                             ;; devce, we also catch "Cannot assign requested address"
                                             (unless (regexp-match?
                                                      #rx"family not supported by protocol|no address associated with name|Cannot assign requested address"
                                                      (exn-message e))
                                               (raise e)))])
    ;; Supply listener hostname, so we can check whether `listen` receives IPv6 connections
    (do-once #f "::1" "::1")
    (do-once #t "::1" "::1")
    ;; If we get this far, then "::1" apparently works, so try listening
    ;; at all interfaces and connecting to "::1":
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
  (test #t port-closed? i)
  (tcp-close l)
  (close-input-port ci)
  (close-output-port co))

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
  (test 'file file-or-directory-type "tmp1")

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
  (err/rt-test (file-or-directory-type "tmp1") (fs-reject? 'file-or-directory-type))
  (err/rt-test (path->complete-path "tmp1") (fs-reject? 'path->complete-path))
  (err/rt-test (filesystem-root-list) (fs-reject? 'filesystem-root-list))
  (err/rt-test (find-system-path 'temp-dir) (fs-reject? 'find-system-path)))

;; Cleanup files created above
(for ([f '("tmp1" "tmp2" "tmp3")] #:when (file-exists? f)) (delete-file f))

(current-directory original-dir)

;; ----------------------------------------

(let ([home-dir (path->directory-path (make-temporary-file "test-home~a" 'directory))]
      [env (environment-variables-copy (current-environment-variables))]
      [racket (find-exe)])
  (environment-variables-set! env
                              #"PLTUSERHOME"
                              (path->bytes home-dir))
  (define (get-dirs)
    (parameterize ([current-environment-variables env])
      (define-values (s i o e) (subprocess #f #f #f racket "-I" "racket/base" "-e"
                                           (string-append
                                            "(map path->bytes "
                                            "     (list (find-system-path 'home-dir)"
                                            "           (find-system-path 'pref-dir)"
                                            "           (find-system-path 'pref-file)"
                                            "           (find-system-path 'init-dir)"
                                            "           (find-system-path 'init-file)"
                                            "           (find-system-path 'addon-dir)"
                                            "           (find-system-path 'cache-dir)))")))
      (begin0
        (cadr (read i))
        (subprocess-wait s)
        (close-input-port i)
        (close-output-port o)
        (close-input-port e))))
  (define (touch f) (close-output-port (open-output-file f #:exists 'truncate)))

  (define dir-syms '(home-dir pref-dir pref-file init-dir init-file addon-dir cache-dir))
  (define expected-default-dirs
    (case (system-type)
      [(unix) (list home-dir
                    (build-path home-dir ".config" "racket/")
                    (build-path home-dir ".config" "racket" "racket-prefs.rktd")
                    (build-path home-dir ".config" "racket/")
                    (build-path home-dir ".config" "racket" "racketrc.rktl")
                    (build-path home-dir ".local" "share" "racket/")
                    (build-path home-dir ".cache" "racket/"))]
      [(macosx) (list home-dir
                      (build-path home-dir "Library" "Preferences/")
                      (build-path home-dir "Library" "Preferences" "org.racket-lang.prefs.rktd")
                      (build-path home-dir "Library" "Racket/")
                      (build-path home-dir "Library" "Racket" "racketrc.rktl")
                      (build-path home-dir "Library" "Racket/")
                      (build-path home-dir "Library" "Caches" "Racket/"))]
      [(windows) (list home-dir
                       (build-path home-dir "Racket\\")
                       (build-path home-dir "Racket" "racket-prefs.rktd")
                       home-dir
                       (build-path home-dir "racketrc.rktl")
                       (build-path home-dir "Racket\\")
                       (build-path home-dir "Racket\\"))]
      [else (error "unexpected system type")]))

  (define default-dirs (get-dirs))
  (for-each (lambda (name expect got)
              (test got `(,name default) expect))
            dir-syms
            (map bytes->path default-dirs)
            expected-default-dirs)

  ;; Create files/directories that trigger legacy paths:
  (case (system-type)
    [(unix)
     (touch (build-path home-dir ".racketrc"))
     (make-directory (build-path home-dir ".racket"))]
    [(macosx)
     (touch (build-path home-dir ".racketrc"))
     ;; Make sure just the existence of the would-be init dir doesn't override legacy:
     (make-directory (build-path home-dir "Library"))
     (make-directory (build-path home-dir "Library" "Racket"))])

  (define legacy-dirs (get-dirs))
  (for-each (lambda (name expect got)
              (test got `(,name legacy) expect))
            dir-syms
            (map bytes->path legacy-dirs)
            (case (system-type)
              [(unix) (list home-dir
                            (build-path home-dir ".racket/")
                            (build-path home-dir ".racket/" "racket-prefs.rktd")
                            home-dir
                            (build-path home-dir ".racketrc")
                            (build-path home-dir ".racket/")
                            (build-path home-dir ".racket/"))]
              [(macosx) (list home-dir
                              (build-path home-dir "Library" "Preferences/")
                              (build-path home-dir "Library" "Preferences" "org.racket-lang.prefs.rktd")
                              home-dir
                              (build-path home-dir ".racketrc")
                              (build-path home-dir "Library" "Racket/")
                              (build-path home-dir "Library" "Caches" "Racket/"))]
              [(windows) expected-default-dirs]
              [else (error "unexpected system type")]))

  ;; Create files/directories that cause legacy paths to be ignored:
  (case (system-type)
    [(unix)
     (make-directory (build-path home-dir ".config"))
     (make-directory (build-path home-dir ".config" "racket"))
     (touch (build-path home-dir ".config" "racket" "racketrc.rktl"))
     (make-directory (build-path home-dir ".local"))
     (make-directory (build-path home-dir ".local" "share"))
     (make-directory (build-path home-dir ".local" "share" "racket"))
     (make-directory (build-path home-dir ".cache"))
     (make-directory (build-path home-dir ".cache" "racket"))]
    [(macosx)
     (touch (build-path (build-path home-dir "Library" "Racket" "racketrc.rktl")))])

  (define back-to-default-dirs (get-dirs))
  (for-each (lambda (name expect got)
              (test got `(,name back-to-default) expect))
            dir-syms
            (map bytes->path back-to-default-dirs)
            expected-default-dirs)

  (delete-directory/files home-dir))

;; ----------------------------------------

(unless (eq? 'windows (system-type))
  (define can-open-nonblocking-fifo?
    ;; The general implementation of fifo-write ports requires
    ;; OS-managed threads internally. Use support forr futures and/or
    ;; places as an indication that OS threads are available.
    (or (place-enabled?)
        (futures-enabled?)))

  (define fifo (build-path work-dir "ff"))
  (system* (find-executable-path "mkfifo") fifo)

  (define i1 (open-input-file fifo))
  (define o1 (open-output-file fifo #:exists 'update))
  (write-bytes #"abc" o1)
  (flush-output o1)
  (test #"abc" read-bytes 3 i1)
  (close-input-port i1)
  (close-output-port o1)

  (define (check-output-blocking do-write-abc)
    ;; Make sure an output fifo blocks until there's a reader
    (define t1
      (thread
       (lambda ()
         (define o2 (open-output-file fifo #:exists 'update))
         (test #t port-waiting-peer? o2)
         (do-write-abc o2)
         (close-output-port o2))))
    (define t2
      (thread
       (lambda ()
         (sync (system-idle-evt))
         (define i2 (open-input-file fifo))
         (test #"abc" read-bytes 3 i2)
         (close-input-port i2))))
    (sync t1)
    (sync t2))

  (when can-open-nonblocking-fifo?
    (check-output-blocking (lambda (o2) (write-bytes #"abc" o2)))
    (check-output-blocking (lambda (o2)
                             (parameterize ([current-output-port o2])
                               (system* (find-executable-path "echo")
                                        "-n"
                                        "abc")))))

  (delete-file fifo))

(test #f port-waiting-peer? (current-input-port))
(test #f port-waiting-peer? (current-output-port))
(test #f port-waiting-peer? (open-input-bytes #""))
(err/rt-test (port-waiting-peer? 10))

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

(when early-udp
  (udp-close early-udp))

;; Interaction with `system-type` - - - - - - - - - - - - - - - - - - -

(parameterize ([current-security-guard (make-file-sg '())])
  (test #f regexp-match? "unknown machine" (system-type 'machine)))


;; The `ffi/file` library - - - - - - - - - - - - - - - - - - -

(define no-op (lambda (x) #f))

(let ()
  (define pub-mod (collection-file-path "list.rkt" "racket"))
  (define priv-mod (collection-file-path "stx.rkt" "racket/private"))

  (define sg0 (current-security-guard))

  (define sg-ro
    (make-security-guard
     sg0
     (lambda (who path modes)
       (when (or (memq 'write modes) (memq 'delete modes))
         (error who "write/delete not allowed")))
     (lambda (who host port mode)
       (unless (eq? mode 'client)
         (error who "servers not allowed")))
     (lambda (who path to)
       (unless (equal? (path->string to) "chain")
         (error who "only chain links allowed")))))

  (define sg-priv
    (make-security-guard
     sg0
     (lambda (who path modes)
       (when (and path (regexp-match #rx"private" (path->string path)))
         (error who "no access to private paths: ~e" path)))
     void void))

  (define (mk-fun modes)
    ;; receives path pointer; the rest doesn't matter
    (cast no-op
          ;; turns `no-op` into a callback:
          (_fun _pointer -> _scheme)
          ;; turns the callback into a callout, which is what we want
          ;; to test `_file/guard`:
          (_fun (path) ::
                (path : (_file/guard modes 'me))
                -> _scheme)))
  
  (define (fun path modes)
    ((mk-fun modes) path))

  (define ok-exn? 
    (lambda (x)
      (and (exn:fail? x)
           (regexp-match #rx"^me: " (exn-message x)))))

  (define (sc-run #:check [security-guard-check security-guard-check-file]
                  ok? . args)
    (if ok?
        (begin
          (apply test (void) security-guard-check 'me args)
          (when (eq? security-guard-check security-guard-check-file)
            (test (void) void (apply fun args))))
        (begin
          (err/rt-test (apply security-guard-check 'me args) ok-exn?)
          (when (eq? security-guard-check security-guard-check-file)
            (err/rt-test (apply fun args) ok-exn?)))))

  (parameterize ((current-security-guard sg0))
    (sc-run #t "foo.txt" '(read))
    (sc-run #t "bar.txt" '(write delete))
    (sc-run #t pub-mod '(read))
    (sc-run #t pub-mod '(write))
    (sc-run #t priv-mod '(read))
    (sc-run #t priv-mod '(read write delete))
    (sc-run #t #:check security-guard-check-file-link
            priv-mod "chain")
    (sc-run #t #:check security-guard-check-file-link
            priv-mod "other")
    (sc-run #t #:check security-guard-check-network
            "localhost" 500 'client)
    (sc-run #t #:check security-guard-check-network
            "localhost" 500 'server)
    (sc-run #t #:check security-guard-check-network
            #f #f 'server))

  (parameterize ((current-security-guard sg-ro))
    (sc-run #t "foo.txt" '(read))
    (sc-run #f "bar.txt" '(write delete))
    (sc-run #t pub-mod '(read))
    (sc-run #f pub-mod '(write))
    (sc-run #t priv-mod '(read))
    (sc-run #f priv-mod '(read write delete))
    (sc-run #t #:check security-guard-check-file-link
            priv-mod "chain")
    (sc-run #f #:check security-guard-check-file-link
            priv-mod "other")
    (sc-run #t #:check security-guard-check-network
            "localhost" 500 'client)
    (sc-run #f #:check security-guard-check-network
            "localhost" 500 'server)
    (sc-run #f #:check security-guard-check-network
            #f #f 'server))

  (parameterize ((current-security-guard sg-priv))
    (sc-run #t pub-mod '(read))
    (sc-run #t pub-mod '(write))
    (sc-run #f priv-mod '(read))
    (sc-run #f priv-mod '(read write delete))))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Check that when flushing a TCP output port fails, it
;; clears the buffer

(let-values ([(subproc stdout stdin stderr) (subprocess #f #f #f (find-exe) "-n")])

  ;; A flush will eventually fail:
  (with-handlers ([exn:fail:filesystem? void])
    (let loop ()
      (write-bytes #"foo" stdin)
      (flush-output stdin)
      (loop)))

  ;; Next flush should not fail, because the buffer content
  ;; should have been discarded by a failed flush:
  (test (void) flush-output stdin)
  
  ;; Closing should not fail:
  (test (void) close-output-port stdin)

  (close-input-port stderr)
  (close-input-port stdout)

  (subprocess-wait subproc))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Check that when flushing an OS-level pipe fails, it
;; clears the buffer

(let-values ([(subproc stdout stdin stderr) (subprocess #f #f #f (find-exe) "-n")])

  ;; A flush will eventually fail:
  (with-handlers ([exn:fail:filesystem? void])
    (let loop ()
      (write-bytes #"foo" stdin)
      (flush-output stdin)
      (loop)))

  ;; Next flush should not fail, because the buffer content
  ;; should have been discarded by a failed flush:
  (test (void) flush-output stdin)
  
  ;; Closing should not fail:
  (test (void) close-output-port stdin)

  (close-input-port stderr)
  (close-input-port stdout)

  (subprocess-wait subproc))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Check that an asynchronous break that interrupts a flush
;; doesn't lose buffered bytes

(let-values ([(subproc stdout stdin stderr) (subprocess #f #f #f (find-exe) "-e"
                                                        (format "~s"
                                                                '(begin
                                                                   (define noise (make-bytes 256 (char->integer #\x)))
                                                                   ;; Fill up the OS-level output pipe:
                                                                   (let loop ()
                                                                     (unless (zero? (write-bytes-avail* noise (current-output-port)))
                                                                       (loop)))
                                                                   ;; Wait until the other end has read:
                                                                   (write-bytes-avail #"noise" (current-output-port))
                                                                   (close-output-port (current-output-port))
                                                                   ;; Drain the OS-level input pipe, succeeding if we
                                                                   ;; find a "!".
                                                                   (let loop ()
                                                                     (define b (read-byte (current-input-port)))
                                                                     (when (eqv? b (char->integer #\!))
                                                                       (exit 0))
                                                                     (when (eof-object? b)
                                                                       (exit 1))
                                                                     (loop)))))])

  ;; Fill up the OS-level output pipe:
  (let loop ()
    (unless (zero? (write-bytes-avail* #"?????" stdin))
      (loop)))

  ;; At this point, the other end is still waiting for us to read.
  ;; Add something to the Racket-level buffer that we want to make sure
  ;; doesn't get lost
  (write-bytes #"!" stdin)

  ;; Thread will get stuck trying to flush:
  (define t (thread (lambda ()
                      (with-handlers ([exn:break? void])
                        (flush-output stdin)))))

  (sync (system-idle-evt))
  (break-thread t)
  (thread-wait t)

  ;; Drain output from subprocess, so it can be unblocked:
  (let loop ()
    (unless (eof-object? (read-bytes-avail! (make-bytes 10) stdout))
      (loop)))

  ;; Subprocess should be reading at this point
  (flush-output stdin)

  (close-output-port stdin)
  (close-input-port stderr)
  (close-input-port stdout)

  (subprocess-wait subproc)

  (test 0 subprocess-status subproc))

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
              (let ([real-f f])
                (set! f 'trying-to-break-in-directory)
                (values real-f #t)))))
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
  (err/rt-test (for/list ([f (in-directory tmp-dir #f '?)]) f))
  (err/rt-test (in-directory tmp-dir #f '?))
  (delete-directory/files tmp-dir))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Test `make[-parent]-directory`

(let ()
  (define tmp (find-system-path 'temp-dir))
  (define (check build-z-dir-path pick-directory)
    (define made (make-temporary-file "check-make-~a" 'directory))
    (define z-dir (build-z-dir-path made "x" "y"))
    (define z (build-path z-dir "z"))
    (parameterize ([current-directory (pick-directory made)])
      (test #f directory-exists? z-dir)
      (test #f file-or-directory-type z-dir)
      (err/rt-test (file-or-directory-type z-dir #t) exn:fail:filesystem?)
      (test #f file-exists? z)
      (make-parent-directory* z)
      (test #t directory-exists? z-dir)
      (test 'directory file-or-directory-type z-dir)
      (make-parent-directory* z)
      (delete-directory/files z-dir)
      (test #f directory-exists? z-dir)
      (make-directory* z)
      (test #t directory-exists? z-dir)
      (test #t directory-exists? z)
      (make-directory* z)
      (make-parent-directory* z))
    (delete-directory/files made))
  (check build-path (lambda (made) (current-directory)))
  (check (lambda args (apply build-path (cdr args))) values)
  (check (lambda args (apply build-path 'same (cdr args))) values))

;; Check on a current directory that does not exist:
(let ()  
  (define made (make-temporary-file "check-make-~a" 'directory))
  (parameterize ([current-directory (build-path made "nonesuch")])
    (make-parent-directory* "z")
    (test #f directory-exists? (current-directory))
    (err/rt-test (make-directory* "z") exn:fail:filesystem?))
  (delete-directory/files made))

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
  (unless (memq 'write (file-or-directory-permissions sub)) ; root can still modify the directory
    (err/rt-test (for ([v (in-directory sub)]) v) exn:fail:filesystem?))
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
;; Make sure a file without write permission can be deleted

(let ()
  (define dir (make-temporary-file "~a-tmp" 'directory))
  (define file (build-path dir "f"))
  (call-with-output-file file void)

  (file-or-directory-permissions file #o555)
  (file-or-directory-permissions dir #o555)

  ;; On Unix, non-writable directory means the file
  ;; can't be deleted.
  (unless (or (eq? 'windows (system-type))
              ;; root can still modify the directory
              (memq 'write (file-or-directory-permissions dir)))
    (unless (eq? 'exn (with-handlers ([void (lambda (exn) 'exn)])
                        (delete-file file)))
      (error "expected an error")))

  (file-or-directory-permissions dir #o777)
  ;; On Windows, non-writable file means the file can't
  ;; be deleted --- but turn off `current-force-delete-permissions`,
  ;; which works around that behavior.
  (when (eq? 'windows (system-type))
    (parameterize ([current-force-delete-permissions #f])
      (unless (eq? 'exn (with-handlers ([void (lambda (exn) 'exn)])
                          (delete-file file)))
        (error "expected an error"))))

  ;; In default mode, non-writable things in writable directories
  ;; can be deleted everywhere:
  (delete-file file)
  (delete-directory dir))

;; Check that permissions for file creation work
(let ()
  (define dir (make-temporary-file "~a-tmp" 'directory))
  (define file (build-path dir "f"))

  (define (check open)
    (for ([replace? (in-list '(#f #t))]
          [mode (in-list '(#o444 #o666))])
      (open file mode replace?)
      (if (eq? 'windows (system-type))
          (test (and (positive? (bitwise-and mode #x2)) '(write read))
                memq 'write (file-or-directory-permissions file))
          (if replace?
              (test mode bitwise-and #o777 (file-or-directory-permissions file 'bits))
              ;; umask might drop additional bits from mode #o444
              (test 0 bitwise-and (bitwise-not mode) (file-or-directory-permissions file 'bits))))
      (delete-file file)))

  (check (lambda (file perms replace?)
           (close-output-port (open-output-file file #:exists 'truncate #:permissions perms
                                                #:replace-permissions? replace?))))
  (check (lambda (file perms replace?)
           (close-output-port (open-output-file file #:exists 'truncate #:permissions perms))
           (close-output-port (open-output-file file #:exists 'replace #:permissions perms
                                                #:replace-permissions? replace?))))
  (check (lambda (file perms replace?)
           (define-values (i o)
             (open-input-output-file file #:exists 'truncate #:permissions perms
                                     #:replace-permissions? replace?))
           (close-input-port i)
           (close-output-port o)))
  (check (lambda (file perms replace?)
           (with-output-to-file file
             #:permissions perms
             #:exists 'truncate
             #:replace-permissions? replace?
             void)))
  (check (lambda (file perms replace?)
           (call-with-output-file file
             #:permissions perms
             #:exists 'truncate
             #:replace-permissions? replace?
             void)))
  (check (lambda (file perms replace?)
           (call-with-output-file* file
             #:permissions perms
             #:exists 'truncate
             #:replace-permissions? replace?
             void)))

  (delete-directory dir))


;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(let ([tf (make-temporary-file)])
  (test tf resolve-path (path->string tf))
  (delete-file tf)
  (define link-created?
    (with-handlers ([(lambda (exn) (and (eq? 'windows (system-type))
                                        (exn:fail:filesystem? exn)))
                     (lambda (exn) #f)])
      (make-file-or-directory-link "other.txt" tf)
      #t))
  (when link-created?
    (err/rt-test (make-file-or-directory-link "other.txt" tf) exn:fail:filesystem? (regexp-quote tf))
    (test (string->path "other.txt") resolve-path tf)
    (test #t link-exists? tf)
    (test 'link file-or-directory-type tf)
    (delete-file tf)

    (make-file-or-directory-link (path->directory-path "other") tf)
    (test #t link-exists? tf)
    (test (if (eq? (system-type) 'windows) 'directory-link 'link) file-or-directory-type tf)
    (if (eq? (system-type) 'windows)
        (delete-directory tf)
        (delete-file tf))

    (test #f link-exists? tf)
    (make-file-or-directory-link (path->directory-path "other") tf)
    (test #t link-exists? tf)
    (delete-directory/files tf)
    (test #f link-exists? tf))

  (case (system-path-convention-type)
    [(unix)
     (test (string->path "/testing-root/testing-dir/testing-file")
           resolve-path
           "//testing-root/testing-dir/testing-file")
     (test (string->path "/testing-root/testing-dir/testing-file")
           resolve-path
           "//testing-root////testing-dir//testing-file")]
    [(windows)
     (test (string->path "C:/testing-root/testing-dir/testing-file")
           resolve-path
           "C://testing-root/testing-dir/testing-file")
     (test (string->path "C:/testing-root/testing-dir\\testing-file")
           resolve-path
           "C://testing-root////testing-dir\\\\testing-file")]))

(unless (link-exists? (current-directory))
  ;; Make sure directoryness is preserved
  (test (current-directory) resolve-path (current-directory)))

(when (eq? (system-type) 'windows)
  ;; special filenames exist everywhere 
  (test #t file-exists? "aux")
  (test #t file-exists? "aux.anything")
  (test #t file-exists? "c:/aux")
  (test #t file-exists? "c:/com1")
  (test #t file-exists? "a:/x/lpt6")
  (test 'file file-or-directory-type "a:/x/lpt6")
  ;; \\?\ paths don't refer to special filenames
  (test #f file-exists? "\\\\?\\C:\\aux")
  (test #f file-or-directory-type "\\\\?\\C:\\aux"))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Make sure `write-byte` and `write-char` don't try to test
;; a non-supplied argument:

(parameterize ([current-output-port (open-output-string)])
  (let ([s (if (zero? (random 1)) "a" "b")])
    (string-append s s) ; causes a clear operation on the runstack for second argument
    (write-byte 65)))

(parameterize ([current-output-port (open-output-string)])
  (let ([s (if (zero? (random 1)) "a" "b")])
    (string-append s s) ; causes a clear operation on the runstack for second argument
    (write-char #\A)))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Check that unpaired surrogates are handled right in
;; Windows paths

(when (eq? 'windows (system-type))
  (let-values ([(surrogate-hi surrogate-lo)
                (let ()
                  (define c0 (bytes-open-converter "platform-UTF-8" "platform-UTF-16"))
                  (let-values ([(bstr a b) (bytes-convert c0 (string->bytes/utf-8 "\U10AABB"))])
                    (define c8 (bytes-open-converter "platform-UTF-16" "platform-UTF-8"))
                    (let-values ([(bstr a b) (bytes-convert c8
                                                            (bytes-append
                                                             #".\0"
                                                             (subbytes bstr 0 2)
                                                             #".\0"
                                                             (subbytes bstr 2 4)
                                                             #".\0"))])
                      (values (subbytes bstr 1 4) (subbytes bstr 5 8)))))])
    (let ([dir (make-temporary-file "weird~a" 'directory)]
          [fns (map bytes->path
                    ;; Each of these byte strings represents a strange
                    ;; filename where one or more of the 16-byte elements
                    ;; at at the `wchar_t*` level is an unpaired surrogate.
                    ;; NTFS filesystems will allow that, though.
                    (list (bytes-append #"a" surrogate-hi #"z")
                          (bytes-append #"a" surrogate-lo #"z")
                          (bytes-append #"a" surrogate-lo surrogate-hi #"z")
                          (bytes-append #"a" surrogate-hi)
                          (bytes-append #"a" surrogate-lo)
                          (bytes-append #"a" surrogate-lo surrogate-hi)))])
      ;; If one of the paths works, we expect them all to work:
      (when (with-handlers ([exn:fail:filesystem? (lambda (x) #f)])
              (let ([p (build-path dir (car fns))])
                (call-with-output-file p void)
                (delete-file p)))
        (for ([fn (in-list fns)])
          (define p (build-path dir fn))
          (call-with-output-file p (lambda (o) (write-bytes (path->bytes fn) o))))
        (for ([fn (in-list fns)])
          (define p (build-path dir fn))
          (call-with-input-file* p (lambda (i) (test (path->bytes fn) read-bytes 100 i))))
        ;; Make sure names are converted correctly back from `directory-list`:
        (test (sort fns path<?) sort (directory-list dir) path<?))
      (delete-directory/files dir))))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(test #"\205\327\305\377@:\276r\337[\212'\b\202\36\343<\320\274\316" sha1-bytes #"abcdefghijklmn")
(test #"\340\373\262\1m\341\6V\352$IR\311}\350x7\337d\263\320\243\247\350\342\31R " sha224-bytes #"abcdefghijklmn")
(test #"\6S\307\351\222\327\252\324\f\262cW8\270p\344\301T\257\263F4\r\2\307\227\324\220\335R\325\371" sha256-bytes #"abcdefghijklmn")

(define (test-sha-more sha-bytes)
  (define (try sha-bytes base)
    (define expect (sha-bytes base))
    (define len (bytes-length base))
    (test expect sha-bytes base 0)
    (test expect sha-bytes base 0 #f)
    (test expect sha-bytes (bytes-append #"__" base) 2)
    (test expect sha-bytes (bytes-append #"__" base) 2 #f)
    (test expect sha-bytes (bytes-append #"__" base) 2 (+ 2 len))
    (test expect sha-bytes (bytes-append #"__" base #"__") 2 (+ 2 len))
    (test expect sha-bytes (bytes-append base #"__") 0 len)
    (test expect sha-bytes (bytes-append (make-bytes 1035 42) base) 1035)
    #;(test expect sha-bytes (bytes-append (make-bytes 1035 42) base #"__") 1035 (+ 1035 len)))
  (define (try-base base)
    (try sha-bytes base)
    #;
    (try (lambda (bstr . args) (apply sha-bytes (open-input-bytes bstr) args)) base))
  (try-base #"abcdefghijklmn")
  (try-base (make-bytes 5077 79)))
(test-sha-more sha1-bytes)
(test-sha-more sha224-bytes)
(test-sha-more sha256-bytes)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; file-or-directory-identity

(test #t number? (file-or-directory-identity (current-directory)))
(test #t = (file-or-directory-identity (current-directory)) (file-or-directory-identity (current-directory)))
(err/rt-test (file-or-directory-identity "thisDoesNotExistAtAll") exn:fail:filesystem?)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; file-or-directory-stat

(arity-test file-or-directory-stat 1 2)

; Write regular file and check stat data.
(let ()
  (define temp-file-path (build-path work-dir "stat-test"))
  (define TEST-CONTENT "stat test content")
  (display-to-file TEST-CONTENT temp-file-path #:exists 'truncate)
  (void (call-with-input-file temp-file-path read-byte))
  (define stat-result (file-or-directory-stat temp-file-path))
  (test #t hash-eq? stat-result)
  (define expected-stat-keys '(device-id
                               inode
                               mode
                               hardlink-count
                               user-id
                               group-id
                               device-id-for-special-file
                               size
                               block-size
                               block-count
                               access-time-seconds
                               modify-time-seconds
                               change-time-seconds
                               creation-time-seconds
                               access-time-nanoseconds
                               modify-time-nanoseconds
                               change-time-nanoseconds
                               creation-time-nanoseconds))
  (test #t 'ok-stat-keys (for/and ([(k v) (in-hash stat-result)])
                           (and (memq k expected-stat-keys)
                                (exact-nonnegative-integer? v))))
  (test (length expected-stat-keys) hash-count stat-result)
  (define (stat-ref key) (hash-ref stat-result key))
  ;
  (test #t = (stat-ref 'size) (string-length TEST-CONTENT))
  (test #t = (stat-ref 'hardlink-count) 1)
  (define (positive-fixnum-key? key)
    (define n (stat-ref key))
    (and (positive-integer? n) (fixnum? n)))
  (unless (eq? (system-type) 'windows)
    (test #t positive-fixnum-key? 'inode)
    (test #t positive-fixnum-key? 'device-id))
  (define stat-mode (stat-ref 'mode))
  (test #t = (bitwise-and stat-mode file-type-bits) regular-file-type-bits)
  (test #f = (bitwise-and stat-mode file-type-bits) directory-type-bits)
  (test #f = (bitwise-and stat-mode file-type-bits) fifo-type-bits)
  (test #f = (bitwise-and stat-mode file-type-bits) symbolic-link-type-bits)
  (test #f = (bitwise-and stat-mode file-type-bits) socket-type-bits)
  (when (eq? (system-type) 'windows)
    ; Windows doesn't provide a user and group id and sets both values to 0.
    (test #t = (stat-ref 'user-id) 0)
    (test #t = (stat-ref 'group-id) 0))
  (unless (eq? (system-type) 'windows)
    ; Check user and group id. Assuming the tests don't run as root, this is
    ; probably all we can sensibly do.
    ; ... but the tests do run as root in some CI configuration, so disabled
    (when #f
      (test #t positive-fixnum-key? 'user-id)
      (test #t positive-fixnum-key? 'group-id)))
  (test #t = (stat-ref 'device-id-for-special-file) 0)
  (unless (eq? (system-type) 'windows)
    (test #t positive-fixnum-key? 'block-size)
    ; On my system, I had expected 1 block, but it's actually 8. This number
    ; is supported by the `stat` command line tool.
    (test #t positive-fixnum-key? 'block-count))
  (test #t >= (stat-ref 'access-time-seconds)
              (stat-ref 'modify-time-seconds))
  (test #t >= (stat-ref 'access-time-nanoseconds)
              (stat-ref 'modify-time-nanoseconds))
  (unless (eq? (system-type) 'windows)
    ; Only true for Poxis, since Windows doesn't provide the status change
    ; time.
    (test #t = (stat-ref 'change-time-nanoseconds)
          (stat-ref 'modify-time-nanoseconds)))
  (define (nano->secs ns) (quotient ns #e1e9))
  (test (stat-ref 'access-time-seconds) nano->secs (stat-ref 'access-time-nanoseconds))
  (test (stat-ref 'modify-time-seconds) nano->secs (stat-ref 'modify-time-nanoseconds))
  (test (stat-ref 'change-time-seconds) nano->secs (stat-ref 'change-time-nanoseconds))
  (test (stat-ref 'creation-time-seconds) nano->secs (stat-ref 'creation-time-nanoseconds))
  (delete-file temp-file-path))

(err/rt-test (file-or-directory-stat "thisDoesNotExistAtAll") exn:fail:filesystem?)

; Test symlink-related features.
(unless (eq? (system-type) 'windows)
  ; Test that stat'ing a dangling link causes a filesystem exception.
  (let ()
    (define link-file-path (build-path work-dir "stat-test-dangling-link"))
    (make-file-or-directory-link "/nonexistent_target" link-file-path)
    (err/rt-test (file-or-directory-stat link-file-path) exn:fail:filesystem?)
    (delete-file link-file-path))

  ; On the other hand, stat'ing the link itself should work.
  (let ()
    (define link-file-path (build-path work-dir "stat-test-dangling-link"))
    (define link-target "/nonexistent_target")
    (make-file-or-directory-link link-target link-file-path)
    (define stat-result (file-or-directory-stat link-file-path #t))
    (define stat-mode (hash-ref stat-result 'mode))
    (test #t = (bitwise-and stat-mode file-type-bits) symbolic-link-type-bits)
    (test #f = (bitwise-and stat-mode file-type-bits) directory-type-bits)
    (test #f = (bitwise-and stat-mode file-type-bits) regular-file-type-bits)
    (test #t = (hash-ref stat-result 'size) (string-length link-target))
    (delete-file link-file-path)))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(delete-directory/files work-dir)

(report-errs)
