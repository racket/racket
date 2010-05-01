
(require mzlib/string
         (only-in net/url get-pure-port string->url)
         (only-in mzlib/port copy-port))

(load-relative "loadtest.rktl")

(Section 'uni-norm)

(define (parse-string m)
  (let ([s (regexp-split #rx" +" m)])
    (apply string
           (map (lambda (x)
                  (integer->char (string->number (bytes->string/latin-1 x) 16)))
                s))))

(define (get-test-file)
  (define name "NormalizationTest.txt")
  (define base "http://www.unicode.org/Public/5.0.0/ucd/")
  (define (existing)
    (let loop ([dirs (list (current-load-relative-directory)
                           (current-directory))])
      (and (pair? dirs)
           (let ([path (build-path (car dirs) name)])
             (if (file-exists? path) path (loop (cdr dirs)))))))
  (define (get-it)
    (parameterize ([current-input-port
                    (get-pure-port (string->url (string-append base name)))])
      (with-output-to-file name
        (lambda () (copy-port (current-input-port) (current-output-port)))
        #:exists 'truncate)))
  (or (existing)
      (begin (get-it) (existing))
      (error "file not found: ~s" (string-append base name))))

(printf "Reading tests...\n")
(define test-strings
  (with-input-from-file (get-test-file)
    (lambda ()
      (unless (regexp-match #rx"^# NormalizationTest-" (read-line))
        (error "Bad test-file contents (couldn't retreive tests?)"))
      (let loop ([a null])
	(let ([l (read-line)])
	  (if (eof-object? l)
            (if (null? a)
              (error "No tests found (couldn't retreive tests?)")
              (reverse a))
            (let ([m (regexp-match #rx#"^([0-9A-F ]+);([0-9A-F ]+);([0-9A-F ]+);([0-9A-F ]+);([0-9A-F ]+)" l)])
              (if m
                (loop (cons (cons l (map parse-string (cdr m))) a))
                (loop a)))))))))


(for-each (lambda (l)
	    (let-values ([(t c1 c2 c3 c4 c5) (apply values l)])
	      (printf "Checking ~a\n" t)

	      (test c2 string-normalize-nfc c1)
	      (test c2 string-normalize-nfc c2)
	      (test c2 string-normalize-nfc c3)
	      (test c4 string-normalize-nfc c4)
	      (test c4 string-normalize-nfc c5)

	      (test c3 string-normalize-nfd c1)
	      (test c3 string-normalize-nfd c2)
	      (test c3 string-normalize-nfd c3)
	      (test c5 string-normalize-nfd c4)
	      (test c5 string-normalize-nfd c5)

	      (test c4 string-normalize-nfkc c1)
	      (test c4 string-normalize-nfkc c2)
	      (test c4 string-normalize-nfkc c3)
	      (test c4 string-normalize-nfkc c4)
	      (test c4 string-normalize-nfkc c5)

	      (test c5 string-normalize-nfkd c1)
	      (test c5 string-normalize-nfkd c2)
	      (test c5 string-normalize-nfkd c3)
	      (test c5 string-normalize-nfkd c4)
	      (test c5 string-normalize-nfkd c5)))
	  test-strings)

(report-errs)
