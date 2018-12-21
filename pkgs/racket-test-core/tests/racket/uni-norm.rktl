
(require racket/string
         (only-in net/url get-pure-port string->url)
         (only-in racket/port copy-port))

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
  (define base "http://www.unicode.org/Public/7.0.0/ucd/")
  (define here (current-load-relative-directory))
  (or (for/or ([dir (list here (current-directory))])
        (define path (build-path dir name))
        (and (file-exists? path) path))
      (let ([path (build-path here name)])
        (define (try)
          (with-handlers ([exn:fail? (lambda (x) #f)])
            (with-output-to-file path #:exists 'replace
              (lambda ()
                (copy-port (get-pure-port (string->url (string-append base name)))
                           (current-output-port))))))
        (for/or ([n 5])
          (unless (zero? n)
            (sleep 0.1))
          (try))
        path)))

(printf "Reading tests...\n")
(define test-strings
  (let* ([test-file (get-test-file)]
         [strs-or-errmsg
          (with-input-from-file test-file
            (lambda ()
              (define first-line (read-line))
              (if (or (eof-object? first-line)
                      (not (regexp-match #rx"^# NormalizationTest-" first-line)))
                "Bad test-file contents (couldn't retrieve tests?)"
                (let loop ([a null])
                  (let ([l (read-line)])
                    (if (eof-object? l)
                      (if (null? a)
                        "No tests found (couldn't retrieve tests?)"
                        (reverse a))
                      (let ([m (regexp-match #rx#"^([0-9A-F ]+);([0-9A-F ]+);([0-9A-F ]+);([0-9A-F ]+);([0-9A-F ]+)" l)])
                        (if m
                          (loop (cons (cons l (map parse-string (cdr m))) a))
                          (loop a)))))))))])
    (cond
     [(pair? strs-or-errmsg)
      strs-or-errmsg]
     [(string? strs-or-errmsg)
      (delete-file test-file)
      (log-warning strs-or-errmsg)
      '()]
     [else
      (raise-argument-error 'test-strings "(or/c pair? string?)" strs-or-errmsg)])))


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

;; Some tests where the composed form is much smaller than decomposed:
(let ()
  (define s (list->string (for/list ([i 64]) #\é)))
  (test s string-normalize-nfc s)
  (test s string-normalize-nfc (string-normalize-nfd s)))

(report-errs)
