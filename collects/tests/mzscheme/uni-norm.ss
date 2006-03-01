
(require (lib "string.ss"))

(load-relative "loadtest.ss")

(define (parse-string m)
  (let ([s (regexp-split #rx" +" m)])
    (apply string (map (lambda (x) (integer->char 
				    (string->number 
				     (bytes->string/latin-1 x)
				     16)))
		       s))))

(printf "Reading tests...\n")
(define test-strings
  (with-input-from-file (build-path (current-load-relative-directory)
				    "NormalizationTest.txt")
    (lambda ()
      (let loop ([a null])
	(let ([l (read-line)])
	  (if (eof-object? l)
	      a
	      (let ([m (regexp-match #rx#"^([0-9A-F ]+);([0-9A-F ]+);([0-9A-F ]+);([0-9A-F ]+);([0-9A-F ]+)" l)])
		(if m
		    (loop (cons (cons l (map parse-string (cdr m)))
				a))
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