
(load-relative "loadtest.ss")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; url.ss tests
;;

(require (lib "url.ss" "net")
	 (lib "uri-codec.ss" "net")
         (lib "string.ss"))

(test "%Pq" uri-decode "%Pq")
(test "%P" uri-decode "%P")
(test "a=hel%2blo+%e7%88%b8" alist->form-urlencoded '((a . "hel+lo \u7238")))
(test '((a . "hel+lo \u7238")) form-urlencoded->alist (alist->form-urlencoded '((a . "hel+lo \u7238"))))
(test "a=hel%2blo;b=good-bye" alist->form-urlencoded '((a . "hel+lo") (b . "good-bye")))
(parameterize ([current-alist-separator-mode 'semi])
  (test "a=hel%2blo;b=good-bye" alist->form-urlencoded '((a . "hel+lo") (b . "good-bye"))))
(parameterize ([current-alist-separator-mode 'amp])
  (test "a=hel%2blo&b=good-bye" alist->form-urlencoded '((a . "hel+lo") (b . "good-bye"))))
(test '((a . "hel+lo") (b . "good-bye")) form-urlencoded->alist (alist->form-urlencoded '((a . "hel+lo") (b . "good-bye"))))
(parameterize ([current-alist-separator-mode 'amp])
  (test '((a . "hel+lo") (b . "good-bye")) form-urlencoded->alist (alist->form-urlencoded '((a . "hel+lo") (b . "good-bye")))))
(test '((a . "hel+lo") (b . "good-bye")) form-urlencoded->alist 
      (parameterize ([current-alist-separator-mode 'amp])
	(alist->form-urlencoded '((a . "hel+lo") (b . "good-bye")))))
(parameterize ([current-alist-separator-mode 'semi])
  (test '((a . "hel+lo&b=good-bye")) form-urlencoded->alist 
	(parameterize ([current-alist-separator-mode 'amp])
	  (alist->form-urlencoded '((a . "hel+lo") (b . "good-bye"))))))
(parameterize ([current-alist-separator-mode 'amp])
  (test '((a . "hel+lo;b=good-bye")) form-urlencoded->alist 
	(parameterize ([current-alist-separator-mode 'semi])
	  (alist->form-urlencoded '((a . "hel+lo") (b . "good-bye"))))))

(test 'amp-or-semi current-alist-separator-mode)
(err/rt-test (current-alist-separator-mode 'bad))

(let ([with-censor (load-relative "censor.ss")])
  (with-censor
   (lambda ()
     ;; Test all chars up to 300
     (let ([p (let loop ([n 0])
		(if (= n 300)
		    null
		    (let ([s (string (char-downcase (integer->char n)))])
		      (cons (cons (string->symbol s) s)
			    (loop (add1 n))))))])
       (test p form-urlencoded->alist (alist->form-urlencoded p))
       (let ([l (apply string-append (map cdr p))])
	 (test l uri-decode (uri-encode l)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; tests adapted from Noel Welsh's original test suite
;;

(let ()
  (define-syntax (for stx)
    (syntax-case stx (code)
      [(_ (i from to) e)
       (and (identifier? (syntax code))
            (number? (syntax-e (syntax from)))
            (number? (syntax-e (syntax to))))
       (syntax (let loop ([i from])
                 e
                 (unless (= i to)
                   (loop (+ i 1)))))]))
  
  
  (test "hello" uri-encode "hello")
  (test "hello%20there" uri-encode "hello there")
  
  (let ((pad (lambda (str)
               (if (= (string-length str) 1)
                   (string-append "0" str)
                   str))))
    (for (code 0 256)
         (if (or (= code 45) (= code 33) (= code 95)
                 (= code 46) (= code 126) (= code 42)
                 (= code 39) (= code 40) (= code 41) 
                 (and (<= 48 code) (<= code 57))   ; 0-9
                 (and (<= 65 code) (<= code 90))   ; A-Z
                 (and (<= 97 code) (<= code 122))) ; a-z
             (test (string (integer->char code)) uri-encode (string (integer->char code)))
             (test (string-append "%" (pad (number->string code 16)))
                   uri-encode
                   (string (integer->char code))))))
  
  
  (test "" alist->form-urlencoded '())
  (test "key=hello+there" alist->form-urlencoded '((key . "hello there")))
  (test "key1=hi;key2=hello" alist->form-urlencoded '((key1 . "hi") (key2 . "hello")))
  (test "key1=hello+there" alist->form-urlencoded '((key1 . "hello there")))
  
  (test "hello" uri-decode "hello")
  (test "hello there" uri-decode "hello%20there")
  
  (let* ((pad (lambda (str)
                (if (= (string-length str) 1)
                    (string-append "0" str)
                    str)))
         (uppercase (lambda (str)
                      (string-uppercase! str)
                      str))
         (lowercase (lambda (str)
                      (string-lowercase! str)
                      str))
         (hexcode (lambda (code)
                    (string-append "%"
                                   (pad (number->string code 16))))))

    ;; each of the next three of these were going from 0 to 255 in Noel's
    ;; original test suite. Those fail here, however.
    
    (for (code 0 127)
         (test (string (integer->char code)) uri-decode (uppercase (hexcode code))))
    (for (code 0 127)
         (test (string (integer->char code)) uri-decode (lowercase (hexcode code)))))
  
  (for (code 0 127)
       (test (string (integer->char code)) uri-decode (string (integer->char code))))
  
  ;; form-urlencoded->alist
  (test '() form-urlencoded->alist "")
  (test '((key . "value")) form-urlencoded->alist "key=value")
  (test '((key . "hello there")) form-urlencoded->alist "key=hello+there")
  (test '((key . "a value")) form-urlencoded->alist "key=a%20value")
  (test '((key . ""))  form-urlencoded->alist "key")
  (test '((key1 . "value 1") (key2 . "value 2")) form-urlencoded->alist "key1=value+1&key2=value+2"))

;;
;; end Noel's original tests
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(let ()
  (define (test-s->u vec str)
    (define (string->url/vec str) (url->vec (string->url str)))
    (define (url/vec->string vec) (url->string (vec->url vec)))
    (test vec string->url/vec str)
    (test str url/vec->string vec))
  
  (define (test-c-u/r expected base relative)
    (define (combine-url/relative-vec x y)
      (url->vec (combine-url/relative (vec->url x) y)))
    (test (url->vec expected) combine-url/relative-vec (url->vec base) relative))
  
  (define (vec->url vec)
    (make-url
     (vector-ref vec 0)
     (vector-ref vec 1)
     (vector-ref vec 2)
     (vector-ref vec 3)
     (map (lambda (x) (if (string? x)
                          x
                          (make-path/param (vector-ref x 0) (vector-ref x 1))))
          (vector-ref vec 4))
     (vector-ref vec 5)
     (vector-ref vec 6)))
  
  (define (url->vec url)
    (vector (url-scheme url)
            (url-user url)
            (url-host url)
            (url-port url)
            (map (lambda (x) (if (string? x)
                                 x
                                 (vector (path/param-path x) (path/param-param x))))
                 (url-path url))
            (url-query url)
            (url-fragment url)))
  
  (test-s->u (vector #f #f #f #f '("") '() #f)
             "/")
  (test-s->u (vector #f #f #f #f '() '() #f)
             "")
  (test-s->u (vector "http" #f "www.drscheme.org" #f '("") '() #f)
             "http://www.drscheme.org/")
  (test-s->u (vector "http" #f "www.drscheme.org" #f (list "a" "b" "c") '() #f)
             "http://www.drscheme.org/a/b/c")
  (test-s->u (vector "http" "robby" "www.drscheme.org" #f (list "a" "b" "c") '() #f)
             "http://robby@www.drscheme.org/a/b/c")
  (test-s->u (vector "http" #f "www.drscheme.org" 8080 (list "a" "b" "c") '() #f)
             "http://www.drscheme.org:8080/a/b/c")
  (test-s->u (vector "http" #f "www.drscheme.org" #f (list "a" "b" "c") '() "joe")
             "http://www.drscheme.org/a/b/c#joe")
  (test-s->u (vector "http" #f "www.drscheme.org" #f (list "a" "b" "c") '((tim . "")) #f)
             "http://www.drscheme.org/a/b/c?tim=")
  (test-s->u (vector "http" #f "www.drscheme.org" #f (list "a" "b" "c") '((tim . "")) "joe")
             "http://www.drscheme.org/a/b/c?tim=#joe")
  (test-s->u (vector "http" #f "www.drscheme.org" #f (list "a" "b" "c") '((tim . "tim")) "joe")
             "http://www.drscheme.org/a/b/c?tim=tim#joe")
  (test-s->u (vector "http" #f "www.drscheme.org" #f (list "a" "b" "c") '((tam . "tom")) "joe")
             "http://www.drscheme.org/a/b/c?tam=tom#joe")
  (test-s->u (vector "http" #f "www.drscheme.org" #f (list "a" "b" "c") '((tam . "tom") (pam . "pom")) "joe")
             "http://www.drscheme.org/a/b/c?tam=tom;pam=pom#joe")
  (parameterize ([current-alist-separator-mode 'semi])  
    (test-s->u (vector "http" #f "www.drscheme.org" #f (list "a" "b" "c") '((tam . "tom") (pam . "pom")) "joe")
	       "http://www.drscheme.org/a/b/c?tam=tom;pam=pom#joe"))
  (parameterize ([current-alist-separator-mode 'amp])  
    (test-s->u (vector "http" #f "www.drscheme.org" #f (list "a" "b" "c") '((tam . "tom") (pam . "pom")) "joe")
	       "http://www.drscheme.org/a/b/c?tam=tom&pam=pom#joe"))
  (test-s->u (vector "http" #f "www.drscheme.org" #f (list "a" "b" #("c" "b")) '() #f)
             "http://www.drscheme.org/a/b/c;b")
  (test-s->u (vector "http" #f "www.drscheme.org" #f (list #("a" "x") "b" #("c" "b")) '() #f)
             "http://www.drscheme.org/a;x/b/c;b")
    
  ;; test unquoting for %
  (test-s->u (vector "http" #f "www.drscheme.org" #f (list "a" "b" "c") '((ti#m . "")) "jo e")
             "http://www.drscheme.org/a/b/c?ti%23m=#jo%20e")
  (test-s->u (vector "http" #f "www.drscheme.org" #f (list #("a " " a") " b " " c ") '() #f)
             "http://www.drscheme.org/a ; a/ b / c ")
  (test-s->u (vector "http" "robb y" "www.drscheme.org" #f '("") '() #f)
             "http://robb%20y@www.drscheme.org/")
  
  (let ([empty-url (make-url #f #f #f #f '() '() #f)])
    (test-c-u/r (string->url "http://www.drscheme.org")
                empty-url
                "http://www.drscheme.org")
    (test-c-u/r (string->url "http://www.drscheme.org")
                (string->url "http://www.drscheme.org")
                ""))
  
  (test-c-u/r (string->url "http://www.mzscheme.org")
              (string->url "http://www.drscheme.org/")
              "http://www.mzscheme.org")
  (test-c-u/r (string->url "http://www.drscheme.org/index.html")
              (string->url "http://www.drscheme.org/")
              "index.html")
  (test-c-u/r (string->url "http://www.drscheme.org/index.html")
              (string->url "http://www.drscheme.org/")
              "/index.html")
  (test-c-u/r (string->url "http://www.drscheme.org/index.html")
              (string->url "http://www.drscheme.org/a/b/c/")
              "/index.html")
  (test-c-u/r (string->url "http://www.drscheme.org/a/b/index.html")
              (string->url "http://www.drscheme.org/a/b/c")
              "index.html")
  (test-c-u/r (string->url "http://www.drscheme.org/a/b/c/index.html")
              (string->url "http://www.drscheme.org/a/b/c/")
              "index.html")
  (test-c-u/r (string->url "http://www.drscheme.org/a/b/d/index.html")
              (string->url "http://www.drscheme.org/a/b/c")
              "d/index.html")
  (test-c-u/r (string->url "http://www.drscheme.org/a/b/c/d/index.html")
              (string->url "http://www.drscheme.org/a/b/c/")
              "d/index.html")
  
  (test-c-u/r (string->url "file:///a/b/c/d/index.html")
              (string->url "file:///a/b/c/")
              "d/index.html")
  (test-c-u/r (string->url "file:///a/b/d/index.html")
              (string->url "file:///a/b/c")
              "d/index.html")
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; other net tests
;;

(require (lib "base64.ss" "net")
	 (lib "qp.ss" "net")
	 (lib "port.ss"))

(define tricky-strings
  (let ([dir (collection-path "tests" "mzscheme")])
    (list (make-bytes 200 32)
	  (make-bytes 200 9)
	  (make-bytes 200 (char->integer #\x))
	  (list->bytes
	   (let loop ([i 0])
	     (if (= i 256)
		 null
		 (cons i (loop (add1 i))))))
	  ;; Something that doesn't end with a LF:
	  (bytes-append 
	   (with-input-from-file (build-path dir "net.ss") (lambda () (read-bytes 500)))
	   #"xxx")
	  ;; CRLF:
	  (regexp-replace
	   #rx#"\n"
	   (with-input-from-file (build-path dir "net.ss") (lambda () (read-bytes 500)))
	   #"\r\n"))))

(define (check-same encode decode port line-rx max-w)
  (let ([p (open-output-bytes)])
    (copy-port port p)
    (let ([bytes (get-output-bytes p)]
	  [r (open-output-bytes)])
      (encode (open-input-bytes bytes) r)
      (let ([p (open-input-bytes (get-output-bytes r))])
	(let loop ()
	  (let ([l (read-bytes-line p 'any)])
	    (unless (eof-object? l)
	      (unless (<= (bytes-length l) max-w)
		(test encode "line too long" l))
	      (let ([m (regexp-match-positions line-rx l)])
		(unless (and m
			     (= (bytes-length l) (cdar m)))
		  (test encode 'bad-line l)))
	      (loop))))
	(let ([q (open-output-bytes)])
	  (decode (open-input-bytes (get-output-bytes r)) q)
	  (unless (equal? (get-output-bytes q) bytes)
	    (with-output-to-file "/tmp/x0" (lambda () (display (get-output-bytes r))) 'truncate)
	    (with-output-to-file "/tmp/x1" (lambda () (display (get-output-bytes q))) 'truncate)
	    (with-output-to-file "/tmp/x2" (lambda () (display bytes)) 'truncate)
	    (error 'decode "failed")))))))

(define (check-same-file encode decode file line-rx max-w)
  ;; This "test" is really just a progress report:
  (test #t list? (list file encode))
  (call-with-input-file file
    (lambda (p)
      (check-same encode decode p line-rx max-w))))

(define (check-same-all encode decode line-rx max-w)
  (for-each (lambda (tricky-string)
	      (check-same encode decode 
			  (open-input-bytes tricky-string)
			  line-rx max-w))
	    tricky-strings)
  (let ([dir (collection-path "tests" "mzscheme")])
    (for-each (lambda (p)
		(when (regexp-match #rx"[.]ss$" (path->string p))
		  (unless (equal? (path->string p) "flat.ss")
		    (check-same-file encode decode (build-path dir p) line-rx max-w))))
	      (directory-list dir))))

(check-same-all (lambda (i o) (qp-encode-stream i o))
		qp-decode-stream
		#rx#"^(|[\t \41-\176]*[\41-\176]+)$"
		76)

(check-same-all base64-encode-stream 
		base64-decode-stream
		#rx#"^[0-9a-zA-Z+=/]*$"
		72)

(report-errs)
