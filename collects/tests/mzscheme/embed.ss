
(load-relative "loadtest.ss")

(SECTION 'embed)

(require (lib "embed.ss" "compiler")
	 (lib "process.ss"))

(define (mk-dest mred?)
  (build-path (find-system-path 'temp-dir) 
	      (case (system-type)
		[(windows) "e.exe"]
		[(unix) "e"]
		[(macosx) (if mred?
			      "e.app"
			      "e")])))

(define mz-dest (mk-dest #f))
(define mr-dest (mk-dest #t))

(define (prepare exe src)
  (printf "Making ~a with ~a...~n" exe src)
  (when (file-exists? exe)
    (delete-file exe)))

(define (try-exe exe expect)
  (let ([out (open-output-bytes)]
	[in (open-input-bytes #"")])
    (parameterize ([current-output-port out]
		   [current-input-port in])
      (system* exe))
    (test expect get-output-string out)))

(define (mz-tests mred?)
  (define dest (if mred? mr-dest mz-dest))
  (define (one-mz-test filename expect)
    ;; Try simple mode: one module, launched from cmd line:
    (prepare dest filename)
    (make-embedding-executable 
     dest mred? #f
     `((#t (lib ,filename "tests" "mzscheme")))
     null
     null
     `("-mvqL" ,filename "tests/mzscheme"))
    (try-exe dest expect)

    ;; Try explicit prefix:
    (let ([w/prefix
	   (lambda (pfx)
	     (prepare dest filename)
	     (make-embedding-executable 
	      dest mred? #f
	      `((,pfx (lib ,filename "tests" "mzscheme")))
	      null
	      null
	      `("-mvqe" ,(format "(require ~a~a)" 
				 (or pfx "")
				 (regexp-replace #rx"[.].*$" filename ""))))
	     (try-exe dest expect))])
      (w/prefix #f)
      (w/prefix 'before:))

    ;; Try full path, and use literal S-exp to start
    (prepare dest filename)
    (let ([path (build-path (collection-path "tests" "mzscheme") filename)])
      (make-embedding-executable 
       dest mred? #f
       `((#t ,path))
       null
       `(require (file ,(path->string path)))
       `("-mvq")))
    (try-exe dest expect)

    ;; Use `file' form:
    (prepare dest filename)
    (let ([path (build-path (collection-path "tests" "mzscheme") filename)])
      (make-embedding-executable 
       dest mred? #f
       `((#t (file ,(path->string path))))
       null
       `(require (file ,(path->string path)))
       `("-mvq")))
    (try-exe dest expect)

    ;; Use relative path
    (prepare dest filename)
    (parameterize ([current-directory (collection-path "tests" "mzscheme")])
      (make-embedding-executable 
       dest mred? #f
       `((#f ,filename))
       null
       `(require ,(string->symbol (regexp-replace #rx"[.].*$" filename "")))
       `("-mvq")))
    (try-exe dest expect)

    ;; Try multiple modules
    (prepare dest filename)
    (make-embedding-executable 
     dest mred? #f
     `((#t (lib ,filename "tests" "mzscheme"))
       (#t (lib "embed-me3.ss" "tests" "mzscheme")))
     null
     `(begin
	(require (lib "embed-me3.ss" "tests" "mzscheme"))
	(require (lib ,filename "tests" "mzscheme")))
     `("-mvq"))
    (try-exe dest (string-append "3 is here, too? #t\n" expect))

    ;; Try a literal file
    (prepare dest filename)
    (make-embedding-executable 
     dest mred? #f
     `((#t (lib ,filename "tests" "mzscheme")))
     (list (build-path (collection-path "tests" "mzscheme") "embed-me4.ss"))
     `(begin (display "... and more!\n"))
     `("-mvqL" ,filename "tests/mzscheme"))
    (try-exe dest (string-append 
		      "This is the literal expression 4.\n" 
		      "... and more!\n"
		      expect)))

  (one-mz-test "embed-me1.ss" "This is 1\n")
  (one-mz-test "embed-me2.ss" "This is 1\nThis is 2: #t\n")

  ;; Try unicode expr and cmdline:
  (prepare dest "unicode")
  (make-embedding-executable 
   dest #f #f
   null
   null
   `(printf "\uA9, \u7238, and \U1D670\n")
   `("-mvqe" "(display \"\u7237...\U1D671\n\")"))
  (try-exe dest "\uA9, \u7238, and \U1D670\n\u7237...\U1D671\n"))

(mz-tests #f)
(mz-tests #t)

(prepare mr-dest "embed-me5.ss")
(make-embedding-executable 
 mr-dest #t #f
 `((#t (lib "embed-me5.ss" "tests" "mzscheme")))
 null
 null
 `("-ZmvqL" "embed-me5.ss" "tests/mzscheme"))
(try-exe mr-dest "This is 5: #<struct:class:button%>\n")

(report-errs)
