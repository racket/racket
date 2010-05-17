
(load-relative "loadtest.rktl")

(Section 'embed)

(require compiler/embed
         mzlib/file
	 mzlib/process
         compiler/distribute)

(define (mk-dest-bin mred?)
  (case (system-type)
    [(windows) "e.exe"]
    [(unix) "e"]
    [(macosx) (if mred?
                  "e.app"
                  "e")]))

(define (mk-dest mred?)
  (build-path (find-system-path 'temp-dir) 
              (mk-dest-bin mred?)))

(define mz-dest (mk-dest #f))
(define mr-dest (mk-dest #t))

(define dist-dir (build-path (find-system-path 'temp-dir)
                             "e-dist"))
(define dist-mz-exe (build-path
                     (case (system-type)
                       [(windows) 'same]
                       [else "bin"])
                     (mk-dest-bin #f)))
(define dist-mred-exe (build-path
                       (case (system-type)
                         [(windows macosx) 'same]
                         [else "bin"])
                       (mk-dest-bin #t)))

(define (prepare exe src)
  (printf "Making ~a with ~a...~n" exe src)
  (when (file-exists? exe)
    (delete-file exe)))

(define (try-one-exe exe expect mred?)
  (printf "Running ~a\n" exe)
  (let ([plthome (getenv "PLTHOME")]
	[collects (getenv "PLTCOLLECTS")])
    ;; Try to hide usual collections:
    (when plthome
      (putenv "PLTHOME" (path->string (build-path (find-system-path 'temp-dir) "NOPE"))))
    (when collects
      (putenv "PLTCOLLECTS" (path->string (build-path (find-system-path 'temp-dir) "NOPE"))))
    ;; Execute:
    (parameterize ([current-directory (find-system-path 'temp-dir)])
      (when (file-exists? "stdout")
	(delete-file "stdout"))
      (test #t
            system* (if (and mred? (eq? 'macosx (system-type)))
                        (let-values ([(base name dir?) (split-path exe)])
                          (build-path exe "Contents" "MacOS"
                                      (path-replace-suffix name #"")))
                        exe)))
    (when plthome
      (putenv "PLTHOME" plthome))
    (when collects
      (putenv "PLTCOLLECTS" collects))
    (test expect with-input-from-file (build-path (find-system-path 'temp-dir) "stdout") 
	  (lambda () (read-string 5000)))))

(define try-exe 
  (case-lambda
   [(exe expect mred?)
    (try-exe exe expect mred? void)]
   [(exe expect mred? dist-hook . collects)
    (try-one-exe exe expect mred?)
    ;; Build a distirbution directory, and try that, too:
    (printf " ... from distribution ...\n")
    (when (directory-exists? dist-dir)
      (delete-directory/files dist-dir))
    (assemble-distribution dist-dir (list exe) #:copy-collects collects)
    (dist-hook)
    (try-one-exe (build-path dist-dir
                             (if mred?
                                 dist-mred-exe
                                 dist-mz-exe))
                 expect mred?)
    (delete-directory/files dist-dir)]))

(define (base-compile e)
  (parameterize ([current-namespace (make-base-namespace)])
    (compile e)))
(define (kernel-compile e)
  (parameterize ([current-namespace (make-base-empty-namespace)])
    (namespace-require ''#%kernel)
    (compile e)))

(define (mz-tests mred?)
  (define dest (if mred? mr-dest mz-dest))
  (define (flags s)
    (string-append "-" s))
  (define (one-mz-test filename expect literal?)
    ;; Try simple mode: one module, launched from cmd line:
    (prepare dest filename)
    (make-embedding-executable 
     dest mred? #f
     `((#t (lib ,filename "tests" "mzscheme")))
     null
     #f
     `(,(flags "l") ,(string-append "tests/mzscheme/" filename)))
    (try-exe dest expect mred?)

    ;; Try explicit prefix:
    (printf ">>>explicit prefix\n")
    (let ([w/prefix
	   (lambda (pfx)
	     (prepare dest filename)
	     (make-embedding-executable 
	      dest mred? #f
	      `((,pfx (lib ,filename "tests" "mzscheme"))
                (#t (lib "scheme/init")))
	      null
	      #f
	      `(,(flags "lne") 
                "scheme/base"
                ,(format "(require '~a~a)" 
                         (or pfx "")
                         (regexp-replace #rx"[.].*$" filename ""))))
	     (try-exe dest expect mred?))])
      (w/prefix #f)
      (w/prefix 'before:))

    (when literal?
      ;; Try full path, and use literal S-exp to start
      (printf ">>>literal sexp\n")
      (prepare dest filename)
      (let ([path (build-path (collection-path "tests" "mzscheme") filename)])
        (make-embedding-executable 
         dest mred? #f
         `((#t ,path))
         null
         (base-compile
          `(namespace-require '(file ,(path->string path))))
         `(,(flags ""))))
      (try-exe dest expect mred?)
      
      ;; Use `file' form:
      (printf ">>>file\n")
      (prepare dest filename)
      (let ([path (build-path (collection-path "tests" "mzscheme") filename)])
        (make-embedding-executable 
         dest mred? #f
         `((#t (file ,(path->string path))))
         null
         (base-compile
          `(namespace-require '(file ,(path->string path))))
         `(,(flags ""))))
      (try-exe dest expect mred?)

      ;; Use relative path
      (printf ">>>relative path\n")
      (prepare dest filename)
      (parameterize ([current-directory (collection-path "tests" "mzscheme")])
        (make-embedding-executable 
         dest mred? #f
         `((#f ,filename))
         null
         (base-compile
          `(namespace-require '',(string->symbol (regexp-replace #rx"[.].*$" filename ""))))
         `(,(flags ""))))
      (try-exe dest expect mred?)

      ;; Try multiple modules
      (printf ">>>multiple\n")
      (prepare dest filename)
      (make-embedding-executable 
       dest mred? #f
       `((#t (lib ,filename "tests" "mzscheme"))
         (#t (lib "embed-me3.ss" "tests" "mzscheme")))
       null
       (base-compile
        `(begin
           (namespace-require '(lib "embed-me3.ss" "tests" "mzscheme"))
           (namespace-require '(lib ,filename "tests" "mzscheme"))))
       `(,(flags "")))
      (try-exe dest (string-append "3 is here, too? #t\n" expect) mred?)

      ;; Try a literal file
      (printf ">>>literal\n")
      (prepare dest filename)
      (let ([tmp (make-temporary-file)])
        (with-output-to-file tmp 
          #:exists 'truncate
          (lambda ()
            (write (kernel-compile
                    '(namespace-require ''#%kernel)))))
        (make-embedding-executable 
         dest mred? #f
         `((#t (lib ,filename "tests" "mzscheme")))
         (list 
          tmp
          (build-path (collection-path "tests" "mzscheme") "embed-me4.ss"))
         `(with-output-to-file "stdout"
            (lambda () (display "... and more!\n"))
            'append)
         `(,(flags "l") ,(string-append "tests/mzscheme/" filename)))
        (delete-file tmp))
      (try-exe dest (string-append 
                     "This is the literal expression 4.\n" 
                     "... and more!\n"
                     expect)
               mred?)))

  (one-mz-test "embed-me1.ss" "This is 1\n" #t)
  (one-mz-test "embed-me1b.ss" "This is 1b\n" #f)
  (one-mz-test "embed-me1c.ss" "This is 1c\n" #f)
  (one-mz-test "embed-me1d.ss" "This is 1d\n" #f)
  (one-mz-test "embed-me1e.ss" "This is 1e\n" #f)
  (one-mz-test "embed-me2.ss" "This is 1\nThis is 2: #t\n" #t)

  ;; Try unicode expr and cmdline:
  (prepare dest "unicode")
  (make-embedding-executable 
   dest mred? #f
   '((#t scheme/base))
   null
   (base-compile
    '(begin 
       (require scheme/base)
       (eval '(define (out s)
                (with-output-to-file "stdout"
                  (lambda () (printf s))
                  #:exists 'append)))
       (out "\uA9, \u7238, and \U1D670\n")))
   `(,(flags "ne") "(out \"\u7237...\U1D671\n\")"))
  (try-exe dest "\uA9, \u7238, and \U1D670\n\u7237...\U1D671\n" mred?))

(mz-tests #f)
(mz-tests #t)

(begin
  (prepare mr-dest "embed-me5.ss")
  (make-embedding-executable 
   mr-dest #t #f
   `((#t (lib "embed-me5.ss" "tests" "mzscheme")))
   null
   #f
   `("-l" "tests/mzscheme/embed-me5.ss"))
  (try-exe mr-dest "This is 5: #<class:button%>\n" #t))

;; Try the mzc interface:
(require setup/dirs
	 mzlib/file)
(define mzc (build-path (find-console-bin-dir) (if (eq? 'windows (system-type))
                                                   "mzc.exe"
                                                   "mzc")))

(define (mzc-tests mred?)
  (parameterize ([current-directory (find-system-path 'temp-dir)])

    (system* mzc 
	     (if mred? "--gui-exe" "--exe")
	     (path->string (mk-dest mred?))
	     (path->string (build-path (collection-path "tests" "mzscheme") "embed-me1.ss")))
    (try-exe (mk-dest mred?) "This is 1\n" mred?)

    ;; Check that etc.ss isn't found if it's not included:
    (printf ">>not included\n")
    (system* mzc 
	     (if mred? "--gui-exe" "--exe")
	     (path->string (mk-dest mred?))
	     (path->string (build-path (collection-path "tests" "mzscheme") "embed-me6.ss")))
    (try-exe (mk-dest mred?) "This is 6\nno etc.ss\n" mred?)

    ;; And it is found if it is included:
    (printf ">>included\n")
    (system* mzc 
	     (if mred? "--gui-exe" "--exe")
	     (path->string (mk-dest mred?))
	     "++lib" "mzlib/etc.ss"
	     (path->string (build-path (collection-path "tests" "mzscheme") "embed-me6.ss")))
    (try-exe (mk-dest mred?) "This is 6\n#t\n" mred?)

    ;; Or, it's found if we set the collection path:
    (printf ">>set coll path\n")
    (system* mzc 
	     (if mred? "--gui-exe" "--exe")
	     (path->string (mk-dest mred?))
	     "--collects-path"
	     (path->string (find-collects-dir))
	     (path->string (build-path (collection-path "tests" "mzscheme") "embed-me6.ss")))
    ;; Don't try a distribution for this one:
    (try-one-exe (mk-dest mred?) "This is 6\n#t\n" mred?)

    ;; Try --collects-dest mode
    (printf ">>--collects-dest\n")
    (system* mzc 
	     (if mred? "--gui-exe" "--exe")
	     (path->string (mk-dest mred?))
	     "++lib" "mzlib/etc.ss"
	     "--collects-dest" "cts"
	     "--collects-path" "cts"
	     (path->string (build-path (collection-path "tests" "mzscheme") "embed-me6.ss")))
    (try-exe (mk-dest mred?) "This is 6\n#t\n" mred? void "cts") ; <- cts copied to distribution
    (delete-directory/files "cts")
    (test #f system* (mk-dest mred?))

    (void)))

(mzc-tests #f)
(mzc-tests #t)

(require dynext/file)
(define (extension-test mred?)
  (parameterize ([current-directory (find-system-path 'temp-dir)])
    
    (define obj-file
      (build-path (find-system-path 'temp-dir) (append-object-suffix "embed-me8")))

    (define ext-base-dir
      (build-path (find-system-path 'temp-dir)
                  "compiled"))

    (define ext-dir
      (build-path ext-base-dir
                  "native"
                  (system-library-subpath)))

    (define ext-file
      (build-path ext-dir (append-extension-suffix "embed-me8_ss")))

    (define ss-file
      (build-path (find-system-path 'temp-dir) "embed-me9.ss"))

    (make-directory* ext-dir)
    
    (system* mzc 
             "--cc"
             "-d" (path->string (path-only obj-file))
             (path->string (build-path (collection-path "tests" "mzscheme") "embed-me8.c")))
    (system* mzc 
             "--ld"
             (path->string ext-file)
             (path->string obj-file))

    (when (file-exists? ss-file)
      (delete-file ss-file))
    (copy-file (build-path (collection-path "tests" "mzscheme") "embed-me9.ss")
               ss-file)

    (system* mzc 
             (if mred? "--gui-exe" "--exe")
             (path->string (mk-dest mred?))
             (path->string ss-file))

    (delete-file ss-file)

    (try-exe (mk-dest mred?) "Hello, world!\n" mred? (lambda ()
                                                       (delete-directory/files ext-base-dir)))

    ;; openssl, which needs extra binaries under Windows
    (system* mzc 
             (if mred? "--gui-exe" "--exe")
             (path->string (mk-dest mred?))
             (path->string (build-path (collection-path "tests" "mzscheme") "embed-me10.ss")))
    (try-exe (mk-dest mred?) "#t\n" mred?)))

(extension-test #f)
(extension-test #t)

;; A GRacket-specific test with mzc:
(parameterize ([current-directory (find-system-path 'temp-dir)])
  (system* mzc 
	   "--gui-exe"
	   (path->string (mk-dest #t))
	   (path->string (build-path (collection-path "tests" "mzscheme") "embed-me5.ss")))
  (try-exe (mk-dest #t) "This is 5: #<class:button%>\n" #t))

;; Another GRacket-specific: try embedding plot, which has extra DLLs and font files:
(parameterize ([current-directory (find-system-path 'temp-dir)])
  (define direct (build-path (find-system-path 'temp-dir) "direct.ps"))

  (test #t
        system* (build-path (find-console-bin-dir) "mred")
        "-qu"
        (path->string (build-path (collection-path "tests" "mzscheme") "embed-me7.ss"))
        (path->string direct))

  (system* mzc 
           "--gui-exe"
           (path->string (mk-dest #t))
           (path->string (build-path (collection-path "tests" "mzscheme") "embed-me7.ss")))
  (try-exe (mk-dest #t) "plotted\n" #t))

;; Try including source that needs a reader extension

(define (try-reader-test mred?)
  (define dest (mk-dest mred?))
  (define filename "embed-me11.ss")
  (define (flags s)
    (string-append "-" s))

  (create-embedding-executable 
   dest
   #:modules `((#t (lib ,filename "tests" "mzscheme")))
   #:cmdline `(,(flags "l") ,(string-append "tests/mzscheme/" filename))
   #:src-filter (lambda (f)
                  (let-values ([(base name dir?) (split-path f)])
                    (equal? name (string->path filename))))
   #:get-extra-imports (lambda (f code)
                         (let-values ([(base name dir?) (split-path f)])
                           (if (equal? name (string->path filename))
                               '((lib "embed-me11-rd.ss" "tests" "mzscheme"))
                               null)))
   #:mred? mred?)

  (putenv "ELEVEN" "eleven")
  (try-exe dest "It goes to eleven!\n" mred?)
  (putenv "ELEVEN" "done"))

(try-reader-test #f)
(try-reader-test #t)

(report-errs)
