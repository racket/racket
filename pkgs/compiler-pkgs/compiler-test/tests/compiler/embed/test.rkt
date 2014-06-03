#lang racket/base

(require compiler/embed
         racket/file
	 racket/system
         racket/port
         launcher
         compiler/distribute)

(define (test expect f/label . args)
  (define r (apply (if (procedure? f/label)
                       f/label
                       values)
                   args))
  (unless (equal? expect r)
    (error "failed\n")))

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
  (printf "Making ~a with ~a...\n" exe src)
  (when (file-exists? exe)
    (delete-file exe)))

(define (try-one-exe exe expect mred?)
  (printf "Running ~a\n" exe)
  (let ([plthome (getenv "PLTHOME")]
	[collects (getenv "PLTCOLLECTS")]
        [out (open-output-string)])
    (define temp-home-dir (make-temporary-file "racket-tmp-home~a" 'directory))
    ;; Try to hide usual collections:
    (parameterize ([current-environment-variables
                    (environment-variables-copy
                     (current-environment-variables))])
      (putenv "PLTUSERHOME" (path->string temp-home-dir))
      (when plthome
        (putenv "PLTHOME" (path->string (build-path (find-system-path 'temp-dir) "NOPE"))))
      (when collects
        (putenv "PLTCOLLECTS" (path->string (build-path (find-system-path 'temp-dir) "NOPE"))))
      ;; Execute:
      (parameterize ([current-directory (find-system-path 'temp-dir)])
        (when (file-exists? "stdout")
          (delete-file "stdout"))
        (let ([path (if (and mred? (eq? 'macosx (system-type)))
                        (let-values ([(base name dir?) (split-path exe)])
                          (build-path exe "Contents" "MacOS"
                                      (path-replace-suffix name #"")))
                        exe)])
          (test #t
                path
                (parameterize ([current-output-port out])
                  (system* path))))))
    (delete-directory/files temp-home-dir)
    (let ([stdout-file (build-path (find-system-path 'temp-dir) "stdout")])
      (if (file-exists? stdout-file)
          (test expect with-input-from-file stdout-file
                (lambda () (read-string 5000)))
          (test expect get-output-string out)))))
  
(define (try-exe exe expect mred? [dist-hook void] #:dist? [dist? #t] . collects)
  (try-one-exe exe expect mred?)
  (when dist?
    ;; Build a distribution directory, and try that, too:
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
    (delete-directory/files dist-dir)))

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
     `((#t (lib ,filename "tests" "compiler" "embed")))
     null
     #f
     `(,(flags "l") ,(string-append "tests/compiler/embed/" filename)))
    (try-exe dest expect mred?)

    ;; As a launcher:
    (prepare dest filename)
    ((if mred? make-gracket-launcher make-racket-launcher)
     (list "-l" (string-append "tests/compiler/embed/" filename))
     dest)
    (try-exe dest expect mred? #:dist? #f)

    ;; Try explicit prefix:
    (printf ">>>explicit prefix\n")
    (let ([w/prefix
	   (lambda (pfx)
	     (prepare dest filename)
	     (make-embedding-executable 
	      dest mred? #f
	      `((,pfx (lib ,filename "tests" "compiler" "embed"))
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
      (let ([path (build-path (collection-path "tests" "compiler" "embed") filename)])
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
      (let ([path (build-path (collection-path "tests" "compiler" "embed") filename)])
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
      (parameterize ([current-directory (collection-path "tests" "compiler" "embed")])
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
       `((#t (lib ,filename "tests" "compiler" "embed"))
         (#t (lib "embed-me3.rkt" "tests" "compiler" "embed")))
       null
       (base-compile
        `(begin
           (namespace-require '(lib "embed-me3.rkt" "tests" "compiler" "embed"))
           (namespace-require '(lib ,filename "tests" "compiler" "embed"))))
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
         `((#t (lib ,filename "tests" "compiler" "embed")))
         (list 
          tmp
          (build-path (collection-path "tests" "compiler" "embed") "embed-me4.rktl"))
         `(with-output-to-file (build-path (find-system-path 'temp-dir) "stdout")
            (lambda () (display "... and more!\n"))
            'append)
         `(,(flags "l") ,(string-append "tests/compiler/embed/" filename)))
        (delete-file tmp))
      (try-exe dest (string-append 
                     "This is the literal expression 4.\n" 
                     "... and more!\n"
                     expect)
               mred?)))

  (one-mz-test "embed-me1.rkt" "This is 1\n" #t)
  (unless mred?
    (one-mz-test "embed-me1b.rkt" "This is 1b\n" #f)
    (one-mz-test "embed-me1c.rkt" "This is 1c\n" #f)
    (one-mz-test "embed-me1d.rkt" "This is 1d\n" #f)
    (one-mz-test "embed-me1e.rkt" "This is 1e\n" #f)
    (one-mz-test "embed-me1f.rkt" "This is 1f\n" #f)
    (one-mz-test "embed-me2.rkt" "This is 1\nThis is 2: #t\n" #t)
    (one-mz-test "embed-me13.rkt" "This is 14\n" #f)
    (one-mz-test "embed-me14.rkt" "This is 14\n" #f)
    (one-mz-test "embed-me15.rkt" "This is 15.\n" #f)
    (one-mz-test "embed-me17.rkt" "This is 17.\n" #f)
    (one-mz-test "embed-me18.rkt" "This is 18.\n" #f)
    (one-mz-test "embed-me19.rkt" "This is 19.\n" #f)
    (one-mz-test "embed-me21.rkt" "This is 21.\n" #f))

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
                (with-output-to-file (build-path (find-system-path 'temp-dir) "stdout")
                  (lambda () (printf s))
                  #:exists 'append)))
       (out "\uA9, \u7238, and \U1D670\n")))
   `(,(flags "ne") "(out \"\u7237...\U1D671\n\")"))
  (try-exe dest "\uA9, \u7238, and \U1D670\n\u7237...\U1D671\n" mred?))

(define (try-basic)
  (mz-tests #f)
  (mz-tests #t)

  (begin
    (prepare mr-dest "embed-me5.rkt")
    (make-embedding-executable 
     mr-dest #t #f
     `((#t (lib "embed-me5.rkt" "tests" "compiler" "embed")))
     null
     #f
     `("-l" "tests/compiler/embed/embed-me5.rkt"))
    (try-exe mr-dest "This is 5: #<class:button%>\n" #t)))

;; Try the raco interface:
(require setup/dirs
	 mzlib/file)
(define mzc (build-path (find-console-bin-dir) (if (eq? 'windows (system-type))
                                                   "mzc.exe"
                                                   "mzc")))
(define raco (build-path (find-console-bin-dir) (if (eq? 'windows (system-type))
                                                    "raco.exe"
                                                    "raco")))

(define (short-mzc-tests mred?)
  (parameterize ([current-directory (find-system-path 'temp-dir)])

    ;; raco exe
    (system* raco
             "exe"
	     "-o" (path->string (mk-dest mred?))
	     (if mred? "--gui" "--")
	     (path->string (build-path (collection-path "tests" "compiler" "embed") "embed-me1.rkt")))
    (try-exe (mk-dest mred?) "This is 1\n" mred?)

    ;; raco exe on a module with a `main' submodule
    (system* raco
             "exe"
	     "-o" (path->string (mk-dest mred?))
	     (if mred? "--gui" "--")
	     (path->string (build-path (collection-path "tests" "compiler" "embed") "embed-me16.rkt")))
    (try-exe (mk-dest mred?) "This is 16.\n" mred?)))

(define (mzc-tests mred?)
  (short-mzc-tests mred?)
  (parameterize ([current-directory (find-system-path 'temp-dir)])

    ;; raco exe
    (system* raco
             "exe"
	     "-o" (path->string (mk-dest mred?))
	     (if mred? "--gui" "--")
	     (path->string (build-path (collection-path "tests" "compiler" "embed") "embed-me1.rkt")))
    (try-exe (mk-dest mred?) "This is 1\n" mred?)

    ;; raco exe on a module with a `main' submodule
    (system* raco
             "exe"
	     "-o" (path->string (mk-dest mred?))
	     (if mred? "--gui" "--")
	     (path->string (build-path (collection-path "tests" "compiler" "embed") "embed-me16.rkt")))
    (try-exe (mk-dest mred?) "This is 16.\n" mred?)

    ;; raco exe on a module with a `main' submodule+
    (system* raco
             "exe"
	     "-o" (path->string (mk-dest mred?))
	     (if mred? "--gui" "--")
	     (path->string (build-path (collection-path "tests" "compiler" "embed") "embed-me20.rkt")))
    (try-exe (mk-dest mred?) "This is 20.\n" mred?)

    ;; raco exe on a module with a `configure-runtime' submodule
    (system* raco
             "exe"
	     "-o" (path->string (mk-dest mred?))
	     (if mred? "--gui" "--")
	     (path->string (build-path (collection-path "tests" "compiler" "embed") "embed-me22.rkt")))
    (try-exe (mk-dest mred?) "Configure!\nThis is 22.\n" mred?)

    ;; raco exe on a module with serialization
    (system* raco
             "exe"
	     "-o" (path->string (mk-dest mred?))
	     (if mred? "--gui" "--")
	     (path->string (build-path (collection-path "tests" "compiler" "embed") "embed-me23.rkt")))
    (try-exe (mk-dest mred?) "1\n2\n" mred?)

    ;; raco exe --launcher
    (system* raco
             "exe"
             "--launcher"
	     "-o" (path->string (mk-dest mred?))
	     (if mred? "--gui" "--")
	     (path->string (build-path (collection-path "tests" "compiler" "embed") "embed-me1.rkt")))
    (try-exe (mk-dest mred?) "This is 1\n" mred? #:dist? #f)

    ;; the rest use mzc...

    (system* mzc 
	     (if mred? "--gui-exe" "--exe")
	     (path->string (mk-dest mred?))
	     (path->string (build-path (collection-path "tests" "compiler" "embed") "embed-me1.rkt")))
    (try-exe (mk-dest mred?) "This is 1\n" mred?)

    (define (check-collection-path prog lib in-main?)
      ;; Check that etc.rkt isn't found if it's not included:
      (printf ">>not included\n")
      (system* mzc 
               (if mred? "--gui-exe" "--exe")
               (path->string (mk-dest mred?))
               (path->string (build-path (collection-path "tests" "compiler" "embed") prog)))
      (try-exe (mk-dest mred?) "This is 6\nno etc.ss\n" mred?)

      ;; And it is found if it is included:
      (printf ">>included\n")
      (system* mzc 
               (if mred? "--gui-exe" "--exe")
               (path->string (mk-dest mred?))
               "++lib" lib
               (path->string (build-path (collection-path "tests" "compiler" "embed") prog)))
      (try-exe (mk-dest mred?) "This is 6\n#t\n" mred?)

      ;; Or, it's found if we set the collection path and the config path (where the latter
      ;; finds links for packages):
      (printf ">>set coll path\n")
      (system* mzc 
               (if mred? "--gui-exe" "--exe")
               (path->string (mk-dest mred?))
               "--collects-path"
               (path->string (find-collects-dir))
               (path->string (build-path (collection-path "tests" "compiler" "embed") prog)))
      ;; Don't try a distribution for this one:
      (try-one-exe (mk-dest mred?) (if in-main? "This is 6\n#t\n" "This is 6\nno etc.ss\n") mred?)

      ;; Or, it's found if we set the collection path and the config path (where the latter
      ;; finds links for packages):
      (printf ">>set coll path plus config\n")
      (system* mzc 
               (if mred? "--gui-exe" "--exe")
               (path->string (mk-dest mred?))
               "--collects-path"
               (path->string (find-collects-dir))
               "--config-path"
               (path->string (find-config-dir))
               (path->string (build-path (collection-path "tests" "compiler" "embed") prog)))
      ;; Don't try a distribution for this one:
      (try-one-exe (mk-dest mred?) "This is 6\n#t\n" mred?)

      ;; Try --collects-dest mode
      (printf ">>--collects-dest\n")
      (system* mzc 
               (if mred? "--gui-exe" "--exe")
               (path->string (mk-dest mred?))
               "++lib" lib
               "--collects-dest" "cts"
               "--collects-path" "cts"
               (path->string (build-path (collection-path "tests" "compiler" "embed") prog)))
      (try-exe (mk-dest mred?) "This is 6\n#t\n" mred? void "cts") ; <- cts copied to distribution
      (delete-directory/files "cts")
      (parameterize ([current-error-port (open-output-nowhere)])
        (test #f system* (mk-dest mred?))))
    (check-collection-path "embed-me6b.rkt" "racket/fixnum.rkt" #t)
    (check-collection-path "embed-me6.rkt" "mzlib/etc.rkt" #f)
  
    (void)))

(define (try-mzc)
  (mzc-tests #f)
  (short-mzc-tests #t))

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
      (build-path ext-dir (append-extension-suffix "embed-me8_rkt")))

    (define ss-file
      (build-path (find-system-path 'temp-dir) "embed-me9.rkt"))

    (make-directory* ext-dir)
    
    (system* mzc 
             "--cc"
             "-d" (path->string (path-only obj-file))
             (path->string (build-path (collection-path "tests" "compiler" "embed") "embed-me8.c")))
    (system* mzc 
             "--ld"
             (path->string ext-file)
             (path->string obj-file))

    (when (file-exists? ss-file)
      (delete-file ss-file))
    (copy-file (build-path (collection-path "tests" "compiler" "embed") "embed-me9.rkt")
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
             (path->string (build-path (collection-path "tests" "compiler" "embed") "embed-me10.rkt")))
    (try-exe (mk-dest mred?) "#t\n" mred?)))

(define (try-extension)
  (extension-test #f)
  (extension-test #t))

(define (try-gracket)
  ;; A GRacket-specific test with mzc:
  (parameterize ([current-directory (find-system-path 'temp-dir)])
    (system* mzc 
             "--gui-exe"
             (path->string (mk-dest #t))
             (path->string (build-path (collection-path "tests" "compiler" "embed") "embed-me5.rkt")))
    (try-exe (mk-dest #t) "This is 5: #<class:button%>\n" #t)))

;; Try including source that needs a reader extension

(define (try-reader-test 12? mred? ss-file? ss-reader?)
  ;; actual "11" files use ".rkt", actual "12" files use ".ss"
  (define dest (mk-dest mred?))
  (define filename (format (if ss-file?
                               "embed-me~a.ss"
                               "embed-me~a.rkt")
                           (if 12? "12" "11")))
  (define (flags s)
    (string-append "-" s))

  (printf "Trying ~s ~s ~s ~s...\n" (if 12? "12" "11") mred? ss-file? ss-reader?)

  (create-embedding-executable 
   dest
   #:modules `((#t (lib ,filename "tests" "compiler" "embed")))
   #:cmdline `(,(flags "l") ,(string-append "tests/compiler/embed/" filename))
   #:src-filter (lambda (f)
                  (let-values ([(base name dir?) (split-path f)])
                    (equal? name (path-replace-suffix (string->path filename) 
                                                      (if 12? #".ss" #".rkt")))))
   #:get-extra-imports (lambda (f code)
                         (let-values ([(base name dir?) (split-path f)])
                           (if (equal? name (path-replace-suffix (string->path filename) 
                                                                 (if 12? #".ss" #".rkt")))
                               `((lib ,(format (if ss-reader?
                                                   "embed-me~a-rd.ss"
                                                   "embed-me~a-rd.rkt")
                                               (if 12? "12" "11"))
                                      "tests" 
                                      "compiler"
                                      "embed"))
                               null)))
   #:mred? mred?)

  (putenv "ELEVEN" "eleven")
  (try-exe dest "It goes to eleven!\n" mred?)
  (putenv "ELEVEN" "done"))

(define (try-reader)
  (for ([12? (in-list '(#f #t))])
    (try-reader-test 12? #f #f #f)
    (try-reader-test 12? #t #f #f)
    (try-reader-test 12? #f #t #f)
    (try-reader-test 12? #f #f #t)))

;; ----------------------------------------

(define planet (build-path (find-console-bin-dir) (if (eq? 'windows (system-type))
                                                      "planet.exe"
                                                      "planet")))

(define (try-planet)
  (system* raco "planet" "link" "racket-tester" "p1.plt" "1" "0"
           (path->string (collection-path "tests" "compiler" "embed" "embed-planet-1")))
  (system* raco "planet" "link" "racket-tester" "p2.plt" "2" "2"
           (path->string (collection-path "tests" "compiler" "embed" "embed-planet-2")))

  (let ([go (lambda (path expected)
              (printf "Trying planet ~s...\n" path)
              (let ([tmp (make-temporary-file)]
                    [dest (mk-dest #f)])
                (with-output-to-file tmp
                  #:exists 'truncate
                  (lambda ()
                    (printf "#lang racket/base (require ~s)\n" path)))
                (system* mzc "--exe" (path->string dest) (path->string tmp))
                (try-exe dest expected #f)

                (delete-directory/files dest)

                (delete-file tmp)))])
    (go '(planet racket-tester/p1) "one\n")
    (go '(planet "racket-tester/p1:1") "one\n")
    (go '(planet "racket-tester/p1:1:0") "one\n")
    (go '(planet "racket-tester/p1:1:0/main.ss") "one\n")
    (go '(planet racket-tester/p2) "two\n")

    (go '(planet racket-tester/p1/alt) "one\nalt\n")
    (go '(planet racket-tester/p1/other) "two\nother\n")
    (go '(planet "private/sub.rkt" ("racket-tester" "p2.plt" 2 0)) "two\nsub\n")
    (go '(planet "private/sub.ss" ("racket-tester" "p2.plt" 2 0)) "two\nsub\n")
    (go '(planet "main.ss" ("racket-tester" "p2.plt" 2 0)) "two\n")

    (go '(planet racket-tester/p1/dyn-sub) "out\n")

    (void))
  
  (system* raco "planet" "unlink" "racket-tester" "p1.plt" "1" "0")
  (system* raco "planet" "unlink" "racket-tester" "p2.plt" "2" "2"))

;; ----------------------------------------

(define (try-*sl)
  (define (try-one src)
    (printf "Trying ~a...\n" src)
    (define exe (path->string (mk-dest #f)))
    (system* raco
             "exe"
             "-o" exe
             "--"
             (path->string (build-path (collection-path "tests" "compiler" "embed") src)))
    (try-exe exe "10\n" #f))

  (try-one "embed-bsl.rkt")
  (try-one "embed-bsla.rkt")
  (try-one "embed-isl.rkt")
  (try-one "embed-isll.rkt")
  (try-one "embed-asl.rkt"))
  
;; ----------------------------------------

(try-basic)
(try-mzc)
(try-extension)
(try-gracket)
(try-reader)
(try-planet)
(try-*sl)

;; ----------------------------------------
;; Make sure that embedding does not break future module declarations

(let ()
  (parameterize ([current-output-port (open-output-bytes)])
    (write-module-bundle
     #:modules (list (list #f (collection-file-path "embed-me24.rkt" "tests" "compiler" "embed")))))
  
  (parameterize ([read-accept-reader #t]
                 [current-namespace (make-base-namespace)])
    (eval (read (open-input-string "#lang racket 10")))))

