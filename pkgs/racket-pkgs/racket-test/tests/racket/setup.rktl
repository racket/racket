(load-relative "loadtest.rktl")

(Section 'setup)

;; ----------------------------------------

(require setup/path-to-relative)

(let ([missing   "/some/inexistent/path"]
      [collects  (build-path (path-only (collection-file-path "main.rkt" "racket")) "foo.rkt")]
      [relative  "some/path"])
  (define (test-both path/str expected-str [lib-expected expected-str])
    (define str  (if (string? path/str) path/str (path->string path/str)))
    (define path (string->path str))
    (test expected-str path->relative-string/setup str)
    (test expected-str path->relative-string/setup path)
    (test lib-expected path->relative-string/library str)
    (test lib-expected path->relative-string/library path))
  (test-both missing  missing)
  (test-both relative relative)
  (test-both collects "<collects>/racket/foo.rkt" "<collects>/racket/foo.rkt")
  (err/rt-test (path->relative-string/setup #f))
  (err/rt-test (path->relative-string/setup #"bleh"))
  (err/rt-test (path->relative-string/setup 'bleh)))

;; ----------------------------------------

(require setup/collects)

(for ([i '([("main.rkt" "racket") (lib "racket/main.rkt")]
           [("reader.rkt" "scribble") (lib "scribble/reader.rkt")])])
  (define p (apply collection-file-path (car i)))
  (test (cadr i) path->module-path p)
  (let ([out (build-path (let-values ([(base name dir?) (split-path p)])
                           base)
                         "../info.rkt")])
    (test out path->module-path out)))
(test "a/b" path->module-path "a/b")
(test (find-system-path 'temp-dir) path->module-path (find-system-path 'temp-dir))

;; ----------------------------------------

(require compiler/find-exe)

(let ()
  (define tmpdir (make-temporary-file "tmp~a" 'directory (current-directory)))
  (define tmppath (build-path tmpdir "tmp.rkt"))
  (with-output-to-file (build-path tmpdir "tmp.rkt")  #:exists 'replace
                       (lambda () 
                         (printf "#lang racket\n")))
  (define exec-path (find-exe))
  (define relpath (find-relative-path (current-directory) tmppath))

  (test #t system* exec-path "-l" "raco" "make" "-j" "2" (path->string relpath))
  (delete-directory/files tmpdir))

;; ----------------------------------------
;; Make sure that setting the reader doesn't break reading a configuration file:

(parameterize ([current-namespace (make-base-namespace)]
               [read-accept-dot #f])
  ((dynamic-require 'setup/dirs 'get-pkgs-search-dirs)))

;; ----------------------------------------

(report-errs)
