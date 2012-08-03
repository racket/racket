(load-relative "loadtest.rktl")

(Section 'setup)

(require setup/path-to-relative)

(let ([missing   "/some/inexistent/path"]
      [collects  (build-path (collection-path "racket") "foo.rkt")]
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
  (test-both collects "racket/foo.rkt" "<collects>/racket/foo.rkt")
  (err/rt-test (path->relative-string/setup #f))
  (err/rt-test (path->relative-string/setup #"bleh"))
  (err/rt-test (path->relative-string/setup 'bleh)))

(require unstable/file)
(let ()
  (define tmpdir (make-temporary-file "tmp~a" 'directory (current-directory)))
  (define tmppath (build-path tmpdir "tmp.rkt"))
  (with-output-to-file (build-path tmpdir "tmp.rkt")  #:exists 'replace
                       (lambda () 
                         (printf "#lang racket\n")))
  (define exec-path (find-system-path 'exec-file))
  (define relpath (find-relative-path (current-directory) tmppath))

  (test #t system* exec-path "-l" "raco" "make" "-j" "2" (path->string relpath))
  (delete-directory/files tmpdir))


(report-errs)
