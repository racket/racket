#lang racket/base
(require racket/file
         racket/path
         racket/system
         compiler/find-exe)

(parameterize ((current-directory (find-system-path 'temp-dir)))
  (define tmpdir (make-temporary-file "tmp~a" 'directory (current-directory)))
  (define tmppath (build-path tmpdir "tmp.rkt"))
  (with-output-to-file (build-path tmpdir "tmp.rkt")  #:exists 'replace
                       (lambda ()
                         (printf "#lang racket\n")))
  (define exec-path (find-exe))
  (define relpath (find-relative-path (current-directory) tmppath))

  (define ok? (system* exec-path "-l" "raco" "make" "-j" "2" (path->string relpath)))
  (delete-directory/files tmpdir)

  (unless ok?
    (error "`raco make` test failed")))
