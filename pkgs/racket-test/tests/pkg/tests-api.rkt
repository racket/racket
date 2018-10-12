#lang racket/base
(require rackunit
         racket/file
         pkg/strip
         "util.rkt")

(this-test-is-run-by-the-main-test)

(pkg-tests

 (parameterize ([current-directory test-source-directory])
   (define tmp-dir (path->directory-path (make-temporary-file "tmp~a" 'directory)))
   (define pkg-path (build-path "test-pkgs" "pkg-strip"))
   (define pkg-dest-path (build-path tmp-dir "pkg-strip"))
   (make-directory pkg-dest-path)

   (define rx:does-not-exist #rx"directory does not exist")

   ;; Giving path to nonexistent directory should raise a contract exception.
   (check-exn rx:does-not-exist (lambda () (check-strip-compatible 'source "pkg-strip" "does not exist" error)))

   (check-exn rx:does-not-exist (lambda () (check-strip-compatible 'binary "pkg-strip" "does not exist" error)))

   (check-exn rx:does-not-exist (lambda () (check-strip-compatible 'binary-lib "pkg-strip" "does not exist" error)))

   (check-exn rx:does-not-exist (lambda () (check-strip-compatible 'built "pkg-strip" "does not exist" error)))

   ;; Path to existing directory should succeed.
   (check-not-exn (lambda () (check-strip-compatible 'source "pkg-strip" pkg-path error)))

   ;; Giving path to nonexistent src directory should raise a contract exception.
   (check-exn exn:fail:contract? (lambda () (generate-stripped-directory 'source 5 pkg-dest-path)))

   (check-exn rx:does-not-exist (lambda () (generate-stripped-directory 'binary "does not exist" pkg-dest-path)))

   (check-exn rx:does-not-exist (lambda () (generate-stripped-directory 'binary-lib "does not exist" pkg-dest-path)))

   (check-exn rx:does-not-exist (lambda () (generate-stripped-directory 'built "does not exist" pkg-dest-path)))

   ;; Giving path to nonexistent dest directory should raise a contract exception.
   (check-exn rx:does-not-exist (lambda () (generate-stripped-directory 'source pkg-path "does not exist")))

   (check-exn rx:does-not-exist (lambda () (generate-stripped-directory 'binary pkg-path "does not exist")))

   (check-exn rx:does-not-exist (lambda () (generate-stripped-directory 'binary-lib pkg-path "does not exist")))

   (check-exn rx:does-not-exist (lambda () (generate-stripped-directory 'built pkg-path "does not exist")))

   ;; Paths to existing src and dest directories should succeed.
   (check-not-exn (lambda () (generate-stripped-directory 'source pkg-path pkg-dest-path)))

   ))
