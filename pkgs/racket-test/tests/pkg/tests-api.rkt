#lang racket/base
(require rackunit
         racket/file
         pkg/strip
         pkg/lib
         setup/getinfo
         "util.rkt")

(this-test-is-run-by-the-main-test)

(pkg-tests

 (parameterize ([current-directory test-source-directory])
   (define tmp-dir (path->directory-path (make-temporary-file "tmp~a" 'directory)))
   (define pkg-path (build-path "test-pkgs" "pkg-strip"))
   (define pkg-dest-path (build-path tmp-dir "pkg-strip"))
   (define pkg-dest-path-for-built (build-path tmp-dir "pkg-strip-built"))
   (make-directory pkg-dest-path)
   (make-directory pkg-dest-path-for-built)

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

   ;; check that stripping from built to source correctly changes pkg type
   (check-equal?
    (begin
      (generate-stripped-directory 'source pkg-path pkg-dest-path-for-built)
      (generate-stripped-directory 'built pkg-dest-path-for-built pkg-dest-path-for-built)
      (generate-stripped-directory 'source pkg-dest-path-for-built pkg-dest-path-for-built)            
      ((get-info/full pkg-dest-path-for-built)
       'package-content-state
       (lambda () 'no-package-content-state)))
    'no-package-content-state)

   (check-equal? (call-in-pkg-timeout-sandbox (lambda () 10)) 10)
   (check-equal? (call-with-values (lambda () (call-in-pkg-timeout-sandbox (lambda () (values 1 2))))
                                   list)
                 (list 1 2))
   (check-exn #rx"something wrong"
              (lambda () (call-in-pkg-timeout-sandbox (lambda () (error "something wrong")))))
   (check-exn #rx"timeout"
              (lambda ()
                (parameterize ([current-pkg-network-timeout 1])
                  (call-in-pkg-timeout-sandbox (lambda () (sleep 100))))))
   (check-exn #rx"alt exn"
              (lambda ()
                (parameterize ([current-pkg-network-timeout 1])
                  (call-in-pkg-timeout-sandbox (lambda () (sleep 100))
                                               #:make-exn (lambda (msg cm) (exn:fail "alt exn" cm))))))

   ))
