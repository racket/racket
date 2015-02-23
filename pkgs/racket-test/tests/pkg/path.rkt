#lang racket/base
(require pkg/path
         setup/dirs)

(module+ test
  (require rackunit)
  
  (check-equal? (path->pkg (collection-file-path "path.rkt" "tests" "pkg"))
                "racket-test")
  (check-equal? (call-with-values
                    (lambda () (path->pkg+subpath (collection-file-path "path.rkt" "tests" "pkg")))
                  list)
                (list "racket-test" (build-path "tests" "pkg" "path.rkt")))

  ;; We don't know the scope where these tests are installed, but we want to
  ;; at least call the `...+scope` variants:
  (define-values (racket-test-pkg racket-test-subpath scope)
    (path->pkg+subpath+scope (collection-file-path "path.rkt" "tests" "pkg")))

  (check-equal? (call-with-values
                    (lambda () (path->pkg+subpath+scope (collection-file-path "path.rkt" "tests" "pkg")))
                  list)
                (list "racket-test" (build-path "tests" "pkg" "path.rkt") scope))
  (check-equal? (call-with-values
                    (lambda () (path->pkg+subpath+collect (collection-file-path "path.rkt" "tests" "pkg")))
                  list)
                (list "racket-test" (build-path "tests" "pkg" "path.rkt") #f))
  (check-equal? (call-with-values
                    (lambda () (path->pkg+subpath+collect+scope (collection-file-path "path.rkt" "tests" "pkg")))
                  list)
                (list "racket-test" (build-path "tests" "pkg" "path.rkt") #f scope))

  (check-equal? (path->pkg (find-system-path 'temp-dir))
                #f)

  (check-equal? (path->pkg (build-path (find-pkgs-dir) "pkgs.rktd"))
                #f)
  (check-equal? (path->pkg (find-pkgs-dir))
                #f)

  (check-equal? (call-with-values
                    (lambda () (path->pkg+subpath+collect (collection-file-path "info.rkt" "icons")))
                  list)
                (list "icons" (build-path "info.rkt") "icons")))
