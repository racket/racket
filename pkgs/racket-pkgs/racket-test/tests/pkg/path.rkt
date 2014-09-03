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
  (check-equal? (call-with-values
                    (lambda () (path->pkg+subpath+collect (collection-file-path "path.rkt" "tests" "pkg")))
                  list)
                (list "racket-test" (build-path "tests" "pkg" "path.rkt") #f))

  (check-equal? (path->pkg (find-system-path 'temp-dir))
                #f)

  (check-equal? (path->pkg (build-path (find-pkgs-dir) "pkgs.rktd"))
                #f)
  (check-equal? (path->pkg (find-pkgs-dir))
                #f)

  (check-equal? (call-with-values
                    (lambda () (path->pkg+subpath+collect (collection-file-path "serve-catalog.rkt" "distro-build")))
                  list)
                (list "distro-build-server" (build-path "serve-catalog.rkt") "distro-build")))
