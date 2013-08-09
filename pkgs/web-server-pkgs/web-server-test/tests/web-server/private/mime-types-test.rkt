#lang racket/base
(require rackunit
         (only-in mzlib/file make-temporary-file)
         racket/runtime-path
         racket/path
         web-server/http
         web-server/private/mime-types)
(provide mime-types-tests)

(define test-file (make-temporary-file))
(with-output-to-file test-file
  (lambda ()
    (printf #<<END
video/mp4                       mp4
video/mpeg                      mpeg mpg mpe
END
            ))
  #:exists 'replace)  

(define default-web-root
  (path-only 
   (collection-file-path "default-web-root/configuration-table.rkt" "web-server")))

(define mime-types-tests
  (test-suite
   "MIME Types"
   
   (test-case
    "Distribution mime.types parses"
    (check-not-false (read-mime-types (build-path default-web-root "mime.types"))))
   
   (test-case
    "Test file parses"
    (check-not-false (read-mime-types test-file)))
   (test-case
    "Default mime-type given"
    (check-equal? ((make-path->mime-type test-file) (build-path "test.html")) #f))
   (test-case
    "MIME type resolves (single in file)"
    (check-equal? ((make-path->mime-type test-file) (build-path "test.mp4")) #"video/mp4"))
   (test-case
    "MIME type resolves (multiple in file)"
    (check-equal? ((make-path->mime-type test-file) (build-path "test.mpeg")) #"video/mpeg"))
   (test-case
    "MIME type resolves (multiple in file)"
    (check-equal? ((make-path->mime-type test-file) (build-path "test.mpg")) #"video/mpeg"))
   (test-case
    "MIME type resolves (multiple in file)"
    (check-equal? ((make-path->mime-type test-file) (build-path "test.mpe")) #"video/mpeg"))))
