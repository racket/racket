#lang racket/base
(require schemeunit
         (only-in mzlib/file make-temporary-file)
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

(define mime-types-tests
  (test-suite
   "MIME Types"
   
   (test-case
    "Distribution mime.types parses"
    (check-not-false (read-mime-types (build-path (collection-path "web-server") "default-web-root" "mime.types"))))
   
   (test-case
    "Test file parses"
    (check-not-false (read-mime-types test-file)))
   (test-case
    "Default mime-type given"
    (check-equal? ((make-path->mime-type test-file) (build-path "test.html")) TEXT/HTML-MIME-TYPE))
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
