#lang racket/base
(require rackunit
         net/url
         xml/xml
         mzlib/contract
         web-server/private/util)
(provide util-tests)

(define util-tests
  (test-suite
   "Utilities"
   
   ; XXX path-element?
   
   (test-suite
    "port-number?"
    (test-not-exn "80" (lambda () (contract port-number? 80 'pos 'neg)))
    (test-not-exn "8080" (lambda () (contract port-number? 8080 'pos 'neg)))
    (test-exn "0" exn:fail:contract? (lambda () (contract port-number? 0 'pos 'neg)))
    (test-exn "10000000" exn:fail:contract? (lambda () (contract port-number? 10000000 'pos 'neg))))
   
   (test-suite
    "url-replace-path"
    (test-case
     "Identity"
     (check-equal? (url->string (url-replace-path (lambda (x) x) (string->url "http://test.com/foo/bar")))
                   "http://test.com/foo/bar"))
    (test-case
     "Remove"
     (check-equal? (url->string (url-replace-path (lambda (x) (list)) (string->url "http://test.com/foo/bar")))
                   "http://test.com")))
   
   (test-suite
    "explode-path*"
    (test-case
     "Simple"
     (check-equal? (explode-path* (build-path "foo" "bar"))
                   (list (build-path "foo") (build-path "bar")))))
   
   (test-suite
    "path-without-base"
    (test-case
     "Simple"
     (check-equal? (path-without-base (build-path "foo")
                                      (build-path "foo" "bar"))
                   (list (build-path "bar"))))
    (test-case
     "Exceptional case"
     (check-exn (lambda _ #t)
                (lambda () (path-without-base 
                            (build-path "foo" "bar")
                            (build-path "foo"))))))
   
   (test-suite
    "list-prefix?"
    (test-case
     "Simple"
     (check-true (list-prefix? '(a b c) '(a b c d))))
    (test-case
     "Not prefix"
     (check-false (list-prefix? '(a b) '(a))))
    (test-case
     "Not prefix"
     (check-false (list-prefix? '(a b c) '(b c d)))))
   
   (test-suite
    "url-path->string"
    (test-case
     "Simple (no param)"
     (check-equal? (url-path->string (url-path (string->url "http://test.com/foo/bar")))
                   "/foo/bar"))
    (test-case
     "Simple (param)"
     (check-equal? (url-path->string (url-path (string->url "http://test.com/foo/bar;zog")))
                   "/foo/bar")))
   
   (test-suite
    "network-error"
    (test-case
     "Simple"
     (check-exn exn:fail:network?
                (lambda () (network-error 'foo "Bar"))))
    (test-case
     "Simple (format succeeds)"
     (check-exn exn:fail:network?
                (lambda () (network-error 'foo "Bar ~a" 1)))))
   
   (test-suite
    "directory-part"
    (test-case
     "Absolute"
     (check-equal? (directory-part (build-path "/" "foo" "bar"))
                   (build-path "/" "foo/")))
    (test-case
     "Relative"
     (check-equal? (directory-part (build-path "foo"))
                   (current-directory)))
    (test-case
     "Error"
     (check-exn (lambda _ #t)
                (lambda () (directory-part (build-path "/"))))))
   
   (test-suite
    "lowercase-symbol!"
    (test-case
     "LC String"
     (check-eq? (lowercase-symbol! "foo")
                'foo))
    (test-case
     "LC Bytes"
     (check-eq? (lowercase-symbol! #"foo")
                'foo))
    (test-case
     "UC String"
     (check-eq? (lowercase-symbol! "FOO")
                'foo))
    (test-case
     "UC Bytes"
     (check-eq? (lowercase-symbol! #"FOO")
                'foo)))
   
   (test-suite
    "exn->string"
    (test-case
     "Exception"
     (check-pred string? (with-handlers ([exn? exn->string])
                           (error 'error "Yup"))))
    (test-case
     "Any"
     (check-equal? (exn->string "foo")
                   (format "~s\n" "foo"))))
   
   (test-suite
    "build-path-unless-absolute"
    (test-case
     "Relative PS"
     (check-equal? (build-path-unless-absolute "foo" "bar")
                   (build-path "foo" "bar")))
    (test-case
     "Absolute PS"
     (check-equal? (build-path-unless-absolute "foo" "/bar")
                   (build-path "/bar")))
    (test-case
     "Relative P"
     (check-equal? (build-path-unless-absolute (build-path "foo") (build-path "bar"))
                   (build-path "foo" "bar")))
    (test-case
     "Absolute P"
     (check-equal? (build-path-unless-absolute (build-path "foo") (build-path "/bar"))
                   (build-path "/bar"))))
   
   (test-suite
    "read/string & write/string"
    (test-case
     "Identity"
     (check-equal? (read/string (write/string (vector 1 2 3)))
                   (vector 1 2 3))))))
