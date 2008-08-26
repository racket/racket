#lang scheme/base
(require (planet "test.ss" ("schematics" "schemeunit.plt" 2))
         mzlib/list
         net/url
         web-server/private/session)
(provide session-tests)

(define url0 (string->url "http://test.com/foo"))
(define url0ps (list "foo"))

(define session-tests
  (test-suite
   "Sessions"
   
   (test-case
    "new-session"
    (check-true (session? (new-session (make-custodian) (make-base-empty-namespace) url0 url0ps))))
   
   (test-case
    "lookup-session"
    (let ([ses (new-session (make-custodian) (make-base-empty-namespace) url0 url0ps)])
      (install-session ses url0ps)
      (check-eq? (lookup-session url0ps)
                 ses)))
   
   (test-case
    "lookup-session (fail)"
    (let ([ses (new-session (make-custodian) (make-base-empty-namespace) url0 url0ps)])
      (install-session ses url0ps)
      (check-false (lookup-session empty))))))
