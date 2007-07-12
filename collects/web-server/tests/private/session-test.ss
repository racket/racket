(module session-test mzscheme
  (require (planet "test.ss" ("schematics" "schemeunit.plt" 2))
           (lib "list.ss")
           (lib "url.ss" "net")
           (lib "session.ss" "web-server" "private"))
  (provide session-tests)
  
  (define url0 (string->url "http://test.com/foo"))
  (define url0ps (list "foo"))
  
  (define session-tests
    (test-suite
     "Sessions"
     
     (test-case
      "new-session"
      (check-true (session? (new-session (make-custodian) (make-namespace) url0 url0ps))))
     
     (test-case
      "lookup-session"
      (let ([ses (new-session (make-custodian) (make-namespace) url0 url0ps)])
        (install-session ses url0ps)
        (check-eq? (lookup-session url0ps)
                   ses)))
     
     (test-case
      "lookup-session (fail)"
      (let ([ses (new-session (make-custodian) (make-namespace) url0 url0ps)])
        (install-session ses url0ps)
        (check-false (lookup-session empty)))))))