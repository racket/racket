(module session-test mzscheme
  (require (planet "test.ss" ("schematics" "schemeunit.plt" 2))
           (lib "url.ss" "net")
           (lib "session.ss" "web-server" "private"))
  (provide session-tests)
  
  (define url0 (string->url "http://test.com/foo"))
  
  (define session-tests
    (test-suite
     "Sessions"
     
     (test-case
      "new-session"
      (check-true (session? (new-session (make-custodian) (make-namespace) url0))))
     
     (test-case
      "lookup-session"
      (let ([ses (new-session (make-custodian) (make-namespace) url0)])
        (check-eq? (lookup-session (session-id ses))
                   ses)))
     
     (test-case
      "lookup-session (fail)"
      (let ([ses (new-session (make-custodian) (make-namespace) url0)])
        (check-false (lookup-session (* 100 (session-id ses)))
                     ses)))
     
     (test-case
      "extract-session"
      (let ([ses (new-session (make-custodian) (make-namespace) url0)])
        (check-equal? (extract-session (session-url ses))
                      (session-id ses))))
     
     (test-case
      "extract-session (fail)"
        (check-false (extract-session url0))))))