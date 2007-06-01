(module dispatch-passwords-test mzscheme
  (require (planet "test.ss" ("schematics" "schemeunit.plt" 2)))
  (provide dispatch-passwords-tests)
  
  (define dispatch-passwords-tests
    (test-suite
     "Passwords")))