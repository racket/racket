(module persistent-close-test mzscheme
  (require (planet "test.ss" ("schematics" "schemeunit.plt" 2))
           (lib "persistent-close.ss" "web-server" "graveyard"))  
  (provide persistent-close-tests)
  
  (define persistent-close-tests
    (test-suite
     "Persistent Closures"
          
     (test-case
      "close/file test"
      (let ([x 7] [y 8])
        (check = 7 (close/file 'f1 (x y) x))
        (check = 15 (close/file 'f2 (x y) (+ x y))))))))
