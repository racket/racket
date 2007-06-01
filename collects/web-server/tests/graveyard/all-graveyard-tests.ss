(module all-graveyard-tests mzscheme
  (require (planet "test.ss" ("schematics" "schemeunit.plt" 2))
           "persistent-close-test.ss")
  (provide all-graveyard-tests)
  
  (define all-graveyard-tests
    (test-suite
     "Graveyard"
     persistent-close-tests)))