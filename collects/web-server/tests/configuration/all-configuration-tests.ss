(module all-configuration-tests mzscheme
  (require (planet "test.ss" ("schematics" "schemeunit.plt" 2)))
  (provide all-configuration-tests)
  
  (define all-configuration-tests  
    (test-suite
     "Configuration")))