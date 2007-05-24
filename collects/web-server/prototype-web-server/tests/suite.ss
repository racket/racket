(module suite mzscheme
  (require (planet "graphical-ui.ss" ("schematics" "schemeunit.plt" 1))
           (planet "test.ss" ("schematics" "schemeunit.plt" 1 1))
           "persistent-close-tests.ss"
           "test-normalizer.ss"
           "closure-tests.ss"
           "labels-tests.ss"
           "persistent-interaction-tests.ss"
           "stuff-url-tests.ss")
  
  (test/graphical-ui
   (make-test-suite
    "Main Tests for Prototype Web Server"
    persistent-close-suite
    stuff-url-suite
    test-normalizer-suite
    closure-tests-suite
    labels-tests-suite
    persistent-interaction-suite
    )))