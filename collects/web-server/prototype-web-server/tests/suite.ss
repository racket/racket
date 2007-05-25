(module suite mzscheme
  (require (planet "graphical-ui.ss" ("schematics" "schemeunit.plt" 1))
           (planet "text-ui.ss" ("schematics" "schemeunit.plt" 1))
           (planet "test.ss" ("schematics" "schemeunit.plt" 1 1))
           "persistent-close-tests.ss"
           "anormal-test.ss"
           "closure-tests.ss"
           "labels-tests.ss"
           "lang-tests.ss"
           "certify-tests.ss"
           "stuff-url-tests.ss")
  
  (test/graphical-ui
   (make-test-suite
    "Main Tests for Prototype Web Server"
    persistent-close-suite
    stuff-url-suite
    anormal-tests
    closure-tests-suite
    labels-tests-suite
    lang-suite
    certify-suite
    )))