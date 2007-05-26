(module suite mzscheme
  (require (planet "graphical-ui.ss" ("schematics" "schemeunit.plt" 2))
           (planet "text-ui.ss" ("schematics" "schemeunit.plt" 2))
           (planet "test.ss" ("schematics" "schemeunit.plt" 2))
           "persistent-close-tests.ss"
           "anormal-test.ss"
           "closure-tests.ss"
           "labels-tests.ss"
           "lang-tests.ss"
           "certify-tests.ss"
           "stuff-url-tests.ss"
           "param-tests.ss")
  
  (test/graphical-ui
   (test-suite
    "Main Tests for Prototype Web Server"
    persistent-close-suite
    stuff-url-suite
    anormal-tests
    closure-tests-suite
    labels-tests-suite
    lang-suite
    certify-suite
    param-suite
    )))