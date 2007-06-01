(module all-web-server-tests mzscheme
  (require (planet "graphical-ui.ss" ("schematics" "schemeunit.plt" 2))
           (planet "text-ui.ss" ("schematics" "schemeunit.plt" 2))
           (planet "test.ss" ("schematics" "schemeunit.plt" 2))
           "./graveyard/persistent-close-test.ss"
           "./lang/anormal-test.ss"
           "./lang/labels-test.ss"
           "./lang/stuff-url-test.ss"
           "./lang/web-param-test.ss"
           "./lang-test.ss"
           "./private/define-closure-test.ss"
           "./private/request-test.ss"
           "./servlet-env-test.ss")
  
  (test/graphical-ui
   (test-suite
    "Web Server"
    persistent-close-tests
    anormal-tests
    labels-tests
    stuff-url-tests
    web-param-tests
    lang-tests
    define-closure-tests
    request-tests)))