(module all-web-server-tests mzscheme
  (require (planet "test.ss" ("schematics" "schemeunit.plt" 2))
           "configuration/all-configuration-tests.ss"
           "dispatchers/all-dispatchers-tests.ss"           
           "lang/all-lang-tests.ss"
           "lang-test.ss"
           "managers/all-managers-tests.ss"
           "private/all-private-tests.ss"
           "servlet/all-servlet-tests.ss"
           "servlet-env-test.ss")
  (provide all-web-server-tests)
  
  (define all-web-server-tests  
    (test-suite
     "Web Server"
     all-configuration-tests
     all-dispatchers-tests
     all-lang-tests
     lang-tests
     all-managers-tests
     all-private-tests
     all-servlet-tests
     servlet-env-tests)))