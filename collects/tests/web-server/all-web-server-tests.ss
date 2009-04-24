#lang scheme/base
(require schemeunit
         "configuration/all-configuration-tests.ss"
         "dispatchers/all-dispatchers-tests.ss"           
         "lang/all-lang-tests.ss"
         "lang-test.ss"
         "managers/all-managers-tests.ss"
         "http/all-http-tests.ss"
         "private/all-private-tests.ss"
         "servlet/all-servlet-tests.ss"
         "stuffers-test.ss"
         "formlets-test.ss"
         "dispatch-test.ss"
         "servlet-env-test.ss")
(provide all-web-server-tests)

(define all-web-server-tests  
  (test-suite
   "Web Server"
   all-http-tests
   all-stuffers-tests
   all-formlets-tests
   all-dispatch-tests
   all-configuration-tests
   all-dispatchers-tests
   all-lang-tests
   lang-tests
   all-managers-tests
   all-private-tests
   all-servlet-tests
   servlet-env-tests))
