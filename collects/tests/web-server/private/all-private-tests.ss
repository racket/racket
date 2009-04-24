#lang scheme/base
(require schemeunit
         "request-test.ss"
         "cache-table-test.ss"
         "response-test.ss"
         "connection-manager-test.ss"
         "define-closure-test.ss"
         "mime-types-test.ss"
         "url-param-test.ss"
         "mod-map-test.ss"
         "gzip-test.ss"
         "util-test.ss")
(provide all-private-tests)

(define all-private-tests
  (test-suite
   "Internal"
   gzip-tests
   cache-table-tests
   connection-manager-tests
   define-closure-tests
   mime-types-tests
   mod-map-tests
   request-tests
   response-tests   
   url-param-tests
   util-tests))
