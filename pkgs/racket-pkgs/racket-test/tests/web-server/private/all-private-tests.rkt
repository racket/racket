#lang racket/base
(require rackunit
         "request-test.rkt"
         "cache-table-test.rkt"
         "response-test.rkt"
         "connection-manager-test.rkt"
         "define-closure-test.rkt"
         "mime-types-test.rkt"
         "url-param-test.rkt"
         "mod-map-test.rkt"
         "gzip-test.rkt"
         "util-test.rkt")
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
