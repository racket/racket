#lang racket/base
(require rackunit
         net/url
         web-server/private/url-param)
(provide url-param-tests)

(define url0 (string->url "http://www.test.com/somewhere"))

(define url-param-tests
  (test-suite
   "URL Parameters"
   
   (test-case 
    "Insert and extract is identity"
    (check-equal? (extract-param (insert-param url0 "key" "val0") "key")
                  "val0"))
   
   (test-case 
    "Insert and extract is identity after multiple"
    (check-equal? (extract-param (insert-param 
                                  (insert-param url0 "key" "val0")
                                  "key" "val1")
                                 "key")
                  "val1"))
   
   (test-case 
    "Insert and extract is identity after multiple different"
    (check-equal? (extract-param (insert-param 
                                  (insert-param url0 "key0" "val0")
                                  "key1" "val1")
                                 "key0")
                  "val0"))))
