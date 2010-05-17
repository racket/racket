#lang racket/base
(require rackunit
         "abort-resume-test.rkt"
         "anormal-test.rkt"
         "defun-test.rkt"
         "file-box-test.rkt"
         "labels-test.rkt"
         "stuff-url-test.rkt"
         "web-param-test.rkt")
(provide all-lang-tests)

(define all-lang-tests  
  (test-suite
   "Web Language"
   abort-resume-tests
   anormal-tests
   defun-tests
   file-box-tests
   labels-tests
   stuff-url-tests
   web-param-tests))
