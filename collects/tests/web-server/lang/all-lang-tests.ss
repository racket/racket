#lang scheme/base
(require (planet schematics/schemeunit:3)
         "abort-resume-test.ss"
         "anormal-test.ss"
         "defun-test.ss"
         "file-box-test.ss"
         "labels-test.ss"
         "stuff-url-test.ss"
         "web-param-test.ss")
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
