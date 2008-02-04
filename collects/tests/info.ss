#lang setup/infotab

(define name "Test Suites")
(define doc-subcollections '("tester"))
(define compile-subcollections '(("tests" "drscheme")
                                 ("tests" "framework")
                                 ("tests" "utils")))
(define tools '(("tool.ss" "drscheme")))
(define tool-names '("DrScheme Test Suites"))
