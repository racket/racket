#lang racket/base

(require rackunit rackunit/text-ui
         mzlib/etc
         syntax/location)

(define here
  (datum->syntax
   #f 'here
   (list (build-path (this-expression-source-directory)
                     (this-expression-file-name))
         1 1 1 1)))

(run-tests
 (test-suite "Syntax Source Locations"
   (test-suite "syntax-source-file-name"
     (test-case "here"
       (check-equal? (syntax-source-file-name here)
                     (this-expression-file-name)))
     (test-case "fail"
       (check-equal? (syntax-source-file-name (datum->syntax #f 'fail))
                     #f)))

   (test-suite "syntax-source-directory"
     (test-case "here"
       (check-equal? (syntax-source-directory here)
                     (this-expression-source-directory)))
     (test-case "fail"
       (check-equal? (syntax-source-directory (datum->syntax #f 'fail))
                     #f)))))
