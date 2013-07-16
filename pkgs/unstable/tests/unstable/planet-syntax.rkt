#lang racket

(require mzlib/etc
         rackunit
         rackunit/text-ui
         planet/syntax
         "helpers.rkt")

(define here
  (datum->syntax
   #f 'here
   (list (build-path (this-expression-source-directory)
                     (this-expression-file-name))
         1 1 1 1)))

(run-tests
 (test-suite "planet-syntax.rkt"

   (test-suite "syntax-source-planet-package"
     (test-case "fail"
       (check-equal? (syntax-source-planet-package (datum->syntax #f 'fail))
                     #f)))

   (test-suite "syntax-source-planet-package-owner"
     (test-case "fail"
       (check-equal? (syntax-source-planet-package-owner
                      (datum->syntax #f 'fail))
                     #f)))

   (test-suite "syntax-source-planet-package-name"
     (test-case "fail"
       (check-equal? (syntax-source-planet-package-name
                      (datum->syntax #f 'fail))
                     #f)))

   (test-suite "syntax-source-planet-package-major"
     (test-case "fail"
       (check-equal? (syntax-source-planet-package-major
                      (datum->syntax #f 'fail))
                     #f)))

   (test-suite "syntax-source-planet-package-minor"
     (test-case "fail"
       (check-equal? (syntax-source-planet-package-minor
                      (datum->syntax #f 'fail))
                     #f)))

   (test-suite "syntax-source-planet-package-symbol"
     (test-case "fail"
       (check-equal? (syntax-source-planet-package-minor
                      (datum->syntax #f 'fail))
                     #f)))

   (test-suite "make-planet-path")))
