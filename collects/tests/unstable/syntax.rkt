#lang racket

(require mzlib/etc
         rackunit
         rackunit/text-ui
         racket/syntax
         unstable/syntax
         "helpers.rkt")

(define here
  (datum->syntax
   #f 'here
   (list (build-path (this-expression-source-directory)
                     (this-expression-file-name))
         1 1 1 1)))

(run-tests
 (test-suite "syntax.ss"

   (test-suite "Syntax Lists"

     (test-suite "syntax-list"
       (test
        (check-equal?
         (with-syntax ([([x ...] ...) #'([1 2] [3] [4 5 6])])
           (map syntax->datum (syntax-list x ... ...)))
         (list 1 2 3 4 5 6))))

     (test-suite "syntax-map"
       (test-case "identifiers to symbols"
         (check-equal? (syntax-map syntax-e #'(a b c)) '(a b c)))))

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
                       #f))))

   (test-suite "Pattern Bindings"

     (test-suite "with-syntax*"
       (test-case "identifier"
         (check bound-identifier=?
                (with-syntax* ([a #'id] [b #'a]) #'b)
                #'id))))))
