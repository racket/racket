#lang racket

(require mzlib/etc
         rackunit
         rackunit/text-ui
         racket/syntax
         (for-syntax unstable/syntax)
         unstable/syntax
         "helpers.rkt")

(define here
  (datum->syntax
   #f 'here
   (list (build-path (this-expression-source-directory)
                     (this-expression-file-name))
         1 1 1 1)))

(run-tests
 (test-suite "syntax.rkt"

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
                #'id))))

   (let ()
     (define-syntax a #'a)
     (define-syntax b #'b)
     (define-syntax c #'(a b c))
     (define-syntax c1 (car (syntax->list (syntax-local-value #'c))))
     (define-syntax c2 (cadr (syntax->list (syntax-local-value #'c))))
     (define-syntax (*syntax-within? stx)
       (syntax-case stx ()
         [(_ x y)
          #`#,(syntax-within? (syntax-local-value #'x)
                              (syntax-local-value #'y))]))
     (test-suite "syntax-within?"
                 (test-case "reflexive"
                            (check-equal? (*syntax-within? a a) #t))
                 (test-case "unrelated"
                            (check-equal? (*syntax-within? a b) #f))
                 (test-case "child"
                            (check-equal? (*syntax-within? c1 c) #t))
                 (test-case "parent"
                            (check-equal? (*syntax-within? c c1) #f))
                 (test-case "sibling"
                            (check-equal? (*syntax-within? c2 c1) #f))))))
