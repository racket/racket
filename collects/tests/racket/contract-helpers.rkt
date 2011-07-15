#lang racket
(require rackunit
         racket/contract/private/arrow)
(check-equal? (matches-arity-exactly? (λ () 1) 0 0 '() '()) #t)
(check-equal? (matches-arity-exactly? (λ () 1) 1 1 '() '()) #f)
(check-equal? (matches-arity-exactly? (λ () 1) 0 1 '() '()) #f)
(check-equal? (matches-arity-exactly? (λ () 1) 0 #f '() '()) #f)
(check-equal? (matches-arity-exactly? (λ (x y) x) 2 2 '() '()) #t)
(check-equal? (matches-arity-exactly? (λ (x y) x) 1 1 '() '()) #f)
(check-equal? (matches-arity-exactly? (λ (x y) x) 2 3 '() '()) #f)
(check-equal? (matches-arity-exactly? (λ (x y) x) 3 #f '() '()) #f)

(check-equal? (matches-arity-exactly? (case-lambda
                                  [() 1]
                                  [(x) 2])
                                0 1 '() '()) #t)
(check-equal? (matches-arity-exactly? (case-lambda
                                  [() 1]
                                  [(x) 2])
                                0 2 '() '()) #f)
(check-equal? (matches-arity-exactly? (case-lambda
                                  [() 1]
                                  [(x y) 2])
                                0 2 '() '()) #f)
(check-equal? (matches-arity-exactly? (case-lambda
                                  [() 1]
                                  [(x y) 2])
                                0 1 '() '()) #f)
(check-equal? (matches-arity-exactly? (case-lambda
                                  [() 1]
                                  [(x y) 2])
                                0 #f '() '()) #f)

(check-equal? (matches-arity-exactly? (lambda (x . y) x)
                                1 #f '() '()) #t)
(check-equal? (matches-arity-exactly? (lambda (x . y) x)
                                0 #f '() '()) #f)
