#lang racket/base

(require rackunit
         syntax/macro-testing)

(check-= (#%namespaced racket/math pi) 3.14159 0.00001)
(check-equal? (#%namespaced racket/list (take '(a b c) 2)) '(a b))
(check-equal? (#%namespaced racket/match (match '(a b c)
                                           [(list _ x _) x]))
              'b)

(check-exn #rx"^quote: unbound identifier"
           (λ () (convert-syntax-error
                  (#%namespaced racket/list '(1 2 3)))))
