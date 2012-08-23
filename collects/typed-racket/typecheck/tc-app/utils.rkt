#lang racket/base

(require syntax/parse
         syntax/parse/experimental/reflect
         (for-syntax racket/base syntax/parse))

(provide define-reified-syntax-class define-tc/app-syntax-class)

(define-syntax-rule (define-reified-syntax-class (id . args) . body)
  (define id
    (let ()
      (define-syntax-class (id . args) . body)
      (reify-syntax-class id))))

(define-syntax define-tc/app-syntax-class
  (syntax-parser
    #:literals (pattern)
    ((_ (id expected) (~and parse-option (~not (pattern . args))) ...
        (pattern syntax-pattern pattern-directive ... body) ...+)
     #'(define-reified-syntax-class (id expected)
        #:attributes (check) parse-option ...
        (pattern syntax-pattern pattern-directive ...
                 #:attr check (lambda () body)) ...))))

