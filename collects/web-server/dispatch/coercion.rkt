#lang racket/base
(require racket/contract
         racket/match
         (for-syntax racket/base))

(define (make-coerce-safe? coerce)
  (lambda (x)
    (with-handlers ([exn:fail? (lambda (x) #f)])
      (and (coerce x) #t))))

(define-syntax (define-coercion-match-expander stx)
  (syntax-case stx ()
    [(_ expander-id test? coerce)
     (syntax/loc stx
       (define-match-expander expander-id
         (lambda (stx)
           (syntax-case stx ()
             [(_ id) (identifier? #'id)
                     (syntax/loc stx
                       (? test? (app coerce id)))]))))]))

(provide/contract
 [make-coerce-safe? ((any/c . -> . any/c) . -> . (any/c . -> . boolean?))])
(provide
 define-coercion-match-expander)
