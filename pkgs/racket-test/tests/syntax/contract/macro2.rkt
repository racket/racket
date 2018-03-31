#lang racket/base
(provide go)

(module defmac racket/base
  (require (for-syntax racket/base syntax/contract))
  (provide m)

  (define-syntax (m stx)
    (syntax-case stx ()
      [(_ e1)
       (with-syntax ([c1 (wrap-expr/c #'string? #'e1
                                      #:context stx
                                      #:positive 'use-site
                                      #:negative 'from-macro)])
         #'(string-length c1))])))
(require 'defmac)

(define (go)
  (m 'not-a-string))

(module+ main
  (m 'also-not-a-string))

(module+ test
  (void))
