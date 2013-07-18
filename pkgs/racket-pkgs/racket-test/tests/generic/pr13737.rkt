#lang racket

(require racket/generic
         rackunit)

;; This tests PR 13737 (keyword arguments and #:defaults did
;; not work together)

(define-generics thing
  (foo thing #:stuff other)
  #:defaults
  {[number?
    (define (foo thing #:stuff other) (+ thing other))]})

(check-equal? (foo 1 #:stuff 2) 3)

;; This tests that the keyword & defaults issue doesn't occur for
;; forged generics either

(let ()
  (local-require racket/private/generic)

  (define-primitive-generics
    (foo gen:foo prop:foo foo-methods foo? foo-supports?)
    #:fast-defaults ([number? number? (define (meth foo #:kw kw) kw)])
    #:defaults ()
    #:fallbacks ()
    #:derive-properties ()
    (meth foo #:kw kw))

  (check-equal? (meth 3 #:kw 5) 5))

