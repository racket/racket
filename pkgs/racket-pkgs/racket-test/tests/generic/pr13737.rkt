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

  (define-values (prop:foo foo? foo-accessor)
    (make-struct-type-property
     'foo
     #f))

  (define-generics (foo gen:foo prop:foo foo?
                        #:defined-table dummy
                        #:defaults ([number? (define (meth foo #:kw kw) kw)])
                        #:prop-defined-already? foo-accessor
                        #:define-contract #f)
    (meth foo #:kw kw))

  (check-equal? (meth 3 #:kw 5) 5))

