#lang racket

(require redex/reduction-semantics
         redex/private/gen-trace
         redex/private/search)

(define-language L
  (v a
     b
     c))

(define-metafunction L
  [(is-a? a)
   T]
  [(is-a? v)
   F])

(enable-gen-trace!)
(test-equal (generate-term L #:satisfying
                           (is-a? b) any
                           +inf.0)
            '((is-a? b) = F))
