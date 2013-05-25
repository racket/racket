#lang racket

(require "TL-semantics.rkt"
         "common.rkt"
         redex)

(test-TL-result
 ∅
 ((λ (x) x) (λ (x) x))
 (λ (x) x))

(test-TL-stuck
 ∅
 (call/cc (λ (x) x)))

(test-predicate
 (curry regexp-match? #rx"not defined")
 (with-handlers ([exn:fail? exn-message])
   (apply-reduction-relation
    -->TL
    (term (∅ / ((κ hole) (λ (x) x)))))))
