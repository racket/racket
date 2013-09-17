#lang scheme/base

(require "test-utils.rkt"
         (for-syntax scheme/base)
         (for-template scheme/base)
         (private type-contract)
         (types abbrev numeric-tower)
         rackunit)

(define-syntax-rule (t e)
  (test-not-exn
   (format "~a" e)
   (λ ()
     (type->contract
      e
      (λ (#:reason [reason #f])
        (error "type could not be converted to contract"))))))

(define-syntax-rule (t/fail e)
  (test-not-exn
   (format "~a" e)
   (λ ()
     (let/ec exit
       (type->contract e (λ (#:reason [reason #f]) (exit #t)))
       (error "type could be converted to contract")))))

(define (contract-tests)
  (test-suite "Contract Tests"
              (t (-Number . -> . -Number))
              (t (-Promise -Number))
              (t (-set Univ)) 
              ;; Adapted from PR 13815
              (t (-poly (a) (-> a a)))
              (t (-poly (a) (-mu X (-> a X))))
              (t (-poly (a) (-poly (b) (-> a a))))
              (t (-poly (a) (-App (-poly (b) (-> a a)) (list -Number) #'#f)))
              (t/fail (-poly (a) -Flonum))
              (t/fail (-poly (a) (-set -Number)))))

(define-go contract-tests)
(provide contract-tests)
