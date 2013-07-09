#lang racket/base
(require "test-util.rkt")

(parameterize ([current-contract-namespace
                (make-basic-contract-namespace 'racket/contract
                                               )])
  
  (contract-eval
   '(begin
      (module blame-ok/c racket/base
        (require racket/contract)
        (define (blame-proj name)
          (lambda (b)
            (unless (blame? b)
              (raise-type-error name "a blame object" b))
            (define src (blame-source b))
            (unless (srcloc? src)
              (raise-type-error name "a srcloc" src))
            (lambda (x) x)))
        (define impersonator-blame-ok/c
          (make-contract
           #:name 'impersonator-blame-ok/c
           #:projection (blame-proj 'impersonator-blame-ok/c)))
        (define chaperone-blame-ok/c
          (make-chaperone-contract
           #:name 'chaperone-blame-ok/c
           #:projection (blame-proj 'chaperone-blame-ok/c)))
        (define flat-blame-ok/c
          (make-flat-contract
           #:name 'flat-blame-ok/c
           #:projection (blame-proj 'flat-blame-ok/c)))
        (provide
         impersonator-blame-ok/c
         chaperone-blame-ok/c
         flat-blame-ok/c))
      (require 'blame-ok/c)
      (module blame-ok-dynamic racket/base
        (require racket/contract 'blame-ok/c)
        (define five 5)
        (provide
         (contract-out
          [rename five impersonator-five impersonator-blame-ok/c]
          [rename five chaperone-five chaperone-blame-ok/c]
          [rename five flat-five flat-blame-ok/c])))))
  
  (begin
    (test/no-error
     '(dynamic-require ''blame-ok-dynamic 'impersonator-five))
    (test/no-error
     '(dynamic-require ''blame-ok-dynamic 'chaperone-five))
    (test/no-error
     '(dynamic-require ''blame-ok-dynamic 'flat-five)))
  
  (begin
    (test/no-error
     '(contract impersonator-blame-ok/c 5 'pos 'neg 'five #',#'location))
    (test/no-error
     '(contract chaperone-blame-ok/c 5 'pos 'neg 'five #',#'location))
    (test/no-error
     '(contract flat-blame-ok/c 5 'pos 'neg 'five #',#'location)))
  
  (begin
    (test/no-error
     '(let ()
        (define/contract five impersonator-blame-ok/c 5)
        five))
    (test/no-error
     '(let ()
        (define/contract five chaperone-blame-ok/c 5)
        five))
    (test/no-error
     '(let ()
        (define/contract five flat-blame-ok/c 5)
        five)))
  
  (begin
    (test/no-error
     '(let ()
        (with-contract internal-region ([five impersonator-blame-ok/c])
                       (define five 5))
        five))
    (test/no-error
     '(let ()
        (with-contract internal-region ([five chaperone-blame-ok/c])
                       (define five 5))
        five))
    (test/no-error
     '(let ()
        (with-contract internal-region ([five flat-blame-ok/c])
                       (define five 5))
        five)))
  
  (begin
    (test/no-error
     '(let ()
        (define five 5)
        (with-contract internal-region () #:freevar five impersonator-blame-ok/c
                       (define six (add1 five)))
        six))
    (test/no-error
     '(let ()
        (define five 5)
        (with-contract internal-region () #:freevar five chaperone-blame-ok/c
                       (define six (add1 five)))
        six))
    (test/no-error
     '(let ()
        (define five 5)
        (with-contract internal-region () #:freevar five flat-blame-ok/c
                       (define six (add1 five)))
        six)))
  
  (begin
    (test/no-error
     '(with-contract internal-region #:result impersonator-blame-ok/c
                     5))
    (test/no-error
     '(with-contract internal-region #:result chaperone-blame-ok/c
                     5))
    (test/no-error
     '(with-contract internal-region #:result flat-blame-ok/c
                     5)))
  
  (begin
    (test/no-error
     '(let ()
        (define five 5)
        (with-contract internal-region #:result any/c #:freevar five impersonator-blame-ok/c
                       five)))
    (test/no-error
     '(let ()
        (define five 5)
        (with-contract internal-region #:result any/c #:freevar five chaperone-blame-ok/c
                       five)))
    (test/no-error
     '(let ()
        (define five 5)
        (with-contract internal-region #:result any/c #:freevar five flat-blame-ok/c
                       five))))
  
  (begin
    (test/no-error
     '(let ()
        (define-struct/contract thing ([stuff impersonator-blame-ok/c]))
        (thing-stuff (thing 5))))
    (test/no-error
     '(let ()
        (define-struct/contract thing ([stuff chaperone-blame-ok/c]))
        (thing-stuff (thing 5))))
    (test/no-error
     '(let ()
        (define-struct/contract thing ([stuff flat-blame-ok/c]))
        (thing-stuff (thing 5))))))
