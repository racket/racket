#lang racket/base
(require "test-util.rkt")

(parameterize ([current-contract-namespace
                (make-basic-contract-namespace 'racket/contract
                                               'racket/contract/private/blame)])

  (test/spec-passed/result
   'blame-selector.1
   '(blame-positive (make-blame (srcloc "src.rkt" #f #f #f #f)
                                'whatever (λ () 'the-name) 'pos 'neg #t))
   'pos)
  (test/spec-passed/result
   'blame-selector.2
   '(blame-negative (make-blame (srcloc "src.rkt" #f #f #f #f)
                                'whatever (λ () 'the-name) 'pos 'neg #t))
   'neg)
  (test/spec-passed/result
   'blame-selector.3
   '(blame-positive
     (blame-swap
      (make-blame (srcloc "src.rkt" #f #f #f #f)
                  'whatever (λ () 'the-name) 'pos 'neg #t)))
   'neg)
  (test/spec-passed/result
   'blame-selector.4
   '(blame-original?
     (make-blame (srcloc "src.rkt" #f #f #f #f)
                 'whatever (λ () 'the-name) 'pos 'neg #t))
   #t)
  (test/spec-passed/result
   'blame-selector.5
   '(blame-original?
     (blame-swap
      (make-blame (srcloc "src.rkt" #f #f #f #f)
                  'whatever (λ () 'the-name) 'pos 'neg #t)))
   #f)
  (test/spec-passed/result
   'blame-selector.6
   '(blame-negative
     (blame-replace-negative
      (make-blame (srcloc "src.rkt" #f #f #f #f)
                  'whatever (λ () 'the-name) 'pos 'neg #t)
      'neg2))
   'neg2)
  (test/spec-passed/result
   'blame-selector.7
   '(blame-positive
     (blame-swap
      (blame-replace-negative
       (make-blame (srcloc "src.rkt" #f #f #f #f)
                   'whatever (λ () 'the-name) 'pos 'neg #t)
       'neg2)))
   'neg2)
  (test/spec-passed/result
   'blame-selector.8
   '(blame-positive
     (make-blame (srcloc "src.rkt" #f #f #f #f)
                 'whatever (λ () 'the-name) 'pos #f #t))
   'pos)
  (test/spec-passed/result
   'blame-selector.9
   '(blame-positive
     (blame-add-missing-party
      (make-blame (srcloc "src.rkt" #f #f #f #f)
                  'whatever (λ () 'the-name) 'pos #f #t)
      'neg))
   'pos)
  (test/spec-passed/result
   'blame-selector.10
   '(blame-negative
     (blame-add-missing-party
      (make-blame (srcloc "src.rkt" #f #f #f #f)
                  'whatever (λ () 'the-name) 'pos #f #t)
      'neg))
   'neg)
  (test/spec-passed/result
   'blame-selector.11
   '(blame-negative
     (blame-add-missing-party
      (blame-swap
       (make-blame (srcloc "src.rkt" #f #f #f #f)
                   'whatever (λ () 'the-name) 'pos #f #t))
      'pos))
   'pos)
  (test/spec-passed/result
   'blame-selector.12
   '(blame-positive
     (blame-add-missing-party
      (blame-swap
       (make-blame (srcloc "src.rkt" #f #f #f #f)
                   'whatever (λ () 'the-name) 'pos #f #t))
      'neg))
   'neg)
  (test/spec-passed/result
   'blame-selector.13
   '(blame-negative
     (blame-add-missing-party
      (blame-replace-negative
       (make-blame (srcloc "src.rkt" #f #f #f #f)
                   'whatever (λ () 'the-name) 'pos #f #t)
       'neg2)
      'neg))
   'neg2)
  (test/spec-passed/result
   'blame-selector.14
   '(blame-positive
     (blame-add-missing-party
      (blame-swap
       (blame-replace-negative
        (make-blame (srcloc "src.rkt" #f #f #f #f)
                    'whatever (λ () 'the-name) 'pos #f #t)
        'neg2))
      'neg))
   'neg2)
  (test/spec-passed/result
   'blame-selector.15
   '(with-handlers ([exn:fail?
                     (λ (x) (regexp-match? #rx"^blame-add-missing-party:"
                                           (exn-message x)))])
      (blame-add-missing-party
       (blame-add-missing-party
        (make-blame (srcloc "src.rkt" #f #f #f #f)
                    'whatever (λ () 'the-name) 'pos #f #t)
        'neg)
       'neg2)
      'no-exn-raised)
   #t)

  (contract-eval
   #:test-case-name "blame.rkt setup.1"
   '(module blame-ok/c racket/base
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
       flat-blame-ok/c)))

  (contract-eval
   #:test-case-name "blame.rkt setup.2"
   '(require 'blame-ok/c))
  
  (contract-eval
   #:test-case-name "blame.rkt setup.3"
   '(module blame-ok-dynamic racket/base
      (require racket/contract 'blame-ok/c)
      (define five 5)
      (provide
       (contract-out
        [rename five impersonator-five impersonator-blame-ok/c]
        [rename five chaperone-five chaperone-blame-ok/c]
        [rename five flat-five flat-blame-ok/c]))))
  
  (test/no-error
   '(contract string? "string" #f 'neg))
  
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
        (thing-stuff (thing 5)))))

  (test/spec-passed/result
   'suggest/c1
   '(with-handlers ([exn:fail?
                     (λ (x)
                       (define m (regexp-match #rx"suggestion:[^\n]*\n"
                                               (exn-message x)))
                       (and m (car m)))])
      (contract (suggest/c zero? "suggestion" "try zero?")
                1
                'pos 'neg))
   "suggestion: try zero?\n")

  )
