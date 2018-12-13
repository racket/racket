#lang racket/base
(require "test-util.rkt")

(parameterize ([current-contract-namespace
                (make-basic-contract-namespace 'racket/contract
                                               'racket/contract/private/blame
                                               'syntax/srcloc)])

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
  (test/spec-passed/result
   'blame-selector.16
   '(blame-context
     (blame-add-context
      (blame-add-context
       (blame-add-context
        (blame-add-context
         (blame-add-context
          (make-blame (srcloc "src.rkt" #f #f #f #f)
                      'whatever (λ () 'the-name) 'pos #f #t)
          "1")
         "2")
        "3")
       "4")
      "5"))
   '("5" "4" "3" "2" "1"))
  (test/spec-passed/result
   'blame-selector.17
   '(blame-context
     (blame-add-context
      (blame-add-context
       (blame-add-context
        (blame-add-context
         (blame-add-context
          (make-blame (srcloc "src.rkt" #f #f #f #f)
                      'whatever (λ () 'the-name) 'pos 'neg #t
                      #:context-limit 2)
          "1")
         "2")
        "3")
       "4")
      "5"))
   '("5" "4"))
  (test/spec-passed/result
   'blame-selector.18
   '(blame-positive
     (blame-add-context
      (blame-add-context
       (blame-add-context
        (blame-add-context
         (blame-add-context
          (blame-swap
           (make-blame (srcloc "src.rkt" #f #f #f #f)
                       'whatever (λ () 'the-name) 'pos 'neg #t
                       #:context-limit 2))
          "1")
         "2")
        "3")
       "4")
      "5"))
   'neg)

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

  (contract-eval '(define (has-complete-blame? v)
                    (and (value-blame v)
                         (not (blame-missing-party? (value-blame v))))))
  (contract-eval '(require racket/class))
  (test/spec-passed/result
   'complete-prop-blame1
   '(has-complete-blame? (contract (-> integer? integer?) add1 'pos 'neg))
   #t)
  (test/spec-passed/result
   'complete-prop-blame2
   '(has-complete-blame? (contract (-> integer? any) add1 'pos 'neg))
   #t)
  (test/spec-passed/result
   'complete-prop-blame3
   '(has-complete-blame? (contract (->m integer?) add1 'pos 'neg))
   #t)
  (test/spec-passed/result
   'complete-prop-blame4
   '(has-complete-blame? (contract (->i ([x integer?]) [res integer?]) add1 'pos 'neg))
   #t)
  (test/spec-passed/result
   'complete-prop-blame5
   '(has-complete-blame? (contract (vectorof integer?) (vector 1 2 3) 'pos 'neg))
   #t)

  (test/spec-passed/result
   'complete-prop-blame-vector/c
   '(let* ([ctc (vector/c (-> integer? integer?))]
           [v (contract
               ctc
               (contract ctc (vector add1) 'inner-pos 'inner-neg)
               'pos 'neg)])
      (has-complete-blame? (vector-ref v 0)))
   #t)

  (test/spec-passed/result
   'blame-selectors
   '(let ()
      (define source "dunno")
      (define pos "dunno")
      (define neg "dunno")
      (define ctc "dunno")
      (define val "dunno")
      (define orig? "dunno")
      (define swapped? "dunno")
      (contract (make-contract #:name 'blame-selector-helper
                               #:late-neg-projection
                               (λ (blame)
                                 (set! source (blame-source blame))
                                 (set! pos (blame-positive blame))
                                 (set! neg (blame-negative blame))
                                 (set! ctc (blame-contract blame))
                                 (set! val (blame-value blame))
                                 (set! orig? (blame-original? blame))
                                 (set! swapped? (blame-swapped? blame))
                                 (λ (val np)
                                   val)))
                'whatevs
                'pos 'neg
                'there-is-no-name
                (build-source-location #f))
      (list source pos neg ctc val orig? swapped?))
   (list (srcloc #f #f #f #f #f)
         'pos #f 'blame-selector-helper 'there-is-no-name #t #f))

  (test/spec-passed/result
   'swapped-blame-selectors
   '(let ()
      (define source "dunno")
      (define pos "dunno")
      (define neg "dunno")
      (define ctc "dunno")
      (define val "dunno")
      (define orig? "dunno")
      (define swapped? "dunno")
      (define the-ctc
        (-> (make-contract #:name 'blame-selector-helper
                           #:late-neg-projection
                           (λ (blame)
                             (set! source (blame-source blame))
                             (set! pos (blame-positive blame))
                             (set! neg (blame-negative blame))
                             (set! ctc (blame-contract blame))
                             (set! val (blame-value blame))
                             (set! orig? (blame-original? blame))
                             (set! swapped? (blame-swapped? blame))
                             (λ (val np)
                               val)))
            any))
      (contract the-ctc
                (λ (x) 'whatevs)
                'pos 'neg
                'there-is-no-name
                (build-source-location #f))
      (list source pos neg ctc val orig? swapped?))
   (list (srcloc #f #f #f #f #f)
         #f 'pos '(-> blame-selector-helper any) 'there-is-no-name #f #t))

  (test/spec-passed/result
   'blame-equality
   '(let ([b
           (make-blame (srcloc "src.rkt" #f #f #f #f)
                       'whatever (λ () 'the-name) 'pos 'neg #t)])
      (equal? (blame-add-context b "thing" #:important 'yes!)
              (blame-add-context b "thing" #:important 'yes!)))
   #t)

  (test/spec-passed/result
   'blame-no-context
   ;; when the "in" has the contract after it, there is no context
   '(regexp-match? #rx"in: [(]list/c"
                   (with-handlers ([exn:fail:contract:blame? exn-message])
                     ((car (contract (list/c (-> integer? integer?))
                                     (list (λ (x) x))
                                     'pos
                                     'neg
                                     #:limit-context 0))
                      #f)))
   #t)

  (test/spec-passed/result
   'blame-1-context
   ;; make sure that, when there is one frame of context,
   ;; we do not see the `list/c` part of the context
   '(regexp-match? #rx"element of"
                   (with-handlers ([exn:fail:contract:blame? exn-message])
                     ((car (contract (list/c (-> integer? integer?))
                                     (list (λ (x) x))
                                     'pos
                                     'neg
                                     #:limit-context 10))
                      #f)))
   #t)

  )
