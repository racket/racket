#lang racket/base
(require "test-util.rkt")
(parameterize ([current-contract-namespace (make-full-contract-namespace)])
  (test/spec-passed/result
   'property/c1
   '(contract (property/c length (=/c 3)) '(1 2 3) 'pos 'neg)
   '(1 2 3))
  (test/pos-blame
   'property/c2
   '(contract (property/c length (=/c 3)) '(1 2) 'pos 'neg))

  (test-true
   'property/c-message1
   '(with-handlers ([exn:fail:contract:blame?
                     (λ (exn)
                       (and (member "the length of"
                                    (blame-context (exn:fail:contract:blame-object exn)))
                            #t))])
      (contract (property/c length (=/c 3)) '(1 2) 'pos 'neg)))
  (test-true
   'property/c-message2
   '(with-handlers ([exn:fail:contract:blame?
                     (λ (exn)
                       (and (member "the vector-length of"
                                    (blame-context (exn:fail:contract:blame-object exn)))
                            #t))])
      (contract (property/c vector-length (=/c 3)) #(1 2) 'pos 'neg)))
  (test-true
   'property/c-message3
   '(with-handlers ([exn:fail:contract:blame?
                     (λ (exn)
                       (and (member "the length of"
                                    (blame-context (exn:fail:contract:blame-object exn)))
                            #t))])
      (contract (property/c vector-length (=/c 3) #:name "length") #(1 2) 'pos 'neg)))

  (test/spec-passed/result
   'property/c-message4
   '(with-handlers ([exn:fail:contract:blame?
                     (λ (exn)
                       (define m (regexp-match #rx"promised:[^\n]*\n" (exn-message exn)))
                       (and m (car m)))])
      (contract (property/c length (=/c 3)) '(1 2) 'pos 'neg))
   "promised: (=/c 3)\n")

  (test/spec-passed/result
   'property/c-name1
   '(contract-name (property/c length (=/c 3)))
   '(property/c length (=/c 3)))
  (test/spec-passed/result
   'property/c-name2
   '(contract-name (property/c vector-length (=/c 3)))
   '(property/c vector-length (=/c 3)))
  (test/spec-passed/result
   'property/c-name3
   '(contract-name (property/c vector-length (=/c 3) #:name "length"))
   '(property/c vector-length (=/c 3)))

  (test/spec-passed/result
   'property/c-stronger1
   '(contract-stronger? (property/c length (integer-in 0 4))
                        (property/c length (integer-in 0 4)))
   #t)
  (test/spec-passed/result
   'property/c-stronger2
   '(contract-stronger? (property/c length (integer-in 0 4))
                        (property/c length (integer-in 1 3)))
   #f)
  (test/spec-passed/result
   'property/c-stronger3
   '(contract-stronger? (property/c length (integer-in 1 3))
                        (property/c length (integer-in 0 4)))
   #t)
  (test/spec-passed/result
   'property/c-stronger4
   '(contract-stronger? (property/c vector-length (integer-in 0 4))
                        (property/c length (integer-in 0 4)))
   #f)
  (test/spec-passed/result
   'property/c-stronger5
   '(contract-stronger? (property/c vector-length (integer-in 1 3))
                        (property/c length (integer-in 0 4)))
   #f)

  (test/spec-passed/result
   'property/c-equivalent1
   '(contract-equivalent? (property/c length (integer-in 0 4))
                          (property/c length (integer-in 0 4)))
   #t)
  (test/spec-passed/result
   'property/c-equivalent2
   '(contract-equivalent? (property/c length (integer-in 0 4))
                          (property/c length (integer-in 1 3)))
   #f)
  (test/spec-passed/result
   'property/c-equivalent3
   '(contract-equivalent? (property/c length (integer-in 1 3))
                          (property/c length (integer-in 0 4)))
   #f)
  (test/spec-passed/result
   'property/c-equivalent4
   '(contract-equivalent? (property/c vector-length (integer-in 0 4))
                          (property/c length (integer-in 0 4)))
   #f)
  (test/spec-passed/result
   'property/c-equivalent5
   '(contract-equivalent? (property/c vector-length (integer-in 1 3))
                          (property/c length (integer-in 0 4)))
   #f)

  (test-true
   'property/c-generate1
   '(let ()
      (define generatable-length
        (flat-named-contract
         'generatable-length
         length
         (λ (fuel) (λ () '(1 2 3)))))
      (and (contract-random-generate (property/c generatable-length (=/c 3))) #t)))
  (test-true
   'property/c-generate2
   '(let ()
      (define generatable-length
        (flat-named-contract
         'generatable-length
         length
         (λ (fuel)
           (define seq (in-cycle (in-list '((1 2) (1 2 3)))))
           (define-values (has-next? get-next) (sequence-generate seq))
           get-next)))
      (and (contract-random-generate (property/c generatable-length (=/c 3))) #t)))
  (test/spec-passed/result
   'property/c-generate3
   '(let ()
      (define generatable-length
        (flat-named-contract
         'generatable-length
         length
         (λ (fuel) (λ () '(1 2)))))
      (contract-random-generate
       (property/c generatable-length (=/c 3))
       5
       (λ (no-generator?) (if no-generator? 'no-generator 'generator-failed))))
   'generator-failed))
