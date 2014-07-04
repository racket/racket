#lang racket/base
(require "test-util.rkt")

(parameterize ([current-contract-namespace
                (make-basic-contract-namespace)])
  
  (test/spec-passed
   'recursive-contract1
   '(letrec ([ctc (-> integer? (recursive-contract ctc))])
      (letrec ([f (λ (x) f)])
        ((((contract ctc f 'pos 'neg) 1) 2) 3))))
  
  (test/neg-blame
   'recursive-contract2
   '(letrec ([ctc (-> integer? (recursive-contract ctc))])
      (letrec ([f (λ (x) f)])
        ((contract ctc f 'pos 'neg) #f))))
  
  (test/neg-blame
   'recursive-contract3
   '(letrec ([ctc (-> integer? (recursive-contract ctc))])
      (letrec ([f (λ (x) f)])
        ((((contract ctc f 'pos 'neg) 1) 2) #f))))
  
  (test/pos-blame
   'recursive-contract4
   '(letrec ([ctc (-> integer? (recursive-contract ctc))])
      (letrec ([c 0]
               [f (λ (x)
                    (set! c (+ c 1))
                    (if (= c 2)
                        'nope
                        f))])
        ((((contract ctc f 'pos 'neg) 1) 2) 3))))
  
  (test/spec-passed
   'recursive-contract5
   '(contract (recursive-contract #f)
              #f
              'pos
              'neg))
  
  (test/spec-passed
   'recursive-contract6
   '(letrec ([ctc (or/c number? (cons/c number? (recursive-contract ctc #:flat)))])
      (contract ctc (cons 1 (cons 2 3)) 'pos 'neg)))
  
  (test/pos-blame
   'recursive-contract7
   '(letrec ([ctc (or/c number? (cons/c number? (recursive-contract ctc #:flat)))])
      (contract ctc (cons 1 (cons 2 #t)) 'pos 'neg)))
  
  (test/pos-blame
   'recursive-contract8
   '(letrec ([ctc (or/c number? (cons/c number? (recursive-contract ctc #:flat)))])
      (contract ctc (cons 1 (cons #t 3)) 'pos 'neg)))
  
  (test/spec-passed
   'recursive-contract9
   '(letrec ([ctc (or/c number? (hash/c (recursive-contract ctc #:chaperone) number?))])
      (make-hash (list (cons (make-hash (list (cons 3 4))) 5)))))
  
  (test/pos-blame
   'recursive-contract10
   '(let ()
      (struct doll (contents))
      (letrec ([doll-ctc (recursive-contract (or/c 'center (struct/c doll doll-ctc)) #:flat)])
        (contract doll-ctc (doll 3) 'pos 'neg))))
  
  (test/pos-blame
   'recursive-contract11
   '(let ()
      (struct doll (contents))
      (letrec ([doll-ctc2 (or/c 'center (struct/c doll (recursive-contract doll-ctc2 #:flat)))])
        (contract doll-ctc2 (doll 4) 'pos 'neg)))))
