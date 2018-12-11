#lang racket/base
(require "test-util.rkt")

(parameterize ([current-contract-namespace
                (make-basic-contract-namespace
                 'racket/contract)])
  
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
        (contract doll-ctc2 (doll 4) 'pos 'neg))))

  (test/spec-passed
   'recursive-contract12
   '(letrec ([ctc (or/c number? (cons/c number? (recursive-contract ctc #:flat #:extra-delay)))])
      (contract ctc (cons 1 (cons 2 3)) 'pos 'neg)))

  (test/pos-blame
   'recursive-contract13
   '(letrec ([ctc (or/c number? (cons/c number? (recursive-contract ctc #:flat #:extra-delay)))])
      (contract ctc (cons 1 (cons 2 'not-a-number)) 'pos 'neg)))

  (test/spec-passed/result
   'recursive-contract14
   '(let ()
      (define c (recursive-contract (or/c integer? (cons/c c integer?))))
      (void (contract (-> c any/c) void 'p 'n))
      ((contract (-> c any/c) values 'p 'n) '(1 . 2)))
   '(1 . 2))

  
  (test/spec-passed/result
   'memoize-applied-blame
   '(let ()
      (define counter 0)
      (define my-ctc
        (make-contract
         #:first-order integer?
         #:late-neg-projection
         (lambda (blame)
           (set! counter (add1 counter))
           (lambda (val neg)
             (if (integer? val)
                 val
                 (raise-blame-error
                  blame
                  val
                  '(expected: "~a" given: "~e")
                  "my-ctc" val))))))
      (define ctc
        (or/c my-ctc (vectorof (recursive-contract ctc))))
      (define/contract v
        ctc
        (vector (vector (vector 5))))
      (for ([i (in-range 100)])
        (void (vector-ref v 0)))
      counter)
   2)

  (test/spec-passed/result
   'recursive-contract-not-too-slow
   '(let ()
      (define c
        (recursive-contract
         (or/c null?
               (cons/c (-> integer? integer? integer?) c)
               (cons/c (-> integer? integer?) (cons/c (-> integer? integer?) c)))))

      (define l (build-list 10000 (λ (x) (λ (x) x))))
      (define-values (_ cpu real gc)
        (time-apply (λ () (contract c l 'pos 'neg)) '()))
      ;; should be substantially less than 5 seconds.
      ;; with the old implementation it is more like 20 seconds
      ;; on my laptop and about .3 seconds with the new one
      (< (- cpu gc) 5000))
   #t
   do-not-double-wrap))
