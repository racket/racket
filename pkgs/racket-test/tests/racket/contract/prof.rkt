#lang racket/base
(require "test-util.rkt")

(parameterize ([current-contract-namespace
                (make-basic-contract-namespace 
                 'racket/contract)])

  (contract-eval
   '(module prof-fun racket/base
      (require (only-in racket/contract/private/guts
                        contract-continuation-mark-key)
               (only-in racket/contract/private/blame
                        blame-positive
                        blame-negative
                        blame?)
               (only-in racket/contract/combinator
                        blame-missing-party?))
      (provide pos-blame? neg-blame? named-blame?)
      (define (named-blame? who)
        (define mark-info
          (continuation-mark-set-first
           (current-continuation-marks)
           contract-continuation-mark-key))
        (define complete-blame
          (or (not mark-info)
              (pair? mark-info) ; missing party is provided
              (not (blame-missing-party? mark-info)))) ; no missing party
        (define (get-party selector)
          (and mark-info
               (if (pair? mark-info)
                   (or (selector (car mark-info))
                       (cdr mark-info))
                   (selector mark-info))))
        (and mark-info
             complete-blame
             (let ([pos (get-party blame-positive)]
                   [neg (get-party blame-negative)])
               (or (equal? pos who)
                   (equal? neg who)))))
      (define (pos-blame? _) (named-blame? 'pos))
      (define (neg-blame? _) (named-blame? 'neg))))

  (contract-eval '(require 'prof-fun))

  (test/spec-passed
   'contract-marks1
   '((contract (-> neg-blame? any/c) (λ (x) x) 'pos 'neg) 1))

  (test/spec-passed
   'contract-marks2
   '((contract (-> any/c pos-blame?) (λ (x) x) 'pos 'neg) 1))

  (test/spec-passed
   'contract-marks3
   '(contract (vector/c pos-blame?) (vector 1) 'pos 'neg))

  (test/spec-passed
   'contract-marks4
   '((contract (parameter/c pos-blame?) (make-parameter #f) 'pos 'neg)))

  (test/spec-passed
   'contract-marks5
   '(contract (unconstrained-domain-> pos-blame?) (λ () 1) 'pos 'neg))

  (test/spec-passed
   'contract-marks6
   '(contract (->* () #:pre neg-blame? any) (λ () 1) 'pos 'neg))

  (test/spec-passed
   'contract-marks7
   '(contract (->* () any/c #:post pos-blame?) (λ () 1) 'pos 'neg))

  (test/spec-passed/result
   'contract-marks8
   '(let ()
      (eval '(module prof1 racket/base
               (require racket/contract 'prof-fun)
               (define (f x) x)
               (define a-contract (-> (λ _ (named-blame? 'prof1)) any/c))
               (provide
                (contract-out
                 [f a-contract]))))
      (eval '(require 'prof1))
      (eval '(f 11)))
   11)

  (test/spec-passed/result
   'contract-marks9
   '(let ()
      (eval '(module prof2 racket/base
               (require racket/contract 'prof-fun)
               (define (f x) x)
               (provide
                (contract-out
                 [f (-> (λ _ (named-blame? 'prof2)) any/c)]))))
      (eval '(require 'prof2))
      (eval '(f 11)))
   11)

  (test/spec-passed/result
   'contract-marks10
   '(let ()
      (eval '(module prof3 racket/base
               (require racket/contract 'prof-fun)
               (define (f #:x x) x)
               (provide
                (contract-out
                 [f (-> #:x (λ _ (named-blame? 'prof3)) any/c)]))))
      (eval '(require 'prof3))
      (eval '(f #:x 11)))
   11)

  (test/spec-passed
   'contract-marks11
   '(let ()
      (struct posn (x y))
      ((contract (-> (struct/dc posn [x neg-blame?]) any/c) (λ (x) x) 'pos 'neg)
       (posn 1 2))))

  (test/spec-passed
   'contract-marks12
   '(let ()
      (struct posn (x y))
      ((contract (-> any/c (struct/dc posn [x pos-blame?])) (λ (x) x) 'pos 'neg)
       (posn 1 2))))

  (test/spec-passed
   'contract-marks13
   '(let ()
      (struct posn (x y))
      ((contract (-> any/c (struct/dc posn [x pos-blame?] #:inv (x) pos-blame?))
                 (λ (x) x) 'pos 'neg)
       (posn 1 2))))

  (test/spec-passed
   'contract-marks14
   '(let ()
      (struct posn (x y) #:mutable)
      ((contract (-> any/c (struct/dc posn [x pos-blame?]))
                 (λ (x) x) 'pos 'neg)
       (posn 1 2))))

  (test/spec-passed
   'contract-marks15
   '(let ()
      (struct posn (x y))
      ((contract (-> any/c (struct/dc posn [x #:lazy pos-blame?]))
                 (λ (x) x) 'pos 'neg)
       (posn 1 2))))

  (test/spec-passed
   'contract-marks16
   '(let ()
      (struct posn (x y))
      ((contract (-> any/c (struct/dc posn
                                      [x pos-blame?]
                                      [y (x) pos-blame?]))
                 (λ (x) x) 'pos 'neg)
       (posn 1 2))))

  (test/spec-passed
   'contract-marks17
   '(let ()
      (struct posn (x y))
      ((contract (-> any/c (struct/dc posn
                                      [x pos-blame?]
                                      [y (x) #:lazy pos-blame?]))
                 (λ (x) x) 'pos 'neg)
       (posn 1 2))))

  (test/spec-passed
   'contract-marks18
   '(let ()
      (struct posn (x y) #:mutable)
      ((contract (-> any/c (struct/dc posn
                                      [x pos-blame?]
                                      [y (x) pos-blame?]))
                 (λ (x) x) 'pos 'neg)
       (posn 1 2))))

  (test/spec-passed
   'contract-marks19
   '(let ()
      (struct posn (x y))
      ((contract (-> any/c (struct/dc posn
                                      [x pos-blame?]
                                      [y (x) #:depends-on-state pos-blame?]))
                 (λ (x) x) 'pos 'neg)
       (posn 1 2))))

  (test/spec-passed
   'contract-marks20
   '(let ()
      (struct posn (x y) #:mutable)
      ((contract (-> any/c (struct/dc posn
                                      [x pos-blame?]
                                      [y (x) #:depends-on-state pos-blame?]))
                 (λ (x) x) 'pos 'neg)
       (posn 1 2))))

  (test/spec-passed
   'contract-marks21
   '(let ()
      ((contract (case-> (-> any/c any/c pos-blame?))
                 (λ (x y) x) 'pos 'neg)
       1 2)))

  (test/spec-passed
   'contract-marks22
   '(let ()
      ((contract (case-> (-> neg-blame? any/c))
                 (λ (x) x) 'pos 'neg)
       1)))

  (test/spec-passed
   'contract-marks23
   '(unbox (contract (box/c neg-blame?) (box 1) 'pos 'neg)))

  (test/spec-passed
   'contract-marks24
   '(set-box! (contract (box/c neg-blame?) (box 1) 'pos 'neg) 2))

  ;; do we catch flat contracts applies with `contract-out`?
  (test/spec-passed/result
   'contract-marks25
   '(let ()
      (eval '(module prof25 racket/base
               (require racket/contract 'prof-fun)
               (define x 3)
               (define a-contract (λ _ (named-blame? 'prof25)))
               (provide
                (contract-out
                 [x a-contract]))))
      (eval '(require 'prof25))
      (eval 'x))
   3)

  (test/spec-passed/result
   'contract-marks26
   '(let ()
      (eval '(define/contract x (λ _ (named-blame? 'top-level)) 3))
      (eval 'x))
   3)

  (test/spec-passed/result
   'contract-marks27
   '(with-contract test27 #:result (λ _ (named-blame? '(region test27))) 3)
   3)

  (test/spec-passed/result
   'contract-marks28
   '(let ()
      (eval '(define-struct/contract foo ([bar (λ _ (named-blame? 'top-level))])))
      (eval '(foo-bar (foo 3))))
   3)

  (test/spec-passed/result
   'contract-marks29
   '(let ()
      (eval '(define f (invariant-assertion (-> (λ _ (named-blame? 'top-level))
                                                (λ _ (named-blame? 'top-level)))
                                            (λ (x) 3))))
      (eval '(f 2)))
   3)

  (test/spec-passed/result
   'contract-marks30
   '(let ()
      (eval '(module test30 racket/base
               (require racket/contract/base 'prof-fun)
               (define (f x) 3)
               (define-module-boundary-contract g f (-> (λ _ (named-blame? 'top-level))
                                                        (λ _ (named-blame? 'top-level))))
               (provide g)))
      (eval '(require 'test30))
      (eval '(f 2)))
   3)

  (test/spec-passed/result
   'contract-marks31
   '((hash-ref (contract (hash/c (-> neg-blame? pos-blame?)
                                 (-> neg-blame? pos-blame?))
                         (hash values values)
                         'pos 'neg)
               values)
     3)
   3)

  (test/spec-passed/result
   'contract-marks32
   '(car (contract (listof pos-blame?) (list 3) 'pos 'neg))
   3)

  (test/spec-passed/result
   'contract-marks33
   '((car (contract (listof (-> neg-blame? pos-blame?)) (list (lambda (x) 3)) 'pos 'neg)) 2)
   3)

  (test/spec-passed/result
   'contract-marks34
   '(begin
      (require racket/promise)
      (force (contract (promise/c pos-blame?) (delay 3) 'pos 'neg)))
   3)

  (test/spec-passed/result
   'contract-marks35
   '(let ()
      (define/contract tag
        (prompt-tag/c (-> (λ _ (named-blame? 'top-level))
                          (λ _ (named-blame? 'top-level))))
        (make-continuation-prompt-tag))
      (call-with-continuation-prompt
       (lambda ()
         (number->string
          (call-with-composable-continuation
           (lambda (k)
            (abort-current-continuation tag k)))))
       tag
       (lambda (k) 3)))
   3)

  (test/spec-passed/result
   'contract-marks36
   '(let ()
      (define/contract mark-key
        (continuation-mark-key/c (-> (λ _ (named-blame? 'top-level))
                                     (λ _ (named-blame? 'top-level))))
        (make-continuation-mark-key))
      (with-continuation-mark
       mark-key
       (lambda (s) (append s '(truffle fudge ganache)))
       (let ([mark-value (continuation-mark-set-first
                          (current-continuation-marks) mark-key)])
         (mark-value '(chocolate-bar)))))
   '(chocolate-bar truffle fudge ganache))

  (test/spec-passed
   'contract-marks37
   '(let ()
      (define/contract my-evt
        (evt/c (λ _ (named-blame? 'top-level)))
        always-evt)
      (sync my-evt)))

  (test/spec-passed
   'contract-marks38
   '(let ()
      (define/contract chan
        (channel/c (λ _ (named-blame? 'top-level)))
        (make-channel))
      (thread (λ () (channel-get chan)))
      (channel-put chan 'not-a-string)))

  (test/spec-passed
   'contract-marks39
   '(let ()
      (eval '(require racket/class))
      (eval '((contract (->m neg-blame? any/c) (λ (_ x) x) 'pos 'neg) 'a 1))))

  (test/spec-passed
   'contract-marks40
   '(let ()
      (define o
        (contract
         (object-contract (field x pos-blame?) (f (->m neg-blame?)))
         (new (class object% (init-field x) (define/public (f) x) (super-new)) [x 3])
         'pos 'neg))
      (get-field x o)
      (set-field! x o 2)
      (send o f)))

  (test/spec-passed
   'contract-marks41
   '(contract (vectorof pos-blame? #:flat? #t) #(1 2 3) 'pos 'neg))

  (test/spec-passed
   'contract-marks42
   '((vector-ref (contract (vectorof (-> pos-blame? neg-blame?)) (vector values)
                           'pos 'neg)
                 0)
     1))

  (test/spec-passed
   'contract-marks43
   '(contract (vector/c pos-blame? #:flat? #t) #(1) 'pos 'neg))

  (test/spec-passed
   'contract-marks42
   '((vector-ref (contract (vector/c (-> pos-blame? neg-blame?)) (vector values)
                           'pos 'neg)
                 0)
     1))

  (test/spec-passed
   'contract-marks43
   '((contract (parametric->/c (X) (-> pos-blame? X neg-blame?))
               (lambda (x y) x)
               'pos 'neg)
     1 2))

  )
