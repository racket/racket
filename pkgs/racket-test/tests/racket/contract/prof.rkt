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
   'contract-marks2b
   '(let ()
      (struct s (x))
      ((contract (-> any/c pos-blame?) s-x 'pos 'neg) (s 1))))

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
      (eval '(define f
               (let ([nb
                      (λ _ (named-blame?
                            (dynamic-require 'racket/contract/private/blame
                                             'invariant-assertion-party)))])
                 (invariant-assertion (-> nb nb)
                                      (λ (x) 3)))))
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
         (object-contract (field x pos-blame?) (f (-> neg-blame?)))
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
   'contract-marks42b
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

  (test/spec-passed
   'contract-marks44
   '(let ()
      (struct s ([x #:mutable]))
      (define s* (contract (struct/dc s [x pos-blame?] #:inv (x) pos-blame?) (s 3) 'pos 'neg))
      (set-s-x! s* 3)
      (s-x s*)))

  (test/spec-passed
   'contract-marks45
   '(let ()
      (eval '(module propmod racket/base
               (require racket/contract 'prof-fun)
               (define-values (prop prop? prop-ref)
                 (make-struct-type-property 'prop))
               (define (app-prop x v)
                 (((prop-ref x) x) v))
               (provide/contract
                [prop (struct-type-property/c
                       (-> (lambda _ (named-blame? 'propmod))
                           (-> (lambda _ (named-blame? 'propmod))
                               (lambda _ (named-blame? 'propmod)))))])
               (provide prop-ref app-prop)))
      (eval '(require 'propmod))
      (eval '(struct s (f) #:property prop (lambda (s) (s-f s))))
      (eval '(define s1 (s even?)))
      (eval '(app-prop s1 5))))

  (test/spec-passed
   'contract-marks46
   '((contract (->i ([x () pos-blame?] [y (x) pos-blame?])
                    #:rest [z (x y) pos-blame?]
                    #:pre (x y z) pos-blame?
                    [res (x y z) neg-blame?]
                    #:post (res x y z) neg-blame?)
               (lambda (x y . z) 3)
               'pos 'neg)
     1 2 3))

  (test/spec-passed
   'contract-marks47
   '((contract (->i ([x () pos-blame?] [y (x) pos-blame?])
                    ([w (x y) pos-blame?])
                    #:rest [z (x y) pos-blame?]
                    #:pre (x y z) pos-blame?
                    [res (x y z) neg-blame?]
                    #:post (res x y z) neg-blame?)
               (lambda (x y [w 3] . z) 3)
               'pos 'neg)
     1 2 3 4))

  (test/spec-passed
   'contract-marks48
   '((contract (->i ([x () pos-blame?] [y (x) pos-blame?])
                    [res (x y) neg-blame?])
               (lambda (x y) 3)
               'pos 'neg)
     1 2))

  (test/spec-passed
   'contract-marks49
   '((contract (->i ([x () pos-blame?])
                    [res (x) neg-blame?])
               (lambda (x) 3)
               'pos 'neg)
     1))

  (test/spec-passed
   'contract-marks50
   '((contract (opt/c (-> neg-blame? any/c)) (λ (x) x) 'pos 'neg) 1))

  (test/spec-passed
   'contract-marks51
   '((contract (opt/c (-> any/c pos-blame?)) (λ (x) x) 'pos 'neg) 1))

  (test/spec-passed
   'contract-marks52
   '((contract (->d ([x pos-blame?] [y pos-blame?])
                    #:rest z pos-blame?
                    #:pre pos-blame?
                    [res neg-blame?]
                    #:post neg-blame?)
               (lambda (x y . z) 3)
               'pos 'neg)
     1 2 3))

  (test/spec-passed
   'contract-marks53
   '((contract (->d ([x pos-blame?] [y pos-blame?])
                    ([w pos-blame?])
                    #:rest z pos-blame?
                    #:pre pos-blame?
                    [res neg-blame?]
                    #:post neg-blame?)
               (lambda (x y [w 3] . z) 3)
               'pos 'neg)
     1 2 3 4))

  (test/spec-passed
   'contract-marks54
   '((contract (->d ([x pos-blame?] [y pos-blame?])
                    [res neg-blame?])
               (lambda (x y) 3)
               'pos 'neg)
     1 2))

  (test/spec-passed
   'contract-marks55
   '((contract (->d ([x pos-blame?])
                    [res neg-blame?])
               (lambda (x) 3)
               'pos 'neg)
     1))

  (test/spec-passed
   'contract-marks56
   '(let ()
      (eval '(require racket/async-channel))
      (eval '(define c (contract (async-channel/c pos-blame?) (make-async-channel) 'pos 'neg)))
      (eval '(async-channel-put c 3))
      (eval '(async-channel-get c))))

  (test/spec-passed
   'contract-marks57
   '(let ()
      (eval '(require racket/generic))
      (eval '(define-generics fooable (foo fooable)))
      (eval '(struct s () #:methods gen:fooable [(define (foo x) x)]))
      (eval '(foo (contract (generic-instance/c gen:fooable [foo (-> pos-blame? neg-blame?)])
                            (s) 'pos 'neg)))))

  (test/spec-passed
   'contract-marks58
   '(let ()
      (eval '(require racket/set))
      (eval '(define s (contract (set/c pos-blame?) (set 1 2 3) 'pos 'neg)))
      (eval '(set-add s 3))
      (eval '(set-member? s 3))))

  (test/spec-passed
   'contract-marks59
   '(let ()
      (eval '(require racket/set))
      (eval '(define s (contract (set/c pos-blame? #:lazy? #t #:kind 'mutable)
                                 (mutable-set 1 2 3) 'pos 'neg)))
      (eval '(set-add! s 3))
      (eval '(set-member? s 3))))

  (test/spec-passed
   'contract-marks60
   '(let ()
      (eval '(require racket/set))
      (eval '(define s (contract (set/c pos-blame? #:kind 'dont-care)
                                 (list 1 2 3) 'pos 'neg)))
      (eval '(set-add s 3))
      (eval '(set-member? s 3))))

  (test/spec-passed
   'contract-marks61
   '(let ()
      (eval '(require racket/stream))
      (eval '(stream-first (contract (stream/c pos-blame?) (in-range 3) 'pos 'neg)))))

  (test/spec-passed/result
   'contract-marks62
   '(let ()
      (define marked? #f) ; check that we measure the cost of contract-stronger?
      (define (make/c) ; the two have to not be eq?, otherwise contract-stronger? is not called
        (make-chaperone-contract
         #:late-neg-projection
         (lambda (b)
           (lambda (val neg-party)
             val))
         #:stronger
         (lambda (c1 c2)
           (when (pos-blame? 'dummy)
             (set! marked? #t)
             #t))))
      ((contract (-> pos-blame? (make/c))
                 (contract (-> pos-blame? (make/c)) values 'pos 'neg)
                 'pos 'neg)
       3)
      marked?)
   #t)

  (test/spec-passed
   'contract-marks63
   '(let ()
      (eval '(require racket/sequence))
      (eval '(sequence->list (contract (sequence/c pos-blame?) (in-range 3) 'pos 'neg)))))

  (test/spec-passed
   'contract-marks64
   '(let ()
      (eval '(require racket/sequence racket/dict))
      (eval '(sequence-ref (contract (sequence/c pos-blame? pos-blame?)
                                     (in-dict '((1 . 2) (3 . 4))) 'pos 'neg)
                           0))))

  (test/spec-passed
   'contract-marks65
   '(let ()
      (eval '(require syntax/id-table))
      (eval '(define t (contract (free-id-table/c pos-blame? neg-blame?)
                                 (make-free-id-table)
                                 'pos 'neg)))
      (eval '(free-id-table-set! t #'a 3))
      (eval '(free-id-table-ref t #'a))))

  (test/spec-passed
   'contract-marks66
   '(let ()
      (eval '(require syntax/id-table))
      (eval '(define t (contract (free-id-table/c pos-blame? neg-blame?)
                                 (make-immutable-free-id-table)
                                 'pos 'neg)))
      (eval '(free-id-table-ref (free-id-table-set t #'a 3) #'a))))

  ;; check that there's no mark when running the body of a contracted function
  ;; (i.e., user code)
  (test/spec-passed/result
   'contract-marks67
   '(let ()
      (eval '(module m racket/base
               (require racket/contract/base
                        (only-in racket/contract/private/guts
                                 contract-continuation-mark-key))
               (provide
                (contract-out
                 [f (-> integer? void?)]))
               (define (f x)
                 (define m
                   (continuation-mark-set->list
                    (current-continuation-marks)
                    contract-continuation-mark-key))
                 (unless (null? m)
                   (error 'ack "~s" m)))))
      (eval '(require 'm))
      (eval '(let ([f f]) (f 1))))
   (void))

  (test/spec-passed
   'contract-marks68
   '(let ()
      (define woody%
        (class object%
          (define/public (draw who)
            (format "reach for the sky, ~a" who))
          (super-new)))
      (define woody+c%
        (contract
         (class/c [draw (->m neg-blame? pos-blame?)])
         woody% 'pos 'neg))
      (send (new woody+c%) draw #f)))

  (test/spec-passed
   'contract-marks69
   '(let ()
      (define woody%
        (class object%
          (define/public (draw who)
            (format "reach for the sky, ~a" who))
          (super-new)))
      (define woody/hat%
        (class woody%
          (field [hat-location 'uninitialized])
          (define/public (lose-hat) (set! hat-location 'lost))
          (define/public (find-hat) (set! hat-location 'on-head))
          (super-new)))
      (define woody/hat+c%
        (contract (class/c [draw (->m neg-blame? pos-blame?)]
                           [lose-hat (->m pos-blame?)]
                           [find-hat (->m pos-blame?)]
                           (field [hat-location pos-blame?]))
                  woody/hat% 'pos 'neg))
      (get-field hat-location (new woody/hat+c%))
      (let ([woody (new woody/hat+c%)])
        (set-field! hat-location woody 'under-the-dresser))))

  (test/spec-passed
   'contract-marks70
   '(let ()
      (define woody%
        (class object%
          (define/public (draw who)
            (format "reach for the sky, ~a" who))
          (super-new)))
      (define woody/hat%
        (class woody%
          (field [hat-location 'uninitialized])
          (define/public (lose-hat) (set! hat-location 'lost))
          (define/public (find-hat) (set! hat-location 'on-head))
          (super-new)))
      (define woody/hat+c%
        (contract (class/c [draw (->m neg-blame? pos-blame?)]
                           [lose-hat (->m pos-blame?)]
                           [find-hat (->m pos-blame?)]
                           (field [hat-location pos-blame?]))
                  woody/hat% 'pos 'neg))
      (define woody/hat2%
        (class woody/hat+c%
          (inherit-field hat-location)
          (define/public (eat-hat) (set! hat-location 'stomach))
          (super-new)))
      (send (new woody/hat2%) eat-hat)))

  (test/spec-passed
   'contract-marks71
   '(let ()
      (define woody%
        (class object%
          (define/public (draw who)
            (format "reach for the sky, ~a" who))
          (super-new)))
      (define woody/init-hat%
        (class woody%
          (init init-hat-location)
          (field [hat-location init-hat-location])
          (define/public (lose-hat) (set! hat-location 'lost))
          (define/public (find-hat) (set! hat-location 'on-head))
          (super-new)))
      (define woody/init-hat+c%
        (contract
         (class/c [draw (->m neg-blame? pos-blame?)]
                  [lose-hat (->m pos-blame?)]
                  [find-hat (->m pos-blame?)]
                  (init [init-hat-location pos-blame?])
                  (field [hat-location pos-blame?]))
         woody/init-hat% 'pos 'neg))
      (get-field hat-location
                 (new woody/init-hat+c%
                      [init-hat-location 'lost]))
      (get-field hat-location
                 (new woody/init-hat+c%
                      [init-hat-location 'slinkys-mouth]))))

  (test/spec-passed
   'contract-marks72
   '(let ()
      (define c% (class object% (define/public (foo) 1) (super-new)))
      (send (contract (object/c (foo (->m pos-blame?)))
                      (new c%) 'pos 'neg)
            foo)))

  (test/spec-passed
   'contract-marks73
   '(let ()
      (define c% (class object% (init-field foo) (super-new)))
      (get-field foo (contract (object/c (field (foo pos-blame?)))
                               (new c% [foo 1]) 'pos 'neg))))

  (test/spec-passed
   'contract-marks74
   '(let ()
      (define c% (class object% (init-field foo) (super-new)))
      (set-field! foo (contract (object/c (field (foo pos-blame?)))
                                (new c% [foo 1]) 'pos 'neg)
                  3)))

  (test/spec-passed
   'contract-marks75
   '(let ()
      (define c% (class object% (define/public (foo) 1) (super-new)))
      (send (contract (dynamic-object/c '(foo) (list (->m pos-blame?)) '() '())
                      (new c%) 'pos 'neg)
            foo)))

  (test/spec-passed
   'contract-marks76
   '(let ()
      (define c% (class object% (init-field foo) (super-new)))
      (get-field foo (contract (dynamic-object/c '() '() '(foo) (list pos-blame?))
                               (new c% [foo 1]) 'pos 'neg))))

  (test/spec-passed
   'contract-marks77
   '(let ()
      (define c% (class object% (init-field foo) (super-new)))
      (set-field! foo (contract (dynamic-object/c '() '() '(foo) (list pos-blame?))
                                (new c% [foo 1]) 'pos 'neg)
                  3)))

  (test/spec-passed
   'contract-marks78
   '(let ()
      (eval '(module server8 racket
               (require racket/contract/option 'prof-fun)
               (provide
                change
                (contract-out
                 [vec (invariant/c
                       (lambda _ (named-blame? 'server8))
                       (lambda _ (named-blame? 'server8)))]))
               (define vec (vector 1 2 3 4 5))
               (define (change) (vector-set! vec 2 42))
               (define (sorted? vec)
                 (for/and ([el vec]
                           [cel (vector-drop vec 1)])
                   (<= el cel)))))
      (eval '(require 'server8))
      (eval '(vector-set! vec 2 42))))

  (test/spec-passed
   'contract-marks79
   '(let ()
      (eval '(module server0 racket
               (require racket/contract/option 'prof-fun)
               (provide
                (contract-out
                 [vec (option/c (lambda _ (named-blame? 'server0)))]))
               (define vec (vector 1 2 3 4))))
      (eval '(require 'server0))
      (eval '(vector-set! vec 1 'foo))
      (eval '(vector-ref vec 1))
      (eval '(module server1 racket
               (require racket/contract/option 'prof-fun)
               (provide
                (contract-out
                 [vec (option/c (lambda _ (named-blame? 'server1)) #:with-contract #t)]))
               (define vec (vector 1 2 3 4))))
      (eval '(require 'server1))
      (eval '(vector-set! vec 1 'foo))
      (eval '(module server2 racket
               (require racket/contract/option 'prof-fun)
               (provide
                (contract-out
                 [vec (option/c (lambda _ (named-blame? 'server2)) #:tester sorted?)]))
               (define vec (vector 1 42 3 4))
               (define (sorted? vec) #t)))
      (eval '(require 'server2))
      ))

  (test/spec-passed
   'contract-marks80
   '(let ()
      (eval '(module server3 racket
               (require racket/contract/option 'prof-fun)
               (provide (contract-out [foo (option/c (-> (lambda _ (named-blame? 'server3))
                                                         (lambda _ (named-blame? 'server3))))]))
               (define foo (λ (x) x))))
      (eval '(require 'server3 racket/contract/option))
      (eval '(define e-foo (exercise-option foo)))
      (eval '(foo 42))
      (eval '(e-foo 'wrong))
      (eval '((exercise-option e-foo) 'wrong))
      ))

  (test/spec-passed/result
   'contract-marks81
   '(let ()
      (eval '(module server4 racket
               (require racket/contract/option 'prof-fun)
               (provide (contract-out [foo (option/c (-> (lambda _ (named-blame? '(middleman server4)))
                                                         (lambda _ (named-blame? '(middleman server4)))))]))
               (define foo (λ (x) x))))
      (eval '(module middleman racket
               (require racket/contract/option 'server4)
               (provide (contract-out [foo transfer/c]))))
      (eval '(require 'middleman racket/contract/option))
      (eval '(define e-foo (exercise-option foo)))
      (eval '(e-foo 1))
      (eval '(module server5 racket
               (require racket/contract/option)
               (provide (contract-out [boo transfer/c]))
               (define (boo x) x)))
      (eval '(require 'server5))
      (eval '(boo 42))
      (void))
   (void))

  )
