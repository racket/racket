#lang racket/base
(require "test-util.rkt")

(parameterize ([current-contract-namespace
                (make-basic-contract-namespace 
                 'racket/contract)])

  (contract-eval
   '(define (add-many-contracts n ctc val [pos 'pos] [neg 'neg])
      (for/fold ([val val])
                ([i (in-range n)])
        (contract ctc val pos neg))))

  (contract-eval '(define ctc (-> (-> integer? integer?) (-> integer? integer?))))
  (contract-eval '(define (wrap x) (add-many-contracts 11 ctc x 'pos 'neg)))
  (contract-eval '(define id (wrap (wrap (lambda (x) x)))))

  (test/spec-passed
   'collapsible1
   '(id add1))
  (test/spec-passed
   'collapsible2
   '((id add1) 1))
  (test/spec-failed
   'collapsible3
   '((id add1) 'a)
   'neg)
  (test/spec-passed
   'collapsible4
   '(((wrap id) add1) 1))
  (test/spec-failed
   'collapsible5
   '(((wrap id) add1) 'a)
   'neg)
  (test/spec-passed
   'collapsible6
   '(((wrap (wrap (wrap (wrap (wrap (wrap (wrap (wrap (wrap id))))))))) add1) 1))
  (test/spec-failed
   'collapsible7
   '(wrap 3)
   'pos)

  ;; works with non-flat contracts at the leaves
  (test/spec-passed
   'collapsible8
   '(let ([ctc (-> (vector/c integer?) (vector/c integer?))])
      (vector-ref ((contract ctc
                             (add-many-contracts 11 ctc
                                       (lambda (x) x)
                                       'inner-pos 'inner-neg)
                             'pos 'neg)
                   (vector 1))
                  0)))
  (test/spec-failed
   'collapsible9
   '(vector-ref ((contract ctc
                           (add-many-contracts 11 ctc
                                     (lambda (x) x)
                                     'inner-pos 'inner-neg)
                           'pos 'neg)
                 (vector 'a))
                0)
   'neg)
  (test/spec-failed
   'collapsible10
   '(let ([ctc (-> any/c (-> integer? integer?))])
      (vector-ref ((contract ctc
                             (add-many-contracts 11 ctc
                                       (lambda (x) x)
                                       'inner-pos 'inner-neg)
                             'pos 'neg)
                   (vector 'a))
                  0))
   "inner-pos")

  (contract-eval '(require (submod racket/contract/private/arrow-collapsible for-testing)))
  (contract-eval '(require (submod racket/contract/private/collapsible-common for-testing)))
  (contract-eval
   '(define (has-num-contracts? f dom rng)
      (unless (has-impersonator-prop:collapsible? f)
        (error "has-num-contracts?: no collapsible contract"))
      (define collapsible/c  (collapsible-property-c-c (get-impersonator-prop:collapsible f)))
      (define domain/c (car (collapsible->-doms collapsible/c)))
      (define range/c  (collapsible->-rng  collapsible/c))
      (unless (= (length (collapsible-leaf/c-proj-list domain/c)) dom)
        (error "has-num-contracts?: wrong number of domain projections"))
      (unless (= (length (collapsible-leaf/c-proj-list range/c))  rng)
        (error "has-num-contracts?: wrong number of range projections"))
      (unless (= (length (collapsible-leaf/c-contract-list domain/c)) dom)
        (error "has-num-contracts?: wrong num of domain contracts"))
      (unless (= (length (collapsible-leaf/c-contract-list range/c))  rng)
        (error "has-num-contracts?: wrong num of range contracts"))))
  (contract-eval '(define (collapsible? val)
                    (and (has-impersonator-prop:collapsible? val)
                         (let ([prop (get-impersonator-prop:collapsible val #f)])
                           (and (collapsible-wrapper-property? prop)
                                (eq? val (collapsible-property-ref prop)))))))

  (contract-eval '(define pos (flat-named-contract
                               'pos
                               (lambda (x) (and (integer? x) (>= x 0))))))
  (contract-eval '(define pos->pos (-> pos pos)))
  (contract-eval '(define pos->pos->pos (-> pos->pos pos)))

  (contract-eval
   '(define guarded
      (add-many-contracts 11 pos->pos (lambda (x) (* x -2)) 'positive 'negative)))
  (contract-eval
   '(define f1 (add-many-contracts 11 pos->pos->pos
                         (flat-named-contract
                          'c
                          (lambda (f)
                            (unless (has-contract? f)
                              (error "f1 should already be contracted"))
                            ;; Check that the already contracted function only
                            ;; has one contract
                            (has-num-contracts? f 1 1)
                            (f 1)))
                          'pos 'neg)))
  (contract-eval
   '(define f2 (add-many-contracts 11 pos->pos->pos
                         (lambda (f)
                           (unless (has-contract? f)
                             (error "f2 should already be contracted"))
                           ;; Check that the already contracted function only
                           ;; has one contract
                           (has-num-contracts? f 1 1)
                           (f -1))
                         'pos 'neg)))

  (test/spec-failed
   'collapsible11
   '(f1 guarded)
   "positive")
  (test/spec-failed
   'collapsible12
   '(f2 guarded)
   'pos)
  ;; check whether it has a contract (but not a collapsible wrapper)
  (test-false
   'collapsible12.5
   '(collapsible->? (value-contract guarded)))
  ;; checking normal blame
  (test/spec-failed
   'collapsible13
   '(guarded -34)
   "negative")
  (test/spec-failed
   'collapsible14
   '(guarded 34)
   "positive")

  (contract-eval
   '(define guarded-twice (add-many-contracts 11 pos->pos guarded 'positive2 'negative2)))
  ;; Reapplying the same contract over the already contracted function
  (test-true
   'collapsible14.5
   '(has-contract? guarded-twice))
  ;; Outer wrapper should be applied first for the domain
  (test/spec-failed
   'collapsible15
   '(guarded-twice -34)
   "negative2")
  ;; Inner wrapper should be applied first for the range
  (test/spec-failed
   'collapsible16
   '(guarded-twice 34)
   "positive")
  ;; Get the domain and range contract from the twice contracted function
  (test/spec-passed
   'collapsible16.1
   '(has-num-contracts? guarded-twice 1 1))
  (test-true
   'collapsible16.5
   '(collapsible? guarded-twice))

  (contract-eval
   '(define (contract-times f c n)
      (if (= n 0)
          f
          (contract-times (contract c f 'positive 'negative) c (- n 1)))))

  (test/spec-passed
   'arrow-false-contracts
   '(let* ([f (lambda (x) x)]
           [ctc (-> #f #f)] ;; defeat opt/c rewriting
           [cf1 (add-many-contracts 11 ctc f 'pos 'neg)]
           [cf2 (contract ctc cf1 'pos 'neg)]
           [cf3 (contract ctc cf2 'pos 'neg)]
           [cf4 (contract ctc cf3 'pos 'neg)])
      (has-num-contracts? cf4 1 1)))

  (test/spec-passed
   'arrow-many-false-contracts
   '(let ([ctc (-> #f #f)])
      (has-num-contracts? (contract-times (lambda (x) x) ctc 1000) 1 1)))

  ;; Apply the contract 1000 times
  (contract-eval
   '(define insanely-contracted (contract-times guarded-twice pos->pos 1000)))
  (test/spec-passed
   'collapsible-wrap0
   '(has-num-contracts? insanely-contracted 1 1))
  ;; not actually doubly-wrapped

  (contract-eval
   '(define (double-wrapped? x)
      (define prop (get-impersonator-prop:collapsible x #f))
      (and
       (collapsible-wrapper-property? prop)
       (and (has-impersonator-prop:collapsible?
             (collapsible-wrapper-property-checking-wrapper prop))
            ;; this is annoying because of how unsafe-chaperones ...
            ;; work in relation to impersonator-properties
            (collapsible-wrapper-property?
             (get-impersonator-prop:collapsible
              (collapsible-wrapper-property-checking-wrapper prop)
              #f))))))

  (test-false
   'collapsible-wrap1
   '(double-wrapped? insanely-contracted))
  (test-true
   'collapsible-wrap2
   '(collapsible? insanely-contracted))

  (test-false
   'collapsible-wrap3
   '(double-wrapped? (id add1)))
  (test-true
   'collapsible-wrap4
   '(collapsible? (id add1)))
  (test-false
   'collapsible-wrap5
   '(double-wrapped? (id (id add1))))
  (test-true
   'collapsible-wrap6
   '(collapsible? (id (id add1))))
  (test-false
   'collapsible-wrap7
   '(double-wrapped? (id (id (id add1)))))
  (test-true
   'collapsible-wrap7
   '(collapsible? (id (id (id add1)))))

  ;; test relying on contract-stronger?
  (contract-eval '(define r-i (contract (-> integer? any/c)
                                        (add-many-contracts 11 (-> integer? integer?)
                                                  add1
                                                  'inner-pos 'inner-neg)
                                        'pos 'neg)))
  (contract-eval '(define r-i2 (contract (-> integer? any/c)
                                         (add-many-contracts 11 (-> integer? integer?)
                                                   (lambda (x) 'a)
                                                   'inner-pos 'inner-neg)
                                         'pos 'neg)))
  (test/spec-passed
   'collapsible-stronger-num1
   '(has-num-contracts? r-i 1 1))
  (test-true
   'collapsible-stronger1
   '(collapsible? r-i))
  (test/spec-passed
   'collapsible-stronger-num2
   '(has-num-contracts? r-i2 1 1))
  (test-true
   'collapsible-stronger2
   '(collapsible? r-i2))
  (test/spec-passed
   'collapsible17
   '(r-i 1))
  (test/spec-failed
   'collapsible18
   '(r-i 'a)
   'neg)
  (test/spec-failed
   'collapsible19
   '(r-i2 1)
   "inner-pos")
  (contract-eval '(define i-r (contract (-> integer? integer?)
                                        (add-many-contracts 11 (-> integer? any/c)
                                                  add1
                                                  'inner-pos 'inner-neg)
                                        'pos 'neg)))
  (contract-eval '(define i-r2 (contract (-> integer? integer?)
                                         (add-many-contracts 11 (-> integer? any/c)
                                                   (lambda (x) 'a)
                                                   'inner-pos 'inner-neg)
                                         'pos 'neg)))
  ;; can't collapse those. any/c must still be checked before integer? on the
  ;; way out, otherwise may blame wrong
  (test/spec-passed
   'collapsible-stronger-num3
   '(has-num-contracts? i-r 1 2))
  (test/spec-passed
   'collapsible-stronger-num4
   '(has-num-contracts? i-r2 1 2))
  (test-true
   'collapsible-stronger3
   '(collapsible? i-r))
  (test-true
   'collapsible-stronger4
   '(collapsible? i-r2))
  (test/spec-passed
   'collapsible20
   '(i-r 1))
  (test/spec-failed
   'collapsible21
   '(i-r 'a)
   'neg)
  (test/spec-failed
   'collapsible22
   '(i-r2 1)
   'pos)

  ;; test mixing chaperone and impersonator contracts
  (contract-eval
   '(define c1 ; this is an impersonator contract
      (make-contract
       #:name 'c1
       #:val-first-projection
       (lambda (blame)
         (lambda (x)
           (lambda (neg-party)
             (unless (integer? x)
               (raise-blame-error (blame-add-missing-party blame neg-party) x "eh"))
             (add1 x))))))) ; does not respect the chaperone property
  (contract-eval
   '(define c2 ; this is an chaperone contract
      (make-chaperone-contract
       #:name 'c2
       #:val-first-projection
       (lambda (blame)
         (lambda (x)
           (lambda (neg-party)
             (unless (integer? x)
               (raise-blame-error (blame-add-missing-party blame neg-party) x "eh"))
             x))))))
  (contract-eval
   '(define (can-combine? val ctc)
      (define cv (contract ctc val 'p 'n))
      (and (collapsible? val)
           (collapsible? cv))))

  (contract-eval '(define ic
                    (contract (-> (-> c1 c1) (-> c1 c1))
                              (add-many-contracts 11 (-> (-> c2 c2) (-> c2 c2))
                                        (lambda (x) x)
                                        'inner-pos 'inner-neg)
                              'pos 'neg)))
  (contract-eval '(define iic
                    (add-many-contracts 11 (-> (-> c1 c1) (-> c1 c1))
                              ic
                              'outer-pos 'outer-neg)))

  (contract-eval
   '(define imp-add1
      (impersonate-procedure add1 (lambda (x) x))))
  (contract-eval
   '(define chap-add1
      (chaperone-procedure add1 (lambda (x) x))))

  (test-true
   'collapsible-imps-on-underlying-chap
   '(collapsible? (add-many-contracts 11 (-> c1 c1) chap-add1)))
  (test-true
   'collapsible-chaps-on-underlying-chap
   '(collapsible? (add-many-contracts 11 (-> c2 c2) chap-add1)))
  (test-true
   'collapsible-imps-on-underlying-imp
   '(collapsible? (add-many-contracts 11 (-> c1 c1) imp-add1)))
  (test-true
   'collapsible-chaps-on-underlying-imp
   '(collapsible? (add-many-contracts 11 (-> c2 c2) imp-add1)))

  (test-false
   'collapsible-chap+imp1
   '(can-combine? ic (-> c1 c1))) ; can collapse impersonators, the inner chaperone is not chaperone*
  (test-false
   'collapsible-chap+imp2
   '(can-combine? ic (-> c2 c2)))
  (test-false
   'collapsible-chap+imp3
   '(can-combine? iic (-> c1 c1))) ; see above
  (test-false
   'collapsible-chap+imp4
   '(can-combine? iic (-> c2 c2)))
  (test/spec-passed
   'collapsible23
   '((iic add1) 1))

  (contract-eval '(define cc
                    (contract (-> (-> c2 c2) (-> c2 c2))
                              (add-many-contracts 11 (-> (-> c2 c2) (-> c2 c2))
                                        (lambda (x) x)
                                        'inner-pos 'inner-neg)
                              'pos 'neg)))
  (contract-eval '(define icc
                    (add-many-contracts 11 (-> (-> c1 c1) (-> c1 c1))
                              cc
                              'outer-pos 'outer-neg)))
  (test-true
   'collapsible-chap+imp5
   '(collapsible? cc))
  (test-false
   'collapsible-chap+imp6
   '(can-combine? cc (-> c1 c1)))
  (test-true
   'collapsible-chap+imp7
   '(can-combine? cc (-> c2 c2)))
  (test-false
   'collapsible-chap+imp8
   '(can-combine? icc (-> c1 c1)))
  (test-false
   'collapsible-chap+imp9
   '(can-combine? icc (-> c2 c2)))
  (test/spec-passed
   'collapsible24
   '((icc add1) 1))

  (contract-eval '(define ci (contract (-> (-> c2 c2) (-> c2 c2))
                                       (add-many-contracts 11 (-> (-> c1 c1) (-> c1 c1))
                                                 (lambda (x) x)
                                                 'inner-pos 'inner-neg)
                                       'pos 'neg)))
  (contract-eval '(define ici (add-many-contracts 11 (-> (-> c1 c1) (-> c1 c1))
                                        ci
                                        'outer-pos 'outer-neg)))
  (test-false
   'collapsible-chap+imp10
   '(can-combine? ci (-> c1 c1)))
  (test-false
   'collapsible-chap+imp11
   '(can-combine? ci (-> c2 c2))) ; it's impersonated before the `cc`, but not impersonator*, sook
  (test-false
   'collapsible-chap+imp12
   '(can-combine? ici (-> c1 c1))) ; ditto
  (test-false
   'collapsible-chap+imp13
   '(can-combine? ici (-> c2 c2)))
  (test/spec-passed
   'collapsible25
   '((ici add1) 1))

  (test/spec-passed
   'collapsible25.5
   ;; using `contract` explicitly, to trigger double-wrapping rewrite
   ;; (that changed something! (but it shouldn't, so it's a bug!))
   '(((contract (-> (-> c1 c1) (-> c1 c1))
                (contract (-> (-> c2 c2) (-> c2 c2))
                          (add-many-contracts 11 (-> (-> c1 c1) (-> c1 c1))
                                    (lambda (x) x)
                                    'inner-pos 'inner-neg)
                          'pos 'neg)
                'outer-pos 'outer-neg)
      add1)
     1))

  (contract-eval '(define cic
                    (add-many-contracts 11 (-> (-> c2 c2) (-> c2 c2))
                              ci
                              'outer-pos 'outer-neg)))
  (test-false
   'collapsible-chap+imp14
   '(can-combine? cic (-> c1 c1)))
  (test-false
   'collapsible-chap+imp15
   '(can-combine? cic (-> c2 c2)))
  (test/spec-passed
   'collapsible26
   '((cic add1) 1))

  ;; can we get collapsible wrappers for impersonator contracts?
  (contract-eval
   '(define imp-imp (contract (-> c1 c1)
                              (add-many-contracts 11 (-> c1 c1) (lambda (x) x) 'pos 'neg)
                              'pos 'neg)))
  (test-true
   'collapsible-imp1
   '(collapsible? imp-imp))
  (test/spec-passed
   'collapsible27
   '(imp-imp 1))
  (test/spec-failed
   'collapsible27f
   '(imp-imp 'a)
   'neg)
  ;; should be an impersonator contract
  (test-false
   'collapsible-imp2
   '(chaperone-contract? (value-contract imp-imp)))


  (contract-eval '(define mix1 (contract (-> any/c any/c)
                                         (add-many-contracts 11 (-> (-> integer? integer?)
                                                       (-> integer? integer?))
                                                   (lambda (x) x)
                                                   'pos 'neg)
                                         'pos 'neg)))
  (contract-eval '(define mix2 (contract (-> (-> integer? integer?)
                                             (-> integer? integer?))
                                         (add-many-contracts 11 (-> any/c any/c)
                                                   (lambda (x) x)
                                                   'pos 'neg)
                                         'pos 'neg)))
  (test-true
   'collapsible-flat-h/o-mix1
   '(collapsible? mix1))
  (test/spec-passed
   'collapsible-flat-h/o-mix2
   '((mix1 add1) 2))
  (test-true
   'collapsible-flat-h/o-mix3
   '(collapsible? mix2))
  (test/spec-passed
   'collapsible-flat-h/o-mix5
   '((mix2 add1) 2))
  (test/neg-blame
   'collapsible-flat-h/o-mix6
   '((mix1 add1) 'a))
  (test/neg-blame
   'collapsible-flat-h/o-mix7
   '((mix1 number->string) 2))
  (test/neg-blame
   'collapsible-flat-h/o-mix8
   '((mix2 add1) 'a))
  (test/neg-blame
   'collapsible-flat-h/o-mix9
   '((mix2 number->string) 2))

  ;; only the outer contract matters for these tests, as the inner one is fully
  ;; checked before we enter collapsible mode
  (test/pos-blame
   'collapsible-first-order-checks1
   '(contract (-> any/c)
              (add-many-contracts 11 (-> any/c any/c) add1 'inner-pos 'inner-neg)
              'pos 'neg))
  (test/pos-blame
   'collapsible-first-order-checks2
   '((contract (-> (-> any/c))
               (add-many-contracts 11 (-> (-> any/c any/c))
                         (lambda () add1)
                         'inner-pos 'inner-neg)
               'pos 'neg)))
  (test/pos-blame
   'collapsible-first-order-checks3
   '((contract (-> (-> any/c any/c))
               (add-many-contracts 11 (-> (-> any/c))
                         (lambda () add1)
                         'pos 'neg)
               'outer-pos 'outer-neg)))
  (test/pos-blame
   'collapsible-first-order-checks4
   '((contract (-> (-> any/c any/c))
               (contract (-> (-> any/c))
                         (add-many-contracts 11 (-> (-> any/c any/c any/c))
                                   (lambda () add1)
                                   'pos 'neg)
                         'mid-pos 'mid-neg)
               'outer-pos 'outer-neg)))
  (test/pos-blame
   'collapsible-first-order-checks5
   '((contract (-> (-> any/c any/c))
               (contract (-> (-> any/c))
                         (add-many-contracts 11 (-> (-> any/c any/c any/c))
                                   (contract (-> (-> any/c any/c)) ; to have next one be collapsible
                                             (lambda () add1)
                                             'inner-pos 'inner-neg)
                                   'pos 'neg)
                         'mid-pos 'mid-neg)
               'outer-pos 'outer-neg)))
  (test/pos-blame
   'collapsible-first-order-checks6
   '((contract (-> (-> any/c any/c))
               (contract (-> (-> any/c))
                         (add-many-contracts 11 (-> (-> any/c any/c))
                                   (contract (-> (-> any/c any/c)) ; to have next one be collapsible
                                             (lambda () add1)
                                             'innermost-pos 'innermost-neg)
                                   'inner-pos 'inner-neg)
                         'pos 'neg)
               'outer-pos 'outer-neg)))

  (test/neg-blame
   'collapsible-first-order-checks7
   ;; both should fail, but want to make sure we drop the right redundant check
   '((contract (-> (-> any/c any/c any/c) any)
               (add-many-contracts 11 (-> (-> any/c any/c any/c) any)
                         (lambda (x) x)
                         'inner-pos 'inner-neg)
               'pos 'neg)
     add1))

  ;; scenario: double-wrap (enter collapsible mode), unrelated chaperone, another contract
  ;; want to make sure no check gets lost
  (test/pos-blame
   'collapsible-chaperone-in-middle
   '(let ([x 0])
      (define f (contract (-> any/c string?)
                          (add-many-contracts 11 (-> any/c string?)
                                    (lambda (x) x)
                                    'pos 'neg)
                          'mid-pos 'mid-neg))
      (define f2 (chaperone-procedure f (lambda (y) y)))
      ((add-many-contracts 11 (-> any/c integer?)
                 f2
                 'outer-pos 'outer-neg)
       4)))

  (test-true
   'collapsible-bail-on-subcontract1
   ;; Contracts lifted to defeat the opt/c rewriting
   '(let ([ctc1 (-> (-> any/c (values any/c any/c)) any/c)]
          [ctc2 (-> (-> any/c (values any/c any/c)) any/c)])
      (collapsible?
       (contract ctc1
                 (add-many-contracts 11 ctc2
                           (lambda (x) x)
                           'pos 'neg)
                 'pos 'neg))))
  (test-true
   'collapsible-bail-on-subcontract2
   ;; Contracts lifted to defeat the opt/c rewriting
   '(let ([ctc1 (-> any/c (-> any/c (values any/c any/c)) any/c)]
          [ctc2 (-> any/c (-> any/c (values any/c any/c)) any/c)])
      (collapsible?
       (contract ctc1
                 (add-many-contracts 11 ctc2
                           (lambda (x y) x)
                           'pos 'neg)
                 'pos 'neg))))
  (test-true
   'collapsible-bail-on-subcontract3
   ;; Contracts lifted to defeat the opt/c rewriting
   '(let ([ctc1 (-> any/c (-> any/c (values any/c any/c)))]
          [ctc2 (-> any/c (-> any/c (values any/c any/c)))])
      (collapsible?
       (contract ctc1
                 (add-many-contracts 11 ctc2
                           (lambda (x) x)
                           'pos 'neg)
                 'pos 'neg))))

  (test/neg-blame
   'collapsible-merge-subcontract1
   '(let ()
      (define id (contract (-> (-> string? string?) (-> string? string?))
                           (add-many-contracts 11 (-> (-> string? string?) (-> string? string?))
                                     (lambda (x) x)
                                     'p1 'n1)
                           'p2 'n2))
      (define a1 (add-many-contracts 11 (-> integer? integer?)
                           add1
                           'pos 'neg))
      ((id a1) "a")))
  (test/neg-blame
   'collapsible-merge-subcontract2
   '(let ()
      (define id (contract (-> (-> string? string?) (-> string? string?))
                           (add-many-contracts 11 (-> (-> string? string?) (-> string? string?))
                                     (lambda (x) x)
                                     'p1 'n2)
                           'pos 'neg))
      (define a1 (add-many-contracts 11 (-> integer? integer?)
                           (lambda (x) x)
                           'p3 'n3))
      ((id a1) 1)))
  (test/spec-passed
   'collapsible-merge-subcontract3
   '(let ()
      ;; lift definitions to defeat the opt/c rewriting
      ;; (otherwise that bypasses the whole collapsible machinery)
      (define ctc1 (-> (-> string? string?) (-> string? string?)))
      (define ctc2 (-> string? string?))
      (define id (contract ctc1
                           (add-many-contracts 11 ctc1
                                     (lambda (x) x)
                                     'pos 'neg)
                           'p2 'n2))
      (define a1 (add-many-contracts 11 ctc2
                           (lambda (x) x)
                           'p3 'n3))
      (has-num-contracts? (id a1) 1 1)))
  (test/neg-blame
   'collapsible-merge-subcontract4
   '(let ()
      (define id (contract (-> (-> string? string?) (-> string? string?))
                           (add-many-contracts 11 (-> (-> string? string?) (-> string? string?))
                                     (lambda (x) x)
                                     'p1 'n1)
                           'p2 'n2))
      (define a1 (contract (-> integer? integer?)
                           (add-many-contracts 11 (-> integer? integer?)
                                     add1
                                     'p3 'n3)
                           'pos 'neg))
      ((id a1) "a")))
  (test/neg-blame
   'collapsible-merge-subcontract5
   '(let ()
      (define id (contract (-> (-> string? string?) (-> string? string?))
                           (add-many-contracts 11 (-> (-> string? string?) (-> string? string?))
                                     (lambda (x) x)
                                     'p1 'n1)
                           'pos 'neg))
      (define a1 (contract (-> integer? integer?)
                           (add-many-contracts 11 (-> integer? integer?)
                                     (lambda (x) x)
                                     'p3 'n3)
                           'p4 'n4))
      ((id a1) 1)))
  (test/spec-passed
   'collapsible-merge-subcontract6
   '(let ()
      ;; lift definitions to defeat the opt/c rewriting
      ;; (otherwise that bypasses the whole collapsible machinery)
      (define ctc1 (-> (-> string? string?) (-> string? string?)))
      (define ctc2 (-> string? string?))
      (define id (contract ctc1
                           (add-many-contracts 11 ctc1
                                     (lambda (x) x)
                                     'pos 'neg)
                           'p2 'n2))
      (define a1 (contract ctc2
                           (add-many-contracts 11 ctc2
                                     (lambda (x) x)
                                     'p3 'n3)
                           'p4 'n4))
      (has-num-contracts? (id a1) 1 1)))

  (test/spec-passed
   'collapsible-multi-args1
   '((contract (-> number? string? number?)
               (add-many-contracts 11 (-> number? string? number?)
                         (lambda (x y) x)
                         'pos 'neg)
               'outer-pos 'outer-neg)
     1 "a"))
  (test/pos-blame
   'collapsible-multi-args2
   '((contract (-> number? string? number?)
               (add-many-contracts 11 (-> number? string? number?)
                         (lambda (x) x)
                         'pos 'neg)
               'outer-pos 'outer-neg)
     1 "a"))
  (test/neg-blame
   'collapsible-multi-args3
   '((contract (-> number? string? number?)
               (add-many-contracts 11 (-> number? string? number?)
                         (lambda (x y) x)
                         'inner-pos 'inner-neg)
               'pos 'neg)
     "a" "a"))
  (test/neg-blame
   'collapsible-multi-args4
   '((contract (-> number? string? number?)
               (add-many-contracts 11 (-> number? string? number?)
                         (lambda (x y) x)
                         'inner-pos 'inner-neg)
               'pos 'neg)
     1 1))
  (test/spec-passed/result
   'collapsible-multi-args5
   '(with-handlers ([exn:fail:contract:arity? (lambda (e) 'ok)])
      ((contract (-> number? string? number?)
                 (add-many-contracts 11 (-> number? string? number?)
                           (lambda (x y) x)
                           'inner-pos 'inner-neg)
                 'pos 'neg)
       1 1 "a"))
   'ok)
  (test/spec-passed/result
   'collapsible-multi-args6
   '(with-handlers ([exn:fail:contract:arity? (lambda (e) 'ok)])
      ((contract (-> number? string? number?)
                 (add-many-contracts 11 (-> number? string? number?)
                           (lambda (x y) x)
                           'inner-pos 'inner-neg)
                 'pos 'neg)
       1))
   'ok)
  (test/spec-passed
   'collapsible-multi-args7
   '(collapsible? (contract (-> number? string? number?)
                                (add-many-contracts 11 (-> number? string? number?)
                                          (lambda (x y) x)
                                          'inner-pos 'inner-neg)
                                'pos 'neg)))

  (contract-eval '(require racket/class))

  (test/spec-passed
   'object/c-->-pass/no-bail
   '(let* ([grid/c (-> (-> (object/c)))]
           [o (new object%)]
           [v (lambda () o)]
           [grid (contract
                  grid/c
                  (add-many-contracts 11
                   grid/c
                   (lambda () v)
                   'inner-pos 'inner-neg)
                  'pos 'neg)])
      ((grid))))

  (test/spec-failed
   'object/c-->-fail/should-bail
   '(let* ([v (add-many-contracts 11 (-> integer?) (lambda () 1) 'p 'n)]
           [grid (contract
                  (-> (-> (object/c)))
                  (add-many-contracts 11
                   (-> (-> (object/c)))
                   (lambda () v)
                   'inner-pos 'inner-neg)
                  'pos 'neg)])
      ((grid)))
   "inner-pos")

  ;; arrow and vector contracts
  (test/spec-failed
   'arrow+vector
   '(let* ([ctc (-> (vectorof integer?))]
           [f (contract ctc
                        (add-many-contracts 11 ctc (lambda () (vector 1)) 'inner-pos 'inner-neg)
                        'pos 'neg)])
      (vector-set! (f) 0 1.5))
   "neg")

  ;; arrow and box
  (test/spec-failed
   'arrow+box
   '(let* ([ctc (-> (box/c integer?))]
           [f (contract ctc (add-many-contracts 11 ctc (lambda () (box 1)) 'inner-pos 'inner-neg) 'pos 'neg)])
      (set-box! (f) 1.5))
   "neg")

  (test/spec-failed
   'arrow-symbol-multi-pos1
   '(let* ([ctc1 (-> integer? (-> symbol? symbol?))]
           [ctc2 (-> integer? symbol?)]
           [f (lambda (x) (lambda (y) y))]
           [cf (contract ctc2 (add-many-contracts 11 ctc1 f 'inner-pos 'inner-neg) 'pos 'neg)])
      (cf 0))
   "pos")

  (test/spec-failed
   'arrow-symbol-multi-pos2
   '(let* ([ctc1 (-> integer? (-> symbol? symbol?))]
           [ctc2 (-> integer? symbol?)]
           [f (lambda (x) 'foo)]
           [cf (contract ctc2 (add-many-contracts 11 ctc1 f 'inner-pos 'inner-neg) 'pos 'neg)])
      (cf 0))
   "inner-pos")

  (test/spec-failed
   'arrow-symbol-multi-neg1
   '(let* ([ctc1 (-> symbol? integer?)]
           [ctc2 (-> (-> symbol? symbol?) integer?)]
           [f (lambda (x) 0)]
           [cf (contract ctc2 (add-many-contracts 11 ctc1 f 'inner-pos 'inner-neg) 'pos 'neg)])
      (cf (lambda (x) x)))
   "inner-neg")

  (test/spec-failed
   'arrow-symbol-multi-neg2
   '(let* ([ctc1 (-> symbol? integer?)]
           [ctc2 (-> (-> symbol? symbol?) integer?)]
           [f (lambda (x) 0)]
           [cf (contract ctc2 (add-many-contracts 11 ctc1 f 'inner-pos 'inner-neg) 'pos 'neg)])
      (cf 'foo))
   "neg")

  (test/spec-passed/result
   'calculate-drops-1
   '(let* ([ctc1 (coerce-contract/f integer?)]
           [ctc2 (coerce-contract/f string?)]
           [ctcs (list ctc1 ctc2 ctc1 ctc2 ctc1)])
      (calculate-drops ctcs))
   '(2))

  (test/spec-passed/result
   'calculate-drops-2
   '(let* ([ctc1 (coerce-contract/f integer?)]
           [ctcs (list ctc1 ctc1 ctc1 ctc1 ctc1)])
      (calculate-drops ctcs))
   '(3 2 1))

  (test/spec-passed/result
   'calculate-drops-2
   '(let* ([ctc1 (coerce-contract/f (object/c))]
           [ctcs (list ctc1 ctc1 ctc1 ctc1 ctc1)])
      (calculate-drops ctcs))
   '())

  (test/spec-passed/result
   'calculate-drops-3
   '(let* ([ctc1 (coerce-contract/f integer?)]
           [ctc2 (coerce-contract/f string?)]
           [ctcs (list ctc1 ctc2 ctc1 ctc2 ctc1)])
      (calculate-drops ctcs))
   '(2))

  (test/spec-passed/result
   'calculate-drops-4
   '(let* ([c1 (coerce-contract/f integer?)]
           [c2 (coerce-contract/f (vectorof integer?))]
           [c3 (coerce-contract/f (-> integer? integer?))]
           [ctcs (list c1 c2 c3 c2 c3 c1 c3 c2 c1)])
      (calculate-drops ctcs))
   '(5 3 4))

  (test/spec-passed/result
   'calculate-drops-5
   '(let* ([c1 (coerce-contract/f integer?)]
           [c2 (coerce-contract/f (vectorof integer?))]
           [c3 (coerce-contract/f (-> integer? integer?))]
           [c4 (coerce-contract/f (object/c))]
           [ctcs (list c1 c2 c3 c4 c4 c2 c3 c1 c3 c2 c4 c1 c4)])
      (calculate-drops ctcs))
   '(7 5 6))

  (test/spec-passed/result
   'calculate-drops-6
   '(let* ([c1 (coerce-contract/f integer?)]
           [ctcs (list c1 c1 c1 c1 c1 c1 c1 c1 c1)])
      (calculate-drops ctcs))
   '(7 6 5 4 3 2 1))
  )
