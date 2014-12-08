#lang racket/base
(require "test-util.rkt")

(parameterize ([current-contract-namespace
                (make-basic-contract-namespace
                 'racket/contract)])
(contract-eval '(define-contract-struct couple (hd tl)))
(contract-eval '(contract-struct triple (a b c)))

(test/spec-passed
 'd-c-s-match1
 '(begin
    (eval '(module d-c-s-match1 scheme/base
             (require scheme/contract
                      mzlib/match)
             
             (define-contract-struct foo (bar baz))
             
             (void
              (match (make-foo #t #f)
                [($ foo bar baz) #t]
                [_ #f]))))
    (eval '(require 'd-c-s-match1))))

(test/spec-passed/result
 'd-c-s-match2
 '(begin
    (eval '(module d-c-s-match2 scheme/base
             (require scheme/contract
                      mzlib/match)
             
             (define-contract-struct foo (bar baz))
             
             (provide d-c-s-match2-f1)
             (define d-c-s-match2-f1
               (match (make-foo 'first 'second)
                 [($ foo bar baz) (list bar baz)]
                 [_ #f]))))
    (eval '(require 'd-c-s-match2))
    (eval 'd-c-s-match2-f1))
 '(first second))

(test/spec-passed/result
 'd-c-s-match3
 '(begin
    (eval '(module d-c-s-match3-a scheme/base
             
             (require scheme/contract)
             
             (define-struct super (a b c) #:transparent)
             (define-struct (sub super) () #:transparent)
             
             (provide/contract
              [struct super       ([a number?] [b number?] [c number?])]
              [struct (sub super) ([a number?] [b number?] [c number?])])))
    (eval '(module d-c-s-match3-b scheme/base
             (require scheme/match)
             
             (require 'd-c-s-match3-a)
             
             (provide d-c-s-match3-ans)
             (define d-c-s-match3-ans
               (match (make-sub 1 2 3)
                 [(struct sub (a b c))
                  (list a b c)]))))
    (eval '(require 'd-c-s-match3-b))
    (eval 'd-c-s-match3-ans))
 '(1 2 3))

(test/pos-blame 'd-c-s1
                '(begin
                   (eval '(module d-c-s1 scheme/base
                            (require scheme/contract)
                            (define-contract-struct couple (hd tl))
                            (contract (couple/c any/c any/c) 1 'pos 'neg)))
                   (eval '(require 'd-c-s1))))

(test/spec-passed 'd-c-s2
                  '(contract (couple/c any/c any/c) (make-couple 1 2) 'pos 'neg))
(test/spec-passed 'd-c-s3
                  '(contract (couple/c number? number?)
                             (make-couple 1 2)
                             'pos 'neg))
(test/spec-passed 'd-c-s4
                  '(couple-hd
                    (contract (couple/c number? number?)
                              (make-couple 1 2)
                              'pos 'neg)))
(test/spec-passed 'd-c-s5
                  '(couple-tl
                    (contract (couple/c number? number?)
                              (make-couple 1 2)
                              'pos 'neg)))


(test/pos-blame
 'd-c-s6
 '(couple-tl
   (contract (couple/c number?
                       number?)
             (make-couple #f 2)
             'pos 'neg)))
(test/pos-blame
 'd-c-s7
 '(couple-hd
   (contract (couple/c number? number?)
             (make-couple #f 2)
             'pos 'neg)))

(test/pos-blame
 'd-c-s8
 '(contract (couple/dc [hd any/c] [tl any/c])
            1
            'pos 'neg))

(test/pos-blame
 'd-c-s9
 '(contract (couple/dc [hd () any/c] [tl () any/c])
            1
            'pos 'neg))


(test/spec-passed 'd-c-s10
                  '(contract (couple/dc [hd any/c] [tl any/c])
                             (make-couple 1 2)
                             'pos 'neg))
(test/spec-passed 'd-c-s11
                  '(contract (couple/dc [hd () any/c] [tl () any/c])
                             (make-couple 1 2)
                             'pos 'neg))

(test/spec-passed 'd-c-s12
                  '(contract (couple/dc [hd number?]
                                        [tl number?])
                             (make-couple 1 2)
                             'pos 'neg))
(test/spec-passed 'd-c-s13
                  '(couple-hd
                    (contract (couple/dc [hd number?]
                                         [tl number?])
                              (make-couple 1 2)
                              'pos 'neg)))
(test/spec-passed 'd-c-s14
                  '(couple-tl
                    (contract (couple/dc [hd number?]
                                         [tl number?])
                              (make-couple 1 2)
                              'pos 'neg)))


(test/pos-blame
 'd-c-s15
 '(couple-hd
   (contract (couple/dc [hd number?]
                        [tl number?])
             (make-couple #f 2)
             'pos 'neg)))

(test/pos-blame
 'd-c-s16
 '(couple-tl
   (contract (couple/dc [hd number?]
                        [tl number?])
             (make-couple #f 2)
             'pos 'neg)))

(test/spec-passed
 'd-c-s17
 '(couple-hd
   (contract (couple/dc [hd number?]
                        [tl (hd) (>=/c hd)])
             (make-couple 1 2)
             'pos 'neg)))

(test/pos-blame
 'd-c-s18
 '(couple-hd
   (contract (couple/dc [hd number?]
                        [tl (hd) (>=/c hd)])
             (make-couple 2 1)
             'pos 'neg)))

(test/spec-passed
 'd-c-s19
 '(couple-tl
   (couple-tl
    (contract (couple/dc [hd number?]
                         [tl (hd)
                             (let ([hd1 hd])
                               (couple/dc [hd (>=/c hd1)]
                                          [tl (hd) (>=/c hd)]))])
              (make-couple 1 (make-couple 2 3))
              'pos 'neg))))

(test/pos-blame
 'd-c-s20
 '(couple-tl
   (couple-tl
    (contract (couple/dc [hd number?]
                         [tl (hd)
                             (let ([hd1 hd])
                               (couple/dc [hd (>=/c hd1)]
                                          [tl (hd) (>=/c hd1)]))])
              (make-couple 1 (make-couple 2 0))
              'pos 'neg))))

(test/spec-passed
 'd-c-s21
 '(couple-hd
   (contract (couple/dc [hd number?]
                        [tl number?])
             (contract (couple/dc [hd number?]
                                  [tl number?])
                       (make-couple 1 2)
                       'pos 'neg)
             'pos 'neg)))

(test/spec-passed
 'd-c-s22
 '(couple-hd
   (contract (couple/dc [hd (>=/c 0)]
                        [tl (>=/c 0)])
             (contract (couple/dc [hd number?]
                                  [tl number?])
                       (make-couple 1 2)
                       'pos 'neg)
             'pos 'neg)))

(test/pos-blame
 'd-c-s23
 '(couple-hd
   (contract (couple/dc [hd (>=/c 0)]
                        [tl (>=/c 0)])
             (contract (couple/dc [hd number?]
                                  [tl number?])
                       (make-couple -1 2)
                       'pos 'neg)
             'pos 'neg)))

(test/pos-blame
 'd-c-s24
 '(couple-hd
   (contract (couple/dc [hd number?]
                        [tl number?])
             (contract (couple/dc [hd (>=/c 0)]
                                  [tl (>=/c 0)])
                       (make-couple -1 2)
                       'pos 'neg)
             'pos 'neg)))

(test/pos-blame
 'd-c-s25
 '(couple-hd
   (contract (couple/dc [hd number?]
                        [tl number?])
             (contract (couple/dc [hd number?]
                                  [tl number?])
                       (contract (couple/dc [hd (>=/c 0)]
                                            [tl (>=/c 0)])
                                 (make-couple -1 2)
                                 'pos 'neg)
                       'pos 'neg)
             'pos 'neg)))

(test/pos-blame
 'd-c-s26
 '(couple-hd
   (contract (couple/dc [hd (>=/c 10)]
                        [tl (>=/c 10)])
             (contract (couple/dc [hd positive?]
                                  [tl positive?])
                       (contract (couple/dc [hd number?]
                                            [tl number?])
                                 (make-couple 1 2)
                                 'pos 'neg)
                       'pos 'neg)
             'pos 'neg)))


;; test caching
(test/spec-passed
 'd-c-s27
 '(let ([ctc (couple/c any/c any/c)])
    (couple-hd (contract ctc (contract ctc (make-couple 1 2) 'pos 'neg) 'pos 'neg))))

;; make sure lazy really is lazy
(test/spec-passed
 'd-c-s28
 '(contract (couple/c number? number?)
            (make-couple #f #f)
            'pos 'neg))

(test/spec-passed
 'd-c-s29
 '(couple-hd
   (contract (couple/c (couple/c number? number?)
                       (couple/c number? number?))
             (make-couple (make-couple #f #f)
                          (make-couple #f #f))
             'pos 'neg)))

(test/spec-passed
 'd-c-s30
 '(couple-tl
   (contract (couple/c (couple/c number? number?)
                       (couple/c number? number?))
             (make-couple (make-couple #f #f)
                          (make-couple #f #f))
             'pos 'neg)))

;; make sure second accesses work
(test/spec-passed/result
 'd-c-s31
 '(let ([v (contract (couple/c number? number?)
                     (make-couple 1 2)
                     'pos 'neg)])
    (list (couple-hd v) (couple-hd v)))
 (list 1 1))

(test/pos-blame
 'd-c-s32
 '(let ([v (contract (couple/c number? boolean?)
                     (make-couple 1 2)
                     'pos 'neg)])
    (with-handlers ([void void]) (couple-hd v))
    (couple-hd v)))

(test/pos-blame
 'd-c-s33
 '(let ([v (contract (couple/c number? number?)
                     (make-couple 1 2)
                     'pos 'neg)])
    (couple-hd v)
    (couple-hd v)
    (couple-hd
     (contract (couple/c boolean? boolean?)
               v
               'pos 'neg))))

(contract-eval '(define-contract-struct single (a)))
;; a related test to the above:
(test/spec-passed/result
 'd-c-s34
 '(let ([v (contract (single/c number?) (make-single 1) 'pos 'neg)])
    (single-a v)
    (let ([v3 (contract (single/c number?) v 'pos 'neg)])
      (single-a v3)))
 1)

;; make sure the caching doesn't break the semantics
(test/pos-blame
 'd-c-s35
 '(let ([v (contract (couple/c any/c
                               (couple/c any/c
                                         (couple/c any/c
                                                   number?)))
                     (make-couple 1
                                  (make-couple 2
                                               (make-couple 3
                                                            #f)))
                     'pos 'neg)])
    (let* ([x (couple-tl v)]
           [y (couple-tl x)])
      (couple-hd (couple-tl x)))))

(test/spec-passed/result
 'd-c-s36
 '(let ([x (make-couple 1 2)]
        [y (make-couple 1 2)]
        [c1 (couple/dc [hd any/c]
                       [tl (hd) any/c])]
        [c2 (couple/c any/c any/c)])
    (couple-hd (contract c1 x 'pos 'neg))
    (couple-hd (contract c2 x 'pos 'neg))
    (couple-hd (contract c2 y 'pos 'neg))
    (couple-hd (contract c1 y 'pos 'neg)))
 1)

;; make sure that define-contract-struct contracts can go at the top level
(test/spec-passed
 'd-c-s37
 '(contract-stronger?
   (couple/dc [hd any/c]
              [tl (hd) any/c])
   (couple/dc [hd any/c]
              [tl (hd) any/c])))

;; test functions inside structs

(test/spec-passed/result
 'd-c-s38
 '(let ([x (make-couple (lambda (x) x) (lambda (x) x))]
        [c (couple/dc [hd (-> integer? integer?)]
                      [tl (hd) any/c])])
    ((couple-hd (contract c x 'pos 'neg)) 1))
 1)

(test/neg-blame
 'd-c-s39
 '(let ([x (make-couple (lambda (x) x) (lambda (x) x))]
        [c (couple/dc [hd (-> integer? integer?)]
                      [tl (hd) any/c])])
    ((couple-hd (contract c x 'pos 'neg)) #f)))

(test/pos-blame
 'd-c-s40
 '(let ([x (make-couple (lambda (x) #f) (lambda (x) #f))]
        [c (couple/dc [hd (-> integer? integer?)]
                      [tl (hd) any/c])])
    ((couple-hd (contract c x 'pos 'neg)) 1)))

(test/spec-passed/result
 'd-c-s41
 '(let ([x (make-couple 5 (lambda (x) x))]
        [c (couple/dc [hd number?]
                      [tl (hd) (-> (>=/c hd) (>=/c hd))])])
    ((couple-tl (contract c x 'pos 'neg)) 6))
 6)

(test/pos-blame
 'd-c-s42
 '(let ([x (make-couple 5 (lambda (x) -10))]
        [c (couple/dc [hd number?]
                      [tl (hd) (-> (>=/c hd) (>=/c hd))])])
    ((couple-tl (contract c x 'pos 'neg)) 6)))

(test/neg-blame
 'd-c-s42
 '(let ([x (make-couple 5 (lambda (x) -10))]
        [c (couple/dc [hd number?]
                      [tl (hd) (-> (>=/c hd) (>=/c hd))])])
    ((couple-tl (contract c x 'pos 'neg)) -11)))

(contract-eval '(contract-struct no-define (x)))
(test/spec-passed/result
 'd-c-s43
 '(no-define-x (no-define 1))
 '1)
(test/spec-passed/result
 'd-c-s44
 '(no-define? (no-define 1))
 '#t)

)