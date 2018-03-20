#lang racket/base
(require "test-util.rkt")

(parameterize ([current-contract-namespace (make-basic-contract-namespace)])
  (define exn:fail:contract:blame? (contract-eval 'exn:fail:contract:blame?))
  
  (test/no-error '(-> integer? integer?))
  (test/no-error '(-> (flat-contract integer?) (flat-contract integer?)))
  (test/no-error '(-> integer? any))
  (test/no-error '(-> (flat-contract integer?) any))
  
  (test/spec-passed
   'contract-arrow-values1
   '(let-values ([(a b) ((contract (-> integer? (values integer? integer?))
                                   (lambda (x) (values x x))
                                   'pos
                                   'neg)
                         2)])
      1))
  
  (test/neg-blame
   'contract-arrow-values2
   '((contract (-> integer? (values integer? integer?))
               (lambda (x) (values x x))
               'pos
               'neg)
     #f))
  
  (test/pos-blame
   'contract-arrow-values3
   '((contract (-> integer? (values integer? integer?))
               (lambda (x) (values 1 #t))
               'pos
               'neg)
     1))
  
  (test/pos-blame
   'contract-arrow-values4
   '((contract (-> integer? (values integer? integer?))
               (lambda (x) (values #t 1))
               'pos
               'neg)
     1))
  
  (test/pos-blame
   'contract-arrow-values5
   '((contract (-> integer? (values integer? integer?))
               (lambda (x) x)
               'pos
               'neg)
     1))
  
  (test/neg-blame
   'contract-arrow-values6
   '(((contract (-> (-> (listof integer?)) any)
                (λ (x) x)
                'pos 'neg)
      (λ () (values 1 2)))))
  
  (test/pos-blame
   'contract-arrow-keyword1
   '(contract (-> integer? any)
              (λ (x #:y y) x)
              'pos
              'neg))
  
  (test/pos-blame
   'contract-arrow-keyword1b
   '(contract (-> integer? #:y integer? any)
              (λ (x) x)
              'pos
              'neg))
  
  (test/spec-passed
   'contract-arrow-keyword2
   '(contract (-> integer? #:y boolean? any)
              (λ (x #:y y) x)
              'pos
              'neg))
  
  (test/spec-passed
   'contract-arrow-keyword2b
   '(contract (-> #:x boolean? #:y boolean? any)
              (λ (#:x x #:y y) x)
              'pos
              'neg))
  
  (test/spec-passed
   'contract-arrow-keyword2c
   '(contract (-> #:y boolean? #:x boolean? any)
              (λ (#:x x #:y y) x)
              'pos
              'neg))
  
  (test/spec-passed
   'contract-arrow-keyword2d
   '(contract (-> #:y boolean? #:x boolean? any)
              (λ (#:y y #:x x) x)
              'pos
              'neg))
  
  (test/spec-passed
   'contract-arrow-keyword2e
   '(contract (-> #:x boolean? #:y boolean?  any)
              (λ (#:y y #:x x) x)
              'pos
              'neg))
  
  (test/neg-blame
   'contract-arrow-keyword3
   '((contract (-> integer? #:y boolean? any)
               (λ (x #:y y) x)
               'pos
               'neg)
     1 #:y 1))
  
  (test/neg-blame
   'contract-arrow-keyword4
   '((contract (-> integer? #:y boolean? any)
               (λ (x #:y y) x)
               'pos
               'neg)
     #t #:y #t))
  
  (test/spec-passed
   'contract-arrow-keyword5
   '((contract (-> integer? #:y boolean? any)
               (λ (x #:y y) x)
               'pos
               'neg)
     1 #:y #t))
  
  (test/pos-blame
   'contract-arrow-keyword6
   '(contract (-> integer? integer?)
              (λ (x #:y y) x)
              'pos
              'neg))
  
  (test/spec-passed
   'contract-arrow-keyword7
   '(contract (-> integer? #:y boolean? integer?)
              (λ (x #:y y) x)
              'pos
              'neg))
  
  (test/neg-blame
   'contract-arrow-keyword8
   '((contract (-> integer? #:y boolean? integer?)
               (λ (x #:y y) x)
               'pos
               'neg)
     1 #:y 1))
  
  (test/neg-blame
   'contract-arrow-keyword9
   '((contract (-> integer? #:y boolean? integer?)
               (λ (x #:y y) x)
               'pos
               'neg)
     #t #:y #t))
  
  (test/spec-passed
   'contract-arrow-keyword10
   '((contract (-> integer? #:y boolean? integer?)
               (λ (x #:y y) x)
               'pos
               'neg)
     1 #:y #t))
  
  (test/pos-blame
   'contract-arrow-keyword11
   '(contract (-> integer? (values integer? integer?))
              (λ (x #:y y) x)
              'pos
              'neg))
  
  (test/spec-passed
   'contract-arrow-keyword12
   '(contract (-> integer? #:y boolean? (values integer? integer?))
              (λ (x #:y y) x)
              'pos
              'neg))
  
  (test/neg-blame
   'contract-arrow-keyword13
   '((contract (-> integer? #:y boolean? (values integer? integer?))
               (λ (x #:y y) x)
               'pos
               'neg)
     1 #:y 1))
  
  (test/neg-blame
   'contract-arrow-keyword14
   '((contract (-> integer? #:y boolean? (values integer? integer?))
               (λ (x #:y y) x)
               'pos
               'neg)
     #t #:y #t))
  
  (test/spec-passed
   'contract-arrow-keyword15
   '((contract (-> integer? #:y boolean? (values integer? integer?))
               (λ (x #:y y) (values x x))
               'pos
               'neg)
     1 #:y #t))
  
  (test/spec-passed
   'contract-arrow-keyword16
   '((contract (-> integer? integer?)
               (λ (x #:y [y #f]) x)
               'pos
               'neg)
     1))
  
  (test/neg-blame
   'contract-arrow-keyword17
   '((contract (-> integer? integer?)
               (λ (x #:y [y #f]) x)
               'pos
               'neg)
     #f))
  
  (test/pos-blame
   'contract-arrow-keyword18
   '((contract (-> integer? integer?)
               (λ (x #:y [y #f]) y)
               'pos
               'neg)
     1))
  
  (test/neg-blame
   'contract-arrow-keyword19
   '((contract (-> boolean?)
               (λ (#:x [x #f]) x)
               'pos
               'neg)
     #:x 1))
  
  (test/spec-passed
   'contract-arrow1
   '(contract (integer? . -> . integer?) (lambda (x) x) 'pos 'neg))
  
  ;; make sure we skip the optimizations
  (test/spec-passed
   'contract-arrow1b
   '(contract (-> integer? integer? integer? integer? integer? 
                  integer? integer? integer? integer? integer?
                  integer?)
              (lambda (x1 x2 x3 x4 x5 x6 x7 x8 x9 x10) x1) 'pos 'neg))
  
  (test/spec-passed/result
   'contract-arrow1c
   '(and (value-contract (contract (integer? . -> . integer?) (lambda (x) x) 'pos 'neg))
         #t)
   #t)

  (test/pos-blame
   'contract-arrow2
   '(contract (integer? . -> . integer?) (lambda (x y) x) 'pos 'neg))
  
  (test/neg-blame
   'contract-arrow3
   '((contract (integer? . -> . integer?) (lambda (x) #f) 'pos 'neg) #t))
  
  (test/pos-blame
   'contract-arrow4
   '((contract (integer? . -> . integer?) (lambda (x) #f) 'pos 'neg) 1))

  (test/pos-blame
   'contract-arrow5
   '(let ()
      (struct s (x))
      ((contract (-> s? integer?) s-x 'pos 'neg) (s #f))))
  
  (test/neg-blame
   'contract-arrow-arity1
   '((contract (-> number? number? number?)
               (λ (x . r) x)
               'pos 'neg)
     1))
  
  (test/spec-passed
   'contract-arrow-any1
   '(contract (integer? . -> . any) (lambda (x) x) 'pos 'neg))
  
  (test/pos-blame
   'contract-arrow-any2
   '(contract (integer? . -> . any) (lambda (x y) x) 'pos 'neg))
  
  (test/neg-blame
   'contract-arrow-any3
   '((contract (integer? . -> . any) (lambda (x) #f) 'pos 'neg) #t))

  (test/spec-passed/result
   'contract-arrow-any4
   '(and (value-contract (contract (-> integer? any) (lambda (x) x) 'pos 'neg))
         #t)
   #t)
  
  (test/spec-passed
   'contract-arrow-all-anys1
   '((contract (-> any) (lambda () #f) 'pos 'neg)))
  
  (test/pos-blame
   'contract-arrow-all-anys2
   '((contract (-> any) (lambda (x) #f) 'pos 'neg)))
  
  (test/spec-passed
   'contract-arrow-all-anys3
   '((contract (-> any) (lambda ([x #f]) #f) 'pos 'neg)))
  
  (test/spec-passed
   'contract-arrow-all-kwds
   '(contract (-> #:a string? string?)
              (make-keyword-procedure void)
              'pos 'neg))
  
  (test/pos-blame
   'contract-arrow-non-function
   '(contract (-> integer? any) 1 'pos 'neg))
  
  (test/pos-blame
   'contract-any/c-arrow1
   '(contract (-> any/c any) 1 'pos 'neg))
  
  (test/spec-passed
   'contract-any/c-arrow2
   '(contract (-> any/c any) (λ (x) 1) 'pos 'neg))
  
  (test/pos-blame
   'contract-any/c-arrow3
   '(contract (-> any/c any) (λ (x y) x) 'pos 'neg))
  
  (test/pos-blame
   'contract-any/c-arrow4
   '(contract (-> any/c any) (λ (x #:y y) x) 'pos 'neg))

  (test/neg-blame
   'contract-any/c-arrow5
   '((contract (-> any/c any) (λ (x [y 1]) x) 'pos 'neg) 1 2))
  
  (test/spec-passed/result
   'contract-any/c-arrow6
   '(let ([f (λ (x) x)])
      (eq? f (contract (-> any/c any) f 'pos 'neg)))
   #t)

  (test/spec-passed/result
   'contract-any/c-arrow7
   '(let ([f (λ (x [y 1]) x)])
      (eq? f (contract (-> any/c any) f 'pos 'neg)))
   #f)
  
  (test/spec-passed/result
   'contract->...1
   '((contract (-> integer? char? ... boolean? any)
               (λ args args)
               'pos 'neg)
     1 #\a #\b #\c #f)
   '(1 #\a #\b #\c #f))
  (test/neg-blame
   'contract->...2
   '((contract (-> integer? char? ... boolean? any)
               (λ args args)
               'pos 'neg)
     1 #\a "b" #\c #f))
  (test/spec-passed/result
   'contract->...3
   '((contract (-> integer? ... any)
               (λ args args)
               'pos 'neg)
     1 2 3 4 5 6 7)
   '(1 2 3 4 5 6 7))
  (test/neg-blame
   'contract->...4
   '((contract (-> integer? ... any)
               (λ args args)
               'pos 'neg)
     1 2 3 4 #f 6 7))
  (test/spec-passed
   'contract->...5
   '(contract (-> procedure? any/c ... list? any)
              (λ (proc last . stuff) stuff)
              'pos 'neg))

  
  (test/spec-passed
   'contract-arrow-all-kwds2
   '((contract (-> #:a string? void?)
               (make-keyword-procedure void)
               'pos 'neg)
     #:a "abcdef"))
  
  (contract-error-test
   'contract-arrow-kwd-name-in-message
   '((contract
      (-> #:a any/c #:the-missing-keyword-arg-b any/c any)
      (λ (#:a [a 0] #:the-missing-keyword-arg-b [b 0]) b)
      'pos
      'neg)
     #:a 0)
   (λ (x)
     (and (exn:fail:contract:blame? x)
          ;; the ? here is to allow the currently pushed buggy version to
          ;; pass; this is fixed in a separate branch that can't 
          (regexp-match #rx"expected:? keyword argument #:the-missing-keyword-arg-b"
                        (exn-message x)))))

  ;; need to preserve the inner contract here
  ;; (not the outer one)
  ;; when dropping redundant tail contracts
  (test/pos-blame
   'tail-wrapping-preserves-blame
   '(let ([c (-> number? number?)])
      ((contract
        c
        (contract
         c
         (λ (x) #f)
         'pos 'neg)
        'something-else 'yet-another-thing)
       1)))

  (test/spec-passed/result
   'chaperone-procedure*-and-contract-interaction
   '(let ()
      (define (f1 x) x)
      
      (define-values (prop:p prop:p? prop:get-p)
        (make-impersonator-property 'p))
      
      (define the-answer 'dont-know)
      
      (define f2 (chaperone-procedure*
                  f1
                  (λ (f x)
                    (set! the-answer (and (prop:p? f) (prop:get-p f)))
                    x)))
      (define f3 (contract (-> integer? integer?) f2 'pos 'neg))
      (define f4 (chaperone-procedure f3 #f prop:p 1234))
      (f4 1)
      the-answer)
   1234)
  
  (test/pos-blame
   'predicate/c1
   '(contract predicate/c 1 'pos 'neg))
  (test/pos-blame
   'predicate/c2
   '(contract predicate/c (λ (x y) 1) 'pos 'neg))
  (test/pos-blame
   'predicate/c3
   '((contract predicate/c (λ (x) 1) 'pos 'neg) 12))
  (test/spec-passed
   'predicate/c4
   '((contract predicate/c (λ (x) #t) 'pos 'neg) 12))
  (test/spec-passed/result
   'predicate/c5
   '(let ()
      (struct s ())
      (eq? (contract (-> any/c boolean?) s? 'pos 'neg) s?))
   #t)
  (test/spec-passed/result
   'predicate/c6
   '(let ()
      (struct s ())
      (eq? (contract predicate/c s? 'pos 'neg) s?))
   #t)
  (test/pos-blame
   'predicate/c7
   '(contract (-> any/c boolean?) 1 'pos 'neg))
  (test/pos-blame
   'predicate/c8
   '(contract (-> any/c boolean?) (λ (x y) 1) 'pos 'neg))
  (test/pos-blame
   'predicate/c9
   '((contract (-> any/c boolean?) (λ (x) 1) 'pos 'neg) 12))
  (test/spec-passed
   'predicate/c10
   '((contract (-> any/c boolean?) (λ (x) #t) 'pos 'neg) 12))
  (test/spec-passed
   'predicate/c11
   '((contract (-> any/c boolean?) (λ x #t) 'pos 'neg) 12))
  (test/neg-blame
   'predicate/c12
   '((contract (-> any/c boolean?) (λ (x #:y [y 1]) #t) 'pos 'neg) 12 #:y 1))
  (test/pos-blame
   'predicate/c13
   '(contract (-> any/c boolean?) (λ (x #:y y) #t) 'pos 'neg))
  (test/pos-blame
   'predicate/c14
   '(contract (-> any/c boolean?)
              (let ()
                (struct s ())
                ((impersonate-procedure s? (λ (x) (values (λ (r) "") x))) 11))
              'pos 'neg))
  (test/spec-passed/result
   'predicate/c15
   '(and (value-contract (contract predicate/c boolean? 'pos 'neg))
         #t)
   #t)

  (test/spec-passed/result
   '->void.1
   '(void? ((contract (-> void?) void 'pos 'neg)))
   #t)
  (test/spec-passed/result
   '->void.2
   '(void? ((contract (-> void?) (λ () (void)) 'pos 'neg)))
   #t)
  (test/spec-passed/result
   '->void.3
   '(void? ((contract (-> void?) (λ args (void)) 'pos 'neg)))
   #t)
  (test/pos-blame
   '->void.4
   '((contract (-> void?) (λ args 11) 'pos 'neg)))
  (test/pos-blame
   '->void.5
   '((contract (-> void?) (λ args (values (void) (void))) 'pos 'neg)))
  (test/pos-blame
   '->void.6
   '(contract (-> void?) 'not-a-function 'pos 'neg))
  (test/pos-blame
   '->void.7
   '(contract (-> void?) (λ (x) 1) 'pos 'neg))
   
  
  (test/spec-passed
   'any/c-in-domain1
   '((contract (-> any/c real?)
               (λ (x) 0)
               'pos 'neg) 0))

  (test/pos-blame
   'any/c-in-domain2
   '((contract (-> any/c real?)
               (λ (x) #f)
               'pos 'neg) 0))

  (test/spec-passed
   'any/c-in-domain3
   '((contract (-> any/c any/c any/c any/c real?)
               (λ (x y z w) 0)
               'pos 'neg) 0 1 2 3))

  (test/pos-blame
   'any/c-in-domain4
   '((contract (-> any/c any/c any/c any/c real?)
               (λ (x y z w) #f)
               'pos 'neg) 0 1 2 3))
  
  ;; this test ensures that no contract wrappers
  ;; are created for struct predicates
  (test/spec-passed/result
   'predicate/c5
   '(let ()
      (struct x (a))
      (eq? (contract predicate/c x? 'pos 'neg) x?))
   #t)

  
  (test/spec-passed/result
   'dynamic->*1
   '((contract (dynamic->* #:mandatory-domain-contracts (list any/c any/c)
                           #:range-contracts (list any/c))
               (λ (x z) (+ x z)) 'pos 'neg)
     2 3)
   5)
  
  (test/pos-blame
   'dynamic->*2
   '((contract (dynamic->* #:mandatory-domain-contracts (list any/c any/c)
                           #:range-contracts (list any/c any/c))
               (λ (x z) (+ x z)) 'pos 'neg)
     2 3))
  
  (test/neg-blame
   'dynamic->*3
   '((contract (dynamic->* #:mandatory-domain-contracts (list integer? integer?)
                           #:range-contracts (list integer?))
               (λ (x z) (+ x z)) 'pos 'neg)
     #f #f))
  
  (test/spec-passed/result
   'dynamic->*5
   '((contract (dynamic->* #:mandatory-keywords '(#:x)
                           #:mandatory-keyword-contracts (list integer?)
                           #:mandatory-domain-contracts (list any/c any/c)
                           #:range-contracts (list any/c))
               (λ (#:x x y z) (+ x z)) 'pos 'neg)
     #:x 1 2 3)
   4)
  
  (test/spec-passed
   'dynamic->*6
   '((contract (dynamic->* #:mandatory-domain-contracts (build-list 11 (λ (x) any/c))
                           #:range-contracts (build-list 11 (λ (x) any/c)))
               values 'pos 'neg)
     1 2 3 4 5 6 7 8 9 10 11))
  
  (test/spec-passed/result
   'dynamic->*7
   '((contract (dynamic->* #:rest-contract (listof any/c)
                           #:range-contracts #f)
               (λ whatever whatever) 'pos 'neg)
     1 2 3 4 5 6 7)
   '(1 2 3 4 5 6 7))
  
  (test/spec-passed/result
   'dynamic->*8
   '((contract (dynamic->* #:range-contracts #f) (λ () 1) 'pos 'neg))
   1)

  (test/spec-passed
   'dynamic->*9
   '(begin
      ((contract (dynamic->* #:range-contracts (list (or/c 1 2) (or/c 3 4)))
                 (λ () (values 1 3))
                 'pos
                 'neg))
      (void)))

  (test/pos-blame
   'dynamic->*10
   '(begin
      ((contract (dynamic->* #:range-contracts (list (or/c 1 2) (or/c 3 4)))
                 (λ () (values #f #f))
                 'pos
                 'neg))
      (void)))

  (test/spec-passed/result
   '->-order-of-evaluation1
   '(let ([l '()])
      (-> (begin (set! l (cons 1 l)) #f)
          (begin (set! l (cons 2 l)) #f)
          (begin (set! l (cons 3 l)) #f)
          (begin (set! l (cons 4 l)) #f)
          (begin (set! l (cons 5 l)) #f))
      (reverse l))
   '(1 2 3 4 5))
  (test/spec-passed/result
   '->-order-of-evaluation2
   '(let ([l '()])
      (-> (begin (set! l (cons 1 l)) #f)
          (begin (set! l (cons 2 l)) #f)
          (begin (set! l (cons 3 l)) #f)
          ...
          (begin (set! l (cons 4 l)) #f)
          (begin (set! l (cons 5 l)) #f)
          (begin (set! l (cons 6 l)) #f))
      (reverse l))
   '(1 2 3 4 5 6))

  (contract-error-test
   '->-arity-error1
   '(contract
     (-> any/c any/c)
     (lambda (x y) #t)
     'pos 'neg)
   (lambda (e)
     (regexp-match? "a procedure that accepts 1 non-keyword argument"
                    (exn-message e))))
  (contract-error-test
   '->-arity-error2
   '(contract
     (-> any/c)
     (lambda (x y) #t)
     'pos 'neg)
   (lambda (e)
     (regexp-match? "a procedure that accepts 0 non-keyword argument"
                    (exn-message e))))
  (contract-error-test
   '->-arity-error3
   '(contract
     (->* (any/c) (#:x any/c) any/c)
     (lambda (x) #t)
     'pos 'neg)
   (lambda (e)
     (regexp-match? "a procedure that accepts the #:x keyword argument"
                    (exn-message e))))

  (contract-syntax-error-test
   '->-duplicate-keywords.1
   #'(->* [] [#:a any/c #:a any/c] void?))
  (contract-syntax-error-test
   '->-duplicate-keywords.2
   #'(->* [#:a any/c #:a any/c] [] void?))
  (contract-error-test
   '->-duplicate-keywords.3
   #'(eval '(->* [#:a any/c] [#:a any/c] void?))
   (λ (x) (and (exn:fail:syntax? x) (regexp-match #rx"->[*]: duplicate keyword" (exn-message x)))))

  )
