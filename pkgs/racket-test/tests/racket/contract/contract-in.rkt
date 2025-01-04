#lang racket/base
(require "test-util.rkt"
         (for-syntax racket/base))

(parameterize ([current-contract-namespace
                (make-basic-contract-namespace 'racket/contract 'racket/list)])

  (contract-syntax-error-test
   'contract-in-not-module-path
   #'(require (contract-in (only-in racket add1)
                           [add1 (-> number? number?)]))
   #rx"expected a module-path")

  (test/spec-passed/result
   'contract-in1
   '(let ()
      (eval '(module contract-in-test-suite1 racket/base
               (require racket/contract
                        (contract-in racket/list
                                     [first (-> list? any)]))
               (provide contract-in1-x)
               (define contract-in1-x (first (list 1 2)))))
      (eval '(require 'contract-in-test-suite1))
      (eval 'contract-in1-x))
   1)

  (test/spec-failed
   'contract-in2
   '(let ()
      (eval '(module contract-in-test-suite1 racket/base
               (require racket/contract
                        (contract-in racket/list
                                     [first (-> boolean? any)]))
               (first "wrong")))
      (eval '(require 'contract-in-test-suite1)))
   "contract-in-test-suite1")

  (test/spec-failed
   'contract-in3
   '(let ()
      (eval '(module contract-in-test-suite2 racket/base
               (require racket/contract
                        (contract-in racket/list
                                     [first (-> any/c boolean?)]))
               (first (list 1))))
      (eval '(require 'contract-in-test-suite2)))
   "racket/list")

  (test/spec-passed/result
   'contract-in4
   '(let ()
      (eval '(module contract-in4a racket/base
               (struct s (f1 f2))
               (provide (struct-out s))))
      (eval '(module contract-in4b racket/base
               (require racket/contract
                        (contract-in
                         'contract-in4a
                         (struct s ([f1 integer?] [f2 integer?]))))
               (provide contract-in4-fail)
               (define (contract-in4-fail) (s "not a number" "also not a number"))))
      (eval '(require 'contract-in4b))
      (eval '(regexp-match?
              #rx"contract from: contract-in4b"
              (with-handlers ([exn:fail? exn-message])
                (contract-in4-fail)))))
   #t)

  (test/spec-passed/result
   'contract-in5
   '(let ()
      (eval '(module contract-in5a racket/base
               (define (f x) "not a number")
               (provide f)))
      (eval '(module contract-in5b racket/base
               (require racket/contract
                        (contract-in
                         'contract-in5a
                         [f (-> integer? integer?)]))
               (provide contract-in5-fail)
               (define (contract-in5-fail) (f 1))))
      (eval '(require 'contract-in5b))
      (eval '(regexp-match?
              #rx"contract from: contract-in5b"
              (with-handlers ([exn:fail? exn-message])
                (contract-in5-fail)))))
   #t)

  (test/spec-failed
   'contract-in6
   '(let ()
      (eval '(module contract-in6a racket/base
               (define (f x) "not a number")
               (provide f)))
      (eval '(module contract-in6b racket/base
               (require racket/contract
                        (contract-in
                         'contract-in6a
                         [rename f g (-> integer? integer?)]))
               (g 1)))
      (eval '(require 'contract-in6b)))
   "(quote contract-in6a)")

  (test/spec-passed/result
   'contract-in7
   '(let ()
      (eval '(module contract-in-test-suite7 racket/base
               (require racket/contract)
               ;; this `values` shadows the one from racket/base
               (require (contract-in racket/base (values (-> string? string?))))
               (provide contract-in-test-suite7-name)
               (define msg
                 (with-handlers ([exn:fail? exn-message])
                   (values 'a)))
               (define contract-in-test-suite7-name
                 (list-ref (regexp-match #rx"^([^:]*):" msg) 1))))
      (eval '(require 'contract-in-test-suite7))
      (eval 'contract-in-test-suite7-name))
   "values")

  (test/spec-passed/result
   'contract-in8
   '(let ()
      (eval '(module contract-in-test-suite8 racket/base
               (require racket/contract)
               (require (contract-in racket/base (rename values desired-name (-> string? string?))))
               (provide contract-in-test-suite8-name)
               (define msg
                 (with-handlers ([exn:fail? exn-message])
                   (desired-name 'a)))
               (define contract-in-test-suite8-name
                 (list-ref (regexp-match #rx"^([^:]*):" msg) 1))))
      (eval '(require 'contract-in-test-suite8))
      (eval 'contract-in-test-suite8-name))
   "desired-name")

  (test/spec-passed/result
   'contract-in-struct1
   '(let ()
      (eval '(module contract-in-struct1a racket/base
               (struct s (f1 f2))
               (provide (struct-out s))))
      (eval '(module contract-in-struct1b racket/base
               (require racket/contract
                        (contract-in
                         'contract-in-struct1a
                         (struct s ([f1 integer?] [f2 boolean?]))))
               (provide conract-in-struct1-x)
               (define conract-in-struct1-x (s-f1 (s 1 #t)))))
      (eval '(require 'contract-in-struct1b))
      (eval 'conract-in-struct1-x))
   1)

  (test/spec-failed
   'contract-in-struct2
   '(let ()
      (eval '(module contract-in-struct2a racket/base
               (struct s (f1 f2))
               (provide (struct-out s))))
      (eval '(module contract-in-struct2b racket/base
               (require racket/contract
                        (contract-in
                         'contract-in-struct2a
                         (struct s ([f1 integer?] [f2 boolean?]))))
               (s-f1 (s #t 1))))
      (eval '(require 'contract-in-struct2b)))
   "contract-in-struct2b")

  (test/spec-passed/result
   'contract-in-struct3
   '(let ()
      (eval '(module contract-in-struct3a racket/base
               (struct s (f1 f2))
               (provide (struct-out s))))
      (eval '(module contract-in-struct3b racket/base
               (require racket/contract
                        (contract-in
                         'contract-in-struct3a
                         (struct s ([f1 integer?] [f2 boolean?]))))
               (provide contract-in-struct3)
               (define contract-in-struct3 (s? (s 1 #f)))))
      (eval '(require 'contract-in-struct3b))
      (eval 'contract-in-struct3))
   #t)

  (test/spec-passed
   'contract-in-struct4
   '(let ()
      (eval '(module contract-in-struct4a racket/base
               (struct s (f1 f2))
               (define an-s (s "not ab" "number"))
               (provide an-s (struct-out s))))
      (eval '(module contract-in-struct4b racket/base
               (require racket/contract
                        (contract-in
                         'contract-in-struct4a
                         [an-s any/c]
                         (struct s ([f1 string?] [f2 integer?]))))
               (void (s-f1 an-s))))
      (eval '(require 'contract-in-struct4b))))

  (test/spec-failed
   'contract-in-struct5
   '(let ()
      (eval '(module contract-in-struct5a racket/base
               (struct s (f1 f2))
               (define an-s (s "not abc" "number"))
               (provide an-s (struct-out s))))
      (eval '(module contract-in-struct5b racket/base
               (require racket/contract
                        (contract-in
                         'contract-in-struct5a
                         [an-s any/c]
                         (struct s ([f1 integer?] [f2 integer?]))))
               (s-f1 an-s)))
      (eval '(require 'contract-in-struct5b)))
   "(quote contract-in-struct5a)")

  (test/spec-passed
   'contract-in-struct6
   '(let ()
      (eval '(module contract-in-struct6a racket/base
               (struct s (f1 f2))
               (define an-s (s 1 2))
               (provide an-s (struct-out s))))
      (eval '(module contract-in-struct6b racket/base
               (require racket/contract
                        (contract-in
                         'contract-in-struct6a
                         (struct s ([f1 integer?] [f2 integer?]))
                         [an-s s?]))
               (void (s-f1 an-s))))
      (eval '(require 'contract-in-struct6b))))

  (test/spec-passed/result
   'contract-in-struct7
   '(begin
      (eval
       '(module contract-in-struct7-a racket
          (define-struct s () #:transparent)
          (provide (struct-out s))))

      (eval
       '(module contract-in-struct7-b racket
          (require (contract-in
                    'contract-in-struct7-a
                    (struct s ())))
          (define a-struct (make-s))
          (provide the-answer)
          (define the-answer (s? (make-s)))))

      (dynamic-require ''contract-in-struct7-b 'the-answer))
   #t)

  (test/spec-passed/result
   'contract-in-struct8
   '(begin
      (eval
       '(module contract-in-struct8-a racket
          (define-struct s () #:transparent)
          (provide (struct-out s))))

      (eval
       '(module contract-in-struct8-b racket
          (require (contract-in
                    'contract-in-struct8-a
                    (struct s ())))
          (define a-struct (make-s))
          (define-values (type _) (struct-info a-struct))
          (provide the-answer)
          (define the-answer (eq? type struct:s))))

      (dynamic-require ''contract-in-struct8-b 'the-answer))
   #t)

  (test/spec-failed
   'contract-in-struct9
   '(let ()
      (eval '(module contract-in-struct9a racket/base
               (struct s (f1 f2) #:mutable)
               (define an-s (s 1 2))
               (provide an-s (struct-out s))))
      (eval '(module contract-in-struct9b racket/base
               (require racket/contract
                        (contract-in
                         'contract-in-struct9a
                         [an-s any/c]
                         (struct s ([f1 integer?] [f2 integer?]))))
               (set-s-f1! an-s 1)
               (set-s-f1! an-s "one")))
      (eval '(require 'contract-in-struct9b)))
   "contract-in-struct9b")

  (test/spec-passed/result
   'contract-in-struct10
   '(let ()
      (eval '(module contract-in-struct10a racket/base
               (struct s (f1 f2) #:mutable)
               (define an-s (s 1 2))
               (provide an-s (struct-out s))))
      (eval '(module contract-in-struct10b racket/base
               (require racket/contract
                        (for-syntax racket/base racket/struct-info)
                        (contract-in
                         'contract-in-struct10a
                         [an-s any/c]
                         (struct s ([f1 integer?] [f2 boolean?]))))
               (define-syntax (m stx)
                 (syntax-case stx ()
                   [(_ id)
                    (let ([info (extract-struct-info (syntax-local-value #'id))])
                      (let loop ([v (list (list-ref info 1)
                                          ;; skip the predicate as
                                          ;; it's contract is optimized away
                                          ;; (list-ref info 2)
                                          (list-ref info 3)
                                          (list-ref info 4))])
                        (cond
                          [(pair? v)
                           #`(cons #,(loop (car v)) #,(loop (cdr v)))]
                          [(null? v) #''()]
                          [(boolean? v) v]
                          [(identifier? v)
                           #`(contract-name (value-contract #,v))]
                          [else (error 'contract-in-struct10-ack "~s" v)])))]))
               (provide contract-in-struct10-x)
               (define contract-in-struct10-x (m s))))
      (eval '(require 'contract-in-struct10b))
      (eval 'contract-in-struct10-x))
   (list '(-> integer? boolean? s?)
         (list '(-> s? boolean?)
               '(-> s? integer?))
         (list '(-> s? boolean? void?)
               '(-> s? integer? void?))))

  (test/spec-passed/result
   'contract-in-struct11
   '(begin
      (eval
       '(module contract-in-struct11-a racket
          (struct s (f))
          (provide (struct-out s))))

      (eval
       '(module contract-in-struct11-b racket
          (require (contract-in
                    'contract-in-struct11-a
                    (struct s ([f integer?]))))
          (provide (struct-out s))))

      (eval
       '(module contract-in-struct11-c racket
          (require 'contract-in-struct11-b)
          (provide the-answer)
          (define the-answer (s-f (s 1)))))

      (dynamic-require ''contract-in-struct11-c 'the-answer))
   1)

  (test/spec-passed/result
   'contract-in-struct12
   '(begin
      (eval
       '(module contract-in-struct12-a racket
          (define-struct s (f))
          (provide (struct-out s))))

      (eval
       '(module contract-in-struct12-b racket
          (require (contract-in
                    'contract-in-struct12-a
                    (struct s ([f integer?]))))
          (provide (struct-out s))))

      (eval
       '(module contract-in-struct12-c racket
          (require 'contract-in-struct12-b)
          (provide the-answer)
          (define the-answer (s-f (make-s 1)))))

      (dynamic-require ''contract-in-struct12-c 'the-answer))
   1)

  (void))
