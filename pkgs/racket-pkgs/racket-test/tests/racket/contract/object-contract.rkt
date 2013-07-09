#lang racket/base

(require "test-util.rkt")

(parameterize ([current-contract-namespace
                (make-basic-contract-namespace 'racket/class)])

(test/spec-passed
 'object-contract0
 '(contract (object-contract)
            (new object%)
            'pos
            'neg))

(test/pos-blame
 'object-contract/field1
 '(contract (object-contract (field x integer?))
            (new object%)
            'pos
            'neg))

(test/pos-blame
 'object-contract/field2
 '(get-field
   x
   (contract (object-contract (field x integer?))
             (new (class object% (field [x #t]) (super-new)))
             'pos
             'neg)))

(test/spec-passed/result
 'object-contract/field3
 '(get-field
   x
   (contract (object-contract (field x integer?))
             (new (class object% (field [x 12]) (super-new)))
             'pos
             'neg))
 12)

(test/pos-blame
 'object-contract/field4
 '(get-field
   y
   (contract (object-contract (field x boolean?) (field y boolean?))
             (new (class object% (field [x #t] [y 'x]) (super-new)))
             'pos
             'neg)))

(test/pos-blame
 'object-contract/field5
 '(get-field
   x
   (contract (object-contract (field x symbol?) (field y symbol?))
             (new (class object% (field [x #t] [y 'x]) (super-new)))
             'pos
             'neg)))

(test/spec-passed/result
 'object-contract/field6
 '(let ([o (contract (object-contract [m (integer? . -> . integer?)])
                     (new (class object% (field [x 1]) (define/public (m y) x) (super-new)))
                     'pos
                     'neg)])
    (list (send o m 2)
          (send/apply o m '(2))
          (let ([x '(2)]) (send o m . x))
          (with-method ([mm (o m)])
            (mm 2))
          (send* o (m 3) (m 4))))
 (list 1 1 1 1 1))

(test/spec-passed/result
 'object-contract/field7
 '(let ([o (contract (object-contract)
                     (new (class object% (field [x 1]) (define/public (m y) x) (super-new)))
                     'pos
                     'neg)])
    (list (send o m 2)
          (send/apply o m '(2))
          (let ([x '(2)]) (send o m . x))
          (with-method ([mm (o m)])
            (mm 2))
          (send* o (m 3) (m 4))))
 (list 1 1 1 1 1))

(test/spec-passed/result
 'object-contract/field8
 '(let ([o (contract (object-contract [m (integer? . -> . integer?)])
                     (new (class object% (define x 6) (define/public (m y) x) (super-new)))
                     'pos
                     'neg)])
    (list (send o m 2)
          (send/apply o m '(2))
          (let ([x '(2)]) (send o m . x))
          (with-method ([mm (o m)])
            (mm 2))
          (send* o (m 3) (m 4))))
 (list 6 6 6 6 6))

(test/spec-passed/result
 'object-contract/field9
 '(let ([o (contract (object-contract)
                     (new (class object% (define x 6) (define/public (m y) x) (super-new)))
                     'pos
                     'neg)])
    (list (send o m 2)
          (send/apply o m '(2))
          (let ([x '(2)]) (send o m . x))
          (with-method ([mm (o m)])
            (mm 2))
          (send* o (m 3) (m 4))))
 (list 6 6 6 6 6))

(test/spec-passed/result
 'object-contract/field10
 '(send (contract (object-contract)
                  (new (class object% (define x 1) (define/public (m y) x) (super-new)))
                  'pos
                  'neg)
        m
        2)
 1)

(test/spec-passed/result
 'object-contract->1
 '(send
   (contract (object-contract (m (integer? . -> . integer?)))
             (new (class object% (define/public (m x) x) (super-new)))
             'pos
             'neg)
   m
   1)
 1)

(test/pos-blame
 'object-contract->2
 '(contract (object-contract (m (integer? . -> . integer?)))
            (make-object object%)
            'pos
            'neg))

(test/neg-blame
 'object-contract->3
 '(send
   (contract (object-contract (m (integer? . -> . integer?)))
             (make-object (class object% (define/public (m x) x) (super-new)))
             'pos
             'neg)
   m
   'x))

(test/pos-blame
 'object-contract->4
 '(send
   (contract (object-contract (m (integer? . -> . integer?)))
             (make-object (class object% (define/public (m x) 'x) (super-new)))
             'pos
             'neg)
   m
   1))

(test/pos-blame
 'object-contract->5
 '(contract (object-contract (m (integer? integer? . -> . integer?)))
            (make-object (class object% (define/public (m x) 'x) (super-new)))
            'pos
            'neg))

(test/spec-passed/result
 'object-contract->6
 '(send
   (contract (object-contract (m (integer? . -> . any)))
             (new (class object% (define/public (m x) x) (super-new)))
             'pos
             'neg)
   m
   1)
 1)

(test/neg-blame
 'object-contract->7
 '(send
   (contract (object-contract (m (integer? . -> . any)))
             (make-object (class object% (define/public (m x) x) (super-new)))
             'pos
             'neg)
   m
   'x))

(test/spec-passed
 'object-contract->8
 '(begin
    (send
     (contract (object-contract (m (integer? . -> . any)))
               (make-object (class object% (define/public (m x) (values 1 2)) (super-new)))
               'pos
               'neg)
     m
     1)
    (void)))

(test/spec-passed
 'object-contract->9
 '(begin
    (send
     (contract (object-contract (m (integer? . -> . any)))
               (make-object (class object% (define/public (m x) (values)) (super-new)))
               'pos
               'neg)
     m
     1)
    (void)))

(test/spec-passed
 'object-contract->10
 '(begin
    (send (contract (object-contract (m (integer? . -> . (values integer? boolean?))))
                    (make-object (class object% (define/public (m x) (values 1 #t)) (super-new)))
                    'pos
                    'neg)
          m 1)
    (void)))

(test/neg-blame
 'object-contract->11
 '(send
   (contract (object-contract (m (integer? . -> . (values integer? boolean?))))
             (make-object (class object% (define/public (m x) (values #t #t)) (super-new)))
             'pos
             'neg)
   m
   #f))

(test/pos-blame
 'object-contract->12
 '(send
   (contract (object-contract (m (integer? . -> . (values integer? boolean?))))
             (make-object (class object% (define/public (m x) (values #t #t)) (super-new)))
             'pos
             'neg)
   m
   1))

(test/pos-blame
 'object-contract->13
 '(send (contract (object-contract (m (integer? . -> . (values integer? boolean?))))
                  (make-object (class object% (define/public (m x) (values #f #t)) (super-new)))
                  'pos
                  'neg)
        m 1))

(test/pos-blame
 'object-contract->14
 '(send (contract (object-contract (m (integer? . -> . (values integer? boolean?))))
                  (make-object (class object% (define/public (m x) (values 5 6)) (super-new)))
                  'pos
                  'neg)
        m 1))

(test/pos-blame
 'object-contract-case->1
 '(contract (object-contract (m (case-> (boolean? . -> . boolean?)
                                        (integer? integer? . -> . integer?))))
            (new object%)
            'pos
            'neg))

(test/pos-blame
 'object-contract-case->2
 '(contract (object-contract (m (case-> (boolean? . -> . boolean?)
                                        (integer? integer? . -> . integer?))))
            (new (class object% (define/public (m x) x) (super-new)))
            'pos
            'neg))

(test/pos-blame
 'object-contract-case->3
 '(contract (object-contract (m (case-> (boolean? . -> . boolean?)
                                        (integer? integer? . -> . integer?))))
            (new (class object% (define/public (m x y) x) (super-new)))
            'pos
            'neg))

(test/spec-passed
 'object-contract-case->4
 '(contract (object-contract (m (case-> (boolean? . -> . boolean?)
                                        (integer? integer? . -> . integer?))))
            (new (class object%
                   (define/public m
                     (case-lambda
                       [(b) (not b)]
                       [(x y) (+ x y)]))
                   (super-new)))
            'pos
            'neg))

(test/spec-passed/result
 'object-contract-case->5
 '(send (contract (object-contract (m (case-> (boolean? . -> . boolean?)
                                              (integer? integer? . -> . integer?))))
                  (new (class object%
                         (define/public m
                           (case-lambda
                             [(b) (not b)]
                             [(x y) (+ x y)]))
                         (super-new)))
                  'pos
                  'neg)
        m
        #t)
 #f)

(test/spec-passed/result
 'object-contract-case->6
 '(send (contract (object-contract (m (case-> (boolean? . -> . boolean?)
                                              (integer? integer? . -> . integer?))))
                  (new (class object%
                         (define/public m
                           (case-lambda
                             [(b) (not b)]
                             [(x y) (+ x y)]))
                         (super-new)))
                  'pos
                  'neg)
        m
        3
        4)
 7)

(test/pos-blame
 'object-contract->*1
 '(contract (object-contract (m (->* (integer?) (symbol? boolean?) number?)))
            (new (class object%
                   (define/public m
                     (lambda (x [y 'a])
                       x))
                   (super-new)))
            'pos
            'neg))

(test/pos-blame
 'object-contract->*2
 '(contract (object-contract (m (->* (integer?) (symbol? boolean?) number?)))
            (new (class object%
                   (define/public m
                     (lambda (x y [z #t])
                       x))
                   (super-new)))
            'pos
            'neg))

(test/spec-passed
 'object-contract->*3
 '(contract (object-contract (m (->* (integer?) (symbol? boolean?) number?)))
            (new (class object%
                   (define/public m
                     (lambda (x [y 'a] [z #t])
                       x))
                   (super-new)))
            'pos
            'neg))

(test/spec-passed/result
 'object-contract->*4
 '(send (contract (object-contract (m (->* (integer?) (symbol? boolean?) number?)))
                  (new (class object%
                         (define/public m
                           (lambda (x [y 'a] [z #t])
                             x))
                         (super-new)))
                  'pos
                  'neg)
        m
        1)
 1)

(test/spec-passed/result
 'object-contract->*5
 '(send (contract (object-contract (m (->* (integer?) (symbol? boolean?) number?)))
                  (new (class object%
                         (define/public m
                           (lambda (x [y 'a] [z #t])
                             x))
                         (super-new)))
                  'pos
                  'neg)
        m
        2
        'z)
 2)

(test/spec-passed/result
 'object-contract->*7
 '(send (contract (object-contract (m (->* (integer?) (symbol? boolean?) number?)))
                  (new (class object%
                         (define/public m
                           (lambda (x [y 'a] [z #t])
                             x))
                         (super-new)))
                  'pos
                  'neg)
        m
        3
        'z
        #f)
 3)

(test/neg-blame
 'object-contract->*8
 '(send (contract (object-contract (m (->* (integer?) (symbol? boolean?) number?)))
                  (new (class object%
                         (define/public m
                           (lambda (x [y 'a] [z #t])
                             x))
                         (super-new)))
                  'pos
                  'neg)
        m
        #f))

(test/neg-blame
 'object-contract->*9
 '(send (contract (object-contract (m (->* (integer?) (symbol? boolean?) number?)))
                  (new (class object%
                         (define/public m
                           (lambda (x [y 'a] [z #t])
                             x))
                         (super-new)))
                  'pos
                  'neg)
        m
        2
        4))

(test/neg-blame
 'object-contract->*10
 '(send (contract (object-contract (m (->* (integer?) (symbol? boolean?) number?)))
                  (new (class object%
                         (define/public m
                           (lambda (x [y 'a] [z #t])
                             x))
                         (super-new)))
                  'pos
                  'neg)
        m
        3
        'z
        'y))

(test/pos-blame
 'object-contract->*11
 '(send (contract (object-contract (m (->* (integer?) (symbol? boolean?) number?)))
                  (new (class object%
                         (define/public m
                           (lambda (x [y 'a] [z #t])
                             'x))
                         (super-new)))
                  'pos
                  'neg)
        m
        3
        'z
        #f))

(test/spec-passed/result
 'object-contract->*12
 '(let-values ([(x y)
                (send (contract (object-contract 
                                 (m (->* (integer?) (symbol? boolean?) (values number? symbol?))))
                                (new (class object%
                                       (define/public m
                                         (lambda (x [y 'a] [z #t])
                                           (values 1 'x)))
                                       (super-new)))
                                'pos
                                'neg)
                      m
                      3
                      'z
                      #f)])
    (cons x y))
 (cons 1 'x))

(test/pos-blame
 'object-contract->*13
 '(send (contract (object-contract (m (->* (integer?) (symbol? boolean?) (values number? symbol?))))
                  (new (class object%
                         (define/public m
                           (lambda (x [y 'a] [z #t])
                             (values 'x 'x)))
                         (super-new)))
                  'pos
                  'neg)
        m
        3
        'z
        #f))

(test/pos-blame
 'object-contract->*14
 '(send (contract (object-contract (m (->* (integer?) (symbol? boolean?) 
                                           (values number? symbol?))))
                  (new (class object%
                         (define/public m
                           (lambda (x [y 'a] [z #t])
                             (values 1 1)))
                         (super-new)))
                  'pos
                  'neg)
        m
        3
        'z
        #f))

(test/pos-blame
 'object-contract->*1
 '(contract (object-contract (m (-> integer? boolean?)))
            (new (class object% (define/public (m x y) x) (super-new)))
            'pos
            'neg))

(test/neg-blame
 'object-contract->*2
 '(send (contract (object-contract (m (-> integer? boolean?)))
                  (new (class object% (define/public (m x) x) (super-new)))
                  'pos
                  'neg)
        m #f))

(test/pos-blame
 'object-contract->*3
 '(send (contract (object-contract (m (-> integer? boolean?)))
                  (new (class object% (define/public (m x) x) (super-new)))
                  'pos
                  'neg)
        m 1))

(test/spec-passed
 'object-contract->*4
 '(send (contract (object-contract (m (-> integer? boolean?)))
                  (new (class object% (define/public (m x) #f) (super-new)))
                  'pos
                  'neg)
        m 1))

(test/pos-blame
 'object-contract->*5
 '(contract (object-contract (m (->* (integer?) () #:rest any/c boolean?)))
            (new (class object% (define/public (m x y . z) x) (super-new)))
            'pos
            'neg))

(test/neg-blame
 'object-contract->*6
 '(send (contract (object-contract (m (->* (integer?) () #:rest any/c boolean?)))
                  (new (class object% (define/public (m x . z) x) (super-new)))
                  'pos
                  'neg)
        m #t))

(test/pos-blame
 'object-contract->*7
 '(send (contract (object-contract (m (->* (integer?) () #:rest any/c boolean?)))
                  (new (class object% (define/public (m x . z) 1) (super-new)))
                  'pos
                  'neg)
        m 1))

(test/spec-passed
 'object-contract->*8
 '(send (contract (object-contract (m (->* (integer?) () #:rest any/c boolean?)))
                  (new (class object% (define/public (m x . z) #f) (super-new)))
                  'pos
                  'neg)
        m 1))

(test/spec-passed
 'object-contract->*9
 '(send (contract (object-contract (m (->* () () #:rest (listof number?) boolean?)))
                  (new (class object% (define/public (m . z) #f) (super-new)))
                  'pos
                  'neg)
        m 1 2 3))

(test/neg-blame
 'object-contract->*10
 '(send (contract (object-contract (m (->* () () #:rest (listof number?) boolean?)))
                  (new (class object% (define/public (m . z) #f) (super-new)))
                  'pos
                  'neg)
        m
        #t))

(test/spec-passed
 'object-contract-->d1
 '(send (contract (object-contract (m (->d ([x number?]) () [range (<=/c x)])))
                  (new (class object% (define/public m (lambda (x) (- x 1))) (super-new)))
                  'pos
                  'neg)
        m
        1))

(test/spec-passed
 'object-contract-->d1b
 '(send (contract (object-contract (m (->d ([x number?]) () [range (<=/c x)])))
                  (new (class object% (define/public m (lambda (x) (- x 1))) (super-new)))
                  'pos
                  'neg)
        m
        1))

(test/pos-blame
 'object-contract-->d2
 '(send (contract (object-contract (m (->d ([x number?]) () [range (<=/c x)])))
                  (new (class object% (define/public m (lambda (x) (+ x 1))) (super-new)))
                  'pos
                  'neg)
        m
        1))

(test/pos-blame
 'object-contract-->d2b
 '(send (contract (object-contract (m (->d ([x number?]) () [range (<=/c x)])))
                  (new (class object% (define/public m (lambda (x) (+ x 1))) (super-new)))
                  'pos
                  'neg)
        m
        1))

(test/spec-passed
 'object-contract-->d3
 '(send (contract (object-contract (m (->d () () #:rest rst (listof number?) [range any/c])))
                  (new (class object% (define/public m (lambda w 1)) (super-new)))
                  'pos
                  'neg)
        m
        1))

(test/neg-blame
 'object-contract-->d4
 '(send (contract (object-contract (m (->d () () #:rest rst (listof number?) [range any/c])))
                  (new (class object% (define/public m (lambda w 1)) (super-new)))
                  'pos
                  'neg)
        m
        #f))

(test/spec-passed
 'object-contract-->d5
 '(send (contract (object-contract (m (->d () () any)))
                  (new (class object% (define/public m (lambda () 1)) (super-new)))
                  'pos
                  'neg)
        m))

(test/spec-passed
 'object-contract-->d6
 '(send (contract (object-contract (m (->d () () (values [x number?] [y (>=/c x)]))))
                  (new (class object% (define/public m (lambda () (values 1 2))) (super-new)))
                  'pos
                  'neg)
        m))

(test/pos-blame
 'object-contract-->d7
 '(send (contract (object-contract (m (->d () () (values [x number?] [y (>=/c x)]))))
                  (new (class object% (define/public m (lambda () (values 2 1))) (super-new)))
                  'pos
                  'neg)
        m))

(test/neg-blame
 'object-contract-->d/this-1
 '(send (contract (object-contract 
                   (m (->d ([x (and/c integer? (lambda (x) (= x (get-field f this))))])
                           ()
                           any)))
                  (new (class object% (field [f 1]) (define/public m (lambda (x) 1)) (super-new)))
                  'pos
                  'neg)
        m
        2))

(test/spec-passed
 'object-contract-->d/this-2
 '(send (contract (object-contract 
                   (m (->d ([x (and/c integer? (lambda (x) (= x (get-field f this))))])
                           ()
                           any)))
                  (new (class object% (field [f 1]) (define/public m (lambda (x) 1)) (super-new)))
                  'pos
                  'neg)
        m
        1))

(test/neg-blame
 'object-contract-->d/this-3
 '(send (contract (object-contract
                   (m (->d ([x (and/c integer? (lambda (x) (= x (get-field f this))))])
                           ()
                           #:rest rest-var any/c
                           any)))
                  (new (class object% (field [f 1]) 
                         (define/public m (lambda (x . rest) 1))
                         (super-new)))
                  'pos
                  'neg)
        m
        2))

(test/spec-passed
 'object-contract-->d/this-4
 '(send (contract (object-contract 
                   (m (->d ([x (and/c integer? (lambda (x) (= x (get-field f this))))])
                           ()
                           #:rest rest-var any/c
                           any)))
                  (new (class object% 
                         (field [f 1]) 
                         (define/public m (lambda (x . rest) 1))
                         (super-new)))
                  'pos
                  'neg)
        m
        1))

(test/spec-passed
 'object-contract-->pp1
 '(send (contract (object-contract 
                   (m (->d ([x number?]) () #:pre-cond #t [unused (<=/c x)] #:post-cond #t)))
                  (new (class object% (define/public m (lambda (x) (- x 1))) (super-new)))
                  'pos
                  'neg)
        m
        1))

(test/spec-passed
 'object-contract-->pp1b
 '(send (contract (object-contract 
                   (m (->d ([x number?]) () #:pre-cond #t [unused (<=/c x)] #:post-cond #t)))
                  (new (class object%
                         (define/public m (case-lambda [(x) (- x 1)]
                                                       [(x y) y]))
                         (super-new)))
                  'pos
                  'neg)
        m
        1))

(test/pos-blame
 'object-contract-->pp2
 '(send (contract (object-contract 
                   (m (->d ([x number?]) () #:pre-cond #t [unused (<=/c x)] #:post-cond #t)))
                  (new (class object% (define/public m (lambda (x) (+ x 1))) (super-new)))
                  'pos
                  'neg)
        m
        1))

(test/pos-blame
 'object-contract-->pp2b
 '(send (contract (object-contract 
                   (m (->d ([x number?]) () #:pre-cond #t [unused (<=/c x)] #:post-cond #t)))
                  (new (class object%
                         (define/public m (case-lambda [(x) (+ x 1)]))
                         (super-new)))
                  'pos
                  'neg)
        m
        1))

(test/spec-passed
 'object-contract-->pp3
 '(send (contract (object-contract 
                   (m (->d () () #:rest rst (listof number?) 
                           #:pre-cond #t [unused any/c] 
                           #:post-cond #t)))
                  (new (class object% (define/public m (lambda w 1)) (super-new)))
                  'pos
                  'neg)
        m
        1))

(test/neg-blame
 'object-contract-->pp4
 '(send (contract (object-contract 
                   (m (->d () () #:rest rst (listof number?) 
                           #:pre-cond #t [unused any/c]
                           #:post-cond #t)))
                  (new (class object% (define/public m (lambda w 1)) (super-new)))
                  'pos
                  'neg)
        m
        #f))

(test/spec-passed
 'object-contract-->pp5
 '(send (contract (object-contract (m (->d () () #:pre-cond #t any)))
                  (new (class object% (define/public m (lambda () 1)) (super-new)))
                  'pos
                  'neg)
        m))

(test/spec-passed
 'object-contract-->pp6
 '(send (contract (object-contract (m (->d () () 
                                           #:pre-cond #t (values [x number?] [y (>=/c x)])
                                           #:post-cond #t)))
                  (new (class object% (define/public m (lambda () (values 1 2))) (super-new)))
                  'pos
                  'neg)
        m))

(test/pos-blame
 'object-contract-->pp7
 '(send (contract (object-contract (m (->d () () #:pre-cond #t (values [x number?] [y (>=/c x)])
                                           #:post-cond #t)))
                  (new (class object% (define/public m (lambda () (values 2 1))) (super-new)))
                  'pos
                  'neg)
        m))

(test/neg-blame
 'object-contract-->pp/this-1
 '(send (contract (object-contract (m (->d ()
                                           ()
                                           #:pre-cond (= 1 (get-field f this))
                                           [result-x any/c]
                                           #:post-cond (= 2 (get-field f this)))))
                  (new (class object% 
                         (field [f 2])
                         (define/public m (lambda () (set! f 3)))
                         (super-new)))
                  'pos
                  'neg)
        m))

(test/pos-blame
 'object-contract-->pp/this-2
 '(send (contract (object-contract (m (->d () ()
                                           #:pre-cond (= 1 (get-field f this))
                                           [result-x any/c]
                                           #:post-cond (= 2 (get-field f this)))))
                  (new (class object%
                         (field [f 1])
                         (define/public m (lambda () (set! f 3)))
                         (super-new)))
                  'pos
                  'neg)
        m))

(test/spec-passed
 'object-contract-->pp/this-3
 '(send (contract (object-contract (m (->d () ()
                                           #:pre-cond (= 1 (get-field f this))
                                           [result-x any/c]
                                           #:post-cond (= 2 (get-field f this)))))
                  (new (class object% 
                         (field [f 1])
                         (define/public m (lambda () (set! f 2)))
                         (super-new)))
                  'pos
                  'neg)
        m))

(test/neg-blame
 'object-contract-->pp/this-4
 '(send (contract (object-contract (m (->d () ()
                                           #:rest rest-id any/c
                                           #:pre-cond (= 1 (get-field f this))
                                           [result-x any/c]
                                           #:post-cond (= 2 (get-field f this)))))
                  (new (class object% (field [f 2]) 
                         (define/public m (lambda args (set! f 3)))
                         (super-new)))
                  'pos
                  'neg)
        m))

(test/pos-blame
 'object-contract-->pp/this-5
 '(send (contract (object-contract (m (->d () ()
                                           #:rest rest-id any/c
                                           #:pre-cond (= 1 (get-field f this))
                                           [result-x any/c]
                                           #:post-cond (= 2 (get-field f this)))))
                  (new (class object% 
                         (field [f 1])
                         (define/public m (lambda args (set! f 3)))
                         (super-new)))
                  'pos
                  'neg)
        m))

(test/spec-passed
 'object-contract-->pp/this-6
 '(send (contract (object-contract (m (->d () ()
                                           #:rest rest-id any/c
                                           #:pre-cond (= 1 (get-field f this))
                                           [result-x any/c]
                                           #:post-cond (= 2 (get-field f this)))))
                  (new (class object% 
                         (field [f 1])
                         (define/public m (lambda args (set! f 2)))
                         (super-new)))
                  'pos
                  'neg)
        m))
#|
  (test/spec-passed
   'object-contract-->i1
   '(send (contract (object-contract (m (->i ([x number?]) () [range (x) (<=/c x)])))
                    (new (class object% (define/public m (lambda (x) (- x 1))) (super-new)))
                    'pos
                    'neg)
          m
          1))

  (test/spec-passed
   'object-contract-->i1b
   '(send (contract (object-contract (m (->i ([x number?]) () [range (x) (<=/c x)])))
                    (new (class object% (define/public m (lambda (x) (- x 1))) (super-new)))
                    'pos
                    'neg)
          m
          1))

  (test/pos-blame
   'object-contract-->i2
   '(send (contract (object-contract (m (->i ([x number?]) () [range (x) (<=/c x)])))
                    (new (class object% (define/public m (lambda (x) (+ x 1))) (super-new)))
                    'pos
                    'neg)
          m
          1))

  (test/pos-blame
   'object-contract-->i2b
   '(send (contract (object-contract (m (->i ([x number?]) () [range (x) (<=/c x)])))
                    (new (class object% (define/public m (lambda (x) (+ x 1))) (super-new)))
                    'pos
                    'neg)
          m
          1))

  (test/spec-passed
   'object-contract-->i3
   '(send (contract (object-contract (m (->i () () #:rest [rst (listof number?)] [range any/c])))
                    (new (class object% (define/public m (lambda w 1)) (super-new)))
                    'pos
                    'neg)
          m
          1))

  (test/neg-blame
   'object-contract-->i4
   '(send (contract (object-contract (m (->i () () #:rest [rst (listof number?)] [range any/c])))
                    (new (class object% (define/public m (lambda w 1)) (super-new)))
                    'pos
                    'neg)
          m
          #f))

  (test/spec-passed
   'object-contract-->i5
   '(send (contract (object-contract (m (->i () () any)))
                    (new (class object% (define/public m (lambda () 1)) (super-new)))
                    'pos
                    'neg)
          m))

  (test/spec-passed
   'object-contract-->i6
   '(send (contract (object-contract (m (->i () () (values [x number?] [y (x) (>=/c x)]))))
                    (new (class object% (define/public m (lambda () (values 1 2))) (super-new)))
                    'pos
                    'neg)
          m))

  (test/pos-blame
   'object-contract-->i7
   '(send (contract (object-contract (m (->i () () (values [x number?] [y (x) (>=/c x)]))))
                    (new (class object% (define/public m (lambda () (values 2 1))) (super-new)))
                    'pos
                    'neg)
          m))

  (test/neg-blame
   'object-contract-->i/this-1
   '(send (contract (object-contract (m (->i ([x (and/c integer? (lambda (x) 
                                                                   (= x (get-field f this))))])
                                             ()
                                             any)))
                    (new (class object% (field [f 1]) (define/public m (lambda (x) 1)) (super-new)))
                    'pos
                    'neg)
          m
          2))

  (test/spec-passed
   'object-contract-->i/this-2
   '(send (contract (object-contract (m (->i ([x (and/c integer? (lambda (x) 
                                                                   (= x (get-field f this))))])
                                             ()
                                             any)))
                    (new (class object% (field [f 1]) (define/public m (lambda (x) 1)) (super-new)))
                    'pos
                    'neg)
          m
          1))

  (test/neg-blame
   'object-contract-->i/this-3
   '(send (contract (object-contract (m (->i ([x (and/c integer? (lambda (x)
                                                                   (= x (get-field f this))))])
                                             ()
                                             #:rest [rest-var any/c]
                                             any)))
                    (new (class object% (field [f 1]) (define/public m (lambda (x . rest) 1)) 
                           (super-new)))
                    'pos
                    'neg)
          m
          2))

  (test/spec-passed
   'object-contract-->i/this-4
   '(send (contract (object-contract (m (->i ([x (and/c integer? (lambda (x) 
                                                                    (= x (get-field f this))))])
                                             ()
                                             #:rest [rest-var any/c]
                                             any)))
                    (new (class object% (field [f 1]) (define/public m (lambda (x . rest) 1)) 
                           (super-new)))
                    'pos
                    'neg)
          m
          1))

  (test/spec-passed
   'object-contract-->i-pp1
   '(send (contract (object-contract
                      (m (->i ([x number?]) () #:pre () #t [unused (x) (<=/c x)] #:post () #t)))
                    (new (class object% (define/public m (lambda (x) (- x 1))) (super-new)))
                    'pos
                    'neg)
          m
          1))

  (test/spec-passed
   'object-contract-->i-pp1b
   '(send (contract (object-contract 
                      (m (->i ([x number?]) () #:pre () #t [unused (x) (<=/c x)] #:post () #t)))
                    (new (class object%
                           (define/public m (case-lambda [(x) (- x 1)]
                                                         [(x y) y]))
                           (super-new)))
                    'pos
                    'neg)
          m
          1))

  (test/pos-blame
   'object-contract-->i-pp2
   '(send (contract (object-contract
                     (m (->i ([x number?]) () #:pre () #t [unused (x) (<=/c x)] #:post () #t)))
                    (new (class object% (define/public m (lambda (x) (+ x 1))) (super-new)))
                    'pos
                    'neg)
          m
          1))

  (test/pos-blame
   'object-contract-->i-pp2b
   '(send (contract (object-contract 
                     (m (->i ([x number?]) () #:pre () #t [unused (x) (<=/c x)] #:post () #t)))
                    (new (class object%
                           (define/public m (case-lambda [(x) (+ x 1)]))
                           (super-new)))
                    'pos
                    'neg)
          m
          1))

  (test/spec-passed
   'object-contract-->i-pp3
   '(send (contract (object-contract 
                      (m (->i () () #:rest [rst (listof number?)] #:pre () #t [unused any/c]
                                    #:post () #t)))
                    (new (class object% (define/public m (lambda w 1)) (super-new)))
                    'pos
                    'neg)
          m
          1))

  (test/neg-blame
   'object-contract-->i-pp4
   '(send (contract (object-contract (m (->i () () #:rest [rst (listof number?)] 
                                             #:pre () #t [unused any/c] #:post () #t)))
                    (new (class object% (define/public m (lambda w 1)) (super-new)))
                    'pos
                    'neg)
          m
          #f))

  (test/spec-passed
   'object-contract-->i-pp5
   '(send (contract (object-contract (m (->i () () #:pre () #t any)))
                    (new (class object% (define/public m (lambda () 1)) (super-new)))
                    'pos
                    'neg)
          m))

  (test/spec-passed
   'object-contract-->i-pp6
   '(send (contract (object-contract (m (->i () () #:pre () #t (values [x number?] [y (x) (>=/c x)])
                                                   #:post () #t)))
                    (new (class object% (define/public m (lambda () (values 1 2))) (super-new)))
                    'pos
                    'neg)
          m))

  (test/pos-blame
   'object-contract-->i-pp7
   '(send (contract (object-contract (m (->i () () #:pre () #t (values [x number?] [y (>=/c x)]) 
                                                   #:post () #t)))
                    (new (class object% (define/public m (lambda () (values 2 1))) (super-new)))
                    'pos
                    'neg)
          m))

  (test/neg-blame
   'object-contract-->i-pp/this-1
   '(send (contract (object-contract (m (->i ()
                                             ()
                                             #:pre () (= 1 (get-field f this))
                                             [result-x any/c]
                                             #:post () (= 2 (get-field f this)))))
                    (new (class object% (field [f 2]) (define/public m (lambda () (set! f 3)))
                           (super-new)))
                    'pos
                    'neg)
          m))

  (test/pos-blame
   'object-contract-->i-pp/this-2
   '(send (contract (object-contract (m (->i () ()
                                             #:pre () (= 1 (get-field f this))
                                             [result-x any/c]
                                             #:post () (= 2 (get-field f this)))))
                    (new (class object% (field [f 1]) (define/public m (lambda () (set! f 3))) 
                           (super-new)))
                    'pos
                    'neg)
          m))

  (test/spec-passed
   'object-contract-->i-pp/this-3
   '(send (contract (object-contract (m (->i () ()
                                             #:pre () (= 1 (get-field f this))
                                              [result-x any/c]
                                              #:post () (= 2 (get-field f this)))))
                    (new (class object% (field [f 1]) (define/public m (lambda () (set! f 2))) 
                           (super-new)))
                    'pos
                    'neg)
          m))

  (test/neg-blame
   'object-contract-->i-pp/this-4
   '(send (contract (object-contract (m (->i () ()
                                             #:rest [rest-id any/c]
                                             #:pre () (= 1 (get-field f this))
                                             [result-x any/c]
                                             #:post () (= 2 (get-field f this)))))
                    (new (class object% (field [f 2]) (define/public m (lambda args (set! f 3))) 
                           (super-new)))
                    'pos
                    'neg)
          m))

  (test/pos-blame
   'object-contract-->i-pp/this-5
   '(send (contract (object-contract (m (->i () ()
                                             #:rest [rest-id any/c]
                                             #:pre () (= 1 (get-field f this))
                                             [result-x any/c]
                                             #:post () (= 2 (get-field f this)))))
                    (new (class object% (field [f 1]) (define/public m (lambda args (set! f 3)))
                           (super-new)))
                    'pos
                    'neg)
          m))

  (test/spec-passed
   'object-contract-->i-pp/this-6
   '(send (contract (object-contract (m (->i () ()
                                             #:rest [rest-id any/c]
                                             #:pre () (= 1 (get-field f this))
                                             [result-x any/c]
                                             #:post () (= 2 (get-field f this)))))
                    (new (class object% (field [f 1]) (define/public m (lambda args (set! f 2)))
                           (super-new)))
                    'pos
                    'neg)
          m))
 |#

(test/spec-passed/result
 'object-contract-drop-method1
 '(send (contract (object-contract (m (-> integer? integer?)))
                  (new (class object% (define/public (m x) x) (define/public (n x) x) (super-new)))
                  'pos
                  'neg)
        n 1)
 1)

(test/spec-passed/result
 'object-contract-drop-method2
 '(let ([o (contract (object-contract (m (-> integer? integer?)))
                     (new (class object% 
                            (define/public (m x) x)
                            (define/public (n x) x)
                            (super-new)))
                     'pos
                     'neg)])
    (with-method ([m (o m)]
                  [n (o n)])
      (list (m 1) (n 2))))
 '(1 2))

(test/spec-passed/result
 'object-contract-drop-field1
 '(get-field g (contract (object-contract (field f integer?))
                         (new (class object% (field [f 1] [g 2]) (super-new)))
                         'pos
                         'neg))
 2)

(test/spec-passed/result
 'object-contract-drop-field2
 '(field-bound? g (contract (object-contract (field f integer?))
                            (new (class object% (field [f 1] [g 2]) (super-new)))
                            'pos
                            'neg))
 #t)

(test/spec-passed/result
 'object-contract-drop-field3
 '(field-names
   (contract (object-contract)
             (new (class object% (field [g 2]) (super-new)))
             'pos
             'neg))
 '(g))


(test/spec-passed/result
 'object-contract-ho-method1
 '(send (contract (object-contract (m (-> (-> integer? integer?) integer?)))
                  (new (class object% (define/public (m f) (f 1)) (super-new)))
                  'pos
                  'neg)
        m
        (λ (x) x))
 1)

(test/spec-passed/result
 'object-contract-ho-method2
 '(send (contract (object-contract (m (-> (->* (integer?) () integer?) integer?)))
                  (new (class object% (define/public (m f) (f 1)) (super-new)))
                  'pos
                  'neg)
        m
        (λ (x) x))
 1)

(test/spec-passed/result
 'object-contract-ho-method3
 '(send (contract (object-contract (m (-> (->i ([x integer?]) () [r integer?]) integer?)))
                  (new (class object% (define/public (m f) (f 1)) (super-new)))
                  'pos
                  'neg)
        m
        (λ (x) x))
 1)

(test/spec-passed/result
 'object-contract-layered1
 '(send (contract (object-contract (m (-> number? number?)))
                  (contract (object-contract)
                            (new (class object% (super-new) (define/public (m x) x)))
                            'pos
                            'neg)
                  'pos
                  'neg)
        m
        5)
 5)

;; Make sure we're not dropping projections on the floor.
(test/neg-blame
 'object-contract-layered2
 '(send (contract (object-contract (m (-> number? number?)))
                  (contract (object-contract (m (-> string? string?)))
                            (new (class object% (super-new) (define/public (m x) x)))
                            'pos
                            'neg)
                  'pos
                  'neg)
        m
        5))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  test ->i works with object-contract
;;

(test/spec-passed/result
 'object-contract/arrow-special-case1
 '(send (contract (object-contract
                   [m (-> any/c boolean?)])
                  (new (class object%
                         (define/public (m x) #t)
                         (super-new)))
                  'pos
                  'neg)
        m 1)
 #t)

(test/spec-passed/result
 'object-contract/arrow-special-case2
 '(send (contract (object-contract
                   [m (-> any/c any)])
                  (new (class object%
                         (define/public (m x) #t)
                         (super-new)))
                  'pos
                  'neg)
        m 1)
 #t)

(test/spec-passed/result
   '->i22
   '(send (contract (object-contract
                     [m (->i ([x any/c] #:y [y any/c]) ([z any/c]) any)])
                    (new (class object%
                           (define/public (m x #:y y [z 1]) x)
                           (super-new)))
                    'pos
                    'neg)
          m 1 #:y 2)
   1)
  
  (test/spec-passed/result
   '->i25
   '(send (contract (object-contract
                     [m (->i ([x any/c]) ([y any/c]) any)])
                    (new (class object%
                           (define/public (m x [y 1]) x)
                           (super-new)))
                    'pos
                    'neg)
          m 1)
   1)
  
  (test/spec-passed/result
   '->i26
   '(send (contract (object-contract
                     [m (->i ([x any/c]) #:rest [rest any/c] any)])
                    (new (class object%
                           (define/public (m x . y) x)
                           (super-new)))
                    'pos
                    'neg)
          m 1)
   1)
  
  (test/spec-passed/result
   '->i27
   '(send (contract (object-contract
                     [m (->i ([x any/c]) any)])
                    (new (class object%
                           (define/public (m x) x)
                           (super-new)))
                    'pos
                    'neg)
          m 1)
   1)
  

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; test error message has right format
;;

(test/spec-passed/result
 'wrong-method-arity-error-message
 '(with-handlers ([exn:fail? exn-message])
    (send (contract (object-contract [m (integer? . -> . integer?)])
                    (new (class object% (define/public (m x) x) (super-new)))
                    'pos
                    'neg)
          m
          1
          2))
 (string-append
  "m method: arity mismatch;\n"
  " the expected number of arguments does not match the given number\n"
  "  expected: 1\n"
  "  given: 2\n"
  "  arguments...:\n"
  "   1\n"
  "   2"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; tests object utilities to be sure wrappers work right
;;

(let* ([o1 (contract-eval '(new object%))]
       [o2 (contract-eval `(contract (object-contract) ,o1 'pos 'neg))])
  (test #t (contract-eval 'object=?) o1 o1)
  (test #f (contract-eval 'object=?) o1 (contract-eval '(new object%)))
  (test #t (contract-eval 'object=?) o1 o2)
  (test #t (contract-eval 'object=?) o2 o1)
  (test #f (contract-eval 'object=?) (contract-eval '(new object%)) o2))

(ctest #t
       method-in-interface?
       'm
       (object-interface
        (contract
         (object-contract (m (integer? . -> . integer?)))
         (new (class object% (define/public (m x) x) (super-new)))
         'pos
         'neg)))

(let* ([i<%> (contract-eval '(interface ()))]
       [c% (contract-eval `(class* object% (,i<%>) (super-new)))]
       [o (contract-eval `(new ,c%))])
  (test #t (contract-eval 'is-a?) o i<%>)
  (test #t (contract-eval 'is-a?) o c%)
  (test #t (contract-eval 'is-a?) (contract-eval `(contract (object-contract) ,o 'pos 'neg)) i<%>)
  (test #t (contract-eval 'is-a?) (contract-eval `(contract (object-contract) ,o 'pos 'neg)) c%))

;; Currently the new object contracts using impersonators don't even attempt to ensure that
;; these reflective operations still work, and I'm not even sure they should.  For now, I
;; just get the class info from the original object, which means that all contracts are evaded.
;;
;; Just as a note, if we move the class-insp-mk
;; values forward in class/c-proj and make-wrapper-class,
;; we get a failure in object->vector for the second
;; testcase because the field-ref/field-set! in the
;; contracted version of the class (for a struct subtype 
;; of the original class's struct type) doesn't
;; know how to get the fields out of the object struct.
;; We can always force it with unsafe-struct-ref,
;; but if we had impersonate-struct-type, with the same ability to replace the prop:object as
;; impersonate-struct has, then we might be able to handle this better.

(let ([c% (parameterize ([current-inspector (make-inspector)])
            (contract-eval '(class object% (super-new))))])
  (test (list c% #f)
        'object-info
        (contract-eval
         `(call-with-values
           (lambda () (object-info (contract (object-contract) (new ,c%) 'pos 'neg)))
           list))))

;; object->vector tests
(let* ([obj
        (parameterize ([current-inspector (make-inspector)])
          (contract-eval '(new (class object% (field [x 1] [y 2]) (super-new)))))]
       [vec (contract-eval `(object->vector ,obj))])
  (test vec
        (contract-eval 'object->vector)
        (contract-eval
         `(contract (object-contract (field x integer?) (field y integer?))
                    ,obj
                    'pos
                    'neg)))))
