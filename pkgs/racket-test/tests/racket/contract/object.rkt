#lang racket/base
(require "test-util.rkt")

(parameterize ([current-contract-namespace
                (make-basic-contract-namespace 'racket/class
                                               'racket/contract/combinator)])
(test/pos-blame
 'object/c-first-order-object-1
 '(contract (object/c)
            3
            'pos
            'neg))

(test/spec-passed
 'object/c-first-order-object-2
 '(contract (object/c)
            (new object%)
            'pos
            'neg))


(test/pos-blame
 'object/c-first-order-method-1
 '(contract (object/c [m (-> any/c number? number?)])
            (new object%)
            'pos
            'neg))

(test/pos-blame
 'object/c-first-order-method-1b
 '(contract (object/c [m (->m boolean? number? number?)])
            (new object%)
            'pos
            'neg))

(test/spec-passed
 'object/c-first-order-method-2
 '(contract (object/c [m (-> any/c number? number?)])
            (new (class object% (super-new) (define/public (m x) (add1 x))))
            'pos
            'neg))

(test/spec-passed
 'object/c-first-order-method-2b
 '(contract (object/c [m (->m number? number?)])
            (new (class object% (super-new) (define/public (m x) (add1 x))))
            'pos
            'neg))

  (test/pos-blame
   'object/c-first-order-method-4
   '(contract (object/c [m (-> any/c number? number?)])
              (new (class object% (super-new) (define/public (m wrong arity too many arguments) 0)))
              'pos
              'neg))

(test/pos-blame
 'object/c-first-order-local-method-1
 '(let ()
    (define-local-member-name m)
    (contract (object/c [m (-> any/c number? number?)])
              (new object%)
              'pos
              'neg)))

(test/spec-passed
 'object/c-first-order-local-method-2
 '(let ()
    (define-local-member-name m)
    (contract (object/c [m (-> any/c number? number?)])
              (new (class object% (super-new) (define/public (m x) (add1 x))))
              'pos
              'neg)))

(test/neg-blame
 'object/c-first-order-local-method-3
 '(let ()
    (define-local-member-name m)
    (send (contract (object/c [m (-> any/c number? number?)])
                    (new (class object% (define/public (m x) x) (super-new)))
                    'pos
                    'neg)
          m "one")))

(test/pos-blame
 'object/c-first-order-local-method-4
 '(let ()
    (define-local-member-name m)
    (send (contract (object/c [m (-> any/c number? number?)])
                    (new (class object% (define/public (m x) "wrong") (super-new)))
                    'pos
                    'neg)
          m 1)))


  (test/spec-passed
   'dynamic-object/c.1
   '(contract (dynamic-object/c null null null null)
              (new object%)
              'pos
              'neg))
  (test/spec-passed
   'dynamic-object/c.2
   '(contract (dynamic-object/c '(m) (list (-> any/c number? number?)) null null)
              (new (class object% (super-new) (define/public (m x) (add1 x))))
              'pos
              'neg))
  (test/pos-blame
   'dynamic-object/c.3
   '(contract (dynamic-object/c '(m) (list (-> any/c number? number?)) null null)
              (new (class object% (super-new)))
              'pos
              'neg))
  (test/spec-passed/result
   'dynamic-object/c.4
   '(send (contract (dynamic-object/c '(m) (list (-> any/c number? number?)) null null)
                    (new (class object% (super-new) (define/public (m x) (add1 x))))
                    'pos
                    'neg)
          m 1)
   2)
  (test/spec-passed
   'dynamic-object/c.5
   '(contract (dynamic-object/c null null '(n) (list number?))
              (new (class object% (super-new) (field [n 3])))
              'pos
              'neg))

  (test/spec-passed/result
   'dynamic-object/c.6
   '(get-field n
               (contract (dynamic-object/c null null '(n) (list number?))
                         (new (class object% (super-new) (field [n 3])))
                         'pos
                         'neg))
   3)
  (test/pos-blame
   'dynamic-object/c.7
   '(get-field n
               (contract (dynamic-object/c null null '(n) (list string?))
                         (new (class object% (super-new) (field [n 3])))
                         'pos
                         'neg)))
  
  (test/neg-blame
   'object/c-multiple-methods.1
   '(send (contract (object/c [m (-> any/c number? any)]
                              [m (-> any/c string? any)])
                    (new (class object% (define/public (m x) x) (super-new)))
                    'pos
                    'neg)
          m "string"))

  (test/neg-blame
   'object/c-multiple-methods.2
   '(send (contract (object/c [m (-> any/c number? any)]
                              [m (-> any/c string? any)])
                    (new (class object% (define/public (m x) x) (super-new)))
                    'pos
                    'neg)
          m 11))

  (test/neg-blame
   'object/c-multiple-methods.3
   '(let ()
      (define-syntax-rule (M id) (object/c [m (-> any/c string? any)] [id (-> any/c number? any)]))
      (send (contract (M m)
                      (new (class object% (define/public (m x) x) (super-new)))
                      'pos
                      'neg)
            m "string")))

  (test/neg-blame
   'object/c-multiple-methods.4
   '(let ()
      (define-syntax-rule (M id) (object/c [m (-> any/c string? any)] [id (-> any/c number? any)]))
      (send (contract (M m)
                      (new (class object% (define/public (m x) x) (super-new)))
                      'pos
                      'neg)
            m 11)))

  (test/neg-blame
   'dynamic-object/c-multiple-methods.1
   '(send (contract (dynamic-object/c '(m m)
                                      (list (-> any/c number? any)
                                            (-> any/c string? any))
                                      '() '())
                    (new (class object% (define/public (m x) x) (super-new)))
                    'pos
                    'neg)
          m "string"))

  (test/neg-blame
   'dynamic-object/c-multiple-methods.2
   '(send (contract (dynamic-object/c '(m m)
                                      (list (-> any/c number? any)
                                            (-> any/c string? any))
                                      '() '())
                    (new (class object% (define/public (m x) x) (super-new)))
                    'pos
                    'neg)
          m 11))

  (test/spec-passed/result
   'object/c-sorting-methods.1
   '(let ()
      (define o
        (contract (object/c
                   [a (->m integer?)]
                   [b (->m string?)]
                   [c (->m list?)]
                   [d (->m char?)])
                  (new (class object%
                         (define/public (a) 1)
                         (define/public (b) "b")
                         (define/public (c) '())
                         (define/public (d) #\d)
                         (super-new)))
                  'pos 'neg))

      (list (send o a)
            (send o b)
            (send o c)
            (send o d)))
   '(1 "b" () #\d))

  (test/spec-passed/result
   'object/c-sorting-methods.2
   '(let ()
      (define o
        (contract (object/c
                   [b (->m string?)]
                   [a (->m integer?)]
                   [d (->m char?)]
                   [c (->m list?)])
                  (new (class object%
                         (define/public (a) 1)
                         (define/public (b) "b")
                         (define/public (c) '())
                         (define/public (d) #\d)
                         (super-new)))
                  'pos 'neg))

      (list (send o a)
            (send o b)
            (send o c)
            (send o d)))
   '(1 "b" () #\d))

  (test/spec-passed/result
   'object/c-sorting-methods.3
   '(let ()
      (define o
        (contract (object/c
                   [b (->m string?)]
                   [d (->m char?)]
                   [a (->m integer?)]
                   [c (->m list?)])
                  (new (class object%
                         (define/public (a) 1)
                         (define/public (b) "b")
                         (define/public (c) '())
                         (define/public (d) #\d)
                         (super-new)))
                  'pos 'neg))

      (list (send o a)
            (send o b)
            (send o c)
            (send o d)))
   '(1 "b" () #\d))

  (test/spec-passed/result
   'object/c-sorting-methods.4
   '(let ()
      (define o
        (contract (object/c
                   [d (->m char?)]
                   [b (->m string?)]
                   [c (->m list?)]
                   [a (->m integer?)])
                  (new (class object%
                         (define/public (a) 1)
                         (define/public (b) "b")
                         (define/public (c) '())
                         (define/public (d) #\d)
                         (super-new)))
                  'pos 'neg))

      (list (send o a)
            (send o b)
            (send o c)
            (send o d)))
   '(1 "b" () #\d))

  (test/spec-passed/result
   'object/c-sorting-methods.5
   '(let ()
      (define o
        (contract (object/c
                   [d (->m char?)]
                   [c (->m list?)]
                   [b (->m string?)]
                   [a (->m integer?)])
                  (new (class object%
                         (define/public (a) 1)
                         (define/public (b) "b")
                         (define/public (c) '())
                         (define/public (d) #\d)
                         (super-new)))
                  'pos 'neg))

      (list (send o a)
            (send o b)
            (send o c)
            (send o d)))
   '(1 "b" () #\d))

  (test/spec-passed
   'object/c-sorting-methods.6
   '(contract (object/c
               [a (-> any/c any)]
               [b (-> any/c integer? any)]
               [c (-> any/c integer? integer? any)]
               [d (-> any/c integer? integer? integer? any)])
              (new (class object%
                     (define/public (a) 1)
                     (define/public (b x) 2)
                     (define/public (c x y) 3)
                     (define/public (d x y z) 4)
                     (super-new)))
              'pos 'neg))

  (test/spec-passed
   'object/c-sorting-methods.7
   '(contract (object/c
               [b (-> any/c integer? any)]
               [a (-> any/c any)]
               [d (-> any/c integer? integer? integer? any)]
               [c (-> any/c integer? integer? any)])
              (new (class object%
                     (define/public (a) 1)
                     (define/public (b x) 2)
                     (define/public (c x y) 3)
                     (define/public (d x y z) 4)
                     (super-new)))
              'pos 'neg))

  (test/spec-passed
   'object/c-sorting-methods.8
   '(contract (object/c
               [b (-> any/c integer? any)]
               [d (-> any/c integer? integer? integer? any)]
               [a (-> any/c any)]
               [c (-> any/c integer? integer? any)])
              (new (class object%
                     (define/public (a) 1)
                     (define/public (b x) 2)
                     (define/public (c x y) 3)
                     (define/public (d x y z) 4)
                     (super-new)))
              'pos 'neg))

  (test/spec-passed
   'object/c-sorting-methods.9
   '(contract (object/c
               [d (-> any/c integer? integer? integer? any)]
               [b (-> any/c integer? any)]
               [c (-> any/c integer? integer? any)]
               [a (-> any/c any)])
              (new (class object%
                     (define/public (a) 1)
                     (define/public (b x) 2)
                     (define/public (c x y) 3)
                     (define/public (d x y z) 4)
                     (super-new)))
              'pos 'neg))

  (test/spec-passed
   'object/c-sorting-methods.10
   '(contract (object/c
               [d (-> any/c integer? integer? integer? any)]
               [c (-> any/c integer? integer? any)]
               [b (-> any/c integer? any)]
               [a (-> any/c any)])
              (new (class object%
                     (define/public (a) 1)
                     (define/public (b x) 2)
                     (define/public (c x y) 3)
                     (define/public (d x y z) 4)
                     (super-new)))
              'pos 'neg))

  (test/spec-passed/result
   'object/c-wrappers.1
   '(send (contract (object/c [m (->m integer? integer?)])
                    (new (class object%  (define/public (m x) x) (super-new)))
                    'pos 'neg)
          m 1)
   '1)

  (test/neg-blame
   'object/c-wrappers.2
   '(send (contract (object/c [m (->m integer? integer?)])
                    (new (class object%  (define/public (m x) x) (super-new)))
                    'pos 'neg)
          m "one"))

  (test/pos-blame
   'object/c-wrappers.3
   '(send (contract (object/c [m (->m integer? integer?)])
                    (new (class object% (define/public (m x) "two") (super-new)))
                    'pos 'neg)
          m 1))

  (test/spec-passed/result
   'object/c-wrappers.4
   '(send (contract (object/c [m (-> any/c integer? integer?)])
                    (new (class object% (define/public (m x) x) (super-new)))
                    'pos 'neg)
          m 1)
   '1)

  (test/neg-blame
   'object/c-wrappers.5
   '(send (contract (object/c [m (-> any/c integer? integer?)])
                    (new (class object% (define/public (m x) x) (super-new)))
                    'pos 'neg)
          m "one"))

  (test/pos-blame
   'object/c-wrappers.6
   '(send (contract (object/c [m (-> any/c integer? integer?)])
                    (new (class object% (define/public (m x) "two") (super-new)))
                    'pos 'neg)
          m 1))

  (test/spec-passed/result
   'object/c-wrappers.7
   '(send (contract (object/c [m (-> any/c integer? any)])
                    (new (class object% (define/public (m x) "two") (super-new)))
                    'pos 'neg)
          m 1)
   "two")

  (test/pos-blame
   'object/c-wrappers.8
   '(send (contract (object/c [m (-> any/c integer? (values integer? string?))])
                    (new (class object% (define/public (m x) (values 1 #f)) (super-new)))
                    'pos 'neg)
          m 1))

  (test/pos-blame
   'object/c-wrappers.9
   '(send (contract (object/c [m (-> any/c integer? (values integer? string?))])
                    (new (class object% (define/public (m x) (values #f "one")) (super-new)))
                    'pos 'neg)
          m 1))

  (test/spec-passed/result
   'object/c-wrappers.10
   '(send (contract (object/c [m (-> any/c integer? string? ... string?)])
                    (new (class object% (define/public (m i . strings) (car strings)) (super-new)))
                    'pos 'neg)
          m 1 "abc")
   "abc")

  (test/spec-passed/result
   'object/c-wrappers.11
   '(send (contract (object/c [m (-> any/c integer? string? ... string?)])
                    (new (class object% (define/public (m i . strings) (cadr strings)) (super-new)))
                    'pos 'neg)
          m 1 "abc" "def")
   "def")

  (test/neg-blame
   'object/c-wrappers.12
   '(send (contract (object/c [m (-> any/c integer? string? ... string?)])
                    (new (class object% (define/public (m i . strings) (cadr strings)) (super-new)))
                    'pos 'neg)
          m 1 "abc" "def" 'not-a-string))

  (test/spec-passed/result
   'object/c-wrappers.13
   '(send (contract (object/c [m (->* (any/c) (integer?) integer?)])
                    (new (class object% (define/public (m [x 1]) x) (super-new)))
                    'pos 'neg)
          m 1)
   1)

  (test/spec-passed/result
   'object/c-wrappers.14
   '(send (contract (object/c [m (->* (any/c) (integer?) integer?)])
                    (new (class object% (define/public (m [x 1]) x) (super-new)))
                    'pos 'neg)
          m 1)
   1)

  (test/spec-passed/result
   'object/c-wrappers.14-b
   '(send (contract (object/c [m (->* (any/c) (integer?) integer?)])
                    (new (class object% (define/public (m [x 1]) x) (super-new)))
                    'pos 'neg)
          m)
   1)

  (test/neg-blame
   'object/c-wrappers.15
   '(send (contract (object/c [m (->* (any/c) (integer?) integer?)])
                    (new (class object% (define/public (m [x 1]) x) (super-new)))
                    'pos 'neg)
          m "one"))

  (test/pos-blame
   'object/c-wrappers.16
   '(send (contract (object/c [m (->* (any/c) (integer?) integer?)])
                    (new (class object% (define/public (m [x 1]) "wrong") (super-new)))
                    'pos 'neg)
          m 1))
  
  (test/neg-blame
   'object/c-wrappers.17
   '(send (contract (object/c [m (->* (any/c) #:pre #f integer?)])
                    (new (class object% (define/public (m) 1) (super-new)))
                    'pos 'neg)
          m))

  (test/neg-blame
   'object/c-wrappers.18
   '(send (contract (object/c [m (->* (any/c) #:pre/desc "wrong" integer?)])
                    (new (class object% (define/public (m) 1) (super-new)))
                    'pos 'neg)
          m))

  (test/pos-blame
   'object/c-wrappers.19
   '(send (contract (object/c [m (->* (any/c) integer? #:post #f)])
                    (new (class object% (define/public (m) 1) (super-new)))
                    'pos 'neg)
          m))

  (test/pos-blame
   'object/c-wrappers.20
   '(send (contract (object/c [m (->* (any/c) integer? #:post/desc "wrong")])
                    (new (class object% (define/public (m) 1) (super-new)))
                    'pos 'neg)
          m))

  (test/neg-blame
   'object/c-wrappers.21
   '(send (contract (object/c [m (->* (any/c #:x integer?) (#:y string?) any)])
                    (new (class object% (define/public (m #:x x #:y [y 1]) 1) (super-new)))
                    'pos 'neg)
          m #:x "wrong" #:y "x"))

  (test/neg-blame
   'object/c-wrappers.22
   '(send (contract (object/c [m (->* (any/c #:x integer?) (#:y string?) any)])
                    (new (class object% (define/public (m #:x x #:y [y 1]) 1) (super-new)))
                    'pos 'neg)
          m #:x 1 #:y 'wrong))

  (test/spec-passed/result
   'object/c-wrappers.23
   '(send (contract (object/c [m (->* (any/c #:x integer?) (#:y string?) any)])
                    (new (class object% (define/public (m #:x x #:y [y 1]) (cons x y)) (super-new)))
                    'pos 'neg)
          m #:x 1 #:y "x")
   (cons 1 "x"))

  (test/spec-passed/result
   'object/c-wrappers.25
   '(send (contract (object/c [m (->* (any/c) (#:y string? #:z string? #:x string?) any)])
                    (new (class object% (define/public (m #:x [x "x"] #:y [y "y"] #:z [z "z"]) (list x y z)) (super-new)))
                    'pos 'neg)
          m)
   (list "x" "y" "z"))

  (test/spec-passed/result
   'object/c-wrappers.26
   '(send (contract (object/c [m (->* (any/c) (#:y string? #:z string? #:x string?) any)])
                    (new (class object% (define/public (m #:x [x "x"] #:y [y "y"] #:z [z "z"]) (list x y z)) (super-new)))
                    'pos 'neg)
          m #:x "xx")
   (list "xx" "y" "z"))

  (test/spec-passed/result
   'object/c-wrappers.27
   '(send (contract (object/c [m (->* (any/c) (#:y string? #:z string? #:x string?) any)])
                    (new (class object% (define/public (m #:x [x "x"] #:y [y "y"] #:z [z "z"]) (list x y z)) (super-new)))
                    'pos 'neg)
          m #:x "xx" #:z "zz")
   (list "xx" "y" "zz"))

  (test/spec-passed/result
   'object/c-wrappers.28
   '(send (contract (object/c [m (->* (any/c) (#:y string? #:z string? #:x string?) any)])
                    (new (class object% (define/public (m #:x [x "x"] #:y [y "y"] #:z [z "z"]) (list x y z)) (super-new)))
                    'pos 'neg)
          m #:x "xx" #:z "zz" #:y "yy")
   (list "xx" "yy" "zz"))

  (test/spec-passed/result
   'object/c-wrappers.29
   '(send (contract (object/c [m (->*m () (integer? #:str string?) any)])
                    (new (class object%
                           (define/public (m [x "z"] #:str [str "y"]) (cons x str))
                           (super-new)))
                    'pos 'neg)
          m 1 #:str "x")
   (cons 1 "x"))

  (test/spec-passed/result
   'object/c-isa.1
   '(let ()
      (define c%
        (class object%
          (define/public (m x)
            x)
          (super-new)))

      (is-a? (contract (object/c [m (-> any/c integer? integer?)])
                       (new c%)
                       'pos 'neg)
             c%))
   #t)

  (test/spec-passed/result
   'object/c-isa.2
   '(let ()
      (define i<%>
        (interface ()))
      (define c%
        (class* object% (i<%>)
          (define/public (m x)
            x)
          (super-new)))

      (is-a? (contract (object/c [m (-> any/c integer? integer?)])
                       (new c%)
                       'pos 'neg)
             i<%>))
   #t)

  (test/spec-passed/result
   'object/c-generics.1
   '(let ()
      (define c%
        (class object%
          (define/public (m x)
            x)
          (super-new)))
      (define gen-m (generic c% m))
      (define o
        (contract (object/c [m (-> any/c integer? integer?)])
                  (new c%)
                  'pos 'neg))
      (send-generic o gen-m 1))
   1)

  (test/neg-blame
   'object/c-generics.2
   '(let ()
      (define c%
        (class object%
          (define/public (m x)
            x)
          (super-new)))
      (define gen-m (generic c% m))
      (define o
        (contract (object/c [m (-> any/c integer? integer?)])
                  (new c%)
                  'pos 'neg))
      (send-generic o gen-m "one")))

  (test/pos-blame
   'object/c-generics.3
   '(let ()
      (define c%
        (class object%
          (define/public (m x)
            "two")
          (super-new)))
      (define gen-m (generic c% m))
      (define o
        (contract (object/c [m (-> any/c integer? integer?)])
                  (new c%)
                  'pos 'neg))
      (send-generic o gen-m 1)))

  (test/spec-passed/result
   'object/c-generics.4
   '(let ()
      (define i<%> (interface () m))
      (define c%
        (class* object% (i<%>)
          (define/public (m x)
            x)
          (super-new)))
      (define gen-m (generic i<%> m))
      (define o
        (contract (object/c [m (-> any/c integer? integer?)])
                  (new c%)
                  'pos 'neg))
      (send-generic o gen-m 1))
   1)

  (test/neg-blame
   'object/c-generics.5
   '(let ()
      (define i<%> (interface () m))
      (define c%
        (class* object% (i<%>)
          (define/public (m x)
            x)
          (super-new)))
      (define gen-m (generic i<%> m))
      (define o
        (contract (object/c [m (-> any/c integer? integer?)])
                  (new c%)
                  'pos 'neg))
      (send-generic o gen-m "one")))

  (test/pos-blame
   'object/c-generics.6
   '(let ()
      (define i<%> (interface () m))
      (define c%
        (class* object% (i<%>)
          (define/public (m x)
            "wrong")
          (super-new)))
      (define gen-m (generic i<%> m))
      (define o
        (contract (object/c [m (-> any/c integer? integer?)])
                  (new c%)
                  'pos 'neg))
      (send-generic o gen-m 1)))

  (test/neg-blame
   'object/c-generics.7
   '(let ()
      (define c%
        (class object%
          (define/public (m x)
            x)
          (super-new)))
      (define gen-m (generic c% m))
      (define o
        (contract (object/c [m (-> any/c any/c any/c)])
                  (contract (object/c [m (-> any/c integer? integer?)])
                            (new c%)
                            'pos 'neg)
                  'pos 'neg))
      (send-generic o gen-m "one")))

  (test/neg-blame
   'object/c-generics.8
   '(let ()
      (define c%
        (class object%
          (define/public (m x)
            x)
          (super-new)))
      (define gen-m (generic c% m))
      (define o
        (contract (object/c [m (-> any/c integer? integer?)])
                  (contract (object/c [m (-> any/c any/c any/c)])
                            (new c%)
                            'pos 'neg)
                  'pos 'neg))
      (send-generic o gen-m "one")))

  (test/spec-passed/result
   'object/c-generics.9
   '(let ()
      (define c%
        (class object%
          (define/public-final (m x)
            x)
          (super-new)))
      (define gen-m (generic c% m))
      (define o
        (contract (object/c [m (-> any/c integer? integer?)])
                  (new c%)
                  'pos 'neg))
      (send-generic o gen-m 1))
   1)

  (test/neg-blame
   'object/c-generics.10
   '(let ()
      (define c%
        (class object%
          (define/public-final (m x)
            x)
          (super-new)))
      (define gen-m (generic c% m))
      (define o
        (contract (object/c [m (-> any/c integer? integer?)])
                  (new c%)
                  'pos 'neg))
      (send-generic o gen-m "one")))

  (test/pos-blame
   'object/c-generics.11
   '(let ()
      (define c%
        (class object%
          (define/public-final (m x)
            "two")
          (super-new)))
      (define gen-m (generic c% m))
      (define o
        (contract (object/c [m (-> any/c integer? integer?)])
                  (new c%)
                  'pos 'neg))
      (send-generic o gen-m 1)))

  (test/spec-passed/result
   'opaque-method.1
   '(send (contract
           (object/c [m (->m integer? integer?)] #:opaque #f)
           (new (class object% (define/public (m x) x) (define/public (n x) x) (super-new)))
           'pos
           'neg)
          n 1)
   1)

  (test/neg-blame
   'opaque-method.2
   '(send (contract
           (object/c [m (-> any/c integer? integer?)] #:opaque #t)
           (new (class object% (define/public (m x) x) (define/public (n x) x) (super-new)))
           'pos
           'neg)
          n 1))

  (test/neg-blame
   'opaque-method.3
   '(let ()
      (define-values (prop pred? get) (make-impersonator-property 'prop))
      (send (contract
             (object/c [m (->m integer? integer?)] #:opaque pred?)
             (new (class object%
                    (define/public (m x) x)
                    (define/public (n x) x)
                    (super-new)))
             'pos
             'neg)
            n 1)))

  (test/spec-passed/result
   'opaque-method.4
   '(let ()
      (define-values (prop pred? get) (make-impersonator-property 'prop))
      (send (contract
             (object/c [m (->m integer? integer?)] #:opaque pred?)
             (new (class object%
                    (define/public (m x) x)
                    (public n)
                    (define n
                      (chaperone-procedure
                       (λ (x) x)
                       #f
                       prop #t))
                    (super-new)))
             'pos
             'neg)
            n 11))
   11
   do-not-double-wrap)

  (test/spec-passed/result
   'opaque-method.5
   '(let ()
      (define-values (prop pred? get) (make-impersonator-property 'prop))
      (send (contract
             (object/c [m (->m integer? integer?)] #:opaque-except pred?)
             (new (class object%
                    (define/public (m x) x)
                    (define/public (n x) x)
                    (super-new)))
             'pos
             'neg)
            n 11))
   11
   do-not-double-wrap)

  (test/neg-blame
   'opaque-method.6
   '(let ()
      (define-values (prop pred? get) (make-impersonator-property 'prop))
      (send (contract
             (object/c [m (->m integer? integer?)] #:opaque-except pred?)
             (new (class object%
                    (define/public (m x) x)
                    (public n)
                    (define n
                      (chaperone-procedure
                       (λ (x) x)
                       #f
                       prop #t))
                    (super-new)))
             'pos
             'neg)
            n 11)))
  
  (test/spec-passed/result
   'object/c-field-accessor.1
   '(let ()
      (define c%
        (class object%
          (field [f 1])
          (super-new)))
      ((class-field-accessor c% f)
       (contract (object/c (field [f integer?])) (new c%) 'pos 'neg)))
   1)

  (test/pos-blame
   'object/c-field-accessor.2
   '(let ()
      (define c%
        (class object%
          (field [f "wrong"])
          (super-new)))
      ((class-field-accessor c% f)
       (contract (object/c (field [f integer?])) (new c%) 'pos 'neg))))

  (test/spec-passed/result
   'object/c-field-accessor.3
   '(let ()
      (define c%
        (class object%
          (field [f "original value"])
          (super-new)))
      (define o (contract (object/c (field [f integer?])) (new c%) 'pos 'neg))
      ((class-field-mutator c% f) o 2)
      (get-field f o))
   2)

  (test/neg-blame
   'object/c-field-accessor.4
   '(let ()
      (define c%
        (class object%
          (field [f 1])
          (super-new)))
      (define o (contract (object/c (field [f integer?])) (new c%) 'pos 'neg))
      ((class-field-mutator c% f)
       (contract (object/c (field [f integer?])) (new c%) 'pos 'neg)
       "wrong")))

  (test/spec-passed/result
   'object/c-field-accessor.5
   '(let ()
      (define c%
        (class object%
          (field [f "wrong"])
          (super-new)))
      ((class-field-accessor c% f)
       (contract (object/c (field [f integer?])
                           #:do-not-check-class-field-accessor-or-mutator-access)
                 (new c%) 'pos 'neg)))
   "wrong")

  (test/spec-passed
   'object/c-field-accessor.6
   '(let ()
      (define c%
        (class object%
          (field [f 1])
          (super-new)))
      (define o (contract (object/c (field [f integer?])) (new c%) 'pos 'neg))
      ((class-field-mutator c% f)
       (contract (object/c (field [f integer?])
                           #:do-not-check-class-field-accessor-or-mutator-access)
                 (new c%) 'pos 'neg)
       "wrong")))

  (test/pos-blame
   'object/c.two-contracts-on-one-field.get
   '(get-field
     f
     (contract (object/c (field [f list?]))
               (contract (object/c (field [f string?]))
                         (new (class object% (field [f 1]) (super-new)))
                         'pos
                         'neg)
               'wrong
               'wrong)))

  (test/neg-blame
   'object/c.two-contracts-on-one-field.set
   '(set-field!
     f
     (contract (object/c (field [f list?]))
               (contract (object/c (field [f string?]))
                         (new (class object% (field [f 1]) (super-new)))
                         'wrong
                         'wrong)
               'pos
               'neg)
     "wrong"))
  
  (test/spec-passed/result
   'object/c-no-ctc-and-field
   '(let ()
      (define c%
        (class object%
          (define x 11)
          (define/public (m1) x)
          (define/public (m2) 12)
          (super-new)))
      (define o
        (contract (object/c [m2 (-> any/c integer?)])
                  (new c%)
                  'pos 'neg))
      (send o m1))
   11)
  
(test/pos-blame
 'object/c-first-order-field-1
 '(contract (object/c (field [n number?]))
            (new object%)
            'pos
            'neg))

(test/spec-passed
 'object/c-first-order-field-2
 '(contract (object/c (field [n number?]))
            (new (class object% (super-new) (field [n 3])))
            'pos
            'neg))


;; don't check fields when wrapping the object, as they are mutable
;; check them when they're accessed
(test/spec-passed
 'object/c-first-order-field-3
 '(contract (object/c (field [n string?]))
            (new (class object% (super-new) (field [n 3])))
            'pos
            'neg))

(test/pos-blame
 'object/c-first-order-local-field-1
 '(let ()
    (define-local-member-name n)
    (contract (object/c (field [n number?]))
              (new object%)
              'pos
              'neg)))

(test/spec-passed
 'object/c-first-order-local-field-2
 '(let ()
    (define-local-member-name n)
    (contract (object/c (field [n number?]))
              (new (class object% (super-new) (field [n 3])))
              'pos
              'neg)))

(test/spec-passed/result
 'object/c-higher-order-field-1
 '(get-field
   n
   (contract (object/c (field [n number?]))
             (new (class object% (super-new) (field [n 3])))
             'pos
             'neg))
 3)

(test/pos-blame
 'object/c-higher-order-field-2
 '(get-field
   n
   (contract (object/c (field [n number?]))
             (new (class object% (super-new) (field [n #t])))
             'pos
             'neg)))

(test/spec-passed/result
 'object/c-higher-order-field-3
 '(let ([o (contract (object/c (field [n number?]))
                     (new (class object% (super-new) (field [n 3])))
                     'pos
                     'neg)])
    (set-field! n o 5)
    (get-field n o))
 5)

(test/neg-blame
 'object/c-higher-order-field-4
 '(let ([o (contract (object/c (field [n number?]))
                     (new (class object% (super-new) (field [n 3])))
                     'pos
                     'neg)])
    (set-field! n o #t)))

(test/spec-passed/result
 'object/c-higher-order-field-5
 '(let* ([pre-o (new (class object% (super-new) (field [n 3])))]
         [o (contract (object/c (field [n number?]))
                      pre-o
                      'pos
                      'neg)])
    (set-field! n pre-o 5)
    (get-field n o))
 5)

(test/spec-passed/result
 'object/c-higher-order-field-6
 '(let* ([pre-o (new (class object% (super-new) (field [n 3])))]
         [o (contract (object/c (field [n number?]))
                      pre-o
                      'pos
                      'neg)])
    (set-field! n o 5)
    (get-field n pre-o))
 5)

(test/neg-blame
 'object/c-higher-order-field-7
 '(let* ([pre-o (new (class object% (super-new) (field [n 3])))]
         [o (contract (object/c (field [n number?]))
                      pre-o
                      'pos
                      'neg)])
    (set-field! n o #t)
    (get-field n pre-o)))

(test/pos-blame
 'object/c-higher-order-field-8
 '(let* ([pre-o (new (class object% (super-new) (field [n 3])))]
         [o (contract (object/c (field [n number?]))
                      pre-o
                      'pos
                      'neg)])
    (set-field! n pre-o #t)
    (get-field n o)))

(test/pos-blame
 'object/c-higher-order-field-9
 '(let* ([pre-o (new (class object% (super-new) (field [n 3])))]
         [o (contract (dynamic-object/c null null '(n) (list number?))
                      pre-o
                      'pos
                      'neg)])
    (set-field! n pre-o #t)
    (get-field n o)))

(test/pos-blame
 'object/c-higher-order-field-10
 '(let* ([pre-o (new (class object% (super-new) (field [n (λ (x) "not a number")])))]
         [o (contract (object/c (field [n (-> number? number?)]))
                      pre-o
                      'pos
                      'neg)])
    ((get-field n o) 11)))

(test/neg-blame
 'object/c-higher-order-field-11
 '(let* ([pre-o (new (class object% (super-new) (field [n (λ (x) 11)])))]
         [o (contract (object/c (field [n (-> number? number?)]))
                      pre-o
                      'pos
                      'neg)])
    ((get-field n o) "not number")))

(test/neg-blame
 'object/c-higher-order-field-12
 '(let* ([pre-o (new (class object% (super-new) (field [n (λ (x) x)])))]
         [o (contract (object/c (field [n (-> number? number?)]))
                      pre-o
                      'pos
                      'neg)])
    (set-field! n o (λ (x) "not a number"))
    ((get-field n o) 11)))


  (test/spec-passed/result
   'opaque-field.1
   '(get-field f
               (contract
                (object/c [m (->m integer? integer?)] #:opaque #f)
                (new (class object% (define/public (m x) x) (field [f 1]) (super-new)))
                'pos
                'neg))
   1)

  (test/neg-blame
   'opaque-field.2
   '(get-field f
               (contract
                (object/c [m (-> any/c integer? integer?)] #:opaque #t)
                (new (class object% (define/public (m x) x) (field [f 1]) (super-new)))
                'pos
                'neg)))

  (test/neg-blame
   'opaque-field.2b
   '(get-field f
               (contract
                (object/c [m (-> any/c integer? integer?)] #:opaque #t
                          #:do-not-check-class-field-accessor-or-mutator-access)
                (new (class object% (define/public (m x) x) (field [f 1]) (super-new)))
                'pos
                'neg)))

  (test/spec-passed/result
   'opaque-field.3
   '(dynamic-get-field 'f
                       (contract
                        (object/c [m (->m integer? integer?)] #:opaque #f)
                        (new (class object% (define/public (m x) x) (field [f 1]) (super-new)))
                        'pos
                        'neg))
   1)

  (test/neg-blame
   'opaque-field.4
   '(dynamic-get-field 'f
                       (contract
                        (object/c [m (-> any/c integer? integer?)] #:opaque #t)
                        (new (class object% (define/public (m x) x) (field [f 1]) (super-new)))
                        'pos
                        'neg)))

  (test/spec-passed/result
   'opaque-field.5
   '(let ([o
           (contract
            (object/c [m (->m integer? integer?)] #:opaque #f)
            (new (class object% (define/public (m x) x) (field [f 1]) (super-new)))
            'pos
            'neg)])
      (set-field! f o 123)
      (get-field f o))
   123)

  (test/neg-blame
   'opaque-field.6
   '(let ([o
           (contract
            (object/c [m (->m integer? integer?)] #:opaque #t)
            (new (class object% (define/public (m x) x) (field [f 1]) (super-new)))
            'pos
            'neg)])
      (set-field! f o 123)
      (get-field f o)))

  (test/spec-passed/result
   'opaque-field.7
   '(let ([o
           (contract
            (object/c [m (->m integer? integer?)] #:opaque #f)
            (new (class object% (define/public (m x) x) (field [f 1]) (super-new)))
            'pos
            'neg)])
      (dynamic-set-field! 'f o 123)
      (get-field f o))
   123)

  (test/neg-blame
   'opaque-field.8
   '(let ([o
           (contract
            (object/c [m (->m integer? integer?)] #:opaque #t)
            (new (class object% (define/public (m x) x) (field [f 1]) (super-new)))
            'pos
            'neg)])
      (dynamic-set-field! 'f o 123)
      (get-field f o)))

  (test/spec-passed/result
   'opaque-field.9
   '(let ([% (class object% (define/public (m x) x) (field [f 1]) (super-new))])
      ((class-field-accessor % f)
       (contract
        (object/c [m (->m integer? integer?)] #:opaque #f)
        (new %)
        'pos
        'neg)))
   1)

  (test/neg-blame
   'opaque-field.10
   '(let ([% (class object% (define/public (m x) x) (field [f 1]) (super-new))])
      ((class-field-accessor % f)
       (contract
        (object/c [m (->m integer? integer?)] #:opaque #t)
        (new %)
        'pos
        'neg))))

  (test/spec-passed/result
   'opaque-field.11
   '(let ([% (class object% (define/public (m x) x) (field [f 1]) (super-new))])
      (let ([o (contract
                (object/c [m (->m integer? integer?)] #:opaque #f)
                (new %)
                'pos
                'neg)])
        ((class-field-mutator % f) o 123)
        (get-field f o)))
   123)

  (test/neg-blame
   'opaque-field.12
   '(let ([% (class object% (define/public (m x) x) (field [f 1]) (super-new))])
      ((class-field-mutator % f)
       (contract
        (object/c [m (->m integer? integer?)] #:opaque #t)
        (new %)
        'pos
        'neg)
       123)))
  
  (test/neg-blame
   'opaque-field.13
   '(get-field f
               (contract
                (object/c [m (->m integer? integer?)] #:opaque #f #:opaque-fields #t)
                (new (class object% (define/public (m x) x) (field [f 1]) (super-new)))
                'pos
                'neg)))

  (test/neg-blame
   'opaque-field.14
   '(let ()
      (define-values (prop pred? get) (make-impersonator-property 'prop))
      (get-field f
                 (contract
                  (object/c [m (->m integer? integer?)] #:opaque pred?)
                  (new (class object%
                         (define/public (m x) x)
                         (field [f 1])
                         (super-new)))
                  'pos
                  'neg))))

  (test/spec-passed/result
   'opaque-field.14
   '(let ()
      (define-values (prop pred? get) (make-impersonator-property 'prop))
      (get-field f
                 (contract
                  (object/c [m (->m integer? integer?)] #:opaque-except pred?)
                  (new (class object%
                         (define/public (m x) x)
                         (field [f 1])
                         (super-new)))
                  'pos
                  'neg)))
   1)

  (test/neg-blame
   'opaque-field.15
   '(get-field f
               (contract
                (object/c [m (->m integer? integer?)] #:opaque-fields #t)
                (contract
                 (object/c [m (->m integer? integer?)] #:opaque-fields #t)
                 (new (class object%
                        (define/public (m x) x)
                        (field [f 1])
                        (super-new)))
                 'wrong
                 'wrong)
                'pos
                'neg)))

  (test/neg-blame
   'opaque-field.16
   '(set-field! f
                (contract
                 (object/c [m (->m integer? integer?)] #:opaque-fields #t)
                 (contract
                  (object/c [m (->m integer? integer?)] #:opaque-fields #t)
                  (new (class object%
                         (define/public (m x) x)
                         (field [f 1])
                         (super-new)))
                  'wrong
                  'wrong)
                 'pos
                 'neg)
                123))

  (test/spec-passed/result
   'opaque-field.17
   '(get-field f1
               (contract
                (object/c
                 (field [f1 integer?])
                 #:opaque #t)
                (new (class object%
                       (field [f1 1] [f2 2])
                       (super-new)))
                'pos 'neg))
   1)

  (test/spec-passed
   'opaque-field.18
   '(set-field! f1
                (contract
                 (object/c
                  (field [f1 integer?])
                  #:opaque #t)
                 (new (class object%
                        (field [f1 1] [f2 1])
                        (super-new)))
                 'pos 'neg)
                3))

  (test/spec-passed/result
   'opaque-field.19
   '(let ([c%
           (class object%
             (field [f1 1] [f2 2])
             (super-new))])
      ((class-field-accessor c% f1)
       (contract
        (object/c
         (field [f1 integer?])
         #:opaque #t)
        (new c%)
        'pos 'neg)))
   1)

  (test/spec-passed
   'opaque-field.20
   '(let ([c%
           (class object%
             (field [f1 1] [f2 1])
             (super-new))])
      ((class-field-mutator c% f1)
       (contract
        (object/c
         (field [f1 integer?])
         #:opaque #t)
        (new c%)
        'pos 'neg)
       3)))
    
(test/spec-passed/result
 'object/c-field-existence
 '(send (contract
         (object/c
          (field foo bar)
          (baz (->m integer? integer?)))
         (new (class object%
                (super-new)
                (field (foo 0) (bar 0))
                (define/public (baz n) n)))
         'pos 'neg)
        baz 1)
 1)

(test/spec-passed/result
 'object/c-field-existence2
 '(send (contract
         (object/c
          (field foo bar)
          (baz (->m integer? (listof integer?))))
         (new (class object%
                (super-new)
                (field (foo 0) (bar 1))
                (define/public (baz n) (list foo bar))))
         'pos 'neg)
        baz 1)
 '(0 1))

(test/spec-passed/result
 'object/c-error-checking-earlier-coerce-contract
 '(with-handlers
      ([exn:fail?
        (λ (x)
          (regexp-match? #rx"^object/c:"
                        (exn-message x)))])
    (object/c (field [f '(1)])))
 #t)

(test/pos-blame
 'object/c-multiple-wrapping-1
 '(let ()
    (define c%
      (class object%
        (define/public (m) (void))
        (define/public (n) (void))
        (define/public (p) (void))
        (super-new)))

    (define a/c (object/c [m (-> any/c symbol?)]))
    (define b/c (object/c [n (-> any/c string?)]))
    (define c/c (object/c [p (-> any/c vector?)]))

    (define a-c (new c%))

    (define x1 (contract a/c a-c 'pos 'neg))
    (define x2 (contract b/c x1 'pos 'neg))
    (define x3 (contract c/c x2 'pos 'neg))

    (send x3 m)))
(test/pos-blame
 'object/c-multiple-wrapping-2
 '(let ()
    (define c%
      (class object%
        (define/public (m) (void))
        (define/public (n) (void))
        (define/public (p) (void))
        (super-new)))

    (define a/c (object/c [m (-> any/c symbol?)]))
    (define b/c (object/c [n (-> any/c string?)]))
    (define c/c (object/c [p (-> any/c vector?)]))

    (define a-c (new c%))

    (define x1 (contract a/c a-c 'pos 'neg))
    (define x2 (contract b/c x1 'pos 'neg))
    (define x3 (contract c/c x2 'pos 'neg))

    (send x3 n)))
(test/pos-blame
 'object/c-multiple-wrapping-3
 '(let ()
    (define c%
      (class object%
        (define/public (m) (void))
        (define/public (n) (void))
        (define/public (p) (void))
        (super-new)))

    (define a/c (object/c [m (-> any/c symbol?)]))
    (define b/c (object/c [n (-> any/c string?)]))
    (define c/c (object/c [p (-> any/c vector?)]))

    (define a-c (new c%))

    (define x1 (contract a/c a-c 'pos 'neg))
    (define x2 (contract b/c x1 'pos 'neg))
    (define x3 (contract c/c x2 'pos 'neg))

    (send x3 p)))

  (test/pos-blame
   'object/c-lots-of-wrapping.1
   '(let ()
      (define N 40)

      (define c
        (for/fold ([c (-> integer? integer?)])
                  ([i (in-range N)])
          (object/c (field [fld c]))))

      (define o
        (for/fold ([v 'not-a-proc])
                  ([i (in-range N)])
          (new
           (class object%
             (field [fld v])
             (super-new)))))

      (let loop ([o (contract c o 'pos 'neg)])
        (loop (get-field fld o)))))

  (test/neg-blame
   'object/c-lots-of-wrapping.2
   '(let ()
      (define N 40)

      (define c
        (for/fold ([c (-> integer? integer?)])
                  ([i (in-range N)])
          (object/c (field [fld c]))))

      (define o
        (for/fold ([v add1])
                  ([i (in-range N)])
          (new
           (class object%
             (field [fld v])
             (super-new)))))

      (let loop ([o (contract c o 'pos 'neg)]
                 [i N])
        (cond
          [(= i 1) (set-field! fld o 'not-a-proc)]
          [else (loop (get-field fld o) (- i 1))]))))

  (test/spec-passed
   'object/c-just-check-existence
   '(contract (object/c m)
              (new
               (class object%
                 (super-new)
                 (define/public (m) 42)))
              'pos 'neg))

  (test/pos-blame
   'object/c-just-check-existence
   '(contract (object/c m)
              (new
               (class object% (super-new)))
              'pos 'neg))

  (test/spec-passed/result
   'object/c-struct-type-properties
   '(let ()
      (define-values (prop pred? get) (make-struct-type-property 'prop))
      (define i<%>
        (interface* ()
                    ([prop 5])))

      (define c%
        (class* object% (i<%>)))

      (define o
        (contract
         (object/c
          [m (->m integer? integer?)])
         (new (class* object% (i<%>) (define/public (m x) x) (super-new)))
         'pos 'neg))

      (list (pred? o)
            (get o)))
   (list #t 5))

  (test/spec-passed/result
   'object/c-printing
   '(let ()
      (define c% (class object% (define/public (m x) x) (super-new)))
      (define name1 (format "~s" (new c%)))
      (define name2
        (format "~s"
                (contract (object/c [m (->m integer? integer?)])
                          (new c%)
                          'pos 'neg)))
      (unless (equal? name1 name2)
        (printf "name1 ~s name2 ~s\n" name1 name2))
      (equal? name1 name2))
   #t)

  (test/spec-passed/result
   'object/c-multi-wrap-just-check-existence/field
   '(let ([ctc (object/c (field foo))]
          [v (new (class object% (super-new) (field (foo 0))))])
      (get-field
       foo
       (contract
        ctc
        (contract
         ctc
         (contract
          ctc
          v
          'p 'n)
         'p 'n)
        'p 'n)))
   0)

  (test/spec-passed/result
   'object/c-multi-wrap-just-check-existence/method
   '(let ([ctc (object/c foo)]
          [v (new (class object% (super-new) (define/public (foo) 0)))])
      (send
       (contract
        ctc
        (contract
         ctc
         (contract
          ctc
          v
          'p 'n)
         'p 'n)
        'p 'n)
       foo))
   0)


  ;; this test makes sure that we don't rewrap
  ;; contracts when we are just putting three
  ;; different contracts on
  (test/spec-passed/result
   'object/c-avoid-redundancy
   '(let ()
      (define log '())

      (define (printing/c l)
        (make-contract
         #:first-order
         (λ (val)
           (set! log (cons l log))
           #t)
         #:late-neg-projection
         (λ (blame)
           (λ (val missing-party)
             val))))

      (define foo%
        (class object%
          (super-new)
          (define/public (m) #f)))

      (define (printing-foo/c l)
        (object/c [m (printing/c l)]))

      (define oa
        (contract (printing-foo/c 'a)
                  (new foo%)
                  'pos 'neg))

      (define ob
        (contract (printing-foo/c 'b)
                  oa
                  'pos 'neg))

      (define oc
        (contract (printing-foo/c 'c)
                  ob
                  'pos 'neg))
      log)

   '(c b a)
   '(c c c c c c c c c c c b b b b b b b b b b b a a a a a a a a a a a))

  ;; this tests the situation where the double-wrapping avoidance
  ;; kicks in. Without wrapping, log2 would have four "a"s in it,
  ;; but the double wrapping checking hits here becaues it is the
  ;; same contract with the same blame that's already wrapped inside,
  ;; so we just go to that one instead
  (test/spec-passed/result
   'object/c-avoid-redundancy.2
   '(let ()
      (define log '())

      (struct logging/c (name)
        #:property prop:chaperone-contract
        (build-chaperone-contract-property
         #:late-neg-projection
         (λ (this)
           (λ (blame)
             (λ (val missing-party)
               (set! log (cons (logging/c-name this) log))
               val)))
         #:name (λ (x) `(logging/c ,(logging/c-name x)))
         #:first-order (λ (ctc) (λ (val) #t))
         #:stronger
         (λ (this that)
           (and (logging/c? that)
                (equal? (logging/c-name this) (logging/c-name that))))
         #:equivalent
         (λ (this that)
           (and (logging/c? that)
                (equal? (logging/c-name this) (logging/c-name that))))))

      (define foo%
        (class object%
          (super-new)
          (define/public (m) #f)))

      (define printing
        (object/c [m (logging/c 'a)]))

      (define a->a (-> printing printing))

      (define f (contract a->a (λ (x) x) 'pos 'neg))
      (define o1 (new foo%))
      (define o2 (f o1))
      (void (send o2 m))
      (define log1 log)
      (set! log '())
      (define o3 (f o2))
      (void (send o3 m))
      (define log2 log)

      (list log log2))

   '((a a) (a a))
   do-not-double-wrap)

  (test/spec-passed/result
   'object/c-avoid-redundancy.3
   '(let ()
      (define log '())

      (struct logging/c (name)
        #:property prop:chaperone-contract
        (build-chaperone-contract-property
         #:late-neg-projection
         (λ (this)
           (λ (blame)
             (λ (val missing-party)
               (set! log (cons (logging/c-name this) log))
               val)))
         #:name (λ (x) `(logging/c ,(logging/c-name x)))
         #:first-order (λ (ctc) (λ (val) #t))
         #:stronger
         (λ (this that)
           (and (logging/c? that)
                (equal? (logging/c-name this) (logging/c-name that))))
         #:equivalent
         (λ (this that)
           (and (logging/c? that)
                (equal? (logging/c-name this) (logging/c-name that))))))

      (define foo%
        (class object%
          (super-new)
          (define/public (m) #f)))

      (define printing-a
        (object/c [m (logging/c 'a)]))
      (define printing-b
        (object/c [m (logging/c 'b)]))

      (define a->b (-> printing-a printing-b))
      (define f (contract a->b (λ (x) x) 'pos 'neg))

      (define o1 (new foo%))
      (define o2 (f o1))
      (void (send o2 m))
      (define log1 log)
      (set! log '())

      (define o3 (f o2))
      (void (send o3 m))
      (define log2 log)
      (set! log '())

      (define o4 (f o3))
      (void (send o4 m))
      (define log3 log)
      (set! log '())

      (define o5 (f o4))
      (void (send o5 m))
      (define log4 log)
      (set! log '())

      (list log1 log2 log3 log4))

   '((a b)     ;; log1
     (a b a b) ;; log2
     (a b a b) ;; log3
     (a b a b) ;; log4
     )
   do-not-double-wrap)
)
