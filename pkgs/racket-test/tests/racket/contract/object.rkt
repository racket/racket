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

(test/spec-passed
 'object/c-first-order-object-3
 '(contract (dynamic-object/c null null null null)
            (new object%)
            'pos
            'neg))

(test/pos-blame
 'object/c-first-order-method-1
 '(contract (object/c [m (-> any/c number? number?)])
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
 'object/c-first-order-method-3
 '(contract (dynamic-object/c '(m) (list (-> any/c number? number?)) null null)
            (new (class object% (super-new) (define/public (m x) (add1 x))))
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

(test/spec-passed
 'object/c-first-order-field-3
 '(contract (dynamic-object/c null null '(n) (list number?))
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
         #:late-neg-projection
         (λ (blame)
           (λ (val missing-party)
             (set! log (cons l log))
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
  ;; kicks in. The second part of the result, '(a b b a a), indicates
  ;; that there are still too many calls to the projection
  ;; (namely there is an extra `a` coming from the creation of
  ;; `new-cls` (in class-c-old.rkt, currently line 1368))
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
         #:first-order (λ (x) #t)
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

      (define oa
        (contract printing-a
                  (new foo%)
                  'pos 'neg))

      (define ob
        (contract printing-b
                  oa
                  'pos 'neg))

      (define oc
        (contract printing-a
                  ob
                  'pos 'neg))

      (define od
        (contract printing-b
                  oc
                  'pos 'neg))

      (define log1 log)
      (set! log '())

      (define oe
        (contract printing-a
                  od
                  'pos 'neg))

      (list log1 log))

   '((b a b a) (a b b a a))
   do-not-double-wrap)
)
