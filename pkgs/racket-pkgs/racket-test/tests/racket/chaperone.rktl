

(load-relative "loadtest.rktl")
(Section 'chaperones)

;; ----------------------------------------

(define (chaperone-of?/impersonator a b)
  (test #t impersonator-of? a b)
  (chaperone-of? a b))

(define (chaperone?/impersonator a)
  (test #t impersonator? a)
  (chaperone? a))

(define-syntax-rule (as-chaperone-or-impersonator ([orig impersonator] ...) body ...)
  (for-each (lambda (orig ...)
              body ...)
            (list orig impersonator) ...))

;; ----------------------------------------

(test #t chaperone-of?/impersonator 10 10)
(test #t chaperone-of?/impersonator '(10) '(10))
(test #t chaperone-of?/impersonator '#(1 2 3) '#(1 2 3))
(test #t chaperone-of?/impersonator '#&(1 2 3) '#&(1 2 3))

(test #f chaperone-of?/impersonator (make-string 1 #\x) (make-string 1 #\x))
(test #t chaperone-of?/impersonator 
      (string->immutable-string (make-string 1 #\x))
      (string->immutable-string (make-string 1 #\x)))

(define (either-chaperone-of?/impersonator a b)
  (or (chaperone-of?/impersonator a b)
      (chaperone-of?/impersonator b a)))
(test #f either-chaperone-of?/impersonator 
      (string->immutable-string "x")
      (make-string 1 #\x))
(test #f either-chaperone-of?/impersonator 
      '#(1 2 3)
      (vector 1 2 3))
(test #f either-chaperone-of?/impersonator 
      '#&17
      (box 17))

(let ()
  (define-struct o (a b))
  (define-struct p (x y) #:transparent)
  (define-struct (p2 p) (z) #:transparent)
  (define-struct q (u [w #:mutable]) #:transparent)
  (define-struct (q2 q) (v) #:transparent)
  (test #f chaperone-of? (make-o 1 2) (make-o 1 2))
  (test #f impersonator-of? (make-o 1 2) (make-o 1 2))
  (test #t chaperone-of?/impersonator (make-p 1 2) (make-p 1 2))
  (test #f chaperone-of?/impersonator (make-p 1 (box 2)) (make-p 1 (box 2)))
  (test #t chaperone-of?/impersonator (make-p2 1 2 3) (make-p2 1 2 3))
  (test #f chaperone-of?/impersonator (make-q 1 2) (make-q 1 2))
  (test #f chaperone-of?/impersonator (make-q2 1 2 3) (make-q2 1 2 3)))

(let* ([p (lambda (x) x)]
       [p1 (impersonate-procedure p (lambda (y) y))]
       [p2 (chaperone-procedure p1 (lambda (y) y))])
  (test #t impersonator-of? p2 p)
  (test #t impersonator-of? p2 p1)
  (test #t impersonator? p1)
  (test #f chaperone? p1)
  (test #t chaperone? p2)
  (test #f chaperone-of? p2 p)
  (test #t chaperone-of? p2 p1))

;; ----------------------------------------

(test #t chaperone?/impersonator (chaperone-box (box 10) (lambda (b v) v) (lambda (b v) v)))
(test #f chaperone?/impersonator (impersonate-box (box 10) (lambda (b v) v) (lambda (b v) v)))
(test #t box? (chaperone-box (box 10) (lambda (b v) v) (lambda (b v) v)))
(test #t box? (impersonate-box (box 10) (lambda (b v) v) (lambda (b v) v)))
(test #t (lambda (x) (box? x)) (chaperone-box (box 10) (lambda (b v) v) (lambda (b v) v)))
(test #t (lambda (x) (box? x)) (impersonate-box (box 10) (lambda (b v) v) (lambda (b v) v)))
(test #t chaperone?/impersonator (chaperone-box (box-immutable 10) (lambda (b v) v) (lambda (b v) v)))
(err/rt-test (impersonate-box (box-immutable 10) (lambda (b v) v) (lambda (b v) v)))

(as-chaperone-or-impersonator
 ([chaperone-box impersonate-box]
  [chaperone-of? impersonator-of?])
 (let* ([b (box 0)]
        [b2 (chaperone-box b
                           (lambda (b v) 
                             (when (equal? v 'bad) (error "bad get"))
                             v) 
                           (lambda (b v) 
                             (when (equal? v 'bad) (error "bad set"))
                             v))])
   (test #t equal? b b2)
   (test #f chaperone-of? b b2)
   (test #t chaperone-of? b2 b)
   (err/rt-test (set-box! b2 'bad) (lambda (exn)
                                     (test "bad set" exn-message exn)))
   (test (void) set-box! b 'bad)
   (err/rt-test (unbox b2) (lambda (exn)
                             (test "bad get" exn-message exn)))
   (err/rt-test (equal-hash-code b2) (lambda (exn)
                                       (test "bad get" exn-message exn)))
   (err/rt-test (equal-secondary-hash-code b2) (lambda (exn)
                                                 (test "bad get" exn-message exn)))
   (err/rt-test (equal? b2 (box 'bad)) (lambda (exn)
                                         (test "bad get" exn-message exn)))
   (test (void) set-box! b 'ok)
   (test 'ok unbox b2)
   (test (void) set-box! b2 'fine)
   (test 'fine unbox b)))

;; test chaperone-of checks in a chaperone:
(parameterize ([print-box #f]) ; avoid problems printing errors
  (let ([b (box 0)])
    (let ([b2 (chaperone-box b 
                             (lambda (b v) #f)
                             (lambda (b v) #f))])
      (err/rt-test (unbox b2))
      (test (void) set-box! b2 #f)
      (test #f unbox b2)
      (err/rt-test (set-box! b2 0)))))

;; no impersonator-of checks in a impersonator:
(let ([b (box 0)])
  (let ([b2 (impersonate-box b 
                             (lambda (b v) #f)
                             (lambda (b v) #f))])
    (test #f unbox b2)
    (test (void) set-box! b2 0)
    (test #f unbox b)
    (test #f unbox b2)))

;; ----------------------------------------

(test #t chaperone?/impersonator (chaperone-vector (vector 1 2 3) (lambda (b i v) v) (lambda (b i v) v)))
(test #t vector? (chaperone-vector (vector 1 2 3) (lambda (b i v) v) (lambda (b i v) v)))
(test #t vector? (impersonate-vector (vector 1 2 3) (lambda (b i v) v) (lambda (b i v) v)))
(test #t (lambda (x) (vector? x)) (chaperone-vector (vector 1 2 3) (lambda (b i v) v) (lambda (b i v) v)))
(test #t (lambda (x) (vector? x)) (impersonate-vector (vector 1 2 3) (lambda (b i v) v) (lambda (b i v) v)))
(test #t chaperone?/impersonator (chaperone-vector (vector-immutable 1 2 3) (lambda (b i v) v) (lambda (b i v) v)))
(err/rt-test (impersonate-vector (vector-immutable 1 2 3) (lambda (b i v) v) (lambda (b i v) v)))

(test #(1 2 3) make-reader-graph (chaperone-vector (vector 1 2 3) (lambda (b i v) v) (lambda (b i v) v)))

(as-chaperone-or-impersonator
 ([chaperone-vector impersonate-vector]
  [chaperone-of? impersonator-of?])
 (let* ([b (vector 1 2 3)]
        [b2 (chaperone-vector b
                              (lambda (b i v) 
                                (when (and (equal? v 'bad) (= i 1))
                                  (error "bad get"))
                                v) 
                              (lambda (b i v) 
                                (when (and (equal? v 'bad) (= i 2))
                                  (error "bad set"))
                                v))])
   (test #t equal? b b2)
   (test #f chaperone-of? b b2)
   (test #t chaperone-of? b2 b)
   (err/rt-test (vector-set! b2 2 'bad) (lambda (exn)
                                          (test "bad set" exn-message exn)))
   (test 3 vector-ref b 2)
   (test (void) vector-set! b2 1 'bad)
   (test 'bad vector-ref b 1)
   (err/rt-test (vector-ref b2 1) (lambda (exn)
                                    (test "bad get" exn-message exn)))
   (err/rt-test (equal-hash-code b2) (lambda (exn)
                                       (test "bad get" exn-message exn)))
   (define b3 (vector 1 'bad 3))
   (err/rt-test (equal? b2 b3) (lambda (exn)
                                 (test "bad get" exn-message exn)))
   (err/rt-test (equal? b3 b2) (lambda (exn)
                                 (test "bad get" exn-message exn)))
   (test (void) vector-set! b 1 'ok)
   (test 'ok vector-ref b2 1)
   (test (void) vector-set! b2 1 'fine)
   (test 'fine vector-ref b 1)))

;; test chaperone-of checks in a chaperone:
(let ([b (vector 0)])
  (let ([b2 (chaperone-vector b 
                              (lambda (b i v) #f)
                              (lambda (b i v) #f))])
    (test 'ok 'bad-vector-ref
          (with-handlers ([exn:fail:contract? (lambda (exn) 'ok)])
            (vector-ref b2 0)))
    (test (void) 'ok-vector-set! (vector-set! b2 0 #f))
    (test #f vector-ref b2 0)
    (err/rt-test (vector-set! b2 0 0))))

;; no impersonator-of checks in a impersonator:
(let ([b (vector 0)])
  (let ([b2 (impersonate-vector b 
                                (lambda (b i v) #f)
                                (lambda (b i v) #f))])
    (test #f vector-ref b2 0)
    (test (void) vector-set! b2 0 #f)
    (test #f vector-ref b 0)
    (test #f vector-ref b2 0)))

;; check interaction of chaperones and vector->values:
(let ([b (vector 1 2 3)])
  (let ([b2 (impersonate-vector b
                                (lambda (b i v) (collect-garbage) v)
                                (lambda (b i v) #f))])
    (define-values (a b c) (vector->values b2))
    (test '(1 2 3) list a b c)))

;; ----------------------------------------

(test #t chaperone?/impersonator (chaperone-procedure (lambda (x) x) (lambda (y) y)))
(test #t impersonator? (impersonate-procedure (lambda (x) x) (lambda (y) y)))
(test #t procedure? (chaperone-procedure (lambda (x) x) (lambda (y) y)))
(test #t procedure? (impersonate-procedure (lambda (x) x) (lambda (y) y)))
(test #t (lambda (x) (procedure? x)) (chaperone-procedure (lambda (x) x) (lambda (y) y)))
(test #t (lambda (x) (procedure? x)) (impersonate-procedure (lambda (x) x) (lambda (y) y)))
(err/rt-test (chaperone-procedure (lambda (x) x) (lambda (y z) y)))
(err/rt-test (impersonate-procedure (lambda (x) x) (lambda (y z) y)))
(err/rt-test (chaperone-procedure (case-lambda [() 0] [(x) x]) (lambda (y) y)))
(err/rt-test (impersonate-procedure (case-lambda [() 0] [(x) x]) (lambda (y) y)))

(test 88 (impersonate-procedure (lambda (x) x) (lambda (y) 88)) 10)
(err/rt-test ((chaperone-procedure (lambda (x) x) (lambda (y) 88)) 10))

(test 89 (impersonate-procedure (lambda (x) x) (lambda (y) (values (lambda (z) 89) y))) 10)
(err/rt-test ((chaperone-procedure (lambda (x) x) (lambda (y) (values (lambda (z) 89) y))) 10))

;; Single argument, no post filter:
(as-chaperone-or-impersonator
 ([chaperone-procedure impersonate-procedure])
 (let* ([f (lambda (x) (list x x))]
        [in #f]
        [f2 (chaperone-procedure 
             f 
             (lambda (x) 
               (set! in x)
               x))])
   (test '(110 110) f 110)
   (test #f values in)
   (test '(111 111) f2 111)
   (test 111 values in)))

;; Multiple arguments, no post filter:
(as-chaperone-or-impersonator
 ([chaperone-procedure impersonate-procedure])
 (let* ([f (lambda (x y) (list x y))]
        [in #f]
        [f2 (chaperone-procedure 
             f 
             (lambda (x y) 
               (set! in (vector x y))
               (values x y)))])
   (test '(1100 1101) f 1100 1101)
   (test #f values in)
   (test '(1110 1111) f2 1110 1111)
   (test (vector 1110 1111) values in)))

;; Single argument, post filter on single value:
(as-chaperone-or-impersonator
 ([chaperone-procedure impersonate-procedure])
 (let* ([f (lambda (x) (list x x))]
        [in #f]
        [out #f]
        [f2 (chaperone-procedure 
             f 
             (lambda (x) 
               (set! in x)
               (values (lambda (y)
                         (set! out y)
                         y)
                       x)))])
   (test '(10 10) f 10)
   (test #f values in)
   (test #f values out)
   (test '(11 11) f2 11)
   (test 11 values in)
   (test '(11 11) values out)))

;; Multiple arguments, post filter on multiple values:
(as-chaperone-or-impersonator
 ([chaperone-procedure impersonate-procedure])
 (let* ([f (lambda (x y z) (values y (list x z)))]
        [in #f]
        [out #f]
        [f2 (chaperone-procedure 
             f 
             (lambda (x y z)
               (set! in (vector x y z))
               (values (lambda (y z)
                         (set! out (vector y z))
                         (values y z))
                       x y z)))])
   (test-values '(b (a c)) (lambda () (f 'a 'b 'c)))
   (test #f values in)
   (test #f values out)
   (test-values '(b (a c)) (lambda () (f2 'a 'b 'c)))
   (test (vector 'a 'b 'c) values in)
   (test (vector 'b '(a c)) values out)))

;; Optional keyword arguments:
(as-chaperone-or-impersonator
 ([chaperone-procedure impersonate-procedure])
 (let* ([f (lambda (x #:a [a 'a] #:b [b 'b]) (list x a b))]
        [in #f]
        [f2 (chaperone-procedure
             f
             (lambda (x #:a [a 'nope] #:b [b 'nope])
               (if (and (eq? a 'nope) (eq? b 'nope))
                   x
                   (values
                    (append 
                     (if (eq? a 'nope) null (list a))
                     (if (eq? b 'nope) null (list b)))
                    x))))])
   (test '(1 a b) f 1)
   (test '(1 a b) f2 1)
   (test '(1 2 b) f 1 #:a 2)
   (test '(1 2 b) f2 1 #:a 2)
   (test '(1 a 3) f 1 #:b 3)
   (test '(1 a 3) f2 1 #:b 3)
   (test '(1 2 3) f 1 #:a 2 #:b 3)
   (test '(1 2 3) f2 1 #:a 2 #:b 3)
   (test 1 procedure-arity f2)
   (test 'f object-name f2)
   (test-values '(() (#:a #:b)) (lambda () (procedure-keywords f2)))))

;; Optional keyword arguments with result chaperone:
(as-chaperone-or-impersonator
 ([chaperone-procedure impersonate-procedure])
 (let* ([f (lambda (x #:a [a 'a] #:b [b 'b]) (list x a b))]
        [in #f]
        [out #f]
        [f2 (chaperone-procedure
             f
             (lambda (x #:a [a 'nope] #:b [b 'nope])
               (set! in (list x a b))
               (if (and (eq? a 'nope) (eq? b 'nope))
                   x
                   (values
                    (lambda (z) (set! out z) z)
                    (append 
                     (if (eq? a 'nope) null (list a))
                     (if (eq? b 'nope) null (list b)))
                    x))))])
   (test '(1 a b) f 1)
   (test '(#f #f) list in out)
   (test '(1 a b) f2 1)
   (test '((1 nope nope) #f) list in out)
   (test '(1 2 b) f 1 #:a 2)
   (test '(1 2 b) f2 1 #:a 2)
   (test '((1 2 nope) (1 2 b)) list in out)
   (test '(1 a 3) f 1 #:b 3)
   (test '(1 a 3) f2 1 #:b 3)
   (test '(1 2 3) f 1 #:a 2 #:b 3)
   (test '(1 2 3) f2 1 #:a 2 #:b 3)
   (test 1 procedure-arity f2)
   (test 'f object-name f2)
   (test-values '(() (#:a #:b)) (lambda () (procedure-keywords f2)))))

;; Required keyword arguments:
(as-chaperone-or-impersonator
 ([chaperone-procedure impersonate-procedure])
 (let* ([f (lambda (x #:a [a 'a] #:b b) (list x a b))]
        [in #f]
        [f2 (chaperone-procedure
             f
             (lambda (x #:a [a 'nope] #:b [b 'nope])
               (if (and (eq? a 'nope) (eq? b 'nope))
                   x
                   (values
                    (append 
                     (if (eq? a 'nope) null (list a))
                     (if (eq? b 'nope) null (list b)))
                    x))))])
   (err/rt-test (f 1))
   (err/rt-test (f2 1))
   (err/rt-test (f 1 #:a 2))
   (err/rt-test (f2 1 #:a 2))
   (test '(1 a 3) f 1 #:b 3)
   (test '(1 a 3) f2 1 #:b 3)
   (test '(1 2 3) f 1 #:a 2 #:b 3)
   (test '(1 2 3) f2 1 #:a 2 #:b 3)
   (test 1 procedure-arity f2)
   (test 'f object-name f2)
   (test-values '((#:b) (#:a #:b)) (lambda () (procedure-keywords f2)))))

;; Required keyword arguments:
(as-chaperone-or-impersonator
 ([chaperone-procedure impersonate-procedure])
 (let* ([f (lambda (x #:a [a 'a] #:b b) (list x a b))]
        [in #f]
        [out #f]
        [f2 (chaperone-procedure
             f
             (lambda (x #:a [a 'nope] #:b [b 'nope])
               (set! in (list x a b))
               (if (and (eq? a 'nope) (eq? b 'nope))
                   x
                   (values
                    (lambda (z) (set! out z) z)
                    (append 
                     (if (eq? a 'nope) null (list a))
                     (if (eq? b 'nope) null (list b)))
                    x))))])
   (err/rt-test (f 1))
   (err/rt-test (f2 1))
   (err/rt-test (f 1 #:a 2))
   (err/rt-test (f2 1 #:a 2))
   (test '(1 a 3) f 1 #:b 3)
   (test '(1 a 3) f2 1 #:b 3)
   (test '((1 nope 3) (1 a 3)) list in out)
   (test '(1 2 3) f 1 #:a 2 #:b 3)
   (test '(1 2 3) f2 1 #:a 2 #:b 3)
   (test 1 procedure-arity f2)
   (test 'f object-name f2)
   (test-values '((#:b) (#:a #:b)) (lambda () (procedure-keywords f2)))))

(err/rt-test ((chaperone-procedure (lambda (x) x) (lambda (y) (values y y))) 1))
(err/rt-test ((impersonate-procedure (lambda (x) x) (lambda (y) (values y y))) 1))
(err/rt-test ((chaperone-procedure (lambda (x) x) (lambda (y) (values y y y))) 1))
(err/rt-test ((impersonate-procedure (lambda (x) x) (lambda (y) (values y y y))) 1))

;; ----------------------------------------

(define is-chaperone #t)
(define is-not-chaperone #f)

(as-chaperone-or-impersonator
 ([chaperone-struct impersonate-struct]
  [is-chaperone is-not-chaperone]
  [chaperone?/impersonator impersonator?])
 (let ()
   (define-values (prop:blue blue? blue-ref) (make-impersonator-property 'blue))
   (define-values (prop:green green? green-ref) (make-struct-type-property 'green 'can-impersonate))
   (define-values (prop:red red? red-ref)
     (make-struct-type-property 'red (lambda (v i) (symbol->string v)) null #t))
   (define-struct a ([x #:mutable] y))
   (define-struct (b a) ([z #:mutable]))
   (define-struct (c b) ([n #:mutable]) #:transparent)
   (define-struct p (u) #:property prop:green 'green)
   (define-struct r (t) #:property prop:red 'red)
   (define-struct (q p) (v w))
   (test #t chaperone?/impersonator (chaperone-struct (make-a 1 2) a-x (lambda (a v) v)
                                                      set-a-x! (lambda (a v) v)))
   (test #t chaperone?/impersonator (chaperone-struct (make-b 1 2 3) a-x (lambda (a v) v)
                                                      set-a-x! (lambda (a v) v)))
   (test #t chaperone?/impersonator (chaperone-struct (make-p 1) green-ref (lambda (a v) v)))
   (test #t chaperone?/impersonator (chaperone-struct (make-r 1) red-ref (lambda (a v) v)))
   (test #t chaperone?/impersonator (chaperone-struct (make-a 1 2) a-x (lambda (a v) v)
                                                      set-a-x! (lambda (a v) v)
                                                      prop:blue 'blue))
   (test #t chaperone?/impersonator (chaperone-struct (make-c 1 2 3 4) c-n (lambda (b v) v)))
   (when is-chaperone
     (test #t chaperone?/impersonator (chaperone-struct 
                                       (chaperone-struct (make-a 1 2) a-x (lambda (a v) v) prop:blue 'blue)
                                       a-x (lambda (a v) v)
                                       prop:blue 'blue)))
   (err/rt-test (chaperone-struct (make-a 1 2) b-z (lambda (a v) v)))
   (err/rt-test (chaperone-struct (make-p 1) a-x (lambda (a v) v)))
   (err/rt-test (chaperone-struct (make-q 1 2 3) a-x (lambda (a v) v)))
   (err/rt-test (chaperone-struct (make-a 1 2) 5 (lambda (a v) v)))
   (err/rt-test (chaperone-struct (make-a 1 2) a-x 5))
   (err/rt-test (chaperone-struct (make-a 1 2) a-x (lambda (x) x)))
   (err/rt-test (chaperone-struct (make-a 1 2) blue-ref (lambda (a v) v)))
   (err/rt-test (chaperone-struct (make-a 1 2) a-x (lambda (a v) v) prop:green 'green))
   (err/rt-test (chaperone-struct
                 (chaperone-struct (make-a 1 2) a-x (lambda (a v) v) prop:blue 'blue)
                 blue-ref (lambda (a v) v)))
   (when is-chaperone
     (let* ([a1 (make-a 1 2)]
            [get #f]
            [set #f]
            [a2 (chaperone-struct a1 a-y (lambda (an-a v) (set! get v) v)
                                  set-a-x! (lambda (an-a v) (set! set v) v))]
            [p1 (make-p 100)]
            [p-get #f]
            [p2 (chaperone-struct p1 green-ref (lambda (p v) (set! p-get v) v))]
            [a3 (chaperone-struct a1 a-x (lambda (a y) y) prop:blue 8)])
       (test 2 a-y a1)
       (test #f values get)
       (test #f values set)
       (test 2 a-y a2)
       (test 2 values get)
       (test #f values set)
       (test (void) set-a-x! a1 0)
       (test 0 a-x a1)
       (test 0 a-x a2)
       (test 2 values get)
       (test #f values set)
       (test (void) set-a-x! a2 10)
       (test 2 values get)
       (test 10 values set)
       (test 10 a-x a1)
       (test 10 a-x a2)
       (test 2 a-y a1)
       (test 2 a-y a2)
       (test #t green? p1)
       (test #t green? p2)
       (test 'green green-ref p1)
       (test #f values p-get)
       (test 'green green-ref p2)
       (test 'green values p-get)
       (test #f blue? a1)
       (test #f blue? a2)
       (test #t blue? a3)
       (test 8 blue-ref a3)))
   (let* ([a1 (make-b 1 2 3)]
          [get #f]
          [set #f]
          [a2 (chaperone-struct a1 b-z (lambda (an-a v) (set! get v) v)
                                set-b-z! (lambda (an-a v) (set! set v) v))])
     (test 1 a-x a2)
     (test 2 a-y a2)
     (test 3 b-z a1)
     (test #f values get)
     (test #f values set)
     (test 3 b-z a2)
     (test 3 values get)
     (test #f values set)
     (test (void) set-b-z! a1 0)
     (test 0 b-z a1)
     (test 3 values get)
     (test #f values set)
     (test 0 b-z a2)
     (test 0 values get)
     (test #f values set)
     (test (void) set-b-z! a2 10)
     (test 0 values get)
     (test 10 values set)
     (test 10 b-z a1)
     (test 10 b-z a2)
     (test 2 a-y a1)
     (test 2 a-y a2)
     (test 10 values get)
     (test 10 values set))
   (let* ([a1 (make-a 0 2)]
          [a2 (chaperone-struct a1 a-x (lambda (a v) (if (equal? v 1) 'bad v))
                                set-a-x! (lambda (a v) (if (equal? v 3) 'bad v)))])
     (test 0 a-x a1)
     (test 0 a-x a2)
     (test (void) set-a-x! a1 1)
     (test 1 a-x a1)
     (if is-chaperone
         (err/rt-test (a-x a2))
         (test 'bad a-x a2))
     (test (void) set-a-x! a1 3)
     (test 3 a-x a1)
     (test 3 a-x a2)
     (test (void) set-a-x! a1 4)
     (if is-chaperone
         (err/rt-test (set-a-x! a2 3))
         (begin
           (test (void) set-a-x! a2 3)
           (test (void) set-a-x! a2 4)))
     (test 4 a-x a1)
     (test 4 a-x a2)
     (let* ([a3 (chaperone-struct a2 a-x (lambda (a v) (if (equal? v 10) 'bad v))
                                  set-a-x! (lambda (a v) (if (equal? v 30) 'bad v)))])
       (set-a-x! a2 30)
       (if is-chaperone
           (begin
             (err/rt-test (set-a-x! a3 30))
             (err/rt-test (set-a-x! a3 3)))
           (begin
             (test (void) set-a-x! a3 30)
             (test 'bad a-x a3)
             (test (void) set-a-x! a3 3)
             (test 'bad a-x a3)
             (test (void) set-a-x! a3 1)))
       (set-a-x! a3 1)
       (test 1 a-x a1)
       (if is-chaperone
           (begin
             (err/rt-test (a-x a2))
             (err/rt-test (a-x a3)))
           (begin
             (test 'bad a-x a2)
             (test 'bad a-x a3)))))))

;; test to see if the guard is actually called even when impersonated
(let ()
  (define-values (prop:red red? red-ref)
    (make-struct-type-property 'red (lambda (v i) (symbol->string v)) null #t))
  (define-struct a (b) #:property prop:red 'red)
  (test "red" red-ref (impersonate-struct (make-a 1) red-ref (lambda (v f-v) f-v)))
  (test 5 red-ref (impersonate-struct (make-a 1) red-ref (lambda (v f-v) 5))))

(as-chaperone-or-impersonator
 ([chaperone-struct impersonate-struct]
  [is-chaperone is-not-chaperone]
  [chaperone?/impersonator impersonator?])
 (struct c ([n #:mutable]) #:transparent)
 (let* ([got? #f]
        [c1 (chaperone-struct (c 1) c-n (lambda (b v) (set! got? #t) v))])
   (void (equal-hash-code c1))
   (test #t values got?)
   (set! got? #f)
   (void (equal-secondary-hash-code c1))
   (test #t values got?)
   (define c3 (c 1))
   (set! got? #f)
   (void (equal? c1 c3))
   (test #t values got?)
   (set! got? #f)
   (void (equal? c3 c1))
   (test #t values got?))

 ;; Hashing with `prop:equal+hash`:
 (let ()
   (define got? #f)
   (define mine? #t)
   (define check! (lambda (o) (set! mine? #t) (d-n o)))
   (struct d ([n #:mutable])
     #:property prop:equal+hash (list
                                 (lambda (a b r) (r (d-n a) (d-n b)))
                                 (lambda (a r) (check! a) 0)
                                 (lambda (a r) (check! a) 0)))
   (define d1 (chaperone-struct (d 1)
                                d-n (lambda (b v) (set! got? #t) v)
                                set-d-n! (lambda (b v) v)))
   (void (equal-hash-code d1))
   (test '(#t #t) list got? mine?)
   (set! got? #f)
   (set! mine? #f)
   (void (equal-secondary-hash-code d1))
   (test '(#t #t) list got? mine?)
   (define d3 (d 1))
   (set! got? #f)
   (set! mine? #f)
   (test #t values (equal? d1 d3))
   (test '(#t #f) list got? mine?)
   (set! got? #f)
   (set! mine? #f)
   (test #t values (equal? d3 d1))
   (test '(#t #f) list got? mine?))

 ;; Hashing without `prop:equal+hash`:
 (let ()
   (define got? #f)
   (struct d ([n #:mutable])
     #:transparent)
   (define d1 (chaperone-struct (d 1) d-n (lambda (b v) (set! got? #t) v)))
   (void (equal-hash-code d1))
   (test '(#t) list got?)
   (set! got? #f)
   (void (equal-secondary-hash-code d1))
   (test '(#t) list got?)
   (define d3 (d 1))
   (set! got? #f)
   (test #t values (equal? d1 d3))
   (test '(#t) list got?)
   (set! got? #f)
   (test #t values (equal? d3 d1))
   (test '(#t) list got?)))

;; Check use of chaperoned accessor to chaperone a structure:
(let ()
  (define-values (prop: prop? prop-ref) (make-struct-type-property 'prop))
  (struct x [a]
    #:mutable
    #:property prop: 'secret)
  (define v1 (x 'secret))
  (define v2 (x 'public))
  (define v3 (x #f))

  ;; Original accessor and mutators can get 'secret and install 'garbage:
  (test 'secret x-a v1)
  (test (void) set-x-a! v3 'garbage)
  (test 'garbage x-a v3)
  (set-x-a! v3 #f)
  (test 'secret prop-ref v1)
  (test 'secret prop-ref struct:x)

  (define get-a
    (chaperone-procedure x-a
                         (lambda (s)
                           (values (lambda (r)
                                     (when (eq? r 'secret)
                                       (error "sssh!"))
                                     r)
                                   s))))
  (define lie-a
    (impersonate-procedure x-a
                           (lambda (s)
                             (values (lambda (r)
                                       'whatever)
                                     s))))
  (define set-a!
    (chaperone-procedure set-x-a!
                         (lambda (s v)
                           (when (eq? v 'garbage)
                             (error "no thanks!"))
                           (values s v))))
  (define mangle-a!
    (impersonate-procedure set-x-a!
                           (lambda (s v)
                             (values s 'garbage))))
  (define get-prop
    (chaperone-procedure prop-ref
                         (case-lambda
                          [(s)
                           (values (lambda (r)
                                     (when (eq? r 'secret)
                                       (error "sssh!"))
                                     r)
                                   s)]
                          [(s def)
                           (values (lambda (r)
                                     (when (eq? r 'secret)
                                       (error "sssh!"))
                                     r)
                                   s
                                   def)])))

  (test 'public get-a v2)
  (err/rt-test (get-a v1) exn:fail?)

  (test (void) set-a! v3 'fruit)
  (test 'fruit x-a v3)
  (err/rt-test (set-a! v3 'garbage) exn:fail?)
  (test 'fruit x-a v3)

  (test 'whatever lie-a v1)
  (test 'whatever lie-a v2)
  (test (void) mangle-a! v3 'fruit)
  (test 'garbage get-a v3)
  (set-a! v3 #f)

  (err/rt-test (get-prop v1) exn:fail?)
  (err/rt-test (get-prop struct:x) exn:fail?)

  (define (wrap v
                #:chaperone-struct [chaperone-struct chaperone-struct]
                #:get-a [get-a get-a]
                #:set-a! [set-a! set-a!]
                #:get-prop [get-prop get-prop])
    (chaperone-struct v
                      get-a (lambda (s v)
                              (when (eq? v 'secret)
                                (raise 'leaked!))
                              v)
                      set-a! (lambda (s v)
                               v)
                      get-prop (lambda (s v)
                                 (when (eq? v 'secret)
                                   (raise 'leaked-via-property!))
                                 v)))

  (test 'public x-a (wrap v2))
  ;; Can't access 'secret by using `get-a` to chaperone:
  (err/rt-test (x-a (wrap v1)) exn:fail?)
  ;; More-nested chaperone takes precedence:
  (err/rt-test (x-a (wrap (chaperone-struct v1 x-a
                                            (lambda (s v)
                                              (raise 'early)))))
               (lambda (exn) (eq? exn 'early)))
  ;; Double chaperone should be ok:
  (err/rt-test (get-a (wrap v1)) exn:fail?)
  ;; Can't allow 'garbage into a value chaperoned using `set-a!`:
  (err/rt-test (set-x-a! (wrap v3) 'garbage) exn:fail?)
  (err/rt-test (set-a! (wrap v3) 'garbage) exn:fail?)
  ;; Can't access 'secret by using `get-prop` to chaperone:
  (err/rt-test (prop-ref (wrap v1)) exn:fail?)

  ;; Cannot chaperone using an impersonated operation:
  (err/rt-test (wrap v2 #:get-a lie-a))
  (err/rt-test (wrap v2 #:set-a! mangle-a!))
  ;; Can impersonate with impersonated operation:
  (test 'whatever x-a (wrap v2
                            #:chaperone-struct impersonate-struct
                            #:get-a lie-a
                            #:get-prop (let ()
                                         (define-values (prop:blue blue? blue-ref) (make-impersonator-property 'blue))
                                         ;; dummy, since property accessor cannot be impersonated:
                                         prop:blue)))

  ;; Currently, `chaperone-struct-type` does not accept
  ;; a property accessor as an argument. Probably it should,
  ;; in which case we need to test a chaperone put in place
  ;; with `get-prop`.
  (void))

;; ----------------------------------------

(as-chaperone-or-impersonator
 ([chaperone-struct impersonate-struct])
 (as-chaperone-or-impersonator
  ([chaperone-procedure impersonate-procedure])
  (let ()
    (define (test-sub linear? rev?)
      (define-struct a (x [y #:mutable]) #:property prop:procedure 0)
      (let* ([a1 (make-a (lambda (x) (list x x)) 10)]
             [get #f]
             [a2 (chaperone-struct a1 a-y (lambda (a v) (set! get v) v)
                                   set-a-y! (lambda (a v) v))]
             [pre #f]
             [post #f]
             [a3 (chaperone-procedure (if linear? a2 a1)
                                      (lambda (z)
                                        (set! pre z)
                                        (values (lambda (r)
                                                  (set! post r)
                                                  r)
                                                z)))]
             [a2 (if rev?
                     (chaperone-struct a3 a-y (lambda (a v) (set! get v) v)
                                       set-a-y! (lambda (a v) v))
                     a2)])
        (test #t a? a1)
        (test #t a? a2)
        (test #t a? a3)
        (test #t procedure? a1)
        (test #t procedure? a2)
        (test #t procedure? a3)
        (test '(12 12) a1 12)
        (test #f values get)
        (test #f values pre)
        (test #f values post)
        (test '(12 12) a2 12)
        (test #f values get)
        (test (if rev? 12 #f) values pre)
        (test (if rev? '(12 12) #f) values post)
        (test '(12 12) a3 12)
        (test #f values get)
        (test 12 values pre)
        (test '(12 12) values post)
        (test 10 a-y a1)
        (test #f values get)
        (test 10 a-y a2)
        (test 10 values get)
        (test 10 a-y a3)
        (test (void) set-a-y! a1 9)
        (test 9 a-y a3)
        (test (if linear? 9 10) values get)
        (test 9 a-y a2)
        (test 9 values get)))
    (test-sub #f #f)
    (test-sub #t #f)
    (test-sub #f #t))))

;; ----------------------------------------

(let ()
  (define-values (prop:blue blue? blue-ref) (make-impersonator-property 'blue))
  (let* ([v1 (vector 1 2 3)]
         [v2 (chaperone-vector v1 (lambda (vec i v) v) (lambda (vec i v) v)
                               prop:blue 89)]
         [v3 (chaperone-vector v1 (lambda (vec i v) v) (lambda (vec i v) v))]
         [b1 (box 0)]
         [b2 (chaperone-box b1 (lambda (b v) v) (lambda (b v) v)
                            prop:blue 99)]
         [b3 (chaperone-box b1 (lambda (b v) v) (lambda (b v) v))]
         [p1 (lambda (z) z)]
         [p2 (chaperone-procedure p1 (lambda (v) v) prop:blue 109)]
         [p3 (chaperone-procedure p1 (lambda (v) v))])
    (define (check v1 v2 v3 val check)
      (test #f blue? v1)
      (test #t blue? v2)
      (test #f blue? v3)
      (test val blue-ref v2)
      (err/rt-test (blue-ref v1))
      (err/rt-test (blue-ref v3))
      (test #t check v1)
      (test #t check v2)
      (test #t check v3))
    (check v1 v2 v3 89 (lambda (v) (= (vector-ref v 1) 2)))
    (check b1 b2 b3 99 (lambda (b) (= (unbox b) 0)))
    (check p1 p2 p3 109 (lambda (p) (= (p 77) 77)))))

;; ----------------------------------------

(for-each
 (lambda (make-hash)
   (let ([h (chaperone-hash (make-hash)
                            (lambda (h k) (values k (lambda (h k v) v)))
                            (lambda (h k v) (values k v))
                            (lambda (h k) k) (lambda (h k) k))])
     (test #t chaperone?/impersonator h)
     (test #t hash? h)
     (test #t (lambda (x) (hash? x)) h)))
 (list
  make-hash make-hasheq make-hasheqv
  (lambda () #hash()) (lambda () #hasheq()) (lambda () #hasheqv())
  make-weak-hash make-weak-hasheq make-weak-hasheqv))

(let ([mk (lambda clear-proc+more
            (apply chaperone-hash (make-hash)
                   (lambda (h k) (values k (lambda (h k v) v)))
                   (lambda (h k v) (values k v))
                   (lambda (h k) k) (lambda (h k) k)
                   clear-proc+more))])
  (test #t chaperone? (mk))
  (test #t chaperone? (mk #f))
  (test #t chaperone? (mk (lambda (ht) (void))))
  (err/rt-test (mk (lambda (a b) (void))))
  (define-values (prop:blue blue? blue-ref) (make-impersonator-property 'blue))
  (test #t chaperone? (mk prop:blue 'ok))
  (test #t chaperone? (mk #f prop:blue 'ok))
  (err/rt-test (mk (lambda (a b) (void)) prop:blue 'ok)))

(for-each
 (lambda (make-hash)
   (let ([h (impersonate-hash (make-hash)
                              (lambda (h k) (values k (lambda (h k v) v)))
                              (lambda (h k v) (values k v))
                              (lambda (h k) k) (lambda (h k) k))])
     (test #t impersonator? h)
     (test #t hash? h)
     (test #t (lambda (x) (hash? x)) h)))
 (list
  make-hash make-hasheq make-hasheqv
  make-weak-hash make-weak-hasheq make-weak-hasheqv))

(for-each 
 (lambda (make-hash)
   (err/rt-test
    (impersonate-hash (make-hash) 
                      (lambda (h k) (values k (lambda (h k v) v)))
                      (lambda (h k v) (values k v))
                      (lambda (h k) k) (lambda (h k) k))))
 (list (lambda () #hash()) (lambda () #hasheq()) (lambda () #hasheqv())))

(as-chaperone-or-impersonator
 ([chaperone-hash impersonate-hash])
 (for-each
  (lambda (make-hash)
    (let* ([h1 (make-hash)]
           [get-k #f]
           [get-v #f]
           [set-k #f]
           [set-v #f]
           [remove-k #f]
           [access-k #f]
           [h2 (chaperone-hash h1
                               (lambda (h k) 
                                 (set! get-k k)
                                 (values k
                                         (lambda (h k v)
                                           (set! get-v v)
                                           v)))
                               (lambda (h k v) 
                                 (set! set-k k)
                                 (set! set-v v)
                                 (values k v))
                               (lambda (h k) 
                                 (set! remove-k k)
                                 k) 
                               (lambda (h k) 
                                 (set! access-k k)
                                 k))]
           [test (lambda (val proc . args)
                   ;; Avoid printign hash-table argument, which implicitly uses `ref':
                   (let ([got (apply proc args)])
                     (test #t (format "~s ~s ~s" proc val got) (equal? val got))))])
      (test #f hash-iterate-first h1)
      (test #f hash-iterate-first h2)
      (test #f hash-ref h1 'key #f)
      (test '(#f #f #f #f #f #f) list get-k get-v set-k set-v remove-k access-k)
      (test 'nope hash-ref h2 'key 'nope)
      (test '(key #f #f #f #f #f) list get-k get-v set-k set-v remove-k access-k)
      (set! get-k #f)
      (test (void) hash-set! h1 'key 'val)
      (test '(#f #f #f #f #f #f) list get-k get-v set-k set-v remove-k access-k)
      (test #f not (hash-iterate-first h1))
      (test #f not (hash-iterate-first h2))
      (test #f hash-iterate-next h2 (hash-iterate-first h2))
      (test 'val hash-ref h1 'key #f)
      (test '(#f #f #f #f #f #f) list get-k get-v set-k set-v remove-k access-k)
      (test 'val hash-ref h2 'key #f)
      (test '(key val #f #f #f #f) list get-k get-v set-k set-v remove-k access-k)
      (test (void) hash-set! h2 'key2 'val2)
      (test '(key val key2 val2 #f #f) list get-k get-v set-k set-v remove-k access-k)
      (test 'val2 hash-ref h1 'key2 #f)
      (test '(key val key2 val2 #f #f) list get-k get-v set-k set-v remove-k access-k)
      (test 'val2 hash-ref h2 'key2 #f)
      (test '(key2 val2 key2 val2 #f #f) list get-k get-v set-k set-v remove-k access-k)
      (test (void) hash-remove! h2 'key3)
      (test '(key2 val2 key2 val2 key3 #f) list get-k get-v set-k set-v remove-k access-k)
      (test 'val2 hash-ref h2 'key2)
      (test '(key2 val2 key2 val2 key3 #f) list get-k get-v set-k set-v remove-k access-k)
      (test (void) hash-remove! h2 'key2)
      (test '(key2 val2 key2 val2 key2 #f) list get-k get-v set-k set-v remove-k access-k)
      (test #f hash-ref h2 'key2 #f)
      (test '(key2 val2 key2 val2 key2 #f) list get-k get-v set-k set-v remove-k access-k)
      (hash-for-each h2 void)
      (test '(key val key2 val2 key2 key) list get-k get-v set-k set-v remove-k access-k)
      (set! get-k #f)
      (set! get-v #f)
      (void (equal-hash-code h2))
      (test '(key val key2 val2 key2 key) list get-k get-v set-k set-v remove-k access-k)
      (set! get-k #f)
      (set! get-v #f)
      (void (equal-secondary-hash-code h2))
      (test '(key val key2 val2 key2 key) list get-k get-v set-k set-v remove-k access-k)
      (set! get-k #f)
      (set! get-v #f)
      (test #t values (equal? h2 (let* ([h2 (make-hash)])
                                   (test (void) hash-set! h2 'key 'val)
                                   h2)))
      (test '(key val key2 val2 key2 key) list get-k get-v set-k set-v remove-k access-k)
      (void)))
  (list
   make-hash make-hasheq make-hasheqv
   make-weak-hash make-weak-hasheq make-weak-hasheqv)))

(for-each
 (lambda (h1)
   (let* ([get-k #f]
          [get-v #f]
          [set-k #f]
          [set-v #f]
          [remove-k #f]
          [access-k #f]
          [h2 (chaperone-hash h1
                              (lambda (h k) 
                                (set! get-k k)
                                (values k
                                        (lambda (h k v)
                                          (set! get-v v)
                                          v)))
                              (lambda (h k v) 
                                (set! set-k k)
                                (set! set-v v)
                                (values k v))
                              (lambda (h k) 
                                (set! remove-k k)
                                k) 
                              (lambda (h k) 
                                (set! access-k k)
                                k))]
          [test (lambda (val proc . args)
                  ;; Avoid printing hash-table argument, which implicitly uses `ref':
                  (let ([got (apply proc args)])
                    (test #t (format "~s ~s ~s" proc val got) (equal? val got))))])
     (test #f hash-ref h1 'key #f)
     (test '(#f #f #f #f #f #f) list get-k get-v set-k set-v remove-k access-k)
     (test 'nope hash-ref h2 'key 'nope)
     (test '(key #f #f #f #f #f) list get-k get-v set-k set-v remove-k access-k)
     (let ([h2 (hash-set h2 'key 'val)])
       (test '(key #f key val #f #f) list get-k get-v set-k set-v remove-k access-k)
       (test 'val hash-ref h2 'key #f)
       (test '(key val key val #f #f) list get-k get-v set-k set-v remove-k access-k)
       (let ([h2 (hash-set h2 'key2 'val2)])
         (test '(key val key2 val2 #f #f) list get-k get-v set-k set-v remove-k access-k)
         (test 'val2 hash-ref h2 'key2 #f)
         (test '(key2 val2 key2 val2 #f #f) list get-k get-v set-k set-v remove-k access-k)
         (let ([h2 (hash-remove h2 'key3)])
           (test '(key2 val2 key2 val2 key3 #f) list get-k get-v set-k set-v remove-k access-k)
           (test 'val2 hash-ref h2 'key2)
           (test '(key2 val2 key2 val2 key3 #f) list get-k get-v set-k set-v remove-k access-k)
           (let ([h2 (hash-remove h2 'key2)])
             (test '(key2 val2 key2 val2 key2 #f) list get-k get-v set-k set-v remove-k access-k)
             (test #f hash-ref h2 'key2 #f)
             (test '(key2 val2 key2 val2 key2 #f) list get-k get-v set-k set-v remove-k access-k)
             (hash-for-each h2 void)
             (test '(key val key2 val2 key2 key) list get-k get-v set-k set-v remove-k access-k)
             (set! get-k #f)
             (set! get-v #f)
             (void (equal-hash-code h2))
             (test '(key val key2 val2 key2 key) list get-k get-v set-k set-v remove-k access-k)
             (set! get-k #f)
             (set! get-v #f)
             (void (equal-secondary-hash-code h2))
             (test '(key val key2 val2 key2 key) list get-k get-v set-k set-v remove-k access-k)
             (set! get-k #f)
             (set! get-v #f)
             (test #t values (equal? h2 (hash-set h1 'key 'val)))
             (test '(key val key2 val2 key2 key) list get-k get-v set-k set-v remove-k access-k)
             (void)))))))
 (list #hash() #hasheq() #hasheqv()))

;; ----------------------------------------

(as-chaperone-or-impersonator
 ([chaperone-hash impersonate-hash]
  [chaperone-procedure impersonate-procedure])
 (letrec ([wrap
           (lambda (v)
             (cond
              [(hash? v)
               (chaperone-hash v
                               (lambda (h k)
                                 (values (wrap k)
                                         (lambda (h k v) (wrap v))))
                               (lambda (h k v)
                                 (values (wrap k) (wrap v)))
                               (lambda (h k)
                                 (wrap k))
                               (lambda (h k)
                                 (wrap k)))]
              [(procedure? v) (chaperone-procedure 
                               v 
                               (lambda args
                                 (apply values
                                        (lambda args
                                          (apply values (map wrap args)))
                                        (map wrap args))))]
              [(number? v) v]
              [else (error 'wrap "cannot wrap: ~v" v)]))])
   (let ([ht (wrap (wrap (make-hash)))])
     (hash-set! ht add1 sub1)
     (test 9 (hash-ref ht add1) 10)
     (test '(10) 'for-hash (for/list ([(k v) (in-hash ht)])
                             (test v hash-ref ht k)
                             (k (v 10))))
     (test (void) hash-clear! ht)
     (test 0 hash-count ht))))

(as-chaperone-or-impersonator
 ([chaperone-hash impersonate-hash]
  [chaperone-procedure impersonate-procedure]
  [sub1 add1])
 (define hit? #f)
 (define (mk ht)
   (chaperone-hash ht
                   (lambda (h k)
                     (values k
                             (lambda (h k v) v)))
                   (lambda (h k v)
                     (values k v))
                   (lambda (h k) k)
                   (lambda (h k) k)
                   (lambda (h)
                     (set! hit? #t)
                     (test #t hash? h))))
 (let* ([ht (make-hash)]
        [ht2 (mk ht)])
   (hash-set! ht2 'a 1)
   (hash-set! ht2 'b 2)
   (test #f values hit?)
   (test (void) hash-clear! ht2)
   (test #t values hit?)
   (test 0 hash-count ht)
   (test 0 hash-count ht2))
 (when (negative? (sub1 0))
   (let* ([ht (hash 'a 1)]
          [ht2 (mk ht)])
     (define ht3 (hash-set ht2 'b 2))
     (set! hit? #f)
     (define ht4 (hash-clear ht2))
     (test #t values hit?)
     (test 0 hash-count ht4))))
 
;; ----------------------------------------

;; Check broken key impersonator:

(let ([check
       (lambda (orig)
         (let ([h (impersonate-hash
                   orig
                   (λ (h k) 
                      (values 'bad1
                              (λ (h k v)
                                 'bad2)))
                   (λ (h k v) (values 'bad3 'bad4))
                   (λ (h k) 'bad5)
                   (λ (h k) 'bad6))])
           (test (void) hash-set! h 1 2)
           (test #f hash-ref h 1 #f)
           (err/rt-test (hash-iterate-value h (hash-iterate-first h)))
           (err/rt-test (hash-map h void))
           (err/rt-test (hash-for-each h void))))])
  (check (make-hash))
  (check (make-hasheq))
  (check (make-weak-hash))
  (check (make-weak-hasheq)))

(let ([check
       (lambda (orig)
         (let ([h (chaperone-hash
                   orig
                   (λ (h k) 
                      (values (chaperone-vector k (lambda (b i v) v) (lambda (b i v) v))
                              (λ (h k v) v)))
                   (λ (h k v) (values (chaperone-vector k (lambda (b i v) v) (lambda (b i v) v))
                                      v))
                   (λ (h k) (chaperone-vector k (lambda (b i v) v) (lambda (b i v) v)))
                   (λ (h k) (chaperone-vector k (lambda (b i v) v) (lambda (b i v) v))))])
           (let* ([vec (vector 1 2 3)]
                  [h (hash-set h vec 2)])
             (test #f hash-ref h vec #f)
             (err/rt-test (hash-iterate-value h (hash-iterate-first h)))
             (err/rt-test (hash-map h void))
             (err/rt-test (hash-for-each h void)))))])
  (check (hasheq)))

;; ----------------------------------------

(let ()
  (define-struct a (x y) #:transparent)
  (let* ([a1 (make-a 1 2)]
         [got? #f]
         [a2 (chaperone-struct a1 a-x (lambda (a v) v)
                               struct-info (lambda (s ?)
                                             (set! got? #t)
                                             (values s ?)))]
         [got2? #f]
         [a3 (chaperone-struct a2 a-x (lambda (a v) v)
                               struct-info (lambda (s ?)
                                             (set! got2? #t)
                                             (values s ?)))])
    (test-values (list struct:a #f) (lambda () (struct-info a1)))
    (test #f values got?)
    (test-values (list struct:a #f) (lambda () (struct-info a2)))
    (test #t values got?)
    (set! got? #f)
    (test-values (list struct:a #f) (lambda () (struct-info a3)))
    (test #t values got?)
    (test #t values got2?)))

;; ----------------------------------------

(let ()
  (define-struct a (x y) #:transparent)
  (let* ([got? #f]
         [constr? #f]
         [guarded? #f]
         [struct:a2 (chaperone-struct-type 
                     struct:a
                     (lambda (name init-cnt auto-cnt acc mut imms super skipped?)
                       (set! got? #t)
                       (values name init-cnt auto-cnt acc mut imms super skipped?))
                     (lambda (c) 
                       (set! constr? #t)
                       c)
                     (lambda (x y name)
                       (set! guarded? #t)
                       (values x y)))])
    (test #t struct-type? struct:a2)
    (let-values ([(name init-cnt auto-cnt acc mut imms super skipped?)
                  (struct-type-info struct:a2)])
      (test #t values got?)
      (test '(a 2 0 #f #f) values (list name init-cnt auto-cnt super skipped?)))
    (test #f values constr?)
    (test #t procedure? (struct-type-make-constructor struct:a2))
    (test #t values constr?)
    (let ()
      (define-struct b (z) #:super struct:a2)
      (test #f values guarded?)
      (make-b 1 2 3)
      (test #t values guarded?))))

;; ----------------------------------------

(as-chaperone-or-impersonator
 ([chaperone-procedure impersonate-procedure])
 (let ()
   (define (check-param current-directory)
     (parameterize ([current-directory (current-directory)])
       (let* ([pre-cd? #f]
              [post-cd? #f]
              [got-cd? #f]
              [post-got-cd? #f]
              [cd1 (chaperone-procedure current-directory (case-lambda 
                                                           [() (set! got-cd? #t) (values)]
                                                           [(v) (set! pre-cd? #t) v]))]
              [cd2 (chaperone-procedure current-directory (case-lambda 
                                                           [() (set! got-cd? #t)
                                                            (lambda (r)
                                                              (set! post-got-cd? #t)
                                                              r)]
                                                           [(v)
                                                            (set! pre-cd? #t)
                                                            (values (lambda (x)
                                                                      (set! post-cd? #t)
                                                                      (void))
                                                                    v)]))])
         (test #t parameter? cd1)
         (test #t parameter? cd2)
         (test '(#f #f #f #f) list pre-cd? post-cd? got-cd? post-got-cd?)
         (test (current-directory) cd1)
         (test '(#f #f #t #f) list pre-cd? post-cd? got-cd? post-got-cd?)
         (test (current-directory) cd2)
         (test '(#f #f #t #t) list pre-cd? post-cd? got-cd? post-got-cd?)
         (cd1 (current-directory))
         (test '(#t #f #t #t) list pre-cd? post-cd? got-cd? post-got-cd?)
         (set! pre-cd? #f)
         (parameterize ([cd1 (current-directory)])
           (test '(#t #f #t #t) list pre-cd? post-cd? got-cd? post-got-cd?))
         (set! pre-cd? #f)
         (cd2 (current-directory))
         (test '(#t #t #t #t) list pre-cd? post-cd? got-cd? post-got-cd?)
         (set! pre-cd? #f)
         (set! post-cd? #f)
         (parameterize ([cd2 (current-directory)])
           (test '(#t #t #t #t) list pre-cd? post-cd? got-cd? post-got-cd?)))))
   (check-param current-directory)
   (let ([p (make-parameter 88)])
     (check-param p))))

;; ----------------------------------------

(test #t equal?
      (chaperone-procedure add1 void) 
      (chaperone-procedure add1 void))
(test #t equal?
      (impersonate-procedure add1 void) 
      (chaperone-procedure add1 void))
(test #t equal?
      (chaperone-procedure add1 void) 
      (impersonate-procedure add1 void))

;; ----------------------------------------

;; evt chaperones

(test #t evt? (chaperone-evt always-evt void))
(test #t chaperone-of?/impersonator (chaperone-evt always-evt void) always-evt)
(test #f chaperone-of? (chaperone-evt always-evt void) (chaperone-evt always-evt void))
(test #t chaperone-of?/impersonator (chaperone-evt (chaperone-evt always-evt void) void) always-evt)
(test always-evt sync (chaperone-evt always-evt (lambda (e) (values e values))))
(test #f sync/timeout 0 (chaperone-evt never-evt (lambda (e) (values e (lambda (v) (error "bad"))))))

(err/rt-test (chaperone-evt always-evt (lambda () 0)))
(test #t evt? (chaperone-evt always-evt (lambda (x) x)))
(err/rt-test (sync (chaperone-evt never-evt (lambda (x) x))))
(err/rt-test (sync (chaperone-evt never-evt (lambda (x) (values x (lambda () 10))))))
(test #f sync/timeout 0 (chaperone-evt never-evt (lambda (x) (values x (lambda (v) 10)))))
(err/rt-test (sync/timeout 0 (chaperone-evt always-evt (lambda (x) (values x (lambda (v) 10))))))
(test #t chaperone-of? 
      (sync/timeout 0 (chaperone-evt always-evt (lambda (x) (values x (lambda (v)
                                                                        (chaperone-evt always-evt void))))))
      always-evt)

(let ([did-0 #f]
      [did-1 #f]
      [did-2 #f]
      [did-3 #f]
      [v 0])
  (define (val) (begin0 v (set! v (add1 v))))
  (test always-evt sync (chaperone-evt always-evt
                                       (lambda (e)
                                         (set! did-0 (val))
                                         (values
                                          (chaperone-evt e (lambda (e)
                                                             (set! did-1 (val))
                                                             (values e (lambda (x)
                                                                         (set! did-2 (val))
                                                                         x))))
                                          (lambda (x)
                                            (set! did-3 (val))
                                            x)))))
  (test '(0 1 2 3) list did-0 did-1 did-2 did-3))

(let ()
  (define-struct e (val)
    #:property prop:procedure (lambda (self x)
                                (check self)
                                (+ x x))
    #:property prop:evt (lambda (self) 
                          (check self)
                          always-evt))
  (define (check self) (unless (e? self) (error "bad self!")))
  (define an-e (make-e 0))
  (test always-evt sync (make-e 0))
  (test 14 (make-e 0) 7)
  (test #t evt? an-e)
  (test #t evt? (chaperone-evt an-e void))
  (test #t chaperone-of? (chaperone-evt an-e void) an-e)
  (test 18 (chaperone-evt an-e void) 9))

;; test multiple valued evts
(let ([evt (handle-evt always-evt (lambda (x) (values 0 0)))]
      [redirect-1 (lambda (evt) (values evt (lambda args (apply values args))))]
      [redirect-2 (lambda (evt) (values evt identity))]
      [redirect-3 (lambda (evt) (values evt (lambda args 5)))]
      [redirect-4 (lambda (evt) (values evt (lambda args (values 1 2))))])
  (test-values '(0 0) (lambda () (sync (chaperone-evt evt redirect-1))))
  (err/rt-test (sync (chaperone-evt evt redirect-2)))
  (err/rt-test (sync (chaperone-evt evt redirect-3)))
  (err/rt-test (sync (chaperone-evt evt redirect-4))))

;; check that evt-chaperone handling doesn't intefere with other chaperones:
(let ()
  (struct e (orig)
    #:property prop:input-port 0)
  (define an-e (e (open-input-string "s")))
  (define checked? #f)
  (test #t input-port? an-e)
  (sync (chaperone-struct an-e
                          e-orig
                          (lambda (self v)
                            (set! checked? #t)
                            v)))
  (test #t values checked?))
(let ()
  (struct e (orig)
    #:property prop:evt 0)
  (define an-e (e always-evt))
  (define checked? #f)
  (test #t evt? an-e)
  (sync (chaperone-struct an-e
                          e-orig
                          (lambda (self v)
                            (set! checked? #t)
                            v)))
  (test #t values checked?))

;; ----------------------------------------
;; channel chaperones

(let ([ch (make-channel)])
  (test #t chaperone-of?/impersonator ch ch))
(test #f chaperone-of? (make-channel) (make-channel))
(test #f impersonator-of? (make-channel) (make-channel))
(test #t chaperone?/impersonator (chaperone-channel (make-channel) (lambda (c) (values c values)) (lambda (c v) v)))
(test #t channel? (chaperone-channel (make-channel) (lambda (c) (values c values)) (lambda (c v) v)))
(let ([ch (make-channel)])
  (test #t
        chaperone-of?/impersonator
        (chaperone-channel ch (lambda (c) (values c values)) (lambda (c v) v))
        ch))
(let ([ch (make-channel)])
  (thread (lambda () (channel-put ch 3.14)))
  (test 3.14 channel-get (chaperone-channel ch (lambda (c) (values c values)) (lambda (c v) v))))
(let ([ch (make-channel)])
  (thread (lambda () (channel-put ch 3.14)))
  (test 2.71 channel-get (impersonate-channel ch (lambda (c) (values c (lambda (x) 2.71))) (lambda (c v) v))))
(let ([ch (make-channel)])
  (thread (lambda () (channel-put (impersonate-channel
                                   ch
                                   (lambda (c) (values c values))
                                   (lambda (c v) 2.71))
                                  3.14)))
  (test 2.71 channel-get ch))
(let ([ch (make-channel)])
  (thread (lambda () (sync (channel-put-evt (impersonate-channel
                                             ch
                                             (lambda (c) (values c values))
                                             (lambda (c v) 2.71))
                                            3.14))))
  (test 2.71 channel-get ch))

(err/rt-test (chaperone-channel ch (lambda (c) (values c values)) (lambda (v) v)))
(err/rt-test (chaperone-channel ch (lambda () (values 0 values)) (lambda (c v) v)))
(err/rt-test (chaperone-channel ch (lambda () (values 0 values)) (lambda (c v) v)))
(err/rt-test (chaperone-channel ch (lambda () (values 0 values))))
(err/rt-test (chaperone-channel ch))
(err/rt-test (chaperone-channel 5 (lambda (c) (values c values)) (lambda (c v) v)))
(let ([ch (make-channel)])
  (thread (lambda () (channel-put ch 3.14)))
  (err/rt-test (channel-get (impersonate-channel ch (lambda (c) c) (lambda (c v) v)))))
(let ([ch (make-channel)])
  (thread (lambda () (channel-put ch 3.14)))
  (err/rt-test (channel-get (chaperone-channel ch (lambda (c) (values c (lambda (x) 2.71))) (lambda (c v) v)))))

;; ----------------------------------------

(let ()
  (define-values (prop:blue blue? blue-ref) (make-impersonator-property 'blue))
  (define-values (prop:green green? green-ref) (make-struct-type-property 'green 'can-impersonate))
  (define (a-impersonator-of v) (a-x v))
  (define a-equal+hash (list
                        (lambda (v1 v2 equal?)
                          (equal? (aa-y v1) (aa-y v2)))
                        (lambda (v1 hash)
                          (hash (aa-y v1)))
                        (lambda (v2 hash)
                          (hash (aa-y v2)))))
  (define (aa-y v) (if (a? v) (a-y v) (pre-a-y v)))
  (define-struct pre-a (x y)
    #:property prop:equal+hash a-equal+hash
    #:property prop:green 'color)
  (define-struct a (x y)
    #:property prop:impersonator-of a-impersonator-of
    #:property prop:equal+hash a-equal+hash)
  (define-struct (a-more a) (z))
  (define-struct (a-new-impersonator a) ()
    #:property prop:impersonator-of a-impersonator-of)
  (define-struct (a-new-equal a) ()
    #:property prop:equal+hash a-equal+hash)

  (let ([a1 (make-a #f 2)])
    (test #t equal? (make-pre-a 17 1) (make-pre-a 18 1))
    (test #t chaperone-of? (make-pre-a 17 1) (make-pre-a 18 1))
    (test #t chaperone-of? (chaperone-struct (make-pre-a 17 1) pre-a-y (lambda (a v) v)) (make-pre-a 18 1))
    (test #f chaperone-of? (make-pre-a 18 1) (chaperone-struct (make-pre-a 17 1) pre-a-y (lambda (a v) v)))
    (test #t impersonator-of? (make-pre-a 17 1) (make-pre-a 18 1))
    (test #f chaperone-of? (make-pre-a 17 1) (make-pre-a 17 2))
    (test #t equal? (make-a #f 2) a1)
    (test #t equal? (make-a-more #f 2 7) a1)
    (test #t equal? (make-a-new-impersonator #f 2) a1)
    (test #f equal? (make-a-new-equal #f 2) a1)
    (test #f equal? (make-a #f 3) a1)
    (test #t impersonator-of? (make-a #f 2) a1)
    (test #t chaperone-of? (make-a #f 2) a1)
    (test #t impersonator-of? (make-a a1 3) a1)
    (test #t impersonator-of? (make-a-more a1 3 8) a1)
    (test #f chaperone-of? (make-a a1 3) a1)
    (test #t equal? (make-a a1 3) a1)
    (test #t equal? (make-a-more a1 3 9) a1)
    (err/rt-test (equal? (make-a 0 1) (make-a 0 1)))
    (err/rt-test (impersonator-of? (make-a-new-impersonator a1 1) a1))
    (err/rt-test (impersonator-of? (make-a-new-equal a1 1) a1))
    (err/rt-test (equal? (make-a-new-equal a1 1) a1))

    (define a-pre-a (chaperone-struct (make-pre-a 17 1) pre-a-y (lambda (a v) v)))
    (test 1 pre-a-y a-pre-a)
    (test #t chaperone-of? a-pre-a a-pre-a)
    (test #t chaperone-of? (make-pre-a 17 1) (chaperone-struct (make-pre-a 17 1) pre-a-y #f prop:blue 'color))
    (test #f chaperone-of? (make-pre-a 17 1) (chaperone-struct a-pre-a pre-a-y #f prop:blue 'color))
    (test #t chaperone-of? a-pre-a (chaperone-struct a-pre-a pre-a-y #f prop:blue 'color))
    (test #t chaperone-of? (chaperone-struct a-pre-a pre-a-y #f prop:blue 'color) a-pre-a)
    (test #f chaperone-of? a-pre-a (chaperone-struct a-pre-a pre-a-y (lambda (a v) v) prop:blue 'color))
    (test #f chaperone-of? a-pre-a (chaperone-struct a-pre-a green-ref (lambda (a v) v)))

    (define (exn:second-time? e) (and (exn? e) (regexp-match? #rx"same value as" (exn-message e))))
    (err/rt-test (chaperone-struct (make-pre-a 1 2) pre-a-y #f pre-a-y #f) exn:second-time?)
    (err/rt-test (chaperone-struct (make-pre-a 1 2) pre-a-y (lambda (a v) v) pre-a-y #f) exn:second-time?)
    (err/rt-test (chaperone-struct (make-pre-a 1 2) pre-a-y #f pre-a-y (lambda (a v) v)) exn:second-time?)

    (eq? a-pre-a (chaperone-struct a-pre-a pre-a-y #f))
    (eq? a-pre-a (chaperone-struct a-pre-a green-ref #f))

    (test #t impersonator-of? (make-pre-a 17 1) (chaperone-struct (make-pre-a 17 1) pre-a-y #f prop:blue 'color))
    (test #f impersonator-of? (make-pre-a 17 1) (chaperone-struct a-pre-a pre-a-y #f prop:blue 'color))
    (void)))

;; ----------------------------------------

(let ()
  (define f1 (λ (k) k))
  (define f2 (λ (#:key k) k))
  (define f3 (λ (#:key [k 0]) k))
  (define wrapper
    (make-keyword-procedure
     (λ (kwds kwd-args . args)
        (apply values kwd-args args))
     (λ args (apply values args))))
  (define-values (prop:blue blue? blue-ref) (make-impersonator-property 'blue))
  
  (define g1 (chaperone-procedure f1 wrapper))
  (define g2 (chaperone-procedure f2 wrapper))
  (define g3 (chaperone-procedure f2 wrapper))
  (define h1 (impersonate-procedure f1 wrapper))
  (define h2 (impersonate-procedure f2 wrapper))
  (define h3 (impersonate-procedure f2 wrapper))

  (test #t chaperone-of? g1 f1)
  (test #t chaperone-of? g2 f2)
  (test #t chaperone-of? g3 f2)
  (test #f chaperone-of? g3 g2)

  (test #t chaperone-of? g1 (chaperone-procedure g1 #f prop:blue 'color))
  (test #t chaperone-of? g2 (chaperone-procedure g2 #f prop:blue 'color))
  (test #t chaperone-of? g3 (chaperone-procedure g3 #f prop:blue 'color))
  (test #t chaperone-of? f3 (chaperone-procedure f3 #f prop:blue 'color))
  (test #f chaperone-of? f3 (chaperone-procedure g3 #f prop:blue 'color))

  (test #t eq? f1 (chaperone-procedure f1 #f))
  (test #t eq? f3 (chaperone-procedure f3 #f))
  (test #f eq? f3 (chaperone-procedure f3 #f prop:blue 'color))
  (test #f eq? f1 (chaperone-procedure f1 #f impersonator-prop:application-mark 'x))
  (test #f eq? f1 (chaperone-procedure f1 #f impersonator-prop:application-mark (cons 1 2)))
  (test 8 (chaperone-procedure f2 #f prop:blue 'color) #:key 8)
  (test 88 (chaperone-procedure f3 #f prop:blue 'color) #:key 88)
  (test 'color blue-ref (chaperone-procedure f3 #f prop:blue 'color))

  (test #t equal? g1 f1)
  (test #t equal? g2 f2)
  (test #t equal? g3 f2)
  (test #t equal? g3 g2)

  (test #t impersonator-of? h1 f1)
  (test #t impersonator-of? h2 f2)
  (test #t impersonator-of? h3 f2)
  (test #f impersonator-of? h3 h2)

  (test #t equal? h1 f1)
  (test #t equal? h2 f2)
  (test #t equal? h3 f2)
  (test #t equal? h3 h2)

  (test #t equal? h1 g1)
  (test #t equal? h2 g2)
  (test #t equal? h3 g3)
  (test #t equal? h3 g2)

  (test #f equal? h1 f3)
  (test #f equal? h2 f1)
  (test #f equal? h3 f1))

;; ----------------------------------------

;; A regression test mixing `procedure-rename',
;; chaperones, and impersonator properties:
(let ()
  (define (f #:key k) k)
  (define null-checker
    (make-keyword-procedure
     (λ (kwds kwd-vals . args) (apply values kwd-vals args))
     (λ args (apply values args))))
  (define-values (impersonator-prop:p p? p-ref) (make-impersonator-property 'p))
  (define new-f
    (chaperone-procedure f null-checker impersonator-prop:p #t))
  
  (test #t procedure? (procedure-rename new-f 'g)))

;; ----------------------------------------

(let ()
  (define (f x)
    (call-with-immediate-continuation-mark
     'z
     (lambda (val)
       (list val
             (continuation-mark-set->list (current-continuation-marks) 'z)))))
  (define saved null)
  (define g (chaperone-procedure
             f
             (lambda (a)
               (set! saved (cons (continuation-mark-set-first #f 'z)
                                 saved))
               (values (lambda (r) r)
                       a))
             impersonator-prop:application-mark
             (cons 'z 12)))
  (define h (chaperone-procedure
             g
             (lambda (a)
               (values (lambda (r) r)
                       a))
             impersonator-prop:application-mark
             (cons 'z 9)))
  (define i (chaperone-procedure
             f
             (lambda (a)
               (set! saved (cons (continuation-mark-set-first #f 'z)
                                 saved))
               a)
             impersonator-prop:application-mark
             (cons 'z 11)))
  (define j (chaperone-procedure
             i
             (lambda (a) a)
             impersonator-prop:application-mark
             (cons 'z 12)))
  (test (list 12 '(12)) g 10)
  (test '(#f) values saved)
  (test (list 12 '(12 9)) h 10)
  (test '(9 #f) values saved)
  (test (list 11 '(11)) i 10)
  (test '(#f 9 #f) values saved)
  (test (list 11 '(11)) j 10)
  (test '(12 #f 9 #f) values saved))

;; Make sure that `impersonator-prop:application-mark'
;; is not propagated to further wrapping chaperones:
(let ()
  (define msgs '())
  (define f
    (chaperone-procedure
     (λ (x) 'wrong)
     (λ (x)
        (call-with-immediate-continuation-mark
         'key
         (λ (m)
            (set! msgs (cons m msgs))
            (values x))))
     impersonator-prop:application-mark
     (cons 'key 'skip-this-check)))
  
  (void ((chaperone-procedure f (lambda (x) x)) 42)
        (f 42))
  (test '(#f #f) values msgs))

;; ----------------------------------------

;; Check that supplying a procedure `to make-keyword-procedure' that 
;; (unnecessarily) accepts accepts 0 or 1 arguments doesn't break
;; the chaperone implementation:
(test (void)
      'chaperoned-void
      ((chaperone-procedure
        (make-keyword-procedure void)
        (make-keyword-procedure
         (lambda (kwds kwd-args . args) (apply values kwd-args args))))
       #:a "x"))

(test (box (list (list (list "x"))))
      'impersonated-void
      ((impersonate-procedure
        (make-keyword-procedure (lambda args (cdr args)))
        (make-keyword-procedure
         (lambda (kwds kwd-args . args) 
           (apply values (lambda (v) (box v)) (map list kwd-args) args))))
       #:a "x"))

;; ----------------------------------------
;; Check that importantor transformations are applied for printing:

(let ()
  (define ht 
    (impersonate-hash 
     (let ([h (make-hash)])
       (hash-set! h 'x' y)
       h)
     (lambda (hash key) 
       (values (car key) (lambda (hash key val) (list val))))
     (lambda (hash key val)
       (values (car key) (list val)))
     (lambda (hash key) (car key))
     (lambda (hash key) (list key))))
  (test '(y) hash-ref ht '(x))
  (test "'#hash(((x) . (y)))" 'print (let ([o (open-output-bytes)])
                                       (print ht o)
                                       (get-output-string o))))

;; ----------------------------------------
;; Check that only key-proc is called during hash-keys

(as-chaperone-or-impersonator
 ([chaperone-hash impersonate-hash])
 (let* ([h1 (make-hash (list (cons 1 2) (cons 3 4) (cons 5 6) (cons 7 8)))]
        [res1 (hash-keys h1)]
        [ref-proc #f]
        [set-proc #f]
        [remove-proc #f]
        [key-proc #f]
        [h2 (chaperone-hash h1
                            (λ (h k)
                              (set! ref-proc #t)
                              (values k (λ (h k v) v)))
                            (λ (h k v)
                              (set! set-proc #t)
                              (values k v))
                            (λ (h k)
                              (set! remove-proc #t)
                              k)
                            (λ (h k)
                              (set! key-proc #t)
                              k))]
        [res2 (hash-keys h2)])
   (test #t equal? res1 res2)
   (test #t values key-proc)
   (test #f values ref-proc)
   (test #f values set-proc)
   (test #f values remove-proc)))

;; ----------------------------------------
;; Check interaciton of chaperones and cycle checks

(let ()
  (struct a ([x #:mutable]) #:transparent)

  (define an-a (a #f))
  (set-a-x! an-a an-a)
  (let ([o (open-output-bytes)])
    (print
     (chaperone-struct an-a
                       a-x (lambda (s v) v))
     o)
    (test #"(a #0=(a #0#))" get-output-bytes o)))

;; ----------------------------------------
;; Impersonators and ephemerons:

(let ()
  (define stuff
    (for/list ([n 100])
      (define v (vector n))
      (define c (chaperone-vector v
                                  (lambda (b i v) v)
                                  (lambda (b i v) v)))
      (define e (impersonator-ephemeron c))
      (test c ephemeron-value e)
      ;; hold on to every other vector:
      (cons e (if (even? n) v #f))))
  (collect-garbage)
  (define n (for/fold ([n 0]) ([p stuff])
              (+ n
                 ;; add 1 if should-retained != is-retained
                 (if (ephemeron-value (car p))
                     (if (vector? (cdr p)) 0 1)
                     (if (vector? (cdr p)) 1 0)))))
  (test #t < n 50))

;; ----------------------------------------

(let ()
  (define (go chaperone-procedure impersonate-procedure)
    (define f (lambda (x y #:z [z 1]) y))

    (define same 
      (make-keyword-procedure
       (lambda (kws kw-args . args)
         (if (null? kws)
             (apply values args)
             (apply values kw-args args)))))

    (struct s2 (v) #:property prop:procedure 0)
    (define f2 (s2 f))
    (test #t chaperone-of? (chaperone-procedure f2 same) f2)
    (test #t impersonator-of? (impersonate-procedure f2 same) f2)
    (test 2 (lambda () ((chaperone-procedure f2 same) 1 2 #:z 3)))
    (test 2 (chaperone-procedure f2 same) 1 2)

    (struct s3 () #:property prop:procedure f)
    (define f3 (s3))
    (test #t chaperone-of? (chaperone-procedure f3 same) f3)
    (test #t impersonator-of? (impersonate-procedure f3 same) f3)
    (test 2 (lambda () ((chaperone-procedure f3 same) 2 #:z 3)))
    (test 2 (chaperone-procedure f3 same) 2))
  (define (add-prop mk)
    (lambda (f wrap)
      (define-values (prop: ? -ref) (make-impersonator-property 'x))
      (define v (mk f wrap prop: 'ex))
      (test #t ? v)
      (test 'ex -ref v)
      v))
  (go chaperone-procedure impersonate-procedure)
  (go (add-prop chaperone-procedure)
      (add-prop impersonate-procedure)))

;; ----------------------------------------

(let ()
  (struct s ([a #:mutable]))
  (err/rt-test (impersonate-struct 5 set-s-a! (lambda (a b) b)))
  (err/rt-test (impersonate-struct (s 1) #f (λ (a b) v))
               (λ (x) 
                 (and (exn:application:type? x)
                      (regexp-match #rx"struct-type-property-accessor-procedure[?]" 
                                    (exn-message x))))))

;; ----------------------------------------

(report-errs)
