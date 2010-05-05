

(load-relative "loadtest.rktl")
(Section 'chaperones)

;; ----------------------------------------

(test #t chaperone-of? 10 10)
(test #t chaperone-of? '(10) '(10))
(test #t chaperone-of? '#(1 2 3) '#(1 2 3))
(test #t chaperone-of? '#&(1 2 3) '#&(1 2 3))

(test #f chaperone-of? (make-string 1 #\x) (make-string 1 #\x))
(test #t chaperone-of? 
      (string->immutable-string (make-string 1 #\x))
      (string->immutable-string (make-string 1 #\x)))

(define (either-chaperone-of? a b)
  (or (chaperone-of? a b)
      (chaperone-of? b a)))
(test #f either-chaperone-of? 
      (string->immutable-string "x")
      (make-string 1 #\x))
(test #f either-chaperone-of? 
      '#(1 2 3)
      (vector 1 2 3))
(test #f either-chaperone-of? 
      '#&17
      (box 17))

(let ()
  (define-struct o (a b))
  (define-struct p (x y) #:transparent)
  (define-struct (p2 p) (z) #:transparent)
  (define-struct q (u [w #:mutable]) #:transparent)
  (define-struct (q2 q) (v) #:transparent)
  (test #f chaperone-of? (make-o 1 2) (make-o 1 2))
  (test #t chaperone-of? (make-p 1 2) (make-p 1 2))
  (test #f chaperone-of? (make-p 1 (box 2)) (make-p 1 (box 2)))
  (test #t chaperone-of? (make-p2 1 2 3) (make-p2 1 2 3))
  (test #f chaperone-of? (make-q 1 2) (make-q 1 2))
  (test #f chaperone-of? (make-q2 1 2 3) (make-q2 1 2 3)))

;; ----------------------------------------

(test #t chaperone? (chaperone-box (box 10) (lambda (b v) v) (lambda (b v) v)))
(test #t box? (chaperone-box (box 10) (lambda (b v) v) (lambda (b v) v)))
(test #t (lambda (x) (box? x)) (chaperone-box (box 10) (lambda (b v) v) (lambda (b v) v)))
(test #t chaperone? (chaperone-box (box-immutable 10) (lambda (b v) v) (lambda (b v) v)))

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
  (test (void) set-box! b 'ok)
  (test 'ok unbox b2)
  (test (void) set-box! b2 'fine)
  (test 'fine unbox b))

;; ----------------------------------------

(test #t chaperone? (chaperone-vector (vector 1 2 3) (lambda (b i v) v) (lambda (b i v) v)))
(test #t vector? (chaperone-vector (vector 1 2 3) (lambda (b i v) v) (lambda (b i v) v)))
(test #t (lambda (x) (vector? x)) (chaperone-vector (vector 1 2 3) (lambda (b i v) v) (lambda (b i v) v)))
(test #t chaperone? (chaperone-vector (vector-immutable 1 2 3) (lambda (b i v) v) (lambda (b i v) v)))

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
  (test (void) vector-set! b 1 'ok)
  (test 'ok vector-ref b2 1)
  (test (void) vector-set! b2 1 'fine)
  (test 'fine vector-ref b 1))

;; ----------------------------------------

(test #t chaperone? (chaperone-procedure (lambda (x) x) (lambda (y) y)))
(test #t procedure? (chaperone-procedure (lambda (x) x) (lambda (y) y)))
(test #t (lambda (x) (procedure? x))(chaperone-procedure (lambda (x) x) (lambda (y) y)))
(err/rt-test (chaperone-procedure (lambda (x) x) (lambda (y z) y)))
(err/rt-test (chaperone-procedure (case-lambda [() 0] [(x) x]) (lambda (y) y)))

;; Single argument, no post filter:
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
  (test 111 values in))

;; Multiple arguments, no post filter:
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
  (test (vector 1110 1111) values in))

;; Single argument, post filter on single value:
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
  (test '(11 11) values out))

;; Multiple arguments, post filter on multiple values:
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
  (test (vector 'b '(a c)) values out))

;; Optional keyword arguments:
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
  (test-values '(() (#:a #:b)) (lambda () (procedure-keywords f2))))

;; Optional keyword arguments with result chaperone:
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
  (test-values '(() (#:a #:b)) (lambda () (procedure-keywords f2))))

;; Required keyword arguments:
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
  (test-values '((#:b) (#:a #:b)) (lambda () (procedure-keywords f2))))

;; Required keyword arguments:
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
  (test-values '((#:b) (#:a #:b)) (lambda () (procedure-keywords f2))))

(err/rt-test ((chaperone-procedure (lambda (x) x) (lambda (y) (values y y))) 1))
(err/rt-test ((chaperone-procedure (lambda (x) x) (lambda (y) (values y y y))) 1))
(err/rt-test ((chaperone-procedure (lambda (x) (values x x)) (lambda (y) y))) 1)

;; ----------------------------------------

(let ()
  (define-values (prop:blue blue? blue-ref) (make-chaperone-property 'blue))
  (define-values (prop:green green? green-ref) (make-struct-type-property 'green))
  (define-struct a ([x #:mutable] y))
  (define-struct (b a) ([z #:mutable]))
  (define-struct p (u) #:property prop:green 'green)
  (define-struct (q p) (v w))
  (test #t chaperone? (chaperone-struct (make-a 1 2) a-x (lambda (a v) v)))
  (test #t chaperone? (chaperone-struct (make-b 1 2 3) a-x (lambda (a v) v)))
  (test #t chaperone? (chaperone-struct (make-p 1) green-ref (lambda (a v) v)))
  (test #t chaperone? (chaperone-struct (make-a 1 2) a-x (lambda (a v) v) prop:blue 'blue))
  (test #t chaperone? (chaperone-struct 
                       (chaperone-struct (make-a 1 2) a-x (lambda (a v) v) prop:blue 'blue)
                       a-x (lambda (a v) v)
                       prop:blue 'blue))
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
    (test 8 blue-ref a3))
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
         [a2 (chaperone-struct a1 a-x (lambda (a v) (if (= v 1) 'bad v))
                               set-a-x! (lambda (a v) (if (= v 3) 'bad v)))])
    (test 0 a-x a1)
    (test 0 a-x a2)
    (test (void) set-a-x! a1 1)
    (test 1 a-x a1)
    (err/rt-test (a-x a2))
    (test (void) set-a-x! a1 3)
    (test 3 a-x a1)
    (test 3 a-x a2)
    (test (void) set-a-x! a1 4)
    (err/rt-test (set-a-x! a2 3))
    (test 4 a-x a1)
    (test 4 a-x a2)
    (let* ([a3 (chaperone-struct a2 a-x (lambda (a v) (if (= v 10) 'bad v))
                                 set-a-x! (lambda (a v) (if (= v 30) 'bad v)))])
      (set-a-x! a2 30)
      (err/rt-test (set-a-x! a3 30))
      (err/rt-test (set-a-x! a3 3))
      (set-a-x! a3 1)
      (test 1 a-x a1)
      (err/rt-test (a-x a2))
      (err/rt-test (a-x a3)))))

;; ----------------------------------------

(let ()
  (define (test-sub linear? rev?)
    (define-struct a (x [y #:mutable]) #:property prop:procedure 0)  
    (let* ([a1 (make-a (lambda (x) (list x x)) 10)]
           [get #f]
           [a2 (chaperone-struct a1 a-y (lambda (a v) (set! get v) v))]
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
                   (chaperone-struct a3 a-y (lambda (a v) (set! get v) v))
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
  (test-sub #f #t))

;; ----------------------------------------

(let ()
  (define-values (prop:blue blue? blue-ref) (make-chaperone-property 'blue))
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
     (test #t chaperone? h)
     (test #t hash? h)
     (test #t (lambda (x) (hash? x)) h)))
 (list
  make-hash make-hasheq make-hasheqv
  (lambda () #hash()) (lambda () #hasheq()) (lambda () #hasheqv())
  make-weak-hash make-weak-hasheq make-weak-hasheqv))

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
     (void)))
 (list
  make-hash make-hasheq make-hasheqv
  make-weak-hash make-weak-hasheq make-weak-hasheqv))

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
                  ;; Avoid printign hash-table argument, which implicitly uses `ref':
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
             (void)))))))
 (list #hash() #hasheq() #hasheqv()))

;; ----------------------------------------

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
                            (k (v 10))))))

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
    (check-param p)))

;; ----------------------------------------

(test #t equal?
      (chaperone-procedure add1 void) 
      (chaperone-procedure add1 void))

;; ----------------------------------------

(report-errs)
