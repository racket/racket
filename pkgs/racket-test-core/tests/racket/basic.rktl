
(load-relative "loadtest.rktl")

(Section 'basic)

(require racket/flonum
         racket/fixnum
         racket/function
         racket/list
         racket/symbol
         racket/keyword
         racket/mutability
         (prefix-in k: '#%kernel))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(test '() 'null null)
(test '() 'null '())

(let ([f (lambda () #&7)])
  (test #t eq? (f) (f)))

;; test that all symbol characters are supported.
'(+ - ... !.. $.+ %.- &.! *.: /:. :+. <-. =. >. ?. ~. _. ^.)

(define disjoint-type-functions
  (list boolean? char? null? number? pair? procedure? string? bytes? symbol? keyword? vector? void?))
(define type-examples
  (list
   #t #f #\a '() 9739 '(test) record-error "test" "" #"test" #"" 'test '#:test '#() '#(a b c) (void)))
(define i 1)
(for-each (lambda (x) (display (make-string i #\ ))
		  (set! i (+ 3 i))
		  (write x)
		  (newline))
	  disjoint-type-functions)
(define type-matrix
  (map (lambda (x)
	 (let ((t (map (lambda (f) (f x)) disjoint-type-functions)))
	   (write t)
	   (write x)
	   (newline)
	   t))
       type-examples))

(define count-in-disjoint-types
  (lambda (x) 
    (apply + (map (lambda (f) 
                    (if (f x) 1 0))
                  disjoint-type-functions))))

(for-each (lambda (x)
            (test 1 count-in-disjoint-types x))
          type-examples)

(test #f not #t)
(test #f not 3)
(test #f not (list 3))
(test #t not #f)
(test #f not '())
(test #f not (list))
(test #f not 'nil)
(arity-test not 1 1)

(test #t k:true-object? #t)
(test #f k:true-object? 3)
(test #f k:true-object? (list 3))
(test #f k:true-object? #f)
(test #f k:true-object? '())
(test #f k:true-object? (list))
(test #f k:true-object? 'nil)
(arity-test k:true-object? 1 1)

(test #t boolean? #f)
(test #t boolean? #t)
(test #f boolean? 0)
(test #f boolean? '())
(arity-test boolean? 1 1)

(test #t eqv? 'a 'a)
(test #f eqv? 'a 'b)
(test #t eqv? 2 2)
(test #f eqv? 2 2.0)
(test #t eqv? '() '())
(test #t eqv? '10000 '10000)
(test #t eqv? 10000000000000000000 10000000000000000000)
(test #f eqv? 10000000000000000000 10000000000000000001)
(test #f eqv? 10000000000000000000 20000000000000000000)
(test #f eqv? (cons 1 2) (cons 1 2))
(test #f eqv? (lambda () 1) (lambda () 2))
(test #f eqv? #f 'nil)
(let ((p (lambda (x) x)))
  (test #t eqv? p p))
(define gen-counter
 (lambda ()
   (let ((n 0))
      (lambda () (set! n (+ n 1)) n))))
(let ((g (gen-counter))) (test #t eqv? g g))
(test #f eqv? (gen-counter) (gen-counter))
(letrec ((f (lambda () (if (eqv? f g) 'f 'both)))
	 (g (lambda () (if (eqv? f g) 'g 'both))))
  (test #f eqv? f g))

(test #t eq? 'a 'a)
(test #f eq? (list 'a) (list 'a))
(test #t eq? '() '())
(test #t eq? car car)
(let ((x '(a))) (test #t eq? x x))
(let ((x '#())) (test #t eq? x x))
(let ((x (lambda (x) x))) (test #t eq? x x))

(test #t equal? 'a 'a)
(test #t equal? '("a") '("a"))
(test #t equal? '(a) '(a))
(test #t equal? '(a (b) c) '(a (b) c))
(test #t equal? '("a" ("b") "c") '("a" ("b") "c"))
(test #t equal? "abc" "abc")
(test #t equal? 2 2)
(test #t equal? (make-vector 5 'a) (make-vector 5 'a))
(test #t equal? (box "a") (box "a"))
(test #t equal? (make-flvector 5 0.0) (make-flvector 5 0.0))
(test #t equal? (make-fxvector 5 0) (make-fxvector 5 0))
(test #t equal? (stencil-vector #b10010 'a 'b) (stencil-vector #b10010 'a 'b))
(test #t eq?
      (equal-hash-code (make-flvector 5 0.0))
      (equal-hash-code (make-flvector 5 0.0)))
(test #t eq?
      (equal-hash-code (make-fxvector 5 0))
      (equal-hash-code (make-fxvector 5 0)))
(test #t eq?
      (equal-hash-code (stencil-vector #b10010 'a 'b))
      (equal-hash-code (stencil-vector #b10010 'a 'b)))

(test #f equal? "" (string #\null))

(test #f equal? 'a "a")
(test #f equal? 'a 'b)
(test #f equal? '(a) '(b))
(test #f equal? '(a (b) d) '(a (b) c))
(test #f equal? '(a (b) c) '(d (b) c))
(test #f equal? '(a (b) c) '(a (d) c))
(test #f equal? "abc" "abcd")
(test #f equal? "abcd" "abc")
(test #f equal? 2 3)
(test #f equal? 2.0 2)
(test #f equal? (make-vector 5 'b) (make-vector 5 'a))
(test #f equal? (box "a") (box "b"))

(test #t equal? #\a #\a)
(test #t equal? (integer->char 1024) (integer->char 1024))
(test #f equal? (integer->char 1024) (integer->char 1025))

(test #t equal-always? 'a 'a)
(test #t equal-always? '("a") '("a"))
(test #t equal-always? '(a) '(a))
(test #t equal-always? '(a (b) c) '(a (b) c))
(test #t equal-always? '("a" ("b") "c") '("a" ("b") "c"))
(test #t equal-always? "abc" "abc")
(test #t equal-always? 2 2)
; immutable versions of these are equal-always
(test #t equal-always? (vector-immutable 5 'a) (vector-immutable 5 'a))
(test #t equal-always? (box-immutable "a") (box-immutable "a"))
(test #t equal-always? (hash 'a 1) (hash 'a 1))
(test #t equal-always? (cons 'a '()) (cons 'a '()))
(test #t equal-always? (string->immutable-string (string #\a)) (string->immutable-string (string #\a)))
(test #f equal-always? "" (string #\null))

; but mutable versions are not equal-always
(test #f equal-always? (make-vector 5 'a) (make-vector 5 'a))
(test #f equal-always? (box "a") (box "a"))
(test #f equal-always? (make-hash '((a . 1))) (make-hash '((a . 1))))
(test #f equal-always? (mcons 'a '()) (mcons 'a '()))
(test #f equal-always? (string #\a) (string #\a))
(test #f equal-always? (make-flvector 5 0.0) (make-flvector 5 0.0))
(test #f equal-always? (make-fxvector 5 0) (make-fxvector 5 0))
(test #f equal-always? (stencil-vector #b10010 'a 'b) (stencil-vector #b10010 'a 'b))
(test #f eq?
      (equal-always-hash-code (make-flvector 5 0.0))
      (equal-always-hash-code (make-flvector 5 0.0)))
(test #f eq?
      (equal-always-hash-code (make-fxvector 5 0))
      (equal-always-hash-code (make-fxvector 5 0)))
(test #f eq?
      (equal-always-hash-code (stencil-vector #b10010 'a 'b))
      (equal-always-hash-code (stencil-vector #b10010 'a 'b)))

(let ()
  (struct s (x) #:property prop:procedure 0)
  (test #t equal-always?/recur '(1 . 2) '(1 . 2) (s (lambda (x y) #t)))
  (test #t equal-always?/recur '#(0) '#(0) (s (lambda (x y) #t)))
  (test #t equal-always?/recur '#&0 '#&0 (s (lambda (x y) #t))))

(arity-test eq? 2 2)
(arity-test eqv? 2 2)
(arity-test equal? 2 2)
(arity-test equal-always? 2 2)

(err/rt-test (set-mcdr! (list 1 2) 4))

(test '(a b c d e) 'dot '(a . (b . (c . (d . (e . ()))))))
(define x (mcons 'a (mcons 'b (mcons 'c null))))
(define y x)
(set-mcdr! x 4)
(test (mcons 'a 4) 'set-mcdr! x)
(set-mcar! x 'z)
(test (mcons 'z 4) 'set-mcar! x)
(test #t eqv? x y)
(test '(a b c . d) 'dot '(a . (b . (c . d))))
(test #f list? y)
(test #f list? (cons 'a 4))
(arity-test list? 1 1)

;; Try to check that `list?` is amortized constant-time
(when (run-unreliable-tests? 'timing)
  (define expected #f)
  (define (check-time thunk)
    (collect-garbage)
    (define start (current-process-milliseconds))
    (thunk)
    (define duration (- (current-process-milliseconds) start))
    (cond
      [(not expected) (set! expected duration)]
      [else
       (test #t < duration (+ (* 2 expected) 10))]))
  (define (try range)
    (for ([n range])
      (define l (for/list ([i (in-range n)]) i))
      (define nl (append l 'no))

      (check-time (lambda ()
                    (test #t 'list
                          (for/and ([i 10000]) (list? l)))))
      (check-time (lambda ()
                    (test #f 'non-list
                          (for/or ([i 10000]) (list? nl)))))))
  (try (in-range  1000  1010))
  (try (in-range 10000 10010)))

(test #t pair? '(a . b))
(test #t pair? '(a . 1))
(test #t pair? '(a b c))
(test #f pair? '())
(test #f pair? '#(a b))
(arity-test pair? 1 1)

(test #f k:list-pair? '(a . b))
(test #f k:list-pair? '(a . 1))
(test #t k:list-pair? '(a b c))
(test #f k:list-pair? '())
(test #f k:list-pair? '#(a b))
(arity-test k:list-pair? 1 1)

(test '(a) cons 'a '())
(test '((a) b c d) cons '(a) '(b c d))
(test '("a" b c) cons "a" '(b c))
(test '(a . 3) cons 'a 3)
(test '((a b) . c) cons '(a b) 'c)
(arity-test cons 2 2) 

(test 'a car '(a b c))
(test '(a) car '((a) b c d))
(test 1 car '(1 . 2))
(arity-test car 1 1)
(err/rt-test (car 1))

(test '(b c d) cdr '((a) b c d))
(test 2 cdr '(1 . 2))
(arity-test cdr 1 1)
(err/rt-test (cdr 1))

(test '(a 7 c) list 'a (+ 3 4) 'c)
(test '() list)

(test 3 length '(a b c))
(test 3 length '(a (b) (c d e)))
(test 0 length '())
(arity-test length 1 1)
(err/rt-test (length 1))
(err/rt-test (length '(1 . 2)))
(err/rt-test (length "a"))
; (err/rt-test (length (quote #0=(1 . #0#))))
(err/rt-test (let ([p (cons 1 (make-placeholder #f))]) 
               (placeholder-set! (cdr p) p)
               (length (make-reader-graph p))))
(define x (cons 4 0))
(err/rt-test (length x))

(arity-test set-mcar! 2 2)
(arity-test set-mcdr! 2 2)
(err/rt-test (set-mcar! 4 4))
(err/rt-test (set-mcdr! 4 4))
(err/rt-test (set-mcar! (cons 1 4) 4))
(err/rt-test (set-mcdr! (cons 1 4) 4))

(define (box-tests box unbox box? set-box! set-box!-name unbox-name 2nd-arg?)
  (define b (box 5))
  (test 5 unbox b)
  (when set-box!
    (set-box! b 6)
    (test 6 unbox b))
  (test #t box? b)
  (test #f box? 5)
  (arity-test box 1 1)
  (arity-test unbox 1 (if 2nd-arg? 2 1))
  (arity-test box? 1 1)
  (when set-box!
    (arity-test set-box! 2 2))
  (err/rt-test (unbox 8))
  (when set-box!
    (err/rt-test (set-box! 8 8))))
(box-tests box unbox box? set-box! 'set-box! 'unbox #f)
(box-tests make-weak-box weak-box-value weak-box? #f #f 'weak-box-value #t)

;; test clearing weak boxes
(unless (eq? 'cgc (system-type 'gc))
  (let* ([s (gensym)]
         [b (make-weak-box s)])
    (test s weak-box-value b)
    (test s weak-box-value b 123)
    (set! s 'something-else)
    (collect-garbage)
    (test #f weak-box-value b)
    (test 123 weak-box-value b 123)))

(test '(x y) append '(x) '(y))
(test '(a b c d) append '(a) '(b c d))
(test '(a (b) (c)) append '(a (b)) '((c)))
(test '() append)
(test '(a b c . d) append '(a b) '(c . d))
(test 'a append '() 'a)
(test 1 append 1)
(test '(1 . 2) append '(1) 2)
(err/rt-test (append '(1 2 . 3) 1))
(err/rt-test (append '(1 2 3) 1 '(4 5 6)))

(define l '(1 2))
(define l2 '(3 4 . 7))
(define l3 (append l l2))
(test '(1 2 3 4 . 7) 'append l3)

(test '(c b a) reverse '(a b c))
(test '((e (f)) d (b c) a) reverse '(a (b c) d (e (f))))
(arity-test reverse 1 1)
(err/rt-test (reverse 1))
(err/rt-test (reverse '(1 . 1)))

(test 'c list-ref '(a b c d) 2)
(test 'c list-ref '(a b c . d) 2)
(arity-test list-ref 2 2)
(err/rt-test (list-ref 1 1) exn:application:mismatch? #rx"contract violation.*pair[?]")
(err/rt-test (list-ref '(a b . c) 2) exn:application:mismatch? #rx"index reaches a non-pair")
(err/rt-test (list-ref '(1 2 3) 2.0) exn:fail:contract? #rx"contract violation.*exact-nonnegative-integer[?]")
(err/rt-test (list-ref '(1) '(1)) exn:fail:contract? #rx"contract violation.*exact-nonnegative-integer[?]")
(err/rt-test (list-ref '(1) 1) exn:application:mismatch? #rx"index too large for list")
(err/rt-test (list-ref '() 0) exn:application:mismatch? #rx"contract violation.*pair[?]")
(err/rt-test (list-ref '(1) -1) exn:fail:contract? #rx"contract violation.*exact-nonnegative-integer[?]")
(err/rt-test (list-ref '(1) 2000000000000) exn:application:mismatch?)

(test '(c d) list-tail '(a b c d) 2)
(test '(a b c d) list-tail '(a b c d) 0)
(test '(b c . d) list-tail '(a b c . d) 1)
(test 1 list-tail 1 0)
(arity-test list-tail 2 2)
(err/rt-test (list-tail 1 1) exn:application:mismatch? #rx"index reaches a non-pair")
(err/rt-test (list-tail null 1) exn:application:mismatch? #rx"index too large for list")
(err/rt-test (list-tail '(1 2 3) 2.0) exn:fail:contract? #rx"contract violation.*exact-nonnegative-integer[?]")
(err/rt-test (list-tail '(1) '(1)) exn:fail:contract? #rx"contract violation.*exact-nonnegative-integer[?]")
(err/rt-test (list-tail '(1) -1)exn:fail:contract? #rx"contract violation.*exact-nonnegative-integer[?]")
(err/rt-test (list-tail '(1) 2) exn:application:mismatch? #rx"index too large for list")
(err/rt-test (list-tail '(1 2 . 3) 3) exn:application:mismatch? #rx"index reaches a non-pair")

(err/rt-test (car 0) exn:fail:contract? #rx"car: contract violation.*expected: pair[?].*given: 0")
(err/rt-test (cdr 0) exn:fail:contract? #rx"cdr: contract violation.*expected: pair[?].*given: 0")
(err/rt-test (cadr 0) exn:fail:contract? #rx"cadr: contract violation.*expected: .cons/c any/c pair[?]..*given: 0")
(err/rt-test (cdadr 0) exn:fail:contract? #rx"cdadr: contract violation.*expected: .cons/c any/c .cons/c pair[?] any/c..*given: 0")
(err/rt-test (cdadar 0) exn:fail:contract? #rx"cdadar: contract violation.*expected: .cons/c .cons/c any/c .cons/c pair[?] any/c.. any/c.*given: 0")

(define (test-mem memq memq-name)
  (test '(a b c) memq 'a '(a b c))
  (test '(b c) memq 'b '(a b c))
  (test '(b . c) memq 'b '(a b . c))
  (test '#f memq 'a '(b c d))

  (if (eq? memq-name 'member)
      (arity-test memq 2 3)
      (arity-test memq 2 2))
  (err/rt-test (memq 'a 1) exn:fail:contract? #rx"not a proper list")
  (err/rt-test (memq 'a '(1 . 2)) exn:fail:contract? #rx"not a proper list")
  (err/rt-test (memq 'a (read (open-input-string "#0=(1 . #0#)"))) exn:fail:contract? #rx"not a proper list"))

(test-mem memq 'memq)
(test-mem memv 'memv)
(test-mem memw 'memw)
(test-mem member 'member)

(test '("apple") memq "apple" '("apple")) ; literals are interned
(test '(#"apple") memq #"apple" '(#"apple")) ; literals are interned
(test #f memq (list->string (string->list "apple")) '("apple"))
(test #f memq (list->bytes (bytes->list #"apple")) '(#"apple"))
(test #f memv (list->string (string->list "apple")) '("apple"))
(test #f memw (list->string (string->list "apple")) '("apple"))
(test '("apple") memw (string->immutable-string (list->string (string->list "apple"))) '("apple"))
(test '("apple") member "apple" '("apple"))

; (test #f memq 1/2 '(1/2)) ; rationals are immutable and we may want to optimize
(test '(1/2) memv 1/2 '(1/2))
(test '(1/2) memw 1/2 '(1/2))
(test '(1/2) member 1/2 '(1/2))

(test '((1 2)) memw '(1 2) '(1 2 (1 2)))
(test '((1 2)) member '(1 2) '(1 2 (1 2)))

;; Additional tests for member with equality check argument
(let ([stxs (list #'a #'b #'c)])
  (test stxs member #'a stxs free-identifier=?)
  (test (cdr stxs) member #'b stxs free-identifier=?)
  (test #f member #'z stxs free-identifier=?))
(test '(2 1 2) member 2 '(1 2 1 2) =)
(test #f member 2 '(3 4 5 6) =)
(test '(#"b" #"c") member #"b" '(#"a" #"b" #"c") bytes=?)
(test #f member #"z" '(#"a" #"b" #"c") bytes=?)

(let ([b1 (box 5)]
      [b2 (box 5)])
  (test (list b2 2)
        memw
        b2
        (list 0 b1 1 b2 2))
  (test (list (list b2) 2)
        memw
        (list b2)
        (list 0 (list b1) 1 (list b2) 2)))

(let ([b1 (box 5)]
      [b2 (box 5)])
  (test (list 0 b1 1 2)
        remw
        b2
        (list 0 b1 1 b2 2))
  (test (list 0 (list b1) 1 2)
        remw
        (list b2)
        (list 0 (list b1) 1 (list b2) 2))
  (test (list 0 b1 1 2 3)
        remw*
        (list b2)
        (list 0 b1 1 b2 2 b2 3))
  (test (list 0 (list b1) 1 2 3)
        remw*
        (list (list b2))
        (list 0 (list b1) 1 (list b2) 2 (list b2) 3)))

(define (test-ass assq assq-name)
  (define e '((a 1) (b 2) (c 3)))
  (test '(a 1) assq 'a e)
  (test '(b 2) assq 'b e)
  (test #f assq 'd e)
  (test '(a 1) assq 'a '((x 0) (a 1) b 2))
  (test '(a 1) assq 'a '((x 0) (a 1) . 0))
  (arity-test assq 2 (if (eq? assq-name 'assoc) 3 2))

  (err/rt-test (assq 1 1) exn:application:mismatch?)
  (err/rt-test (assq 1 '(1 2))  exn:application:mismatch?)
  (err/rt-test (assq 1 '((0) . 2)) exn:application:mismatch?))

(test-ass assq 'assq)
(test-ass assv 'assv)
(test-ass assw 'assw)
(test-ass assoc 'assoc)

(test #f assq '(a) '(((a)) ((b)) ((c))))
(test #f assv '(a) '(((a)) ((b)) ((c))))
(test '((b) 1) assw '(b) '(((a)) ((b) 1) ((c))))
(test '((b) 1) assoc '(b) '(((a)) ((b) 1) ((c))))

; (test #f assq '1/2 '(((a)) (1/2) ((c)))) ; rationals are immutable and we may want to optimize
(test '(1/2) assv '1/2 '(((a)) (1/2) ((c))))
(test '(1/2) assw '1/2 '(((a)) (1/2) ((c))))
(test '(1/2) assoc '1/2 '(((a)) (1/2) ((c))))

(test '(3 4) assw 3 '((1 2) (3 4) (5 6)))
(let ([b1 (box 0)]
      [b2 (box 0)])
  (test (cons b2 2)
        assw
        b2
        (list (cons b1 1) (cons b2 2)))
  (test (cons (list b2) 2)
        assw
        (list b2)
        (list (cons (list b1) 1) (cons (list b2) 2))))

(arity-test placeholder-set! 2 2)
(err/rt-test (placeholder-set! #f #f))
(arity-test placeholder-get 1 1)
(err/rt-test (placeholder-get #f))

(test #f immutable? (cons 1 null))
(test #f immutable? (list 1))
(test #f immutable? (list 1 2))
(test #f immutable? (list* 1 null))
(test #f immutable? (list* 1 2 null))
(test #f immutable? 1)
(test #t immutable? #(1 2 3))
(test #f immutable? (vector 1 2 3))
(test #f immutable? (vector))
(test #t immutable? #())
(test #f immutable? (string-copy "hi"))

(test #t immutable? "hi")
(test #t immutable? (string->immutable-string "hi"))
(test #t immutable? (string->immutable-string (string-copy "hi")))

(test #t immutable? (make-immutable-hasheq))
(test #t immutable? (make-immutable-hasheq null))
(test #t immutable? (make-immutable-hasheq '((a . b))))
(test #t immutable? (make-immutable-hash '((a . b))))
(test #t immutable? (make-immutable-hashalw '((a . b))))
(test #f immutable? (make-hasheq))
(test #f immutable? (make-hasheqv))
(test #f immutable? (make-hash))
(test #f immutable? (make-hashalw))
(test #f immutable? (make-weak-hasheq))
(test #f immutable? (make-weak-hash))
(test #f immutable? (make-weak-hashalw))
(test #f immutable? (make-ephemeron-hasheq))
(test #f immutable? (make-ephemeron-hash))
(test #f immutable? (make-ephemeron-hashalw))

(test #t eq? (hash) #hash())
(test #t eq? (hasheq) #hasheq())
(test #t eq? (hasheqv) #hasheqv())
(test #t eq? (hashalw) #hashalw())
(test #t eq? (make-immutable-hash) #hash())
(test #t eq? (make-immutable-hasheq) #hasheq())
(test #t eq? (make-immutable-hasheqv) #hasheqv())
(test #t eq? (make-immutable-hashalw) #hashalw())
(test #t eq? (hash) (hash-remove (hash 3 4) 3))
(test #t eq? (hasheq) (hash-remove (hasheq 3 4) 3))
(test #t eq? (hasheqv) (hash-remove (hasheqv 3 4) 3))
(test #t eq? (hashalw) (hash-remove (hashalw 3 4) 3))
(let ([ht (hash 3 4)])
  (test #t eq? ht (hash-remove ht 5)))
(let ([ht (hasheq 3 4)])
  (test #t eq? ht (hash-remove ht 5)))
(let ([ht (hasheqv 3 4)])
  (test #t eq? ht (hash-remove ht 5)))
(let ([ht (hashalw 3 4)])
  (test #t eq? ht (hash-remove ht 5)))

(err/rt-test (hash 1))
(err/rt-test (hasheqv 1))
(err/rt-test (hasheq 1))
(err/rt-test (hashalw 1))
(err/rt-test (make-hash 1))
(err/rt-test (make-hasheqv 1))
(err/rt-test (make-hasheq 1))
(err/rt-test (make-hashalw 1))
(err/rt-test (make-weak-hash 1))
(err/rt-test (make-weak-hasheqv 1))
(err/rt-test (make-weak-hasheq 1))
(err/rt-test (make-weak-hashalw 1))
(err/rt-test (make-ephemeron-hash 1))
(err/rt-test (make-ephemeron-hasheqv 1))
(err/rt-test (make-ephemeron-hasheq 1))
(err/rt-test (make-ephemeron-hashalw 1))

(test #t symbol? 'foo)
(test #t symbol? (car '(a b)))
(test #f symbol? "bar")
(test #t symbol? 'nil)
(test #f symbol? '())
(test #f symbol? #f)
;;; But first, what case are symbols in?  Determine the standard case:
#ci(parameterize ([read-case-sensitive #f])
     (define char-standard-case char-upcase)
     (if (string=? (symbol->string 'A) "a")
	 (set! char-standard-case char-downcase)
         (void))
     (test #t 'standard-case
	   (string=? (symbol->string 'a) (symbol->string 'A)))
     (test #t 'standard-case
	   (or (string=? (symbol->string 'a) "A")
	       (string=? (symbol->string 'A) "a")))
     (let ()
       (define (str-copy s)
	 (let ((v (make-string (string-length s))))
	   (do ((i (- (string-length v) 1) (- i 1)))
	       ((< i 0) v)
	     (string-set! v i (string-ref s i)))))
       (define (string-standard-case s)
	 (set! s (str-copy s))
	 (do ((i 0 (+ 1 i))
	      (sl (string-length s)))
	     ((>= i sl) s)
	   (string-set! s i (char-standard-case (string-ref s i)))))
       (test (string-standard-case "flying-fish") symbol->string 'flying-fish)
       (test (string-standard-case "martin") symbol->string 'Martin)
       (test "Malvina" symbol->string (string->symbol "Malvina"))
       (test #t 'standard-case (eq? 'a 'A))))

(define x (string #\a #\b))
(define y (string->symbol x))
(string-set! x 0 #\c)
(test "cb" 'string-set! x)
(test "ab" symbol->string y)
(test y string->symbol "ab")
(err/rt-test (string->symbol 10))
(err/rt-test (string->symbol 'oops))

(test #f eq? (symbol->string 'apple) (symbol->string 'apple))
(test "apple" symbol->immutable-string 'apple)
(test #t immutable? (symbol->immutable-string 'apple))
(test #t immutable? (symbol->immutable-string 'box))

#ci(test #t eq? 'mISSISSIppi 'mississippi)
#ci(test #f 'string->symbol (eq? 'bitBlt (string->symbol "bitBlt")))
#cs(test #t 'string->symbol (eq? 'bitBlt (string->symbol "bitBlt")))
(test 'JollyWog string->symbol (symbol->string 'JollyWog))
#ci(test 'JollyWog string->symbol (symbol->string 'JollyWog))

(test #t symbol<? 'a 'b)
(test #t symbol<? 'a 'b 'c)
(test #f symbol<? 'a 'c 'b)
(test #t symbol<? 'a 'aa)
(test #f symbol<? 'aa 'a)

(arity-test symbol? 1 1)

(test #t keyword? '#:a)
(test #f keyword? 'a)
(test '#:apple string->keyword "apple")
(test "apple" keyword->string '#:apple)
(test #t keyword<? '#:a)
(test #t keyword<? '#:a '#:b)
(test #f keyword<? '#:b '#:b)
(test #t keyword<? '#:b '#:bb)
(test #f keyword<? '#:b '#:)
(test #t keyword<? '#:b '#:c '#:d)
(test #f keyword<? '#:b '#:c '#:c)
(test #t keyword<? (string->keyword "a") (string->keyword "\uA0"))
(test #t keyword<? (string->keyword "a") (string->keyword "\uFF"))
(test #f keyword<? (string->keyword "\uA0") (string->keyword "a"))
(test #f keyword<? (string->keyword "\uFF") (string->keyword "a"))
(test #t keyword<? (string->keyword "\uA0") (string->keyword "\uFF"))
(test #f keyword<? (string->keyword "\uFF") (string->keyword "\uA0"))
(test #f keyword<? (string->keyword "\uA0") (string->keyword "\uA0"))

(test #f eq? (keyword->string '#:apple) (keyword->string '#:apple))
(test "apple" keyword->immutable-string '#:apple)
(test #t immutable? (keyword->immutable-string '#:apple))


(arity-test keyword? 1 1)
(arity-test keyword<? 1 -1)

(define (char-tests)
  (test #t eqv? '#\  #\Space)
  (test #t eqv? #\space '#\Space)
  (test #t char? #\a)
  (test #t char? #\()
  (test #t char? #\ )
  (test #t char? '#\newline)
  (test #t char? #\u100)
  (test #f char? 7)
  (test #f char? #t)
  (test #f char? 'x)
  (arity-test char? 1 1)
  (test #t k:interned-char? #\a)
  (test #t k:interned-char? #\()
  (test #t k:interned-char? #\ )
  (test #t k:interned-char? '#\newline)
  (test (eq? 'chez-scheme (system-type 'vm)) k:interned-char? #\u100)
  (test #f k:interned-char? 7)
  (test #f k:interned-char? #t)
  (test #f k:interned-char? #t)
  (test #f k:interned-char? 'x)
  (arity-test k:interned-char? 1 1)

  (test #f char=? #\A #\B)
  (test #f char=? #\A #\A #\B)
  (test #f char=? #\A #\B #\A)
  (test #f char=? #\a #\b)
  (test #f char=? #\9 #\0)
  (test #t char=? #\A #\A)
  (test #t char=? #\A #\A #\A)
  (test #t char=? #\370 #\370)
  (test #f char=? #\371 #\370)
  (test #f char=? #\370 #\371)
  (arity-test char=? 1 -1)
  (err/rt-test (char=? #\a 1)) 
  (err/rt-test (char=? #\a #\b 1)) 
  (err/rt-test (char=? 1 #\a))

  (test #t char<? #\A #\B)
  (test #t char<? #\A #\B #\C)
  (test #f char<? #\A #\B #\A)
  (test #f char<? #\A #\A #\C)
  (test #t char<? #\a #\b)
  (test #f char<? #\9 #\0)
  (test #f char<? #\A #\A)
  (test #f char<? #\370 #\370)
  (test #f char<? #\371 #\370)
  (test #t char<? #\370 #\371)
  (arity-test char<? 1 -1)
  (err/rt-test (char<? #\a 1)) 
  (err/rt-test (char<? #\a #\a 1)) 
  (err/rt-test (char<? 1 #\a))

  (test #f char>? #\A #\B)
  (test #t char>? #\B #\A)
  (test #f char>? #\A #\B #\C)
  (test #f char>? #\B #\A #\C)
  (test #t char>? #\C #\B #\A)
  (test #f char>? #\a #\b)
  (test #t char>? #\9 #\0)
  (test #f char>? #\A #\A)
  (test #f char>? #\370 #\370)
  (test #t char>? #\371 #\370)
  (test #f char>? #\370 #\371)
  (arity-test char>? 1 -1)
  (err/rt-test (char>? #\a 1)) 
  (err/rt-test (char>? #\a #\a 1)) 
  (err/rt-test (char>? 1 #\a))

  (test #t char<=? #\A #\B)
  (test #t char<=? #\A #\B #\C)
  (test #t char<=? #\A #\A #\C)
  (test #f char<=? #\A #\B #\A)
  (test #f char<=? #\B #\A #\C)
  (test #t char<=? #\a #\b)
  (test #f char<=? #\9 #\0)
  (test #t char<=? #\A #\A)
  (test #t char<=? #\370 #\370)
  (test #f char<=? #\371 #\370)
  (test #t char<=? #\370 #\371)
  (arity-test char<=? 1 -1)
  (err/rt-test (char<=? #\a 1)) 
  (err/rt-test (char<=? #\b #\a 1)) 
  (err/rt-test (char<=? 1 #\a))

  (test #f char>=? #\A #\B)
  (test #f char>=? #\a #\b)
  (test #t char>=? #\9 #\0)
  (test #t char>=? #\A #\A)
  (test #t char>=? #\370 #\370)
  (test #t char>=? #\371 #\370)
  (test #f char>=? #\370 #\371)
  (arity-test char>=? 1 -1)
  (err/rt-test (char>=? #\a 1)) 
  (err/rt-test (char>=? #\a #\b 1)) 
  (err/rt-test (char>=? 1 #\a))

  (test #f char-ci=? #\A #\B)
  (test #f char-ci=? #\A #\A #\B)
  (test #f char-ci=? #\a #\B)
  (test #f char-ci=? #\A #\b)
  (test #f char-ci=? #\a #\b)
  (test #f char-ci=? #\9 #\0)
  (test #t char-ci=? #\A #\A)
  (test #t char-ci=? #\A #\a)
  (test #t char-ci=? #\A #\a #\A)
  (test #t char-ci=? #\370 #\370)
  (test #f char-ci=? #\371 #\370)
  (test #f char-ci=? #\370 #\371)
  (arity-test char-ci=? 1 -1)
  (err/rt-test (char-ci=? #\a 1)) 
  (err/rt-test (char-ci=? #\a #\b 1)) 
  (err/rt-test (char-ci=? 1 #\a))

  (test #t char-ci<? #\A #\B)
  (test #t char-ci<? #\A #\B #\C)
  (test #t char-ci<? #\a #\B)
  (test #t char-ci<? #\A #\b)
  (test #t char-ci<? #\A #\b #\C)
  (test #t char-ci<? #\a #\b)
  (test #f char-ci<? #\9 #\0)
  (test #f char-ci<? #\A #\A)
  (test #f char-ci<? #\A #\a)
  (test #f char-ci<? #\A #\b #\B)
  (test #f char-ci<? #\370 #\370)
  (test #f char-ci<? #\371 #\370)
  (test #t char-ci<? #\370 #\371)
  (arity-test char-ci<? 1 -1)
  (err/rt-test (char-ci<? #\a 1)) 
  (err/rt-test (char-ci<? #\b #\a 1)) 
  (err/rt-test (char-ci<? 1 #\a))

  (test #f char-ci>? #\A #\B)
  (test #f char-ci>? #\B #\A #\C)
  (test #t char-ci>? #\C #\B #\A)
  (test #f char-ci>? #\a #\B)
  (test #f char-ci>? #\A #\b)
  (test #f char-ci>? #\a #\b)
  (test #t char-ci>? #\C #\b #\A)
  (test #t char-ci>? #\9 #\0)
  (test #f char-ci>? #\A #\A)
  (test #f char-ci>? #\A #\a)
  (test #f char-ci>? #\370 #\370)
  (test #t char-ci>? #\371 #\370)
  (test #f char-ci>? #\370 #\371)
  (arity-test char-ci>? 1 -1)
  (err/rt-test (char-ci>? #\a 1)) 
  (err/rt-test (char-ci>? #\a #\b 1)) 
  (err/rt-test (char-ci>? 1 #\a))

  (test #t char-ci<=? #\A #\B)
  (test #t char-ci<=? #\a #\B)
  (test #t char-ci<=? #\a #\B #\C)
  (test #f char-ci<=? #\a #\b #\A)
  (test #t char-ci<=? #\A #\b)
  (test #t char-ci<=? #\a #\b)
  (test #f char-ci<=? #\9 #\0)
  (test #t char-ci<=? #\A #\A)
  (test #t char-ci<=? #\A #\a)
  (test #t char-ci<=? #\370 #\370)
  (test #f char-ci<=? #\371 #\370)
  (test #t char-ci<=? #\370 #\371)
  (arity-test char-ci<=? 1 -1)
  (err/rt-test (char-ci<=? #\a 1)) 
  (err/rt-test (char-ci<=? #\b #\a 1)) 
  (err/rt-test (char-ci<=? 1 #\a))

  (test #f char-ci>=? #\A #\B)
  (test #f char-ci>=? #\B #\A #\C)
  (test #t char-ci>=? #\B #\B #\A)
  (test #f char-ci>=? #\a #\B)
  (test #f char-ci>=? #\A #\b)
  (test #f char-ci>=? #\a #\b)
  (test #t char-ci>=? #\9 #\0)
  (test #t char-ci>=? #\A #\A)
  (test #t char-ci>=? #\A #\a)
  (test #t char-ci>=? #\370 #\370)
  (test #t char-ci>=? #\371 #\370)
  (test #f char-ci>=? #\370 #\371)
  (arity-test char-ci>=? 1 -1)
  (err/rt-test (char-ci>=? #\a 1)) 
  (err/rt-test (char-ci>=? #\a #\b 1)) 
  (err/rt-test (char-ci>=? 1 #\a)))

(char-tests)      

(define (ascii-range start end)
  (let ([s (or (and (number? start) start) (char->integer start))]
	[e (or (and (number? end) end) (char->integer end))])
    (let loop ([n e][l (list (integer->char e))])
      (if (= n s)
	  l
	  (let ([n (sub1 n)])
	    (loop n (cons (integer->char n) l)))))))

(define uppers (ascii-range #\A #\Z))
(define lowers (ascii-range #\a #\z))


(define alphas (append uppers lowers))
(define digits (ascii-range #\0 #\9))
(define whites (list #\newline #\return #\space #\page #\tab #\vtab))

(define (test-all is-a? name members)
  (let loop ([n 0])
    (unless (= n 128)
      (let ([c (integer->char n)])
	(test (and (memq c members) #t) `(,is-a? (integer->char ,n)) (is-a? c))
	(loop (add1 n)))))
  (arity-test is-a? 1 1)
  (err/rt-test (is-a? 1)))

(test-all char-alphabetic? 'char-alphabetic? alphas) 
(test-all char-numeric? 'char-numeric? digits) 
(test-all char-whitespace? 'char-whitespace? whites) 
(test-all char-upper-case? 'char-upper-case? uppers) 
(test-all char-lower-case? 'char-lower-case? lowers) 

(let loop ([n 0])
  (unless (= n 512)
     (test n 'integer->char (char->integer (integer->char n)))
     (loop (add1 n))))

(test 0 char->integer #\nul)
(test 10 char->integer #\newline)
(test 13 char->integer #\return)
(test 9 char->integer #\tab)
(test 8 char->integer #\backspace)
(test 12 char->integer #\page)
(test 32 char->integer #\space)
(test 127 char->integer #\rubout)
(test #\null 'null #\nul)
(test #\newline 'linefeed #\linefeed)

(test #\. integer->char (char->integer #\.))
(test #\A integer->char (char->integer #\A))
(test #\a integer->char (char->integer #\a))
(test #\371 integer->char (char->integer #\371))
(test #\U12345 integer->char (char->integer #\U12345))
(test #\0 integer->char (char->integer #\0))
(test #\U10FFFF integer->char (char->integer #\U10FFFF))
(arity-test integer->char 1 1)
(arity-test char->integer 1 1)
(let ([rx #rx"[(]and/c [(]integer-in 0 #x10FFFF[)] [(]not/c [(]integer-in #xD800 #xDFFF[)][)][)]"])
  (err/rt-test (integer->char 5.0) exn:fail:contract? rx)
  (err/rt-test (integer->char 'a) exn:fail:contract? rx)
  (err/rt-test (integer->char -1) exn:fail:contract? rx)
  (err/rt-test (integer->char (expt 2 32) exn:fail:contract? rx))
  (err/rt-test (integer->char 10000000000000000) exn:fail:contract? rx)
  (err/rt-test (integer->char #xD800) exn:fail:contract? rx)
  (err/rt-test (integer->char #xDFFF) exn:fail:contract? rx))
(err/rt-test (char->integer 5) exn:fail:contract? #rx"char[?]")

(define (test-up/down case case-name members memassoc)
  (let loop ([n 0])
    (unless (= n 128)
      (let ([c (integer->char n)])
	(if (memq c members)
	    (test (cdr (assq c memassoc)) case c)
	    (test c case c)))
      (loop (add1 n))))
  (arity-test case 1 1)
  (err/rt-test (case 2)))

(test-up/down char-upcase 'char-upcase lowers (map cons lowers uppers))
(test-up/down char-downcase 'char-downcase uppers (map cons uppers lowers))

(define 64-bit-machine? (eq? (expt 2 40) (eq-hash-code (expt 2 40))))

(test #t string? "The word \"recursion\\\" has many meanings.")
(test #t string? "")
(arity-test string? 1 1)
(test 3 'make-string (string-length (make-string 3)))
(test "" make-string 0)
(arity-test make-string 1 2)
(err/rt-test (make-string "hello") exn:fail:contract? "exact-nonnegative-integer[?]")
(err/rt-test (make-string 5 "hello") exn:fail:contract? "char[?]")
(err/rt-test (make-string 5.0 #\b) exn:fail:contract? "exact-nonnegative-integer[?]")
(err/rt-test (make-string 5.2 #\a) exn:fail:contract? "exact-nonnegative-integer[?]")
(err/rt-test (make-string -5 #\f) exn:fail:contract? "exact-nonnegative-integer[?]")
(unless 64-bit-machine?
  (err/rt-test (make-string 500000000000000 #\f) exn:fail:out-of-memory?)) ;; bignum on 32-bit machines
(err/rt-test (make-string 50000000000000000000 #\f) exn:fail:out-of-memory?)  ;; bignum on 64-bit machines

(test #t vector? (make-vector 0))
(test #(0 0 0 0 0) make-vector 5)
(test #(0 0 0 0 0) make-vector 5 0)
(err/rt-test (make-vector "oops") exn:fail:contract? "exact-nonnegative-integer[?]")
(err/rt-test (make-vector 5.0 0) exn:fail:contract? "exact-nonnegative-integer[?]")
(err/rt-test (make-vector 5.2 0) exn:fail:contract? "exact-nonnegative-integer[?]")
(err/rt-test (make-vector -5 0) exn:fail:contract? "exact-nonnegative-integer[?]")
(unless 64-bit-machine?
  (err/rt-test (make-vector 1234567890 #\f) exn:fail:out-of-memory?)
  (err/rt-test (make-vector 500000000000000 0) exn:fail:out-of-memory?))
(err/rt-test (make-vector 50000000000000000000 0) exn:fail:out-of-memory?)

(unless 64-bit-machine?
  (err/rt-test (read (open-input-string "#1234567890(0)")) exn:fail:out-of-memory?))

(let ([b (vector 1 2 3)])
  (vector-copy! b 0 b 1)
  (test '#(2 3 3) values b))
(let ([b (vector 2 3 4)])
  (vector-copy! b 1 b 0 2)
  (test '#(2 2 3) values b))

(define f (make-string 3 #\*))
(test "?**" 'string-set! (begin (string-set! f 0 #\?) f))
(arity-test string-set! 3 3)
(test #t immutable? "hello")
(err/rt-test (string-set! "hello" 0 #\a)) ; immutable string constant
(define hello-string (string-copy "hello"))
(err/rt-test (string-set! hello-string 'a #\a))
(err/rt-test (string-set! 'hello 4 #\a))
(err/rt-test (string-set! hello-string 4 'a))
(err/rt-test (string-set! hello-string 4.0 'a))
(err/rt-test (string-set! hello-string 5 #\a) exn:application:mismatch?)
(err/rt-test (string-set! hello-string -1 #\a))
(err/rt-test (string-set! hello-string (expt 2 100) #\a) exn:application:mismatch?)
(err/rt-test (string-set! (string #\4 #\5 #\6) 4 #\?) exn:fail:contract? #rx"[[]0, 2[]]")
(test "abc" string #\a #\b #\c)
(test "" string)
(err/rt-test (string #\a 1))
(err/rt-test (string 1 #\a))
(err/rt-test (string 1))
(test 3 string-length "abc")
(test 0 string-length "")
(arity-test string-length 1 1)
(err/rt-test (string-length 'apple))
(test #\a string-ref "abc" 0)
(test #\c string-ref "abc" 2)
(arity-test string-ref 2 2)
(err/rt-test (string-ref 'apple 4))
(err/rt-test (string-ref "apple" 4.0))
(err/rt-test (string-ref "apple" '(4)))
(err/rt-test (string-ref "apple" 5) exn:application:mismatch?)
(err/rt-test (string-ref "" 0) exn:application:mismatch? #rx"empty string")
(err/rt-test (string-ref "" (expt 2 100)) exn:application:mismatch?)
(err/rt-test (string-ref "apple" -1))
(err/rt-test (string-ref "456" 4) exn:fail:contract? #rx"[[]0, 2[]]")
(test "" substring "ab" 0 0)
(test "" substring "ab" 1 1)
(test "" substring "ab" 2 2)
(test "a" substring "ab" 0 1)
(test "b" substring "ab" 1 2)
(test "ab" substring "ab" 0 2)
(test "ab" substring "ab" 0)
(test "b" substring "ab" 1)
(test "" substring "ab" 2)
(test (string #\a #\nul #\b) substring (string #\- #\a #\nul #\b #\*) 1 4)
(arity-test substring 2 3)
(err/rt-test (substring 'hello 2 3))
(err/rt-test (substring "hello" "2" 3))
(err/rt-test (substring "hello" 2.0 3))
(err/rt-test (substring "hello" 2 3.0))
(err/rt-test (substring "hello" 2 "3"))
(err/rt-test (substring "hello" 2 7) exn:application:mismatch?)
(err/rt-test (substring "hello" -2 3))
(err/rt-test (substring "hello" 4 3) exn:application:mismatch?)
(err/rt-test (substring "hello" (expt 2 100) 3) exn:application:mismatch?)
(err/rt-test (substring "hello" (expt 2 100) 5) exn:application:mismatch?)
(err/rt-test (substring "hello" 3 (expt 2 100)) exn:application:mismatch?)
(test "foobar" string-append "foo" "bar")
(test "foo" string-append "foo")
(test "foo" string-append "foo" "")
(test "foogoo" string-append "foo" "" "goo")
(test "foo" string-append "" "foo")
(test "" string-append)
(test (string #\a #\nul #\b #\c #\nul #\d) 
      string-append (string #\a #\nul #\b) (string #\c #\nul #\d))
(err/rt-test (string-append 1))
(err/rt-test (string-append "hello" 1))
(err/rt-test (string-append "hello" 1 "done"))
(test "foobar" string-append-immutable "foo" "bar")
(test "foo" string-append-immutable "foo")
(test "" string-append-immutable)
(test "" string-append-immutable "" "")
(test #t immutable? (string-append-immutable "foo" "bar"))
(test #t immutable? (string-append-immutable "foo"))
(test #t immutable? (string-append-immutable "" ""))
(test #t immutable? (string-append-immutable))
(test #f immutable? (string-append (string->immutable-string "hello")))
(test "" make-string 0)
(define s (string-copy "hello"))
(define s2 (string-copy s))
(test "hello" 'string-copy s2)
(string-set! s 2 #\x)
(test "hello" 'string-copy s2)
(test (string #\a #\nul #\b) string-copy (string #\a #\nul #\b))
(string-fill! s #\x)
(test "xxxxx" 'string-fill! s)
(arity-test string-copy 1 1)
(arity-test string-fill! 2 2)
(err/rt-test (string-copy 'blah))
(err/rt-test (string-fill! 'sym #\1))
(err/rt-test (string-fill! "static" #\1))
(err/rt-test (string-fill! (string-copy "oops") 5))

(let ([f (lambda (l) (apply string-append l))])
  (test "abcdef" f '("a" "bc" "def"))
  (test #f immutable? (f '("a" "bc" "def")))
  (test "" f null)
  (test #f immutable? (f '()))
  (err/rt-test (f 1) exn:fail:contract? #rx"^apply:")
  (err/rt-test (f '(1)) exn:fail:contract? #rx"^string-append:"))

(let ([f (lambda (a b l) (apply string-append a b l))])
  (test "012abcdef" f "0" "12" '("a" "bc" "def"))
  (test #f immutable? (f "0" "12" '("a" "bc" "def")))
  (err/rt-test (f 1 "2" '()) exn:fail:contract? #rx"^string-append:")
  (err/rt-test (f "1" "2" 1) exn:fail:contract? #rx"^apply:"))

(let ([f (lambda (l) (apply string-append-immutable l))])
  (test "abcdef" f '("a" "bc" "def"))
  (test #t immutable? (f '("a" "bc" "def")))
  (test "" f null)
  (test #t immutable? (f '()))
  (err/rt-test (f 1) exn:fail:contract? #rx"^apply:")
  (err/rt-test (f '(1)) exn:fail:contract? #rx"^string-append-immutable:"))

(let ([f (lambda (a b l) (apply string-append-immutable a b l))])
  (test "012abcdef" f "0" "12" '("a" "bc" "def"))
  (test #t immutable? (f "0" "12" '("a" "bc" "def")))
  (err/rt-test (f 1 "2" '()) exn:fail:contract? #rx"^string-append-immutable:")
  (err/rt-test (f "1" "2" 1) exn:fail:contract? #rx"^apply:"))

(let ([s (make-string 10 #\x)])
  (test (void) string-copy! s 0 "hello")
  (test "helloxxxxx" values s)
  (test (void) string-copy! s 3 "hello")
  (test "helhelloxx" values s)
  (err/rt-test (string-copy! s 6 "hello") exn:application:mismatch?)
  (test (void) string-copy! s 5 "hello" 3)
  (test "helhelooxx" values s)
  (test (void) string-copy! s 5 "hello" 3)
  (test "helhelooxx" values s)
  (test (void) string-copy! s 0 "hello" 3 4)
  (test "lelhelooxx" values s)
  (test (void) string-copy! s 1 "hello" 3 5)
  (test "llohelooxx" values s)
  (err/rt-test (string-copy! s 1 "hello" 3 6) exn:application:mismatch?))

(arity-test string-copy! 3 5)
(let ([s (string-copy x)])
  (err/rt-test (string-copy! "x" 0 "x"))
  (err/rt-test (string-copy! s "x" "x"))
  (err/rt-test (string-copy! 0 0 "x"))
  (err/rt-test (string-copy! s 0 "x" -1))
  (err/rt-test (string-copy! s 0 "x" 1 0) exn:application:mismatch?)
  (err/rt-test (string-copy! s 2 "x" 0 1) exn:application:mismatch?))

(test "Hello, and how are you?" string->immutable-string "Hello, and how are you?")
(arity-test string->immutable-string 1 1)
(err/rt-test (string->immutable-string 'hello))

(define ax (string #\a #\nul #\370 #\x))
(define abigx (string #\a #\nul #\370 #\X))
(define ax2 (string #\a #\nul #\370 #\x))
(define ay (string #\a #\nul #\371 #\x))

(define (string-tests)
  (test #t string=? "")
  (test #t string=? "A")

  (test #t string=? "" "")
  (test #f string<? "" "")
  (test #f string>? "" "")
  (test #t string<=? "" "")
  (test #t string>=? "" "")
  (test #t string-ci=? "" "")
  (test #f string-ci<? "" "")
  (test #f string-ci>? "" "")
  (test #t string-ci<=? "" "")
  (test #t string-ci>=? "" "")

  (test #f string=? "A" "B")
  (test #f string=? "a" "b")
  (test #f string=? "9" "0")
  (test #t string=? "A" "A")
  (test #f string=? "A" "AB")
  (test #t string=? ax ax2)
  (test #f string=? ax abigx)
  (test #f string=? ax ay)
  (test #f string=? ay ax)

  (test #t string<? "A")
  (test #t string<? "A" "B")
  (test #t string<? "a" "b")
  (test #f string<? "9" "0")
  (test #f string<? "A" "A")
  (test #t string<? "A" "AB")
  (test #f string<? "AB" "A")
  (test #f string<? ax ax2)
  (test #t string<? ax ay)
  (test #f string<? ay ax)

  (test #t string>? "A")
  (test #f string>? "A" "B")
  (test #f string>? "a" "b")
  (test #t string>? "9" "0")
  (test #f string>? "A" "A")
  (test #f string>? "A" "AB")
  (test #t string>? "AB" "A")
  (test #f string>? ax ax2)
  (test #f string>? ax ay)
  (test #t string>? ay ax)

  (test #t string<=? "A")
  (test #t string<=? "A" "B")
  (test #t string<=? "a" "b")
  (test #f string<=? "9" "0")
  (test #t string<=? "A" "A")
  (test #t string<=? "A" "AB")
  (test #f string<=? "AB" "A")
  (test #t string<=? ax ax2)
  (test #t string<=? ax ay)
  (test #f string<=? ay ax)

  (test #t string>=? "A")
  (test #f string>=? "A" "B")
  (test #f string>=? "a" "b")
  (test #t string>=? "9" "0")
  (test #t string>=? "A" "A")
  (test #f string>=? "A" "AB")
  (test #t string>=? "AB" "A")
  (test #t string>=? ax ax2)
  (test #f string>=? ax ay)
  (test #t string>=? ay ax)

  (test #t string-ci=? "A")
  (test #f string-ci=? "A" "B")
  (test #f string-ci=? "a" "B")
  (test #f string-ci=? "A" "b")
  (test #f string-ci=? "a" "b")
  (test #f string-ci=? "9" "0")
  (test #t string-ci=? "A" "A")
  (test #t string-ci=? "A" "a")
  (test #f string-ci=? "A" "AB")
  (test #t string-ci=? ax ax2)
  (test #t string-ci=? ax abigx)
  (test #f string-ci=? ax ay)
  (test #f string-ci=? ay ax)
  (test #f string-ci=? abigx ay)
  (test #f string-ci=? ay abigx)

  (test #t string-ci<? "A")
  (test #t string-ci<? "A" "B")
  (test #t string-ci<? "a" "B")
  (test #t string-ci<? "A" "b")
  (test #t string-ci<? "a" "b")
  (test #f string-ci<? "9" "0")
  (test #f string-ci<? "A" "A")
  (test #f string-ci<? "A" "a")
  (test #t string-ci<? "A" "AB")
  (test #f string-ci<? "AB" "A")
  (test #f string-ci<? ax ax2)
  (test #f string-ci<? ax abigx)
  (test #t string-ci<? ax ay)
  (test #f string-ci<? ay ax)
  (test #t string-ci<? abigx ay)
  (test #f string-ci<? ay abigx)

  (test #t string-ci>? "A")
  (test #f string-ci>? "A" "B")
  (test #f string-ci>? "a" "B")
  (test #f string-ci>? "A" "b")
  (test #f string-ci>? "a" "b")
  (test #t string-ci>? "9" "0")
  (test #f string-ci>? "A" "A")
  (test #f string-ci>? "A" "a")
  (test #f string-ci>? "A" "AB")
  (test #t string-ci>? "AB" "A")
  (test #f string-ci>? ax ax2)
  (test #f string-ci>? ax abigx)
  (test #f string-ci>? ax ay)
  (test #t string-ci>? ay ax)
  (test #f string-ci>? abigx ay)
  (test #t string-ci>? ay abigx)

  (test #t string-ci<=? "A")
  (test #t string-ci<=? "A" "B")
  (test #t string-ci<=? "a" "B")
  (test #t string-ci<=? "A" "b")
  (test #t string-ci<=? "a" "b")
  (test #f string-ci<=? "9" "0")
  (test #t string-ci<=? "A" "A")
  (test #t string-ci<=? "A" "a")
  (test #t string-ci<=? "A" "AB")
  (test #f string-ci<=? "AB" "A")
  (test #t string-ci<=? ax ax2)
  (test #t string-ci<=? ax abigx)
  (test #t string-ci<=? ax ay)
  (test #f string-ci<=? ay ax)
  (test #t string-ci<=? abigx ay)
  (test #f string-ci<=? ay abigx)

  (test #t string-ci>=? "A")
  (test #f string-ci>=? "A" "B")
  (test #f string-ci>=? "a" "B")
  (test #f string-ci>=? "A" "b")
  (test #f string-ci>=? "a" "b")
  (test #t string-ci>=? "9" "0")
  (test #t string-ci>=? "A" "A")
  (test #t string-ci>=? "A" "a")
  (test #f string-ci>=? "A" "AB")
  (test #t string-ci>=? "AB" "A")
  (test #t string-ci>=? ax ax2)
  (test #t string-ci>=? ax abigx)
  (test #f string-ci>=? ax ay)
  (test #t string-ci>=? ay ax)
  (test #f string-ci>=? abigx ay)
  (test #t string-ci>=? ay abigx))

(string-tests)

(map (lambda (pred)
       (arity-test pred 1 -1)
       (err/rt-test (pred "a" 1))
       (err/rt-test (pred "a" "b" 5))
       (err/rt-test (pred 1 "a")))
     (list string=? 
	   string>? 
	   string<? 
	   string>=? 
	   string<=? 
	   string-ci=? 
	   string-ci>? 
	   string-ci<? 
	   string-ci>=? 
	   string-ci<=?
	   string-locale=? 
	   string-locale>? 
	   string-locale<? 
	   string-locale-ci=? 
	   string-locale-ci>? 
	   string-locale-ci<?))

(test #t byte? 10)
(test #t byte? 0)
(test #t byte? 255)
(test #f byte? 256)
(test #f byte? -1)
(test #f byte? (expt 2 40))
(test #f byte? (expt 2 100))
(test #f byte? #\newline)

(test #t bytes? #"The word \"recursion\\\" has many meanings.")
(test #t bytes? #"")
(arity-test bytes? 1 1)
(test 3 'make-bytes (bytes-length (make-bytes 3)))
(test #"" make-bytes 0)
(arity-test make-bytes 1 2)
(err/rt-test (make-bytes #"hello") exn:fail:contract? "exact-nonnegative-integer[?]")
(err/rt-test (make-bytes 5 #"hello") exn:fail:contract? "byte[?]")
(err/rt-test (make-bytes 5.0 98) exn:fail:contract? "exact-nonnegative-integer[?]")
(err/rt-test (make-bytes 5.2 97) exn:fail:contract? "exact-nonnegative-integer[?]")
(err/rt-test (make-bytes -5 98) exn:fail:contract? "exact-nonnegative-integer[?]")
(err/rt-test (make-bytes 50000000000000000000 #\f) exn:fail:contract? "byte?")
(unless 64-bit-machine?
  (err/rt-test (make-bytes 500000000000000 45) exn:fail:out-of-memory?)) ;; bignum on 32-bit machines
(err/rt-test (make-bytes 50000000000000000000 45) exn:fail:out-of-memory?)  ;; bignum on 64-bit machines


(define f (make-bytes 3 (char->integer #\*)))
(test #"?**" 'bytes-set! (begin (bytes-set! f 0 (char->integer #\?)) f))
(arity-test bytes-set! 3 3)
(err/rt-test (bytes-set! #"hello" 0 #\a) exn:fail:contract? #rx"[(]and/c bytes[?] [(]not/c immutable[?][)][)]") ; immutable bytes constant
(define hello-bytes (bytes-copy #"hello"))
(err/rt-test (bytes-set! hello-bytes 'a 97) exn:fail:contract? #rx"exact-nonnegative-integer[?]")
(err/rt-test (bytes-set! 'hello 4 97) exn:fail:contract? #rx"[(]and/c bytes[?] [(]not/c immutable[?][)][)]")
(err/rt-test (bytes-set! hello-bytes 4 'a) exn:fail:contract? #rx"byte[?]")
(err/rt-test (bytes-set! hello-bytes 4.0 'a) exn:fail:contract? #rx"exact-nonnegative-integer[?]")
(err/rt-test (bytes-set! hello-bytes 5 97) exn:fail:contract? #rx"[[]0, 4[]]")
(err/rt-test (bytes-set! hello-bytes -1 97) exn:fail:contract? #rx"exact-nonnegative-integer[?]")
(err/rt-test (bytes-set! hello-bytes (expt 2 100) 97) exn:fail:contract? #rx"[[]0, 4[]]")
(err/rt-test (bytes-set! (bytes 4 5 6) 4 0) exn:fail:contract? #rx"[[]0, 2[]]")
(test #"abc" bytes 97 98 99)
(test #"" bytes)
(err/rt-test (bytes #\a 1))
(err/rt-test (bytes 1 #\a))
(err/rt-test (bytes #\1))
(test 3 bytes-length #"abc")
(test 0 bytes-length #"")
(arity-test bytes-length 1 1)
(err/rt-test (bytes-length 'apple))
(test 97 bytes-ref #"abc" 0)
(test 99 bytes-ref #"abc" 2)
(arity-test bytes-ref 2 2)
(err/rt-test (bytes-ref 'apple 4))
(err/rt-test (bytes-ref #"apple" 4.0))
(err/rt-test (bytes-ref #"apple" '(4)))
(err/rt-test (bytes-ref #"apple" 5) exn:application:mismatch?)
(err/rt-test (bytes-ref #"" 0) exn:application:mismatch? #rx"empty byte string")
(err/rt-test (bytes-ref #"" (expt 2 100)) exn:application:mismatch?)
(err/rt-test (bytes-ref #"apple" -1))
(err/rt-test (bytes-ref (bytes 4 5 6) 4) exn:fail:contract? #rx"[[]0, 2[]]")
(test #"" subbytes #"ab" 0 0)
(test #"" subbytes #"ab" 1 1)
(test #"" subbytes #"ab" 2 2)
(test #"a" subbytes #"ab" 0 1)
(test #"b" subbytes #"ab" 1 2)
(test #"ab" subbytes #"ab" 0 2)
(test #"ab" subbytes #"ab" 0)
(test #"b" subbytes #"ab" 1)
(test #"" subbytes #"ab" 2)
(test (bytes 97 0 98) subbytes (bytes 32 97 0 98 45) 1 4)
(arity-test subbytes 2 3)
(err/rt-test (subbytes 'hello 2 3))
(err/rt-test (subbytes #"hello" #"2" 3))
(err/rt-test (subbytes #"hello" 2.0 3))
(err/rt-test (subbytes #"hello" 2 3.0))
(err/rt-test (subbytes #"hello" 2 #"3"))
(err/rt-test (subbytes #"hello" 2 7) exn:application:mismatch?)
(err/rt-test (subbytes #"hello" -2 3))
(err/rt-test (subbytes #"hello" 4 3) exn:application:mismatch?)
(err/rt-test (subbytes #"hello" (expt 2 100) 3) exn:application:mismatch?)
(err/rt-test (subbytes #"hello" (expt 2 100) 5) exn:application:mismatch?)
(err/rt-test (subbytes #"hello" 3 (expt 2 100)) exn:application:mismatch?)
(test #"foobar" bytes-append #"foo" #"bar")
(test #"foo" bytes-append #"foo")
(test #"foo" bytes-append #"foo" #"")
(test #"foogoo" bytes-append #"foo" #"" #"goo")
(test #"foo" bytes-append #"" #"foo")
(test #"" bytes-append)
(test (bytes 97 0 98 99 0 100) 
      bytes-append (bytes 97 0 98) (bytes 99 0 100))
(err/rt-test (bytes-append 1))
(err/rt-test (bytes-append #"hello" 1))
(err/rt-test (bytes-append #"hello" 1 #"done"))
(test #"" make-bytes 0)
(define s (bytes-copy #"hello"))
(define s2 (bytes-copy s))
(test #"hello" 'bytes-copy s2)
(bytes-set! s 2 (char->integer #\x))
(test #"hello" 'bytes-copy s2)
(test (bytes 97 0 98) bytes-copy (bytes 97 0 98))
(bytes-fill! s (char->integer #\x))
(test #"xxxxx" 'bytes-fill! s)
(let ([bstr (make-bytes 10)])
  (test (void) bytes-copy! bstr 1 #"testing" 2 6)
  (test #"\0stin\0\0\0\0\0" values bstr)
  (test (void) bytes-copy! bstr 0 #"testing")
  (test #"testing\0\0\0" values bstr))
(arity-test bytes-copy 1 1)
(arity-test bytes-fill! 2 2)
(err/rt-test (bytes-copy 'blah))
(err/rt-test (bytes-fill! 'sym 1))
(err/rt-test (bytes-fill! #"static" 1))
(err/rt-test (bytes-fill! (bytes-copy #"oops") #\5))
(err/rt-test (bytes-copy! (bytes-copy #"oops") #\5))
(err/rt-test (bytes-copy! (bytes-copy #"oops") 0 -1))
(err/rt-test (bytes-copy! (bytes-copy #"oops") 0 #"src" #f))
(err/rt-test (bytes-copy! (bytes-copy #"oops") 0 #"src" -1))
(err/rt-test (bytes-copy! (bytes-copy #"oops") 0 #"src" 1 #f))
(err/rt-test (bytes-copy! (bytes-copy #"oops") 0 #"src" 1 0))
(err/rt-test (bytes-copy! (bytes-copy #"oops") 0 #"src" 1 17))
(err/rt-test (bytes-copy! (bytes-copy #"o") 0 #"src"))
(err/rt-test (bytes-copy! (bytes-copy #"o") 0 #"src" 1))
(test #t bytes=? #"a" #"a" #"a")
(test #t bytes=? #"a" #"a")
(test #t bytes=? #"a")
(test #f bytes=? #"a" #"a" #"c")
(test #f bytes=? #"a" #"b" #"c")
(test #f bytes=? #"a" #"b")
(test #f bytes=? #"c" #"a" #"a")
(test #f bytes=? #"c" #"b" #"a")
(test #f bytes=? #"b" #"a")
(err/rt-test (bytes=? 1))
(err/rt-test (bytes=? #"a" 1))
(err/rt-test (bytes=? #"a" #"a" 1))
(err/rt-test (bytes=? #"a" #"b" 1))

(let ([f (lambda (l) (apply bytes-append l))])
  (test #"abcdef" f '(#"a" #"bc" #"def"))
  (test #f immutable? (f '(#"a" #"bc" #"def")))
  (test #"" f null)
  (test #f immutable? (f '()))
  (err/rt-test (f 1) exn:fail:contract? #rx"^apply:")
  (err/rt-test (f '(1)) exn:fail:contract? #rx"^bytes-append:"))

(let ([f (lambda (a b l) (apply bytes-append a b l))])
  (test #"012abcdef" f #"0" #"12" '(#"a" #"bc" #"def"))
  (test #f immutable? (f #"0" #"12" '(#"a" #"bc" #"def")))
  (err/rt-test (f 1 #"2" '()) exn:fail:contract? #rx"^bytes-append:")
  (err/rt-test (f #"1" #"2" 1) exn:fail:contract? #rx"^apply:"))

(test #f bytes<? #"a" #"a" #"a")
(test #f bytes<? #"a" #"a")
(test #t bytes<? #"a")
(test #f bytes<? #"a" #"a" #"c")
(test #t bytes<? #"a" #"b" #"c")
(test #t bytes<? #"a" #"b")
(test #f bytes<? #"c" #"a" #"a")
(test #f bytes<? #"c" #"b" #"a")
(test #f bytes<? #"b" #"a")
(err/rt-test (bytes<? 1))
(err/rt-test (bytes<? #"a" 1))
(err/rt-test (bytes<? #"a" #"a" 1))
(err/rt-test (bytes<? #"b" #"a" 1))

(test #f bytes>? #"a" #"a" #"a")
(test #f bytes>? #"a" #"a")
(test #t bytes>? #"a")
(test #f bytes>? #"a" #"a" #"c")
(test #f bytes>? #"a" #"b" #"c")
(test #f bytes>? #"a" #"b")
(test #f bytes>? #"c" #"a" #"a")
(test #t bytes>? #"c" #"b" #"a")
(test #t bytes>? #"b" #"a")
(err/rt-test (bytes>? 1))
(err/rt-test (bytes>? #"a" 1))
(err/rt-test (bytes>? #"a" #"a" 1))
(err/rt-test (bytes>? #"a" #"b" 1))


(define r (regexp "(-[0-9]*)+"))
(test '("-12--345" "-345") regexp-match r "a-12--345b")
(test '((1 . 9) (5 . 9)) regexp-match-positions r "a-12--345b")
(test '("--345" "-345") regexp-match r "a-12--345b" 2)
(test '("--34" "-34") regexp-match r "a-12--345b" 2 8)
(test '((4 . 9) (5 . 9)) regexp-match-positions r "a-12--345b" 2)
(test '((4 . 8) (5 . 8)) regexp-match-positions r "a-12--345b" 2 8)
(test '("a-b") regexp-match "a[-c]b" "a-b")
(test '("a-b") regexp-match "a[c-]b" "a-b")
(test #f regexp-match "x+" "12345")
(test "su casa" regexp-replace "mi" "mi casa" "su")
(define r2 (regexp "([Mm])i ([a-zA-Z]*)"))
(define insert "\\1y \\2")
(test "My Casa" regexp-replace r2 "Mi Casa" insert)
(test "my cerveza Mi Mi Mi" regexp-replace r2 "mi cerveza Mi Mi Mi" insert)
(test "my cerveza My Mi Mi" regexp-replace* r2 "mi cerveza Mi Mi Mi" insert)
(test "bbb" regexp-replace* "a" "aaa" "b")
(test '(#"") regexp-match "" (open-input-string "123") 3)
(test '(#"") regexp-match "$" (open-input-string "123") 3)
(test '(#"") regexp-match-peek "" (open-input-string "123") 3)

(test "b1b2b3b" regexp-replace* "" "123" "b")
(test "1b23" regexp-replace* "(?=2)" "123" "b")
(test "xax\u03BBx" regexp-replace* "" "a\u03BB" "x")
(test "xax\u03BBxbx" regexp-replace* "" "a\u03BBb" "x")
(test #"xax\316x\273xbx" regexp-replace* #"" "a\u03BBb" #"x")
(test "==1=2===3==" regexp-replace* "2*" "123" (lambda (s) (string-append "=" s "=")))
(test "==1=2===3==4==" regexp-replace* "2*" "1234" (lambda (s) (string-append "=" s "=")))

(test "x&b\\ab=cy&w\\aw=z" regexp-replace* #rx"a(.)" "xabcyawz" "\\&\\1\\\\&\\99=")
(test "x&cy&z" regexp-replace* #rx"a(.)" "xabcyawz" "\\&")
(test "x\\cy\\z" regexp-replace* #rx"a(.)" "xabcyawz" "\\\\")

(test "ap0p0le" regexp-replace* #rx"p" "apple" "\\0\\$0")

(test "allle" regexp-replace* #rx"p" "apple" "l" 1)
(test "allle" regexp-replace* #rx"p" "apple" "l" 1 3)
(test "allle" regexp-replace* #rx"p" "apple" "l" 1 3 #"a")

(test #"allle" regexp-replace* #rx#"p" "apple" #"l" 1)
(test #"allle" regexp-replace* #rx#"p" "apple" #"l" 1 3)
(test #"allle" regexp-replace* #rx#"p" "apple" #"l" 1 3 #"a")

(test #"allle" regexp-replace* #rx#"p" #"apple" #"l" 1)
(test #"allle" regexp-replace* #rx#"p" #"apple" #"l" 1 3)
(test #"allle" regexp-replace* #rx#"p" #"apple" #"l" 1 3 #"a")

;; Test sub-matches with procedure replace (second example by synx)
(test "myCERVEZA myMI Mi"
      regexp-replace* "([Mm])i ([a-zA-Z]*)" "mi cerveza Mi Mi Mi"
      (lambda (all one two)
        (string-append (string-downcase one) "y"
                       (string-upcase two))))
(test #"fox in socks, blue seal. trout in socks, blue fish!"
      regexp-replace*
      #rx#"([a-z]+) ([a-z]+)"
      #"red fox, blue seal. red trout, blue trout!"
      (lambda (total color what)
        (cond
         ((equal? color #"red") (bytes-append what #" in socks"))
         ((equal? what #"trout") (bytes-append color #" fish"))
         (else (bytes-append color #" " what)))))

(test "foofoo" regexp-replace* #px"(.)?" "a" (lambda args "foo"))

(test "xxxxx world" regexp-replace* #px"\\w" "hello world" "x" 0 5)
(test "HELLO world" regexp-replace* #px"\\w" "hello world" string-upcase 0 5)
(test "hello world" regexp-replace* #px"o" "ohello world" "" 0 3)
(test "hell world" regexp-replace* #px"o" "ohello world" "" 0 6)

(let ([rx #rx"regexp-replace: contract violation.+expected:.+or/c regexp\\? byte-regexp\\? string\\? bytes\\?"])
  (err/rt-test (regexp-replace 'not-regexp "string" "rep") exn:fail:contract? rx)
  (err/rt-test (regexp-replace 'not-regexp "string" "rep" #"ipre") exn:fail:contract? rx))

(let ([rx #rx"regexp-replace\\*: contract violation.+expected:.+or/c regexp\\? byte-regexp\\? string\\? bytes\\?"])
  (err/rt-test (regexp-replace* 'not-regexp "string" "rep") exn:fail:contract? rx)
  (err/rt-test (regexp-replace* 'not-regexp "string" "rep" 1) exn:fail:contract? rx)
  (err/rt-test (regexp-replace* 'not-regexp "string" "rep" 1 2) exn:fail:contract? rx)
  (err/rt-test (regexp-replace* 'not-regexp "string" "rep" 1 2 #"ipre") exn:fail:contract? rx))

;; Test weird port offsets:
(define (test-weird-offset regexp-match regexp-match-positions)
  (test #f regexp-match "e" (open-input-string ""))
  (test #f regexp-match "e" (open-input-string "") (expt 2 100))
  (test #f regexp-match "e" (open-input-string "") (expt 2 100) (expt 2 101))
  (test #f regexp-match "e" (open-input-string "") (expt 2 100) (expt 2 101))
  (test '((3 . 4)) regexp-match-positions "e" (open-input-string "eaae") 2 (expt 2 101))
  (test #f regexp-match "" (open-input-string "123") 4)
  (test #f regexp-match-positions "" (open-input-string "123") 4)
  (test #f regexp-match "" (open-input-string "123") 999)
  (test #f regexp-match-positions "" (open-input-string "123") 999)
  (test #f regexp-match "" (open-input-string "123") (expt 2 101)))
(test-weird-offset regexp-match regexp-match-positions)
(test-weird-offset regexp-match-peek regexp-match-peek-positions)

;; Check greedy and non-greedy operators:
(define (do-the-tests prefix suffix start end)
  (define input (format "~a~a~a" prefix "<tag1 b=5> <tag2 bb=7>" suffix))
  (define (check-greedy-stuff mk-input regexp-match regexp-match-positions)
    (define (testre s-answer p-answer pattern)
      (let ([p-answer (if (and p-answer start)
			  (list (cons (+ start (caar p-answer))
				      (+ start (cdar p-answer))))
			  p-answer)])
	(cond
	 [end
	  (test s-answer regexp-match pattern (mk-input) start (+ end (string-length input)))
	  (test p-answer regexp-match-positions pattern (mk-input) start (+ end (string-length input)))]
	 [start
	  (test s-answer regexp-match pattern (mk-input) start)
	  (test p-answer regexp-match-positions pattern (mk-input) start)]
	 [else
	  (test s-answer regexp-match pattern (mk-input))
	  (test p-answer regexp-match-positions pattern (mk-input))])))
    (define strs
      (if (string? (mk-input))
	  list
	  (lambda l (map string->bytes/utf-8 l))))
    
    (testre (strs "<tag1 b=5> <tag2 bb=7>") '((0 . 22)) "<.*>")
    (testre (strs "<tag1 b=5>") '((0 . 10)) "<.*?>")
    (testre (strs "<tag1 b=5> <tag2 bb=7>") '((0 . 22)) "<.*?>$")
    (testre (strs "") '((0 . 0)) "b*")
    (testre (strs "<tag") '((0 . 4)) "^<[tag]*")
    (testre (strs "<tag") '((0 . 4)) "<[tag]*")
    (testre (strs "<tag1") '((0 . 5)) "<[tag]*1")
    (testre (strs "") '((0 . 0)) "b*?")
    (testre (strs "<") '((0 . 1)) "<[tag]*?")
    (testre (strs "<tag1") '((0 . 5)) "<[tag]*?1")
    (testre (strs "b") '((6 . 7)) "b+?")
    (testre #f #f "^b+?")
    (testre (strs "<t") '((0 . 2)) "<[tag]+?")
    (testre (strs "<tag1") '((0 . 5)) "<[tag]+?1")
    (testre (strs "") '((0 . 0)) "b??")
    (testre (strs "") '((0 . 0)) "[tag]??")
    (testre (strs "g1") '((3 . 5)) "[tag]??1")
    (testre (strs "ag") '((2 . 4)) "[a-m]+"))
  
  (check-greedy-stuff (lambda () input) regexp-match regexp-match-positions)
  (check-greedy-stuff (lambda () (open-input-string input)) regexp-match regexp-match-positions)
  (let ([p (open-input-string input)])
    (check-greedy-stuff (lambda () p) regexp-match-peek regexp-match-peek-positions))
  (let ([mk (lambda ()
	      (let-values ([(r w) (make-pipe)])
		(thread (lambda ()
			  (let loop ([s 0])
			    (let ([e (min (+ s 1)
					  (string-length input))])
			      (display (substring input s e) w)
			      (sleep)
			      (unless (= e s)
				(loop e))))
			  (close-output-port w)))
		r))])
    (check-greedy-stuff mk regexp-match regexp-match-positions)
    (let ([p (mk)])
      (check-greedy-stuff (lambda () p) regexp-match-peek regexp-match-peek-positions))))

(do-the-tests "" "" #f #f)
(do-the-tests "" "" 0 #f)
(do-the-tests "" "" 0 0)
(do-the-tests "ack" "" 3 #f)
(do-the-tests "ack" "" 3 0)
(do-the-tests "ack" "hmm" 3 -3)

(test '((10002 . 10003)) regexp-match-positions "a" (open-input-string (format "~abbac" (make-string 10000 #\x))))

;; Test regexp with null chars:
(let* ([s (string #\a #\b #\nul #\c)]
       [3s (string-append s s s)])
  (test #f regexp-match (string #\nul) "no nulls")
  (test (list s) regexp-match s s)
  (test (list 3s s) regexp-match (format "(~a)*" s) 3s)
  (test (list (string #\b #\nul #\c)) regexp-match (string #\[ #\nul #\b #\] #\* #\c) s)
  (test (list (string #\a #\b #\nul)) regexp-match (string #\a #\[ #\b #\nul #\] #\+) s)
  (test "hihihi" regexp-replace* (string #\nul) (string #\nul #\nul #\nul) "hi"))
(test (string #\- #\nul #\+ #\- #\nul #\+ #\- #\nul #\+)
      regexp-replace* "a" "aaa" (string #\- #\nul #\+))

(test "xpple" regexp-replace #rx"a" "apple" "x")
(test #"xpple" regexp-replace #rx#"a" "apple" "x")
(test #"xpple" regexp-replace #rx"a" #"apple" "x")
(test #"xpple" regexp-replace #rx#"a" #"apple" "x")
(err/rt-test (regexp-replace #rx"a" "apple" #"x"))

(test "pAPple" regexp-replace #rx"a(.)" "apple" (lambda (a b) (string-append b (string-upcase a))))
(test #"p.ap.ple" regexp-replace #rx#"a(.)" "apple" (lambda (a b) (bytes-append b #"." a #".")))
(test #"p.ap.ple" regexp-replace #rx"a(.)" #"apple" (lambda (a b) (bytes-append b #"." a #".")))
(test #"p.ap.ple" regexp-replace #rx#"a(.)" #"apple" (lambda (a b) (bytes-append b #"." a #".")))
(err/rt-test (regexp-replace #rx#"a(.)" #"apple" (lambda (a b) "string")))
(err/rt-test (regexp-replace #rx#"a(.)" "apple" (lambda (a b) "string")))
(err/rt-test (regexp-replace #rx"a(.)" #"apple" (lambda (a b) "string")))
(err/rt-test (regexp-replace #rx"a(.)" "apple" (lambda (a b) #"bytes")))

;; Check extremely many subexpressions:
(for-each
 (lambda (mx)
   (let* ([v (make-vector mx null)]
	  [open (make-vector mx #t)])
     (let loop ([n 0][m 0][s null])
       (cond
	[(and (= n mx) (zero? m))
	 (let* ([s (list->string (reverse s))]
		[plain (regexp-replace* "[()]" s "")])
	   (test (cons plain (map list->string (map reverse (vector->list v)))) regexp-match s plain))]
	[(or (= n mx) (< (random 10) 3))
	 (if (and (positive? m)
		  (< (random 10) 7))
	     (begin
	       (let loop ([p 0][m (sub1 m)])
		 (if (vector-ref open p)
		     (if (zero? m)
			 (vector-set! open p #f)
			 (loop (add1 p) (sub1 m)))
		     (loop (add1 p) m)))
	       (loop n (sub1 m) (cons #\) s)))

	     (let ([c (integer->char (+ (char->integer #\a) (random 26)))])
	       (let loop ([p 0])
		 (unless (= p n)
		   (when (vector-ref open p)
		     (vector-set! v p (cons c (vector-ref v p))))
		   (loop (add1 p))))
	       (loop n m (cons c s))))]
	[else
	 (loop (add1 n) (add1 m) (cons #\( s))]))))
 '(1 10 100 500))

(define (test-bad-re-args who)
  (err/rt-test (who 'e "hello"))
  (err/rt-test (who "e" 'hello))
  (err/rt-test (who "e" "hello" -1 5))
  (err/rt-test (who "e" "hello" (- (expt 2 100)) 5))
  (err/rt-test (who "e" (open-input-string "") (- (expt 2 100)) 5))
  (err/rt-test (who "e" "hello" 1 (- (expt 2 100))))
  (err/rt-test (who "e" (open-input-string "") 1 (- (expt 2 100))))
  (err/rt-test (who "e" "hello" 1 +inf.0))
  (err/rt-test (who "e" "" 0 1) exn:application:mismatch?)
  (err/rt-test (who "e" "hello" 3 2) exn:application:mismatch?)
  (err/rt-test (who "e" "hello" 3 12) exn:application:mismatch?)
  (err/rt-test (who "e" "hello" (expt 2 100) 5)  exn:application:mismatch?)
  (err/rt-test (who "e" (open-input-string "") (expt 2 100) 5)  exn:application:mismatch?)
  (err/rt-test (who "e" (open-input-string "") (expt 2 100) (sub1 (expt 2 100))) exn:application:mismatch?))
(test-bad-re-args regexp-match)
(test-bad-re-args regexp-match-positions)

;; Test non-capturing parens
(test '("1aaa2" "a") regexp-match #rx"1(a)*2" "01aaa23")
(test '("1aaa2") regexp-match #rx"1(?:a)*2" "01aaa23")
(test '("1akakak2" "ak") regexp-match #rx"1(ak)*2" "01akakak23")
(test '("1akakak2") regexp-match #rx"1(?:ak)*2" "01akakak23")
(test '("1akakkakkkk2" "akkkk") regexp-match #rx"1(ak*)*2" "01akakkakkkk23")
(test '("1akakkakkkk2") regexp-match #rx"1(?:ak*)*2" "01akakkakkkk23")
(test '("01akakkakkkk23" "1akakkakkkk2" "1" "a" "k" "2") 
      regexp-match #rx"(?:0)(((?:1))(?:(a)(?:(k))*)*((?:2)))(?:3)" "_01akakkakkkk23_")

(test '((1 . 10) (7 . 9)) regexp-match-positions #rx"1(ak*)*2" "01akakkak23")
(test '((1 . 10)) regexp-match-positions #rx"1(?:ak*)*2" "01akakkak23")

;; Regexps that shouldn't work:
(err/rt-test (regexp "[a--b]") exn:fail?)
(err/rt-test (regexp "[a-b-c]") exn:fail?)
(err/rt-test (regexp "abc)") exn:fail?)
(err/rt-test (pregexp "abc)") exn:fail?)

;; A good test of unicode-friendly ".":
(test '("load-extension: couldn't open \\\" (%s)\"") 
      regexp-match 
      (regexp "^(?:[^\\\"]|\\\\.)*\"") "load-extension: couldn't open \\\" (%s)\"")

;; Test bounded byte consumption on failure:
(let ([is (open-input-string "barfoo")]) 
  (test '(#f #\f) list (regexp-match "^foo" is 0 3) (read-char is)))
(let ([is (open-input-string "barfoo")]) 
  (test '(#f #\f) list (regexp-match "foo" is 0 3) (read-char is)))

;; Check that lazy decoding of strings works right with sending
;; unmatched output to a port:
(for* ([succeed? '(#f #t)]
       [char '(#\x #\u3BB)])
  (for ([N '(1 100 1000 1023 1024 10000)])
    (for ([M (list 0 (quotient N 2))])
      (define o (open-output-bytes))
      (void (regexp-match-positions #rx"y" 
                                    (string-append
                                     (make-string N char)
                                     (if succeed? "y" ""))
                                    M
                                    (+ N (if succeed? 1 0))
                                    o))
    (test (- N M) string-length (get-output-string o)))))

(arity-test regexp 1 2)
(arity-test regexp? 1 1)
(arity-test regexp-match 2 6)
(arity-test regexp-match-positions 2 6)
(arity-test regexp-match-peek 2 6)
(arity-test regexp-match-peek-positions 2 6)
(arity-test regexp-replace 3 4)
(arity-test regexp-replace* 3 6)

(err/rt-test (regexp-match 'oops "input")
             exn:fail:contract?
             #rx"regexp-match: contract violation.*expected:.+or/c.+regexp[?].+byte-regexp[?].+string[?].+bytes[?]")
(err/rt-test (regexp-match #rx"pattern" 'oops)
             exn:fail:contract?
             #rx"regexp-match: contract violation.*expected:.+or/c.+string[?].+bytes[?].+path[?].+input-port[?]")
(err/rt-test (regexp-match #rx"pattern" "input" "oops")
             exn:fail:contract?
             #rx"regexp-match: contract violation.*expected: exact-nonnegative-integer[?]")
(err/rt-test (regexp-match #rx"pattern" "input" 0 "oops")
             exn:fail:contract?
             #rx"regexp-match: contract violation.*expected:.+or/c.+exact-nonnegative-integer[?].+#f")
(err/rt-test (regexp-match #rx"pattern" "input" 0 #f "oops")
             exn:fail:contract?
             #rx"regexp-match: contract violation.*expected:.+or/c.+output-port[?].+#f")
(err/rt-test (regexp-match #rx"pattern" "input" 0 #f #f "oops")
             exn:fail:contract?
             #rx"regexp-match: contract violation.*expected: bytes[?]")

(err/rt-test (regexp-match-peek 'oops (open-input-string "input"))
             exn:fail:contract?
             #rx"regexp-match-peek: contract violation.*expected:.+or/c.+regexp[?].+byte-regexp[?].+string[?].+bytes[?]")
(err/rt-test (regexp-match-peek #rx"pattern" 'oops)
             exn:fail:contract?
             #rx"regexp-match-peek: contract violation.*expected: input-port[?]")
(err/rt-test (regexp-match-peek #rx"pattern" "oops")
             exn:fail:contract?
             #rx"regexp-match-peek: contract violation.*expected: input-port[?]")
(err/rt-test (regexp-match-peek #rx"pattern" (open-input-string "input") "oops")
             exn:fail:contract?
             #rx"regexp-match-peek: contract violation.*expected: exact-nonnegative-integer[?]")
(err/rt-test (regexp-match-peek #rx"pattern" (open-input-string "input") 0 "oops")
             exn:fail:contract?
             #rx"regexp-match-peek: contract violation.*expected:.+or/c.+exact-nonnegative-integer[?].+#f")
(err/rt-test (regexp-match-peek #rx"pattern" (open-input-string "input") 0 #f "oops")
             exn:fail:contract?
             #rx"regexp-match-peek: contract violation.*expected:.+or/c.+progress-evt[?].+#f")
(err/rt-test (regexp-match-peek #rx"pattern" (open-input-string "input") 0 #f #f "oops")
             exn:fail:contract?
             #rx"regexp-match-peek: contract violation.*expected: bytes[?]")

(test 3 regexp-max-lookbehind #rx#"(?<=abc)d")
(test 2 regexp-max-lookbehind #rx#"e(?<=a..)d")
(test 2 regexp-max-lookbehind #rx#"(?:a|ab)(?<!a..)d")
(test 1 regexp-max-lookbehind #rx"^")
(test 1 regexp-max-lookbehind #px"\\b")
(test 0 regexp-max-lookbehind #rx"$")
(test 0 regexp-max-lookbehind #rx".")
(let ([rx #rx"regexp-max-lookbehind: contract violation.*expected:.+or/c.+regexp[?].+byte-regexp[?]"])
  (err/rt-test (regexp-max-lookbehind "string") exn:fail:contract? rx)
  (err/rt-test (regexp-max-lookbehind #"bytes") exn:fail:contract? rx)
  (err/rt-test (regexp-max-lookbehind 'obviously-not-regexp) exn:fail:contract? rx))

(test #t procedure? car)
(test #f procedure? 'car)
(test #t procedure? (lambda (x) (* x x)))
(test #f procedure? '(lambda (x) (* x x)))
(test #t call-with-current-continuation procedure?)
(test #t call-with-escape-continuation procedure?)
(test #t procedure? (case-lambda ((x) x) ((x y) (+ x y))))
(arity-test procedure? 1 1)

(test 7 apply + (list 3 4))
(test 7 apply (lambda (a b) (+ a b)) (list 3 4))
(test 17 apply + 10 (list 3 4))
(test '() apply list '())
(define compose (lambda (f g) (lambda args (f (apply g args)))))
(test 30 (compose sqrt *) 12 75)
(err/rt-test (apply) exn:application:arity?)
(err/rt-test (apply (lambda x x)) exn:application:arity?)
(err/rt-test (apply (lambda x x) 1))
(err/rt-test (apply (lambda x x) 1 2))
(err/rt-test (apply (lambda x x) 1 '(2 . 3)))
(err/rt-test (apply 10 '(2 . 3)))
(err/rt-test (apply 10 0 '(2 . 3)))

(test '(b e h) map cadr '((a b) (d e) (g h)))
(test '(5 7 9) map + '(1 2 3) '(4 5 6))
(test '#(0 1 4 9 16) 'for-each
	(let ((v (make-vector 5)))
		(for-each (lambda (i) (vector-set! v i (* i i)))
			'(0 1 2 3 4))
		v))
(test '(1 2 3) map (lambda (s #:c [c string->number]) (c s)) '("1" "2" "3"))

(define (map-tests map)
  (define ((name-and pred) exn)
    (and (pred exn)
         (regexp-match? (format "^~a:" name) (exn-message exn))))
  (let ([size? (name-and exn:application:mismatch?)]
	[non-list? (name-and type?)]
        [keywords? (lambda (exn)
                     (and (exn:fail:contract? exn)
                          (regexp-match #rx"expects keyword arguments" (exn-message exn))))])
    (err/rt-test (map (lambda (x y) (+ x y)) '(1 2) '1))
    (err/rt-test (map (lambda (x y) (+ x y)) '2 '(1 2)))
    (err/rt-test (map (lambda (x y) (+ x y)) '(1 2) '(1 2 3)) size?)
    (err/rt-test (map (lambda (x y) (+ x y)) '(1 2 3) '(1 2)) size?)
    (err/rt-test (map (lambda (x) (+ x)) '(1 2 . 3)) non-list?)
    (err/rt-test (map (lambda (x y) (+ x y)) '(1 2 . 3) '(1 2)) non-list?)
    (err/rt-test (map (lambda (x y) (+ x y)) '(1 2 . 3) '(1 2 3)) non-list?)
    (err/rt-test (map (lambda (x y) (+ x y)) '(1 2) '(1 2 . 3)) non-list?)
    (err/rt-test (map (lambda (x y) (+ x y)) '(1 2 3) '(1 2 . 3)) non-list?)
    (err/rt-test (map) exn:application:arity?)
    (err/rt-test (map (lambda (x y) (+ x y))) exn:application:arity?)
    (err/rt-test (map (lambda () 10) null) exn:application:mismatch?)
    (err/rt-test (map (case-lambda [() 9] [(x y) 10]) '(1 2 3)) exn:application:mismatch?)
    (err/rt-test (map (lambda (x) 10) '(1 2) '(3 4)) exn:application:mismatch?)
    (err/rt-test (map (lambda (x #:y y) 10) '(1 2)) keywords?)
    (err/rt-test (map (lambda (x #:y y) 10) '()) keywords?)))
(map-tests map)
(map-tests for-each)
(map-tests andmap)
(map-tests ormap)

(test-values (list (void)) (lambda () (for-each (lambda (x) (values 1 2)) '(1 2))))
(err/rt-test (map (lambda (x) (values 1 2)) '(1 2)) arity?)

;; Make sure `values' isn't JIT-inlined in a single-value position:
(err/rt-test ((if (zero? (random 1)) (lambda () (+ (values) 3)) #f)) arity?)
(err/rt-test ((if (zero? (random 1)) (lambda () (+ (values 1 2) 3)) #f)) arity?)
(err/rt-test ((if (zero? (random 1)) (lambda () (+ (values 1 2 4) 3)) #f)) arity?)

(test #t andmap add1 null)
(test #t andmap < null null)
(test #f ormap add1 null)
(test #f ormap < null null)
(test #f andmap positive? '(1 -2 3))
(test #t ormap positive? '(1 -2 3))
(test #f andmap < '(1 -2 3) '(2 2 3))
(test #t ormap < '(1 -2 3) '(0 2 4))
(test #f andmap negative? '(1 -2 3))
(test #t ormap negative? '(1 -2 3))
(test #t andmap < '(1 -2 3) '(2 2 4))
(test #f ormap < '(1 -2 3) '(0 -2 3))
(test 4 andmap add1 '(1 2 3))
(test 2 ormap add1 '(1 2 3))
(test #t andmap < '(1 -2 3) '(2 2 4) '(5 6 7))
(test #t ormap < '(1 -2 3) '(0 -2 4) '(0 0 8))

(err/rt-test (ormap (lambda (x) (values 1 2)) '(1 2)) arity?)
(err/rt-test (andmap (lambda (x) (values 1 2)) '(1 2)) arity?)

(test-values '(1 2) (lambda () (ormap (lambda (x) (values 1 2)) '(1))))
(test-values '(1 2) (lambda () (andmap (lambda (x) (values 1 2)) '(1))))

(test -3 call-with-current-continuation
      (lambda (exit)
        (for-each (lambda (x) (if (negative? x) (exit x) (void)))
                  '(54 0 37 -3 245 19))
        #t))
(define list-length
 (lambda (obj)
  (call-with-current-continuation
   (lambda (return)
    (letrec ((r (lambda (obj) (cond ((null? obj) 0)
                                    ((pair? obj) (+ (r (cdr obj)) 1))
                                    (else (return #f))))))
      (r obj))))))
(test 4 list-length '(1 2 3 4))
(test #f list-length '(a b . c))
(test '() map cadr '())

(arity-test map 2 -1)
(arity-test for-each 2 -1)
(arity-test andmap 2 -1)
(arity-test ormap 2 -1)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; exceptions

(test 10 'exns
      (with-handlers ([integer? (lambda (x) 10)])
        (raise 12)))
(test '(apple) 'exns
      (with-handlers ([void (lambda (x) (list x))])
        (with-handlers ([integer? (lambda (x) 10)])
          (raise 'apple))))
(test '((20)) 'exns
      (with-handlers ([void (lambda (x) (list x))])
        (with-handlers ([integer? (lambda (x) (raise (list x)))])
          (raise 20))))
(test '((30)) 'exns
      (let/ec esc
        (parameterize ([uncaught-exception-handler (lambda (x) (esc (list x)))])
          (with-handlers ([integer? (lambda (x) (raise (list x)))])
            (raise 30)))))
(test '#((40)) 'exns
      (let/ec esc
        (with-handlers ([void (lambda (x) (vector x))])
          (parameterize ([uncaught-exception-handler (lambda (x) (esc (list x)))])
            (with-handlers ([integer? (lambda (x) (raise (list x)))])
              (raise 40))))))

(test '(except) 'escape
      (let/ec k
        (call-with-exception-handler
         (lambda (exn)
           (k (list exn)))
         (lambda () (raise 'except)))))
(test '#&except 'escape
      (let/ec k
        (call-with-exception-handler
         (lambda (exn)
           (k (list exn)))
         (lambda ()
           (call-with-exception-handler
            (lambda (exn)
              (k (box exn)))
            (lambda ()
              (raise 'except)))))))
(test '#(except) 'escape
      (with-handlers ([void (lambda (x) x)])
        (values
         (call-with-exception-handler
          (lambda (exn)
            (vector exn))
          (lambda ()
            (raise 'except))))))
(test '#((except)) 'escape
      (with-handlers ([void (lambda (x) x)])
        (values
         (call-with-exception-handler
          (lambda (exn)
            (vector exn))
          (lambda ()
            ;; (Used to replace enclosing, but not any more)
            (call-with-exception-handler
             (lambda (exn)
               (list exn))
             (lambda ()
               (raise 'except))))))))
(test '#((except)) 'escape
      (with-handlers ([void (lambda (x) x)])
        (values
         (call-with-exception-handler
          (lambda (exn)
            (vector exn))
          (lambda ()
            (values
             (call-with-exception-handler
              (lambda (exn)
                (list exn))
              (lambda ()
                (raise 'except)))))))))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; This tests full conformance of call-with-current-continuation.  It
;;; is a separate test because some schemes do not support call/cc
;;; other than escape procedures.  I am indebted to
;;; raja@copper.ucs.indiana.edu (Raja Sooriamurthi) for fixing this
;;; code.  The function leaf-eq? compares the leaves of 2 arbitrary
;;; trees constructed of conses.  
(define (next-leaf-generator obj eot)
  (letrec ((return #f)
	   (cont (lambda (x)
		   (recurx obj)
		   (set! cont (lambda (x) (return eot)))
		   (cont #f)))
	   (recurx (lambda (obj)
		      (if (pair? obj)
			  (for-each recurx obj)
			  (call-with-current-continuation
			   (lambda (c)
			     (set! cont c)
			     (return obj)))))))
    (lambda () (call-with-current-continuation
		(lambda (ret) (set! return ret) (cont #f))))))
(define (leaf-eq? x y)
  (let* ((eot (list 'eot))
	 (xf (next-leaf-generator x eot))
	 (yf (next-leaf-generator y eot)))
    (letrec ((loop (lambda (x y)
		     (cond ((not (eq? x y)) #f)
			   ((eq? eot x) #t)
			   (else (loop (xf) (yf)))))))
      (loop (xf) (yf)))))
(define (test-cont)
  (newline)
  (display ";testing continuations; ")
  (test #t leaf-eq? '(a (b (c))) '((a) b c))
  (test #f leaf-eq? '(a (b (c))) '((a) b c d))
  '(report-errs))

(define (test-cc-values test-call/cc)
  (test '(a b c)
	call-with-values
	(lambda ()
	  (test-call/cc
	   (lambda (k)
	     (dynamic-wind
	      void
	      (lambda ()
		(k 'a 'b 'c))
	      (lambda ()
		(values 1 2))))))
	list)

  (test 1 dynamic-wind
	(lambda () (test-call/cc void))
	(lambda () 1)
	(lambda () (test-call/cc void)))

  ; Try devious jumping with pre- and post-thunks:
  (test 2 test-call/cc
	(lambda (exit)
	  (dynamic-wind
	   (lambda () (exit 2))
	   void
	   void)))
  (test 3 test-call/cc
	(lambda (exit)
	  (dynamic-wind
	   void
	   void
	   (lambda () (exit 3)))))

  (let ([rv
	 (lambda (get-v)
	   (let ([x 0])
	     (test-call/cc
	      (lambda (exit)
		(dynamic-wind
		 void
		 (lambda () (exit))
		 (lambda () (set! x (get-v))))))
	     x))]
	[r56
	 (lambda ()
	   (let ([x 0]
		 [y 1]
		 [c1 #f])
	     (dynamic-wind
	      (lambda () (set! x (add1 x)))
	      (lambda () 
		(let/cc k (set! c1 k))
		(if (>= x 5)
		    (set! c1 #f)
                    (void)))
	      (lambda () (set! y (add1 y))))
	     (when c1 (c1))
	     (list x y)))]
	[rx.y
	 (lambda (get-x get-y)
	   (let ([c1 #f]
		 [x 0]
		 [y 0])
	     (let ([v
		    (dynamic-wind
		     (lambda () (set! y x))
		     (lambda () (let/cc k (set! c1 k)))
		     (lambda () 
		       (set! x (get-x))
		       (when c1
			 ((begin0
			   c1
			   (set! c1 #f))
			  (get-y)))))])
	       (cons y v))))]
	[rv2
	 (lambda (get-v)
	   (let ([c1 #f]
		 [give-up #f])
	     (test-call/cc
	      (lambda (exit)
		(dynamic-wind
		 (lambda () (when give-up (give-up (get-v))))
		 (lambda () (let/cc k (set! c1 k)))
		 (lambda () (set! give-up exit) (c1)))))))]
	[r10-11-12
	 (lambda ()
	   (let ([c2 #f]
		 [x 10]
		 [y 11])
	     (let ([v (dynamic-wind
		       (lambda () (set! y (add1 y)))
		       (lambda () (begin0 x (set! x (add1 x))))
		       (lambda () (let/cc k (set! c2 k))))])
	       (when c2 ((begin0
			  c2
			  (set! c2 #f))))
	       (list v x y))))]
	[r13.14
	 (lambda ()
	   (let ([c0 #f]
		 [x 11]
		 [y 12])
	     (dynamic-wind
	      (lambda () (let/cc k (set! c0 k)))
	      (lambda () (set! x (add1 x)))
	      (lambda () 
		(set! y (add1 y))
		(when c0 ((begin0
			   c0
			   (set! c0 #f))))))
	     (cons x y)))]
	[ra-b-a-b
	 (lambda (get-a get-b)
	   (let ([l null])
	     (let ((k-in (test-call/cc (lambda (k1)
					 (dynamic-wind
					  (lambda () (set! l (append l (list (get-a)))))
					  (lambda ()
					    (call/cc (lambda (k2) (k1 k2))))
					  (lambda ()  (set! l (append l (list (get-b))))))))))
	       (k-in (lambda (v) l)))))])

  (test 4 rv (lambda () 4))
  (test '(5 6) r56)

  (test '(7 . 8) rx.y (lambda () 7) (lambda () 8))

  (test 9 rv2 (lambda () 9))

  (test '(10 11 12) r10-11-12)

  (test '(13 . 14) r13.14)

  ; !!! fixed in 50:
  (test '(enter exit enter exit)
	ra-b-a-b (lambda () 'enter) (lambda () 'exit))

  (test '((13 . 14) (10 11 12) (13 . 14) (10 11 12))
	ra-b-a-b r13.14 r10-11-12)
  (test '((10 11 12) (13 . 14) (10 11 12) (13 . 14))
	ra-b-a-b r10-11-12 r13.14)

  (test 10 call/cc (lambda (k) (k 10)))
  
  (test '((enter exit enter exit)
	  (exit enter exit enter)
	  (enter exit enter exit)
	  (exit enter exit enter))
	ra-b-a-b 
        (lambda () (ra-b-a-b (lambda () 'enter) (lambda () 'exit)))
	(lambda () (ra-b-a-b (lambda () 'exit) (lambda () 'enter))))

  (test '(enter exit enter exit)
	rv (lambda () (ra-b-a-b (lambda () 'enter) (lambda () 'exit))))
  (test '(enter exit enter exit)
	rv2 (lambda () (ra-b-a-b (lambda () 'enter) (lambda () 'exit))))

  (test '(10 11 12) rv r10-11-12)
  (test '(10 11 12) rv2 r10-11-12)

  (test '(13 . 14) rv r13.14)
  (test '(13 . 14) rv2 r13.14)

  (test 12 'dw/ec (test-call/cc
		   (lambda (k0)
		     (test-call/cc
		      (lambda (k1)
			(test-call/cc
			 (lambda (k2)
			   (dynamic-wind
			    void
			    (lambda () (k1 6))
			    (lambda () (k2 12))))))))))

  ;; !!! fixed in 53 (for call/ec)
  (test 13 'dw/ec (test-call/cc
		   (lambda (k0)
		     (test-call/cc
		      (lambda (k1)
			(test-call/cc
			 (lambda (k2)
			   (dynamic-wind
			    void
			    (lambda () (k1 6))
			    (lambda () (k2 12)))))
			(k0 13))))))


  ;; Interaction with exceptions:
  (test 42 test-call/cc (lambda (k)
                          (call-with-exception-handler k (lambda () (add1 (raise 42))))))

  (let ([x 0])
    ;; Make sure inner `k2' doesn't escape using outer `k':
    (let/cc k (+ 1 (dynamic-wind 
                       (lambda () (set! x (add1 x)))
                       (lambda () (let/cc k (k 2))) 
                       void)))
    (test 1 values x))

  ))
	      

(test-cc-values call/cc)
(test-cc-values call/ec)



(test 'ok
      'ec-cc-exn-combo
      (with-handlers ([void (lambda (x) 'ok)])
	(define f
	  (let ([k #f])
	    (lambda (n)
	      (case n
		[(0) (let/ec r (r (set! k (let/cc k k))))]
		[(1) (k)]))))
	(f 0)
	(f 1)))

(test '(1 2 3 4 1 2 3 4) 'dyn-wind-pre/post-order
      (let ([x null]
	    [go-back #f])
	(dynamic-wind
	 (lambda () (set! x (cons 4 x)))
	 (lambda () (dynamic-wind
		     (lambda () (set! x (cons 3 x)))
		     (lambda () (set! go-back (let/cc k k)))
		     (lambda () (set! x (cons 2 x)))))
	 (lambda () (set! x (cons 1 x))))
	(if (procedure? go-back)
	    (go-back 1)
	    x)))

(test '(5 . 5) 'suspended-cont-escape
      (let ([retry #f])
	(let ([v (let/ec exit
		   (dynamic-wind
		    void
		    (lambda () (exit 5))
		    (lambda ()
		      (let/ec inner-escape
			(set! retry (let/cc k k))
			(inner-escape 12)
			10))))])
	  (if (procedure? retry)
	      (retry 10)
	      (cons v v)))))

(test '(here) 'escape-interrupt-full-jump-up
      (let ([b #f]
	    [v null])
	(define (f g)
	  (dynamic-wind
	   void
	   g
	   (lambda () 
	     (set! v (cons 'here v))
	     (b 10))))
	
	(let/ec big
	  (set! b big)
	  (let/cc ok
	    (f (lambda ()
		 (ok #f)))))
	
	v))

;; Check interaction of map and call/cc
(let ()
  (define (try n m)
    (let ([retries (make-vector n)]
	  [copy #f]
	  [special -1]
	  [in (let loop ([i n])
		(if (= i 0)
		    null
		    (cons (- n i) (loop (sub1 i)))))])
      (let ([v (apply
		map 
		(lambda (a . rest)
		  (+ (let/cc k (vector-set! retries a k) 1)
		     a))
		(let loop ([m m])
		  (if (zero? m)
		      null
		      (cons in (loop (sub1 m))))))])
	(test (map (lambda (i)
		     (if (= i special)
			 (+ i 2)
			 (add1 i)))
		   in)
	      `(map/cc ,n ,m)
	      v))
      (if copy
	  (when (pair? copy)
	    (set! special (add1 special))
	    ((begin0 (car copy) (set! copy (cdr copy)))
	     2))
	  (begin
	    (set! copy (vector->list retries))
	    ((vector-ref retries (random n)) 1)))))
  (try 3 1)
  (try 10 1)
  (try 3 2)
  (try 10 2)
  (try 5 3)
  (try 3 5)
  (try 10 5))

;; Make sure let doesn't allocate a mutatble cell too early:
(test 2 'let+call/cc
      (let ([count 0])
        (let ([first-time? #t]
              [k (call/cc values)])
          (if first-time?
              (begin
                (set! first-time? #f)
                (set! count (+ count 1))
                (k values))
              (void)))
        count))

;; Letrec must allocate early, though:
(test #f 'letrec+call/cc
      (letrec ((x (call-with-current-continuation list)))
        (if (pair? x)
            ((car x) (lambda () x))
            (pair? (x)))))

;; Check shared escape continuation of nested call/cc:
(let ([ch (make-channel)])
 (thread
  (lambda ()
    (channel-put 
     ch
     (call/cc
      (lambda (escape)
        (call/cc
         (lambda (escape1)
           (escape1 3))))))))
 (sync ch))

;; Make sure allocation and continuation capture are left-to-right in
;; a function call:
(let ([join (if (zero? (random 1)) list 'oops)])
  (let ([k0 (cadr
             (call-with-continuation-prompt
              (lambda ()
                (join (cons 1 2)
                      (call/cc (lambda (k) k))))))]
        [k1 (car
             (call-with-continuation-prompt
              (lambda ()
                (join (call/cc (lambda (k) k))
                      (cons 1 2)))))])
    (define (do-k k) (call-with-continuation-prompt
                      (lambda ()
                        (k k))))
    (test #t eq? (car (do-k k0)) (car (do-k k0)))
    (test #f eq? (cadr (do-k k1)) (cadr (do-k k1)))))

(arity-test call/cc 1 2)
(arity-test call/ec 1 1)
(err/rt-test (call/cc 4))
(err/rt-test (call/cc (lambda () 0)))
(err/rt-test (call/ec 4))
(err/rt-test (call/ec (lambda () 0)))

(test #t primitive? car)
(test #f primitive? leaf-eq?)
(arity-test primitive? 1 1)

(test 1 procedure-arity procedure-arity)
(test 2 procedure-arity cons)
(test (make-arity-at-least 1) procedure-arity >)
(test (list 0 1) procedure-arity current-output-port)
(test (list 1 3 (make-arity-at-least 5))
      procedure-arity (case-lambda [(x) 0] [(x y z) 1] [(x y z w u . rest) 2]))
(test (make-arity-at-least 0) procedure-arity (lambda x 1))
(test (make-arity-at-least 0) procedure-arity (case-lambda [() 10] [x 1]))
(test (make-arity-at-least 0) procedure-arity (lambda x x))
(arity-test procedure-arity 1 1)

;; Tests for normalize-arity without arity-at-least
(test '() normalize-arity '())
(test 1 normalize-arity 1)
(test 1 normalize-arity '(1))
(test '(1 2) normalize-arity '(1 2))
(test '(1 2) normalize-arity '(2 1))
(test (list 1 2) normalize-arity (list 1 1 2 2))
(test 1 normalize-arity (list 1 1 1))

;; Tests for normalize-arity where everything collapses into arity-at-least
(test (make-arity-at-least 2) normalize-arity (list (make-arity-at-least 2) 3))
(test (make-arity-at-least 1)
      normalize-arity (list (make-arity-at-least 2) 1))
(test (make-arity-at-least 1)
      normalize-arity (list (make-arity-at-least 2) 1 3))
(test (make-arity-at-least 0)
      normalize-arity (list (make-arity-at-least 2) 1 0 3))
(test (make-arity-at-least 0)
      normalize-arity (list (make-arity-at-least 2)
                            (make-arity-at-least 4) 1 0 3))
(test (make-arity-at-least 0)
      normalize-arity (list (make-arity-at-least 4)
                            (make-arity-at-least 2) 1 0 3))
(test (make-arity-at-least 1)
      normalize-arity (list (make-arity-at-least 2) 1 1))
(test (make-arity-at-least 1)
      normalize-arity 
      (list (make-arity-at-least 2)
            (make-arity-at-least 2) 1 1))

;; Tests for normalize-arity that result in a list with arity-at-least.
(test (list 1 (make-arity-at-least 3))
      normalize-arity (list (make-arity-at-least 3) 1))
(test (list 1 (make-arity-at-least 3))
      normalize-arity (list (make-arity-at-least 3) 1 4))
(test (list 0 1 (make-arity-at-least 3))
      normalize-arity (list (make-arity-at-least 3) 1 0 4))
(test (list 0 1 (make-arity-at-least 3))
      normalize-arity (list (make-arity-at-least 3)
                            (make-arity-at-least 5) 1 0 4))
(test (list 0 1 (make-arity-at-least 3))
      normalize-arity (list (make-arity-at-least 5)
                            (make-arity-at-least 3) 1 0 4))
(test (list 1 (make-arity-at-least 3))
      normalize-arity (list (make-arity-at-least 3) 1 1))
(test (list 1 (make-arity-at-least 3))
      normalize-arity 
      (list (make-arity-at-least 3)
            (make-arity-at-least 3) 1 1))
(test (list 0 1 (make-arity-at-least 3))
      normalize-arity (list 0 1 3 (make-arity-at-least 4)))
(test (list 0 1 (make-arity-at-least 3))
      normalize-arity (list (make-arity-at-least 4) 3 1 0))
(test (list 0 1 (make-arity-at-least 3))
      normalize-arity (list 0 1 3 (make-arity-at-least 4)
                                5 (make-arity-at-least 6)))

(let ()
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;
  ;; randomized testing
  ;; predicate: normalize-arity produces a normalized arity
  ;;

  (define (normalized-arity=? original normalized)
    (and
      (normalized-arity? normalized)
      (arity=? original normalized)))
  
  (for ((i (in-range 1 2000)))
    (define rand-bound (ceiling (/ i 10)))
    (define l
      (build-list (random rand-bound)
        (λ (i) (if (zero? (random 5))
                   (make-arity-at-least (random rand-bound))
                   (random rand-bound)))))
    (define res (normalize-arity l))
    #:final (not (normalized-arity=? l res))
    (test #t normalized-arity=? l res)))

(test #t procedure-arity-includes? cons 2)
(test #f procedure-arity-includes? cons 0)
(test #f procedure-arity-includes? cons 3)
(test #t procedure-arity-includes? list 3)
(test #t procedure-arity-includes? list 3000)
(test #t procedure-arity-includes? (lambda () 0) 0)
(test #f procedure-arity-includes? (lambda () 0) 1)
(test #f procedure-arity-includes? cons 10000000000000000000000000000)
(test #t procedure-arity-includes? list 10000000000000000000000000000)
(test #t procedure-arity-includes? (lambda x x) 10000000000000000000000000000)

(err/rt-test (procedure-arity-includes? cons -1))
(err/rt-test (procedure-arity-includes? cons 1.0))
(err/rt-test (procedure-arity-includes? 'cons 1))

(arity-test procedure-arity-includes? 2 3)

(newline)
(display ";testing scheme 4 functions; ")
(test '(#\P #\space #\l) string->list "P l")
(test '() string->list "")
(test "1\\\"" list->string '(#\1 #\\ #\"))
(test "" list->string '())
(arity-test list->string 1 1)
(arity-test string->list 1 1)
(err/rt-test (string->list 'hello))
(err/rt-test (list->string 'hello))
(err/rt-test (list->string '(#\h . #\e)))
(err/rt-test (list->string '(#\h 1 #\e)))

(test '(dah dah didah) vector->list '#(dah dah didah))
(test '() vector->list '#())
(test '#(dididit dah) list->vector '(dididit dah))
(test '#() list->vector '())
(arity-test list->vector 1 1)
(arity-test vector->list 1 1)
(err/rt-test (vector->list 'hello))
(err/rt-test (list->vector 'hello))
(err/rt-test (list->vector '(#\h . #\e)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; errors

(err/rt-test (raise-arity-error 'f 5) exn:fail:contract:arity?)
(err/rt-test (raise-arity-error 'f (make-arity-at-least 5)) exn:fail:contract:arity?)
(err/rt-test (raise-arity-error 'f (list 1 (make-arity-at-least 5))) exn:fail:contract:arity?)
(err/rt-test (raise-arity-error + 5) exn:fail:contract:arity?)
(err/rt-test (raise-arity-error + (make-arity-at-least 5)) exn:fail:contract:arity?)
(err/rt-test (raise-arity-error + (list 1 (make-arity-at-least 5))) exn:fail:contract:arity?)

(err/rt-test (raise-result-arity-error 'f 5 #f) exn:fail:contract:arity?)
(err/rt-test (raise-result-arity-error #f 5 #f) exn:fail:contract:arity?)
(err/rt-test (raise-result-arity-error #f (expt 2 100) #f) exn:fail:contract:arity?)
(err/rt-test (raise-result-arity-error #f (expt 2 100) "\n  in: extra") exn:fail:contract:arity?)
(err/rt-test (raise-result-arity-error #f (expt 2 100) "\n  in: extra" 1 2 3 4 5) exn:fail:contract:arity?)
(err/rt-test (raise-result-arity-error 'oops 5 "%") exn:fail:contract:arity?)

(define (exn:fail:contract:arity?/#f e) (not (exn:fail:contract:arity? e)))

(err/rt-test (raise-arity-error 'f -5) exn:fail:contract:arity?/#f)
(err/rt-test (raise-arity-error 'f -5) exn:fail:contract?)
(err/rt-test (raise-arity-error 'f (list 1 'x)) exn:fail:contract:arity?/#f)
(err/rt-test (raise-arity-error 'f (list 1 'x)) exn:fail:contract?)
(err/rt-test (raise-arity-error 1 1) exn:fail:contract:arity?/#f)
(err/rt-test (raise-arity-error 1 1) exn:fail:contract?)

(err/rt-test (raise-result-arity-error "f" 7 #f) exn:fail:contract:arity?/#f)
(err/rt-test (raise-result-arity-error 'f -7 #f) exn:fail:contract:arity?/#f)
(err/rt-test (raise-result-arity-error 'f 7 #"oops") exn:fail:contract:arity?/#f)


(err/rt-test (raise-arity-mask-error 'f 4) exn:fail:contract:arity?)
(err/rt-test (raise-arity-mask-error 'f -8) exn:fail:contract:arity?)
(err/rt-test (raise-arity-mask-error 'f 5) exn:fail:contract:arity?)
(err/rt-test (raise-arity-mask-error 'f -5) exn:fail:contract:arity?)

(err/rt-test (raise-arity-mask-error 'f (arity-at-least 7)) exn:fail:contract:arity?/#f)
(err/rt-test (raise-arity-mask-error 'f -5.0) exn:fail:contract?)
(err/rt-test (raise-arity-mask-error 1 1) exn:fail:contract?)


(err/rt-test (raise-arguments-error 'proc-with-error "invalid") exn:fail:contract? #rx"proc-with-error: invalid")
(err/rt-test (raise-arguments-error "f" "invalid") exn:fail:contract? #rx"raise-arguments-error:")
(err/rt-test (raise-arguments-error 'f 'invalid) exn:fail:contract? #rx"raise-arguments-error:")
(err/rt-test (raise-arguments-error 'f "invalid" "x") exn:fail:contract? #rx"raise-arguments-error:")
(err/rt-test (raise-arguments-error 'f "invalid" 'x 5) exn:fail:contract? #rx"raise-arguments-error:")
(err/rt-test (raise-arguments-error 'f "invalid" "x" 5 "y") exn:fail:contract? #rx"raise-arguments-error:")

(err/rt-test (raise-arguments-error* "f" 'mars "invalid") exn:fail:contract? #rx"raise-arguments-error\\*:")
(err/rt-test (raise-arguments-error* 'f 'mars 'invalid) exn:fail:contract? #rx"raise-arguments-error\\*:")
(err/rt-test (raise-arguments-error* 'f 'mars "invalid" "x") exn:fail:contract? #rx"raise-arguments-error\\*:")
(err/rt-test (raise-arguments-error* 'f 'mars "invalid" 'x 5) exn:fail:contract? #rx"raise-arguments-error\\*:")
(err/rt-test (raise-arguments-error* 'f 'mars "invalid" "x" 5 "y") exn:fail:contract? #rx"raise-arguments-error\\*:")


(let ([vals (build-list 125 (lambda (_) #f))])
  (err/rt-test (apply raise-argument-error 'f "something/c" 0 vals)
               exn:fail:contract? #rx"argument position: 1st")
  (err/rt-test (apply raise-argument-error 'f "something/c" 1 vals)
               exn:fail:contract? #rx"argument position: 2nd")
  (err/rt-test (apply raise-argument-error 'f "something/c" 2 vals)
               exn:fail:contract? #rx"argument position: 3rd")
  (err/rt-test (apply raise-argument-error 'f "something/c" 4 vals)
               exn:fail:contract? #rx"argument position: 5th")
  (err/rt-test (apply raise-argument-error 'f "something/c" 10 vals)
               exn:fail:contract? #rx"argument position: 11th")
  (err/rt-test (apply raise-argument-error 'f "something/c" 11 vals)
               exn:fail:contract? #rx"argument position: 12th")
  (err/rt-test (apply raise-argument-error 'f "something/c" 12 vals)
               exn:fail:contract? #rx"argument position: 13th")
  (err/rt-test (apply raise-argument-error 'f "something/c" 14 vals)
               exn:fail:contract? #rx"argument position: 15th")
  (err/rt-test (apply raise-argument-error 'f "something/c" 20 vals)
               exn:fail:contract? #rx"argument position: 21st")
  (err/rt-test (apply raise-argument-error 'f "something/c" 21 vals)
               exn:fail:contract? #rx"argument position: 22nd")
  (err/rt-test (apply raise-argument-error 'f "something/c" 22 vals)
               exn:fail:contract? #rx"argument position: 23rd")
  (err/rt-test (apply raise-argument-error 'f "something/c" 24 vals)
               exn:fail:contract? #rx"argument position: 25th")
  (err/rt-test (apply raise-argument-error 'f "something/c" 100 vals)
               exn:fail:contract? #rx"argument position: 101st")
  (err/rt-test (apply raise-argument-error 'f "something/c" 101 vals)
               exn:fail:contract? #rx"argument position: 102nd")
  (err/rt-test (apply raise-argument-error 'f "something/c" 102 vals)
               exn:fail:contract? #rx"argument position: 103rd")
  (err/rt-test (apply raise-argument-error 'f "something/c" 104 vals)
               exn:fail:contract? #rx"argument position: 105th")
  (err/rt-test (apply raise-argument-error 'f "something/c" 110 vals)
               exn:fail:contract? #rx"argument position: 111th")
  (err/rt-test (apply raise-argument-error 'f "something/c" 111 vals)
               exn:fail:contract? #rx"argument position: 112th")
  (err/rt-test (apply raise-argument-error 'f "something/c" 112 vals)
               exn:fail:contract? #rx"argument position: 113th")
  (err/rt-test (apply raise-argument-error 'f "something/c" 114 vals)
               exn:fail:contract? #rx"argument position: 115th")
  (err/rt-test (apply raise-argument-error 'f "something/c" 120 vals)
               exn:fail:contract? #rx"argument position: 121st")
  (err/rt-test (apply raise-argument-error 'f "something/c" 121 vals)
               exn:fail:contract? #rx"argument position: 122nd")
  (err/rt-test (apply raise-argument-error 'f "something/c" 122 vals)
               exn:fail:contract? #rx"argument position: 123rd")
  (err/rt-test (apply raise-argument-error 'f "something/c" 124 vals)
               exn:fail:contract? #rx"argument position: 125th")
  )

(parameterize ([error-value->string-handler
                (lambda (v len)
                  (if (string? v)
                      v
                      (format "~s" v)))])
  (err/rt-test/once (raise-argument-error 'f "something" "one line")
                    exn:fail:contract?
                    #rx"given: one line")
  (err/rt-test/once (raise-argument-error 'f "something" "two\n lines")
                    exn:fail:contract?
                    #rx"given: \n   two\n    lines")
  (err/rt-test/once (raise-argument-error 'f "something" "\ntwo\n lines")
                    exn:fail:contract?
                    #rx"given: \ntwo\n lines")
  (err/rt-test/once (raise-arguments-error
                     'f
                     "fail"
                     "something" "two\n lines"
                     "more" "three\nmore\nlines")
                    exn:fail:contract?
                    #rx"something: \n   two\n    lines.*more: \n   three\n   more\n   lines")
  (err/rt-test/once (raise-arguments-error
                     'f
                     "fail"
                     "something" "two\n lines"
                     "more" (unquoted-printing-string "three\nmore\nlines"))
                    exn:fail:contract?
                    #rx"something: \n   two\n    lines.*more: \n   three\n   more\n   lines")
  (err/rt-test/once (raise-argument-error
                     'f
                     "something"
                     0
                     "whatever"
                     "two\n lines"
                     "three\nmore\nlines")
                    exn:fail:contract?
                    #rx"other arguments[.][.][.]:\n   two\n    lines\n   three\n   more\n   lines")
  (err/rt-test/once (+ 1 "two\n lines")
                    exn:fail:contract?
                    #rx"given: \n   two\n    lines")
  (err/rt-test/once ("two\n lines" 1)
                    exn:fail:contract?
                    #rx"given: \n   two\n    lines")
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; continuations

(test-cont)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; hash tables

(arity-test make-hash 0 1)
(arity-test make-hasheq 0 1)
(arity-test make-hasheqv 0 1)
(arity-test make-hashalw 0 1)
(arity-test make-weak-hash 0 1)
(arity-test make-weak-hasheq 0 1)
(arity-test make-weak-hasheqv 0 1)
(arity-test make-weak-hashalw 0 1)
(arity-test make-ephemeron-hash 0 1)
(arity-test make-ephemeron-hasheq 0 1)
(arity-test make-ephemeron-hasheqv 0 1)
(arity-test make-ephemeron-hashalw 0 1)

(define (hash-tests make-hash make-hasheq make-hasheqv make-hashalw
                    make-weak-hash make-weak-hasheq make-weak-hasheqv make-weak-hashalw
                    make-ephemeron-hash make-ephemeron-hasheq make-ephemeron-hasheqv make-ephemeron-hashalw
                    hash-ref hash-set! hash-ref! hash-update! hash-has-key?
                    hash-remove! hash-count
                    hash-map hash-for-each
                    hash-iterate-first hash-iterate-next
                    hash-iterate-value hash-iterate-key
                    hash-copy
                    hash-clear! hash-clear
                    hash-empty?
                    hash-keys-subset?)
  (define-struct ax (b c)) ; opaque
  (define-struct a (b c) #:inspector (make-inspector))

  (define save
    (let ([x null]) (case-lambda [() x] [(a) (set! x (cons a x)) a])))
  (define an-ax (make-ax 1 2))

  (define (check-hash-tables weak-kind reorder?)
    (struct wrap (f) #:property prop:procedure 0)
    (let ([h1 (case weak-kind
                [(weak) (make-weak-hasheq)]
                [(ephemeron) (make-ephemeron-hasheq)]
                [else (make-hasheq)])]
          [l (list 1 2 3)])
      (test #t eq? (eq-hash-code l) (eq-hash-code l))
      (test #t eq? (eqv-hash-code l) (eqv-hash-code l))
      (test #t eq? (equal-hash-code l) (equal-hash-code l))
      (test #t eq? (equal-hash-code l) (equal-hash-code (list 1 2 3)))
      (test #t eq? (equal-always-hash-code l) (equal-always-hash-code l))
      (test #t eq? (equal-always-hash-code l) (equal-always-hash-code (list 1 2 3)))
      (hash-set! h1 l 'ok)
      (test 'ok hash-ref h1 l)
      (err/rt-test (hash-ref h1 'nonesuch (lambda (x) 'bad-proc)) exn:fail:contract:arity? "hash-ref")
      (test #t hash-has-key? h1 l)
      (test #f hash-has-key? h1 (cdr l))
      (when hash-ref!
        (test 'ok hash-ref! h1 l 'blah)
        (test 'blah hash-ref! h1 (cdr l) 'blah)
        (test #t hash-has-key? h1 (cdr l))
        (test 'blah hash-ref h1 (cdr l))
        (hash-remove! h1 (cdr l)))
      (hash-update! h1 l (curry cons 'more))
      (test '(more . ok) hash-ref h1 l)
      (hash-update! h1 l cdr)
      (test 'nope hash-ref h1 (list 1 2 3) (lambda () 'nope))
      (test '(((1 2 3) . ok)) hash-map h1 (lambda (k v) (cons k v)))
      (test '(((1 2 3) . ok)) hash-map h1 (lambda (k v) (cons k v)) #t)
      (test '(((1 2 3) . ok)) hash-map h1 (wrap (lambda (k v) (cons k v))))
      (test '(((1 2 3) . ok)) hash-map h1 (wrap (lambda (k v) (cons k v))) #t)
      (hash-remove! h1 l)
      (test 'nope hash-ref h1 l (lambda () 'nope))
      (err/rt-test (hash-update! h1 l add1))
      (hash-update! h1 l add1 0)
      (test 1 hash-ref h1 l)
      (hash-remove! h1 l))

    (let ([h1 (case weak-kind
                [(weak) (make-weak-hasheqv)]
                [(ephemeron) (make-ephemeron-hasheqv)]
                [else (make-hasheqv)])]
          [n (expt 2 500)]
          [q (/ 1 2)]
          [s (make-string 2 #\q)])
      (hash-set! h1 n 'power)
      (hash-set! h1 q 'half)
      (hash-set! h1 s 'string)
      (test 'power hash-ref h1 (expt (read (open-input-string "2")) 500))
      (test 'half hash-ref h1 (/ 1 (read (open-input-string "2"))))
      (test #f hash-ref h1 (make-string (read (open-input-string "2")) #\q) #f))

    (let ([h1 (case weak-kind
                [(weak) (make-weak-hash)]
                [(ephemeron) (make-ephemeron-hash)]
                [else (make-hash)])]
          [l (list 1 2 3)]
          [v (vector 5 6 7)]
          [a (make-a 1 (make-a 2 3))]
          [b (box (list 1 2 3))]
          [fl (flvector 1.0 +nan.0 0.0)]
          [fx (fxvector 1 -2 0)]
          [cyclic-list (read (open-input-string "#2=(#1=(#2#) #2#)"))])

      (test 0 hash-count h1)

      ;; Fill in table. Use `puts1' and `puts2' so we can
      ;; vary the order of additions.
      (let ([puts1 (lambda ()
                     (hash-set! h1 (save l) 'list)
                     (hash-set! h1 (save "Hello World!") 'string)
                     (hash-set! h1 (save 123456789123456789123456789) 'bignum)
                     (hash-set! h1 (save 3.45) 'flonum)
                     (hash-set! h1 (save 3/45) 'rational)
                     (hash-set! h1 (save 3+45i) 'complex)
                     (hash-set! h1 (save (integer->char 955)) 'char)
                     (hash-set! h1 (save fl) 'flvector)
                     (hash-set! h1 (save fx) 'fxvector))]
            [puts2 (lambda ()
                     (hash-set! h1 (save (list 5 7)) 'another-list)
                     (hash-set! h1 (save 3+0.0i) 'izi-complex)
                     (hash-set! h1 (save v) 'vector)
                     (hash-set! h1 (save a) 'struct)
                     (hash-set! h1 (save an-ax) 'structx)
                     (hash-set! h1 (save b) 'box)
                     (hash-set! h1 (save cyclic-list) 'cyclic-list))])
        (if reorder?
          (begin
            (puts2)
            (test 7 hash-count h1)
            (puts1))
          (begin
            (puts1)
            (test 9 hash-count h1)
            (puts2))))

      (when reorder?
        ;; Add 1000 things and take them back out in an effort to
        ;; trigger GCs that somehow affect hashing:
        (let loop ([i 0.0])
          (unless (= i 1000.0)
            (hash-set! h1 i #t)
            (loop (add1 i))
            (hash-remove! h1 i))))

      (test 16 hash-count h1)
      (test 'list hash-ref h1 l)
      (test 'list hash-ref h1 (list 1 2 3))
      (test 'another-list hash-ref h1 (list 5 7))
      (test 'string hash-ref h1 "Hello World!")
      (test 'bignum hash-ref h1 123456789123456789123456789)
      (test 'flonum hash-ref h1 3.45)
      (test 'rational hash-ref h1 3/45)
      (test 'complex hash-ref h1 3+45i)
      (test 'izi-complex hash-ref h1 3+0.0i)
      (test 'vector hash-ref h1 v)
      (test 'vector hash-ref h1 #(5 6 7))
      (test 'struct hash-ref h1 a)
      (test 'struct hash-ref h1 (make-a 1 (make-a 2 3)))
      (test 'structx hash-ref h1 an-ax)
      (test #f hash-ref h1 (make-ax 1 2) (lambda () #f))
      (test 'box hash-ref h1 b)
      (test 'box hash-ref h1 #&(1 2 3))
      (test 'char hash-ref h1 (integer->char 955))
      (test 'flvector hash-ref h1 (flvector 1.0 +nan.0 0.0))
      (test 'fxvector hash-ref h1 (fxvector 1 -2 0))
      (test 'cyclic-list hash-ref h1 cyclic-list)
      (test #t
            andmap
            (lambda (i)
              (and (member i (hash-map h1 (lambda (k v) (cons k v))))
                   #t))
            `(((1 2 3) . list)
              ((5 7) . another-list)
              ("Hello World!" . string)
              (123456789123456789123456789 . bignum)
              (3.45 . flonum)
              (3/45 . rational)
              (3+45i . complex)
              (3+0.0i . izi-complex)
              (#(5 6 7) . vector)
              (,(make-a 1 (make-a 2 3)) . struct)
              (,an-ax . structx)
              (#\u3BB . char)
              (#&(1 2 3) . box)
              (,(flvector 1.0 +nan.0 0.0) . flvector)
              (,(fxvector 1 -2 0) . fxvector)
              (,cyclic-list . cyclic-list)))

      (hash-remove! h1 (list 1 2 3))
      (test 15 hash-count h1)
      (test 'not-there hash-ref h1 l (lambda () 'not-there))
      (let ([c 0])
        (hash-for-each h1 (lambda (k v) (set! c (add1 c))))
        (test 15 'count c))
      (hash-for-each h1 void #t)
      (hash-for-each h1 (wrap void))
      (hash-for-each h1 (wrap void) #t)
      ;; return the hash table:
      h1))

  (define (check-tables-equal mode t1 t2 weak-kind)
    (test #t equal? t1 t2)
    (test #t hash-keys-subset? t1 t2)
    (test (equal-hash-code t1) equal-hash-code t2)
    (test #t equal? t1 (hash-copy t1))
    (let ([again (case weak-kind
                   [(weak) (make-weak-hash)]
                   [(ephemeron) (make-ephemeron-hash)]
                   [else (make-hash)])])
      (let loop ([i (hash-iterate-first t1)])
        (when i
          (hash-set! again
                     (hash-iterate-key t1 i)
                     (hash-iterate-value t1 i))
          (loop (hash-iterate-next t1 i))))
      (test #t equal? t1 again))
    (let ([meta-ht (make-hash)])
      (hash-set! meta-ht t1 mode)
      (test mode hash-ref meta-ht t2 (lambda () #f)))
    (test (hash-count t1) hash-count t2))

  (check-tables-equal 'the-norm-table
                      (check-hash-tables #f #f)
                      (check-hash-tables #f #t)
                      #f)
  (when make-weak-hash
    (check-tables-equal 'the-weak-table
                        (check-hash-tables 'weak #f)
                        (check-hash-tables 'weak #t)
                        'weak)
    (check-tables-equal 'the-ephemeron-table
                        (check-hash-tables 'ephemeron #f)
                        (check-hash-tables 'ephemeron #t)
                        'ephemeron))
  
  ;; Make sure copy doesn't share:
  (for ([make-hash (list make-hash
                         make-weak-hash
                         make-ephemeron-hash)])
    (when make-hash
      (define c1 (make-hash))
      (hash-set! c1 'the-key1 'the-val1)
      (hash-set! c1 'the-key2 'the-val2)
      (hash-set! c1 'the-key3 'the-val3)
      (hash-set! c1 'the-key4 'the-val4)
      (define c2 (hash-copy c1))
      (hash-set! c1 'the-key3 'the-val5)
      (hash-set! c2 'the-key4 'the-val6)
      (hash-remove! c1 'the-key1)
      (hash-remove! c2 'the-key2)
      (test 'the-val1 hash-ref c2 'the-key1)
      (test 'the-val2 hash-ref c1 'the-key2)
      (test 'the-val3 hash-ref c2 'the-key3)
      (test 'the-val4 hash-ref c1 'the-key4)))

  (for ([make-hash (list make-hash
                         make-weak-hash
                         make-ephemeron-hash)])
    (when make-hash
      (define c1 (make-hash))
      (hash-set! c1 'the-key1 'the-val1)
      (hash-set! c1 'the-key2 'the-val2)
      (hash-set! c1 'the-key3 'the-val3)
      (hash-set! c1 'the-key4 'the-val4)
      (test #f hash-empty? c1)
      (hash-clear! c1)
      (test #t hash-empty? c1)))

  (save)) ; prevents gcing of the ht-registered values

(hash-tests make-hash make-hasheq make-hasheqv make-hashalw
            make-weak-hash make-weak-hasheq make-weak-hasheqv make-weak-hashalw
            make-ephemeron-hash make-ephemeron-hasheq make-ephemeron-hasheqv make-ephemeron-hashalw
            hash-ref hash-set! hash-ref! hash-update! hash-has-key?
            hash-remove! hash-count
            hash-map hash-for-each
            hash-iterate-first hash-iterate-next
            hash-iterate-value hash-iterate-key
            hash-copy
            hash-clear! hash-clear
            hash-empty?
            hash-keys-subset?)
(let ([ub-wrap (lambda (proc)
                 (lambda (ht . args)
                   (apply proc (unbox ht) args)))])
  (hash-tests (lambda () (box #hash()))
              (lambda () (box #hasheq()))
              (lambda () (box #hasheqv()))
              (lambda () (box #hashalw()))
              #f #f #f #f
              #f #f #f #f
              (ub-wrap hash-ref)
              (lambda (ht k v) (set-box! ht (hash-set (unbox ht) k v)))
              #f
              (case-lambda
               [(ht k u) (set-box! ht (hash-update (unbox ht) k u))]
               [(ht k u def) (set-box! ht (hash-update (unbox ht) k u def))])
              (ub-wrap hash-has-key?)
              (lambda (ht k) (set-box! ht (hash-remove (unbox ht) k)))
              (ub-wrap hash-count)
              (ub-wrap hash-map)
              (ub-wrap hash-for-each)
              (ub-wrap hash-iterate-first)
              (ub-wrap hash-iterate-next)
              (ub-wrap hash-iterate-value)
              (ub-wrap hash-iterate-key)
              (lambda (ht) (box (unbox ht)))
              (lambda (ht) (set-box! ht (hash-clear (unbox ht))))
              #f
              (ub-wrap hash-empty?)
              (lambda (ht1 ht2)
                (hash-keys-subset? (unbox ht1) (unbox ht2)))))

(test #f hash? 5)
(test #t hash? (make-hasheq))
(test #t hash? (make-hasheqv))
(test #t hash? (make-hashalw))
(test #t hash-eq? (hasheq))
(test #f hash-eq? (hash))
(test #f hash-eq? (hasheqv))
(test #f hash-eq? (hashalw))
(test #t hash-eq? (make-hasheq))
(test #f hash-eq? (make-hash))
(test #f hash-eq? (make-hasheqv))
(test #f hash-eq? (make-hashalw))
(test #t hash-eq? (make-weak-hasheq))
(test #f hash-eq? (make-weak-hash))
(test #f hash-eq? (make-weak-hasheqv))
(test #f hash-eq? (make-weak-hashalw))
(test #t hash-eq? (make-ephemeron-hasheq))
(test #f hash-eq? (make-ephemeron-hash))
(test #f hash-eq? (make-ephemeron-hasheqv))
(test #f hash-eq? (make-ephemeron-hashalw))
(test #f hash-eqv? (hasheq))
(test #f hash-eqv? (hash))
(test #t hash-eqv? (hasheqv))
(test #f hash-eqv? (hashalw))
(test #f hash-eqv? (make-hasheq))
(test #f hash-eqv? (make-hash))
(test #t hash-eqv? (make-hasheqv))
(test #f hash-eqv? (make-hashalw))
(test #f hash-eqv? (make-weak-hasheq))
(test #f hash-eqv? (make-weak-hash))
(test #t hash-eqv? (make-weak-hasheqv))
(test #f hash-eqv? (make-weak-hashalw))
(test #f hash-eqv? (make-ephemeron-hasheq))
(test #f hash-eqv? (make-ephemeron-hash))
(test #t hash-eqv? (make-ephemeron-hasheqv))
(test #f hash-eqv? (make-ephemeron-hashalw))
(test #f hash-equal-always? (hasheq))
(test #f hash-equal-always? (hash))
(test #f hash-equal-always? (hasheqv))
(test #t hash-equal-always? (hashalw))
(test #f hash-equal-always? (make-hasheq))
(test #f hash-equal-always? (make-hash))
(test #f hash-equal-always? (make-hasheqv))
(test #t hash-equal-always? (make-hashalw))
(test #f hash-equal-always? (make-weak-hasheq))
(test #f hash-equal-always? (make-weak-hash))
(test #f hash-equal-always? (make-weak-hasheqv))
(test #t hash-equal-always? (make-weak-hashalw))
(test #f hash-equal-always? (make-ephemeron-hasheq))
(test #f hash-equal-always? (make-ephemeron-hash))
(test #f hash-equal-always? (make-ephemeron-hasheqv))
(test #t hash-equal-always? (make-ephemeron-hashalw))
(test #f hash-weak? (hasheq))
(test #f hash-weak? (hash))
(test #f hash-weak? (hasheqv))
(test #f hash-weak? (hashalw))
(test #f hash-weak? (make-hasheq))
(test #f hash-weak? (make-hash))
(test #f hash-weak? (make-hasheqv))
(test #f hash-weak? (make-hashalw))
(test #t hash-weak? (make-weak-hasheq))
(test #t hash-weak? (make-weak-hash))
(test #t hash-weak? (make-weak-hasheqv))
(test #t hash-weak? (make-weak-hashalw))
(test #f hash-weak? (make-ephemeron-hasheq))
(test #f hash-weak? (make-ephemeron-hash))
(test #f hash-weak? (make-ephemeron-hasheqv))
(test #f hash-weak? (make-ephemeron-hashalw))
(test #f hash-ephemeron? (hasheq))
(test #f hash-ephemeron? (hash))
(test #f hash-ephemeron? (hasheqv))
(test #f hash-ephemeron? (hashalw))
(test #f hash-ephemeron? (make-hasheq))
(test #f hash-ephemeron? (make-hash))
(test #f hash-ephemeron? (make-hasheqv))
(test #f hash-ephemeron? (make-hashalw))
(test #f hash-ephemeron? (make-weak-hasheq))
(test #f hash-ephemeron? (make-weak-hash))
(test #f hash-ephemeron? (make-weak-hasheqv))
(test #f hash-ephemeron? (make-weak-hashalw))
(test #t hash-ephemeron? (make-ephemeron-hasheq))
(test #t hash-ephemeron? (make-ephemeron-hash))
(test #t hash-ephemeron? (make-ephemeron-hasheqv))
(test #t hash-ephemeron? (make-ephemeron-hashalw))
(test #t hash-strong? (hasheq))
(test #t hash-strong? (hash))
(test #t hash-strong? (hasheqv))
(test #t hash-strong? (hashalw))
(test #t hash-strong? (make-hasheq))
(test #t hash-strong? (make-hash))
(test #t hash-strong? (make-hasheqv))
(test #t hash-strong? (make-hashalw))
(test #f hash-strong? (make-weak-hasheq))
(test #f hash-strong? (make-weak-hash))
(test #f hash-strong? (make-weak-hasheqv))
(test #f hash-strong? (make-weak-hashalw))
(test #f hash-strong? (make-ephemeron-hasheq))
(test #f hash-strong? (make-ephemeron-hash))
(test #f hash-strong? (make-ephemeron-hasheqv))
(test #f hash-strong? (make-ephemeron-hashalw))

(let ([ht (make-hasheqv)]
      [l (list #x03B1 #x03B2 #x03B3)]
      [l2 '(1 2 3)])
  (for-each (lambda (a b)
              (hash-set! ht (integer->char a) b))
            l l2)
  (test '(3 2 1) 
        map
        (lambda (a)
          (hash-ref ht (integer->char a) #f))
        (reverse l)))

(err/rt-test (hash-eq? 5))
(err/rt-test (hash-eqv? 5))
(err/rt-test (hash-equal-always? 5))
(err/rt-test (hash-equal? 5))
(err/rt-test (hash-weak? 5))
(err/rt-test (hash-ephemeron? 5))
(err/rt-test (hash-strong? 5))

(let ([a (expt 2 500)]
      [b (expt (read (open-input-string "2")) 500)])
  (test #t equal? (eqv-hash-code a) (eqv-hash-code b))
  (test #t equal? (equal-hash-code a) (equal-hash-code b))
  (test #t equal? (equal-always-hash-code a) (equal-always-hash-code b)))

;; Check for proper clearing of weak hash tables
;; (internally, value should get cleared along with key):
(let ([ht (make-weak-hasheq)]
      [et (make-ephemeron-hasheq)])
  (let loop ([n 10])
    (unless (zero? n)
      (hash-set! ht (make-string 10) #f)
      (hash-set! et (make-string 10) #f)
      (loop (sub1 n))))
  (collect-garbage)
  (map (lambda (i) (format "~a" i)) (hash-map ht cons))
  (map (lambda (i) (format "~a" i)) (hash-map et cons)))

;; Double check that table are equal after deletions
(let ([test-del-eq
       (lambda (mk)
	 (let ([ht1 (mk)]
	       [ht2 (mk)])
	   (test #t equal? ht1 ht2)
	   (hash-set! ht1 'apple 1)
	   (hash-set! ht2 'apple 1)
	   (test #t equal? ht1 ht2)
	   (hash-set! ht2 'banana 2)
	   (test #f equal? ht1 ht2)
	   (hash-remove! ht2 'banana)
	   (test #t equal? ht1 ht2)))])
  (test-del-eq make-hasheq)
  (test-del-eq make-hash)
  (test-del-eq make-weak-hasheq)
  (test-del-eq make-weak-hash)
  (test-del-eq make-ephemeron-hasheq)
  (test-del-eq make-ephemeron-hash))

(err/rt-test (hash-count 0))
(err/rt-test (hash-set! 1 2 3))
(err/rt-test (hash-ref 1 2))
(err/rt-test (hash-remove! 1 2))
(err/rt-test (hash-ref (make-hasheq) 2) exn:application:mismatch?)

(let ([mk (lambda (mk)
            (let ([ht (mk)])
              (hash-set! ht make-hash 2)
              ht))])
  (test #t equal? (mk make-hash) (mk make-hash))
  (test #t equal? (mk make-hasheq) (mk make-hasheq))
  (test #t equal? (mk make-hasheqv) (mk make-hasheqv))
  (test #t equal? (mk make-hashalw) (mk make-hashalw))
  (test #f equal? (mk make-hash) (mk make-hasheq))
  (test #f equal? (mk make-hash) (mk make-hasheqv))
  (test #f equal? (mk make-hash) (mk make-hashalw))
  (test #f equal? (mk make-hasheq) (mk make-hasheqv))
  (test #f equal? (mk make-hasheq) (mk make-hashalw))
  (test #f equal? (mk make-hasheqv) (mk make-hashalw))
  (test #f equal? (mk make-hash) (mk make-weak-hash))
  (test #f equal? (mk make-hasheq) (mk make-weak-hasheq))
  (test #f equal? (mk make-hasheqv) (mk make-weak-hasheqv))
  (test #f equal? (mk make-hashalw) (mk make-weak-hashalw))
  (test #f equal? (mk make-hash) (mk make-ephemeron-hash))
  (test #f equal? (mk make-hasheq) (mk make-ephemeron-hasheq))
  (test #f equal? (mk make-hasheqv) (mk make-ephemeron-hasheqv))
  (test #f equal? (mk make-hashalw) (mk make-ephemeron-hashalw))
  (test #f equal? (mk make-weak-hash) (mk make-ephemeron-hash))
  (test #f equal? (mk make-weak-hasheq) (mk make-ephemeron-hasheq))
  (test #f equal? (mk make-weak-hasheqv) (mk make-ephemeron-hasheqv))
  (test #f equal? (mk make-weak-hashalw) (mk make-ephemeron-hashalw)))
(let ([mk (lambda (mk)
            (mk `((1 . 2))))])
  (test #t equal? (mk make-immutable-hash) (mk make-immutable-hash))
  (test #t equal? (mk make-immutable-hasheq) (mk make-immutable-hasheq))
  (test #t equal? (mk make-immutable-hasheqv) (mk make-immutable-hasheqv))
  (test #t equal? (mk make-immutable-hashalw) (mk make-immutable-hashalw))
  (test #f equal? (mk make-immutable-hash) (mk make-immutable-hasheq))
  (test #f equal? (mk make-immutable-hash) (mk make-immutable-hasheqv))
  (test #f equal? (mk make-immutable-hash) (mk make-immutable-hashalw))
  (test #f equal? (mk make-immutable-hasheq) (mk make-immutable-hasheqv))
  (test #f equal? (mk make-immutable-hasheq) (mk make-immutable-hashalw))
  (test #f equal? (mk make-immutable-hasheqv) (mk make-immutable-hashalw)))

(let ([check-subset (lambda (mk1 mk2 [v2 2] #:k1 [k1 'a] #:k2 [k2 'a])
                      (define h1 (mk1 k1 #t 'b v2))
                      (define h2 (mk2 k2 #t))
                      (test #t hash-keys-subset? h2 h1)
                      (test #f hash-keys-subset? h1 h2)
                      (define h3 (mk2 k2 'something-else))
                      (test #t hash-keys-subset? h3 h1)
                      (test #t hash-keys-subset? h3 h2))]
      [make-make-hash (lambda (mk)
                        (lambda args
                          (define ht (mk))
                          (let loop ([args args])
                            (cond
                             [(null? args) (void)]
                             [else
                              (hash-set! ht (car args) (cadr args))
                              (loop (cddr args))]))
                          ht))])
                        
  (check-subset hasheq hasheq #t)
  (check-subset hasheq hasheq)
  (check-subset hasheqv hasheqv)
  (check-subset hasheqv hasheqv #:k1 (expt 2 70) #:k2 (expt 2 70))
  (check-subset hashalw hashalw)
  (check-subset hashalw hashalw #:k1 (cons 1 2) #:k2 (cons 1 2))
  (check-subset hash hash)
  (check-subset hash hash #:k1 (cons 1 2) #:k2 (cons 1 2))
  (check-subset hasheq (make-make-hash make-hasheq))
  (check-subset hasheq (make-make-hash make-weak-hasheq))
  (check-subset hasheq (make-make-hash make-ephemeron-hasheq))
  (check-subset hasheqv (make-make-hash make-hasheqv))
  (check-subset hasheqv (make-make-hash make-weak-hasheqv))
  (check-subset hasheqv (make-make-hash make-ephemeron-hasheqv))
  (check-subset hashalw (make-make-hash make-hashalw))
  (check-subset hashalw (make-make-hash make-weak-hashalw))
  (check-subset hashalw (make-make-hash make-ephemeron-hashalw))
  (check-subset hash (make-make-hash make-hash))
  (check-subset hash (make-make-hash make-weak-hash))
  (check-subset hash (make-make-hash make-ephemeron-hash))
  (check-subset (make-make-hash make-hash) (make-make-hash make-weak-hash))
  (check-subset (make-make-hash make-hash) (make-make-hash make-ephemeron-hash))
  (check-subset hash (make-make-hash make-hash) #:k1 (expt 2 70) #:k2 (expt 2 70)))

(let ([not-same-comparison? (lambda (x)
                              (regexp-match? #rx"do not use the same key comparison" (exn-message x)))])
  (err/rt-test (hash-keys-subset? #hash() #hasheq()) not-same-comparison?)
  (err/rt-test (hash-keys-subset? #hash() #hasheqv()) not-same-comparison?)
  (err/rt-test (hash-keys-subset? #hash() #hashalw()) not-same-comparison?)
  (err/rt-test (hash-keys-subset? #hasheq() #hasheqv()) not-same-comparison?)
  (err/rt-test (hash-keys-subset? #hasheq() #hashalw()) not-same-comparison?)
  (err/rt-test (hash-keys-subset? #hasheqv() #hashalw()) not-same-comparison?)
  (err/rt-test (hash-keys-subset? (make-hasheq #hasheqv()) not-same-comparison?))
  (err/rt-test (hash-keys-subset? (make-weak-hasheq #hasheqv()) not-same-comparison?))
  (err/rt-test (hash-keys-subset? (make-ephemeron-hasheq #hasheqv()) not-same-comparison?)))

(define im-t (make-immutable-hasheq null))
(test #t hash? im-t)
(test #t hash-eq? im-t)
(test null hash-map im-t cons)
(err/rt-test (hash-set! im-t 1 2))
(test #f hash-ref im-t 5 (lambda () #f))
(define im-t (make-immutable-hasheq '((1 . 2))))
(test '((1 . 2)) hash-map im-t cons)
(test 2 hash-ref im-t 1)
(define im-t (make-immutable-hasheq '(("hello" . 2))))
(test 2 hash-ref im-t "hello" (lambda () 'none)) ; literals interned
(test 'none hash-ref im-t (list->string (string->list "hello")) (lambda () 'none))
(define im-t (make-immutable-hash '(("hello" . 2))))
(test 2 hash-ref im-t "hello" (lambda () 'none))
(test #f hash-eq? im-t)

(test #f equal? #hash((x . 0)) #hash((y . 0)))
(test #t equal? #hash((y . 0)) #hash((y . 0)))

(err/rt-test (hash-set! im-t 1 2))
(err/rt-test (hash-remove! im-t 1))
(err/rt-test (make-immutable-hasheq '(1)))
(err/rt-test (make-immutable-hasheq '((1 . 2) . 2)))
(err/rt-test (make-immutable-hasheq '((1 . 2) 3)))
(define cyclic-alist (read (open-input-string "#0=((1 . 2) . #0#)")))
(err/rt-test (make-immutable-hasheq cyclic-alist))
(err/rt-test (make-immutable-hasheq '((1 . 2)) 'weak))

(test 2 hash-ref (hash-copy #hasheq((1 . 2))) 1)
(test (void) hash-set! (hash-copy #hasheq((1 . 2))) 3 4)

(test #f hash-iterate-first (make-hasheq))
(test #f hash-iterate-first (make-weak-hasheq))
(test #f hash-iterate-first (make-ephemeron-hasheq))
(test #f hash-iterate-next (make-hasheq) 0)
(test #f hash-iterate-next (make-weak-hasheq) 0)
(test #f hash-iterate-next (make-ephemeron-hasheq) 0)

(let ([hts (list (make-hash)
                 (make-hasheq)
                 (make-hasheqv)
                 (make-hashalw)
                 (make-weak-hash)
                 (make-weak-hasheq)
                 (make-weak-hasheqv)
                 (make-weak-hashalw)
                 (make-ephemeron-hash)
                 (make-ephemeron-hasheq)
                 (make-ephemeron-hasheqv)
                 (make-ephemeron-hashalw)
                 (hash)
                 (hasheq)
                 (hasheqv)
                 (hashalw))])
  (let* ([check-all-bad
          (lambda (op)
            (err/rt-test (op #f 0))
            (err/rt-test (op (make-hasheq) -1))
            (err/rt-test (op (make-hasheq) (- (expt 2 100))))
            (err/rt-test (op (make-hasheq) 1.0)))]
         [check-all-bad-v
          (lambda (op)
            (check-all-bad op)
            (for ([ht (in-list hts)])
              (test 'nope op ht 17 'nope)))]
         [check-all-bad-pair
          (lambda (op)
            (check-all-bad op)
            (for ([ht (in-list hts)])
              (test '(nope . nope) op ht 17 'nope)))]
         [check-all-bad-values
          (lambda (op)
            (check-all-bad op)
            (for ([ht (in-list hts)])
              (test-values '(nope nope)
                           (lambda () (op ht 17 'nope)))))])
    (check-all-bad hash-iterate-next)
    (check-all-bad-v hash-iterate-key)
    (check-all-bad-v hash-iterate-value)
    (check-all-bad-pair hash-iterate-pair)
    (check-all-bad-values hash-iterate-key+value)))

(test (list 1 2 3) sort (hash-keys #hasheq((1 . a) (2 . b) (3 . c))) <)
(test (list 1 2 3) hash-keys #hasheq((1 . a) (2 . b) (3 . c)) #t)
(test (list 'a 'b 'c) 
      sort (hash-values #hasheq((1 . a) (2 . b) (3 . c))) string<? #:key symbol->string)
(test (list 'a 'b 'c)
      hash-values #hasheq((1 . a) (2 . b) (3 . c)) #t)
(test (list (cons 1 'a) (cons 2 'b) (cons 3 'c)) 
      sort (hash->list #hasheq((1 . a) (2 . b) (3 . c))) < #:key car)
(test (list (cons 'a 1) (cons 'b 2))
      hash->list #hash((a . 1) (b . 2)) #t)

(err/rt-test (hash-set*! im-t 1 2) exn:fail?)
(err/rt-test (hash-set* (make-hasheq null) 1 2) exn:fail?)
(err/rt-test (hash-set* im-t 1 2 3) exn:fail?)
(err/rt-test (hash-set*! (make-hasheq null) 1 2 3) exn:fail?)

(test #t equal? (hash-set* (hasheq 1 'a 3 'b)) (hasheq 1 'a 3 'b))
(test #t equal? (hasheq 1 2 3 4) 
      (hash-set* (hasheq 1 'a 3 'b)
                 1 (gensym)
                 1 2
                 3 (gensym)
                 3 4))
(test #t equal? (make-hasheq (list (cons 1 'a) (cons 3 'b))) 
      (let ([ht (make-hasheq (list (cons 1 'a) (cons 3 'b)))])
        (hash-set*! ht)
        ht))
(test #t equal? (make-hasheq (list (cons 1 2) (cons 3 'b))) 
      (let ([ht (make-hasheq (list (cons 1 'a) (cons 3 'b)))])
        (hash-set*! ht
                    1 2)
        ht))
(test #t equal? (make-hasheq (list (cons 1 2) (cons 3 4))) 
      (let ([ht (make-hasheq (list (cons 1 'a) (cons 3 'b)))])
        (hash-set*! ht
                    1 (gensym)
                    1 2
                    3 (gensym)
                    3 4)
        ht))

(arity-test make-immutable-hash 0 1)
(arity-test make-immutable-hasheq 0 1)
(arity-test make-immutable-hasheqv 0 1)
(arity-test make-immutable-hashalw 0 1)
(arity-test hash-keys 1 2)
(arity-test hash-values 1 2)
(arity-test hash-count 1 1)
(arity-test hash-ref 2 3)
(arity-test hash-set! 3 3)
(arity-test hash-set 3 3)
(arity-test hash-remove! 2 2)
(arity-test hash-remove 2 2)
(arity-test hash-map 2 3)
(arity-test hash-for-each 2 3)
(arity-test hash? 1 1)
(arity-test hash-eq? 1 1)
(arity-test hash-eqv? 1 1)
(arity-test hash-equal-always? 1 1)
(arity-test hash-equal? 1 1)
(arity-test hash-weak? 1 1)
(arity-test hash-ephemeron? 1 1)
(arity-test hash-strong? 1 1)

;; Ensure that hash-table hashing is not sensitive to the
;; order of key+value additions
(let ()
  (define ht (make-hash))
  (define ht2 (make-hash))
  (define wht (make-weak-hash))
  (define wht2 (make-weak-hash))
  (define eht (make-ephemeron-hash))
  (define eht2 (make-ephemeron-hash))
  (define keys (make-hasheq))

  (struct a (x) #:transparent)
  
  (define (shuffle c l)
    (if (zero? c)
        l
        (shuffle
         (sub1 c)
         (let ([n (quotient (length l) 2)])
           (let loop ([a (take l n)][b (drop l n)])
             (cond
              [(null? a) b]
              [(null? b) a]
              [(zero? (random 2))
               (cons (car a) (loop (cdr a) b))]
              [else
               (cons (car b) (loop a (cdr b)))]))))))
  
  (define l (for/list ([i (in-range 1000)]) 
              i))
  
  (define l2 (shuffle 7 l))

  (define (reg v)
    (hash-set! keys v #t)
    v)
  
  (for ([i (in-list l)])
    (hash-set! ht (a i) (a (a i))))
  (for ([i (in-list l2)])
    (hash-set! ht2 (a i) (a (a i))))
  
  (for ([i (in-list l)])
    (hash-set! wht (reg (a i)) (a (a i))))
  (for ([i (in-list l2)])
    (hash-set! wht2 (reg (a i)) (a (a i))))
  
  (for ([i (in-list l)])
    (hash-set! eht (reg (a i)) (a (a i))))
  (for ([i (in-list l2)])
    (hash-set! eht2 (reg (a i)) (a (a i))))
  
  (test (equal-hash-code ht) values (equal-hash-code ht2))
  (test (equal-hash-code wht) values (equal-hash-code wht2))
  (test (equal-hash-code eht) values (equal-hash-code eht2))
  (test (equal-secondary-hash-code ht) values (equal-secondary-hash-code ht2))
  (test (equal-secondary-hash-code wht) values (equal-secondary-hash-code wht2))
  (test (equal-secondary-hash-code eht) values (equal-secondary-hash-code eht2))

  (let ([ht (for/hash ([i (in-list l)])
              (values (a i) (a (a i))))]
        [ht2 (for/hash ([i (in-list l2)])
               (values (a i) (a (a i))))])
    (test (equal-hash-code ht) values (equal-hash-code ht2))
    (test (equal-secondary-hash-code ht) values (equal-secondary-hash-code ht2))
    (test (equal-always-hash-code ht) values (equal-always-hash-code ht2))
    (test (equal-always-secondary-hash-code ht) values (equal-always-secondary-hash-code ht2)))

  ;; make sure `key's is retained until here:
  (when (positive? (random 1))
    (display keys)))

;; Check that immutable hash trees aren't confused by an
;; "is a list" bit set in a key:
(let ()
  (define p (list 1 2 3 4))
  (define ht (hasheq p 1 'a 7 'b 10 'c 13))
  (test 1 hash-ref ht p #f)
  (list? p)
  (list? p)
  (list? (list* 1 2 p))
  (list? (list* 1 2 p))
  (list? (list* 1 2 p))
  (test 1 hash-ref ht p #f))

;; Check that hash-table cycles don't lead to an
;;  explosion in the time to compute a hash code.
(let ()
  (define ht (make-hash))
  (hash-set! ht 'a ht)
  (hash-set! ht 'b ht)
  (eq-hash-code ht)
  (equal-hash-code ht)
  (equal-secondary-hash-code ht)
  (equal-always-hash-code ht)
  (equal-always-secondary-hash-code ht))

;; Check that an equal hash code on an
;;  mutable, opaque structure does not
;;  see mutation.
(let ()
  (struct a (x [y #:mutable]))
  (define an-a (a 1 2))
  (define v (equal-hash-code an-a))
  (define v2 (equal-always-hash-code an-a))
  (set-a-y! an-a 8)
  (test v equal-hash-code an-a)
  (test v2 equal-always-hash-code an-a))

;; Check that an equal-always hash code on a
;;  mutable, transparent structure does not
;;  see mutation.
(let ()
  (struct a (x [y #:mutable]) #:transparent)
  (define an-a (a 1 2))
  (define v (equal-always-hash-code an-a))
  (set-a-y! an-a 8)
  (test v equal-always-hash-code an-a))

;; Check that `equal-hash-code` is consistent for interned symbols:
(let ()
  (define v (random))
  (define k (equal-hash-code (string->symbol (format "sym:~a" v))))
  (define k2 (equal-always-hash-code (string->symbol (format "sym:~a" v))))
  (collect-garbage 'minor)
  (test k equal-hash-code (string->symbol (format "sym:~a" v)))
  (test k2 equal-always-hash-code (string->symbol (format "sym:~a" v))))

;; Try to build a hash table whose indexes don't fit in 32 bits:
(let ()
  (struct a (x)
    #:property 
    prop:equal+hash
    (list
     (lambda (a b eql?) (eql? (a-x a) (a-x b)))
     (lambda (a hash) (expt 2 15))
     (lambda (b hash) 1)))
  

  (define (same-ish i) (a i))

  ;; No collisions: min depth 17, tree might be as
  ;; deep as 1.44 * 17 = 24
  (define ht (for/hash ([i (in-range (expt 2 17))])
               (values i i)))

  ;; All collissions: subtree min depth is 11, might
  ;; be as deep as 1.44*11 = 15
  (define ht2 (for/fold ([ht ht]) ([i (in-range (expt 2 11))])
                (hash-set ht (same-ish i) i)))

  ;; `ht2' depth is between 28 and 39

  ;; If the indexes go bad, this loop fails:
  (for ([(k v) (in-hash ht2)])
    v))

;; Check remove in the vicinity of a hash collision:
(let ()
  (struct a (x y)
          #:property prop:equal+hash
          (list
           (lambda (a b eql?) (and (equal? (a-x a)
                                      (a-x b))
                              (equal? (a-y a)
                                      (a-y b))))
           (lambda (a hc) (a-x a))
           (lambda (a hc) 1)))

  (define k (+ (arithmetic-shift 1 10) 1))
  (define k2 (+ (arithmetic-shift 1 15) 1))

  ;; The second hash here is intended to provoke a
  ;; collision in a subtable, and then remove an
  ;; element that causes the subtable, in which
  ;; case the collision should be moved up a layer.
  (equal? (hash (a 1 'a) 1
                (a 1 'b) 2
                (a 2 'c) 3)
          (hash-remove (hash (a 1 'a) 1
                             (a 1 'b) 2
                             (a 2 'c) 3
                             (a k 'd) 4)
                       (a k 'd)))

  ;; The second hash here is meanto to provoke
  ;; a similar shape as above, but where the
  ;; nested table is created to distinguish
  ;; hash keys instead of handle a collision,
  ;; and so it should not be moved up.
  (equal? (hash (a 1 'a) 1
                (a k2 'b) 2
                (a 2 'c) 3)
          (hash-remove (hash (a 1 'a) 1
                             (a k2 'b) 2
                             (a 2 'c) 3
                             (a k 'd) 4)
                       (a k 'd))))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Misc

(test #t string? (version))
(test #t string? (banner))
(test #t symbol? (system-type))
(test (system-type) system-type 'os)
(test #t string? (system-type 'machine))
(test #t symbol? (system-type 'link))
(test #t symbol? (system-type 'os*))
(test #t symbol? (system-type 'arch))
(test #t relative-path? (system-library-subpath))
(test #t relative-path? (system-library-subpath #f))

(test #t pair? (memv (system-type 'word) '(32 64)))
(test (fixnum? (expt 2 32)) = (system-type 'word) 64)

(test #t 'cmdline (let ([v (current-command-line-arguments)])
		    (and (vector? v)
			 (andmap string? (vector->list v)))))
(err/rt-test (current-command-line-arguments '("a")))
(err/rt-test (current-command-line-arguments #("a" 1)))

(arity-test version 0 0)
(arity-test banner 0 0)
(arity-test system-type 0 1)
(arity-test system-library-subpath 0 1)
(arity-test current-command-line-arguments 0 1)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; procedure-closure-contents-eq?

(for-each
 (lambda (jit?)
   (parameterize ([eval-jit-enabled jit?])
     (let ([f #f])
       (set! f (eval '(lambda (x) (lambda () x))))
       ((f 'c)) ; forced JIT compilation
       (test #t procedure-closure-contents-eq? (f 'a) (f 'a))
       (test #f procedure-closure-contents-eq? (f 'a) (f 'b))
       (set! f (eval '(case-lambda
		       [(x) (lambda () 12)]
		       [(x y) (lambda () (list x y))])))
       ((f 'c)) ; forces JIT compilation
       ((f 'c 'd)) ; forces JIT compilation
       (test #t procedure-closure-contents-eq? (f 'a) (f 'a))
       (test #t procedure-closure-contents-eq? (f 'a 'b) (f 'a 'b))
       (test #f procedure-closure-contents-eq? (f 'a 'b) (f 'c 'b)))))
 '(#t #f))
(test #t procedure-closure-contents-eq? add1 add1)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; procedure-reduce-arity
(let ([check-ok
       (lambda (proc ar inc not-inc)
         (for-each
          (lambda (proc)
            (let ([a (procedure-reduce-arity proc ar)])
              (test #t procedure? a)
              (test (normalize-arity ar) procedure-arity a)
              (map (lambda (i)
                     (test #t procedure-arity-includes? a i)
                     (when (i . < . 100)
                       (test i apply a (let loop ([i i])
                                         (if (zero? i) 
                                             null
                                             (cons 1 (loop (sub1 i))))))))
                   inc)
              (map (lambda (i)
                     (test #f procedure-arity-includes? a i)
                     (err/rt-test (procedure-reduce-arity a i))
                     (err/rt-test (procedure-reduce-arity a (make-arity-at-least i)))
                     (err/rt-test (procedure-reduce-arity a (list 0 i)))
                     (err/rt-test (procedure-reduce-arity a (list 0 (make-arity-at-least i))))
                     (err/rt-test (procedure-reduce-arity a (make-arity-at-least 0)))
                     (when (i . < . 100)
                       (err/rt-test (apply a (let loop ([i i])
                                               (if (zero? i) 
                                                   null
                                                   (cons 1 (loop (sub1 i))))))
                                    exn:fail:contract?)))
                   not-inc)))
          (list proc (procedure-reduce-arity proc ar))))]
      [representable-arity? (lambda (a)
                              (a . < . 4096))])
  (let ([check-all-but-one
         (lambda (+)
           (check-ok + 0 '(0) '(1))
           (check-ok + 2 '(2) '(0 1 3 4))
           (check-ok + 10 '(10) (filter representable-arity? (list 0 11 (expt 2 70))))
           (when (representable-arity? (expt 2 70))
             (check-ok + (expt 2 70) (list (expt 2 70)) (filter representable-arity? (list 0 10 (add1 (expt 2 70))))))
           (check-ok + (make-arity-at-least 2) (filter representable-arity? (list 2 5 (expt 2 70))) (list 0 1))
           (check-ok + (list 2 4) '(2 4) '(0 3))
           (check-ok + (list 2 4) '(4 2) '(0 3))
           (check-ok + (list 0 (make-arity-at-least 2)) (filter representable-arity? (list 0 2 5 (expt 2 70))) (list 1))
           (check-ok + (list 4 (make-arity-at-least 2)) '(2 3 4 10) '(0 1))
           (check-ok + (list 2 (make-arity-at-least 4)) '(2 4 10) '(0 1 3)))])
    (check-all-but-one +)
    (check-all-but-one (procedure-rename + 'plus))
    (check-all-but-one (lambda args (apply + args)))
    (check-all-but-one (procedure-rename (lambda args (apply + args)) 'PLUS))
    (check-all-but-one (case-lambda
                        [() 0]
                        [(a b . args) (apply + a b args)]))
    (check-all-but-one (case-lambda
                        [(b . args) (apply + b args)]
                        [() 0]))
    (check-all-but-one (case-lambda
                        [(a b c) (+ a b c)]
                        [(a b) (+ a b)]
                        [(a b c d) (+ a b c d)]
                        [() 0]
                        [(a b c d . e) (apply + a b c d e)]))
    (check-all-but-one (case-lambda
                        [(a b) (+ a b)]
                        [(a b c d) (+ a b c d)]
                        [(a b c) (+ a b c)]
                        [() 0]
                        [(a b c d . e) (apply + a b c d e)]))))

(test '+ object-name (procedure-reduce-arity + 3))
(test 'plus object-name (procedure-rename + 'plus))
(test 'again object-name (procedure-rename (procedure-rename + 'plus) 'again))
(test 'again object-name (procedure-rename (procedure-reduce-arity + 3) 'again))
(test 3 procedure-arity (procedure-rename (procedure-reduce-arity + 3) 'again))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(test #f equal?/recur 1 2 (lambda (a b) 'yes))
(test #t equal?/recur 1 1 (lambda (a b) 'yes))
(test #t equal?/recur '(1 . 2) '(1 . 2) (lambda (a b) 'yes))
(test #f equal?/recur '(1 . 2) '(1 . 2) (lambda (a b) (eq? a 1)))
(test #t equal?/recur '(1 . 1) '(1 . 2) (lambda (a b) (or (eq? a b) (eq? a 1))))

(test #t equal?/recur '#(1 2 3) '#(1 2 3) (lambda (a b) 'yes))
(test #f equal?/recur '#(1 2 3) '#(1 2 3) (lambda (a b) (not (eqv? a 2))))

(test #t equal?/recur '#&1 '#&1 (lambda (a b) 'yes))
(test #f equal?/recur '#&1 '#&1 (lambda (a b) #f))

(test #t equal?/recur '#hash((1 . x)) '#hash((1 . x)) (lambda (a b) 'yes))
(test #t equal?/recur '#hash((1 . x)) '#hash((1 . z)) (lambda (a b) (or (eq? a b) (eq? 'z b))))
(test #f equal?/recur '#hash(("2" . x)) (hash (string-copy "2") 'x) (lambda (a b) (eq? a b)))
(test #t equal?/recur '#hash(("2" . x)) (hash (string-copy "2") 'x) (lambda (a b) (or (eq? a b) (eq? "2" a))))
(test #f equal?/recur '#hash((1 . x)) '#hash((1 . x)) (lambda (a b) #f))
(test #f equal?/recur '#hash((1 . x)) '#hash((1 . x)) (lambda (a b) (eq? a 1)))
(test #f equal?/recur '#hash((1 . x)) '#hash((1 . x)) (lambda (a b) (eq? a 'x)))

(let ()
  (struct a (x) #:transparent)
  (test #t equal?/recur (a 1) (a 2) (lambda (a b) 'yes))
  (test #f equal?/recur (a 1) (a 1) (lambda (a b) (not (eq? a 1)))))

(let ()
  (struct s (x) #:property prop:procedure 0)
  (test #t equal?/recur (cons 1 2) (cons 1 2) (s (lambda (x y) #t)))
  (test #t equal?/recur (vector 1) (vector 1) (s (lambda (x y) #t)))
  (test #t equal?/recur (box 0) (box 0) (s (lambda (x y) #t)))
  (test #t equal?/recur (mcons 3 4) (mcons 3 4) (s (lambda (x y) #t))))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(test #t immutable-string? "apple")
(test #f mutable-string? "apple")
(test #f immutable-string? (string-copy "apple"))
(test #t mutable-string? (string-copy "apple"))
(test #f immutable-string? (void))
(test #f mutable-string? (void))

(test #t immutable-bytes? #"apple")
(test #f mutable-bytes? #"apple")
(test #f immutable-bytes? (bytes-copy #"apple"))
(test #t mutable-bytes? (bytes-copy #"apple"))
(test #f immutable-bytes? (void))
(test #f mutable-bytes? (void))

(test #t immutable-vector? #(1 2 3))
(test #f mutable-vector? #(1 2 3))
(test #f immutable-vector? (vector 1 2 3))
(test #t mutable-vector? (vector 1 2 3))
(test #f immutable-vector? (void))
(test #f mutable-vector? (void))

(test #t immutable-box? #&1)
(test #f mutable-box? #&1)
(test #f immutable-box? (box 1))
(test #t mutable-box? (box 1))
(test #f immutable-box? (void))
(test #f mutable-box? (void))

(test #t immutable-hash? #hash((1 . 2)))
(test #t immutable-hash? #hasheqv((1 . 2)))
(test #t immutable-hash? #hasheq((1 . 2)))
(test #t immutable-hash? #hashalw((1 . 2)))
(test #f mutable-hash? #hash((1 . 2)))
(test #f mutable-hash? #hasheqv((1 . 2)))
(test #f mutable-hash? #hasheq((1 . 2)))
(test #f mutable-hash? #hashalw((1 . 2)))
(test #f immutable-hash? (make-hash '((1 . 2))))
(test #f immutable-hash? (make-hasheqv '((1 . 2))))
(test #f immutable-hash? (make-hasheq '((1 . 2))))
(test #f immutable-hash? (make-hashalw '((1 . 2))))
(test #f immutable-hash? (make-weak-hash '((1 . 2))))
(test #f immutable-hash? (make-weak-hasheqv '((1 . 2))))
(test #f immutable-hash? (make-weak-hasheq '((1 . 2))))
(test #f immutable-hash? (make-weak-hashalw '((1 . 2))))
(test #f immutable-hash? (make-ephemeron-hash '((1 . 2))))
(test #f immutable-hash? (make-ephemeron-hasheqv '((1 . 2))))
(test #f immutable-hash? (make-ephemeron-hasheq '((1 . 2))))
(test #f immutable-hash? (make-ephemeron-hashalw '((1 . 2))))
(test #t mutable-hash? (make-hash '((1 . 2))))
(test #t mutable-hash? (make-hasheqv '((1 . 2))))
(test #t mutable-hash? (make-hasheq '((1 . 2))))
(test #t mutable-hash? (make-hashalw '((1 . 2))))
(test #t mutable-hash? (make-weak-hash '((1 . 2))))
(test #t mutable-hash? (make-weak-hasheqv '((1 . 2))))
(test #t mutable-hash? (make-weak-hasheq '((1 . 2))))
(test #t mutable-hash? (make-weak-hashalw '((1 . 2))))
(test #t mutable-hash? (make-ephemeron-hash '((1 . 2))))
(test #t mutable-hash? (make-ephemeron-hasheqv '((1 . 2))))
(test #t mutable-hash? (make-ephemeron-hasheq '((1 . 2))))
(test #t mutable-hash? (make-ephemeron-hashalw '((1 . 2))))
(test #f immutable-hash? (void))
(test #f mutable-hash? (void))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(report-errs)

"last item in file"
