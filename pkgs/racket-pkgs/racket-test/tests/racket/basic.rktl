
(load-relative "loadtest.rktl")

(Section 'basic)

(require racket/flonum
         racket/function)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(test '() 'null null)
(test '() 'null '())

(let ([f (lambda () #&7)])
  (test #t eq? (f) (f)))

;; test that all symbol characters are supported.
'(+ - ... !.. $.+ %.- &.! *.: /:. :+. <-. =. >. ?. ~. _. ^.)

(define disjoint-type-functions
  (list boolean? char? null? number? pair? procedure? string? symbol? vector?))
(define type-examples
  (list
   #t #f #\a '() 9739 '(test) record-error "test" "" 'test '#() '#(a b c) ))
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

(test #f not #t)
(test #f not 3)
(test #f not (list 3))
(test #t not #f)
(test #f not '())
(test #f not (list))
(test #f not 'nil)
(arity-test not 1 1)

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

(arity-test eq? 2 2)
(arity-test eqv? 2 2)
(arity-test equal? 2 2)

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

(test #t pair? '(a . b))
(test #t pair? '(a . 1))
(test #t pair? '(a b c))
(test #f pair? '())
(test #f pair? '#(a b))
(arity-test pair? 1 1)

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
(when (eq? '3m (system-type 'gc))
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
(err/rt-test (list-ref 1 1) exn:application:mismatch?)
(err/rt-test (list-ref '(a b . c) 2) exn:application:mismatch?)
(err/rt-test (list-ref '(1 2 3) 2.0))
(err/rt-test (list-ref '(1) '(1)))
(err/rt-test (list-ref '(1) 1) exn:application:mismatch?)
(err/rt-test (list-ref '() 0) exn:application:mismatch?)
(err/rt-test (list-ref '() 0) exn:application:mismatch?)
(err/rt-test (list-ref '(1) -1))
(err/rt-test (list-ref '(1) 2000000000000) exn:application:mismatch?)

(test '(c d) list-tail '(a b c d) 2)
(test '(a b c d) list-tail '(a b c d) 0)
(test '(b c . d) list-tail '(a b c . d) 1)
(test 1 list-tail 1 0)
(arity-test list-tail 2 2)
(err/rt-test (list-tail 1 1) exn:application:mismatch?)
(err/rt-test (list-tail '(1 2 3) 2.0))
(err/rt-test (list-tail '(1) '(1)))
(err/rt-test (list-tail '(1) -1))
(err/rt-test (list-tail '(1) 2) exn:application:mismatch?)
(err/rt-test (list-tail '(1 2 . 3) 3) exn:application:mismatch?)

(define (test-mem memq memq-name)
  (test '(a b c) memq 'a '(a b c))
  (test '(b c) memq 'b '(a b c))
  (test '(b . c) memq 'b '(a b . c))
  (test '#f memq 'a '(b c d))

  (if (eq? memq-name 'member)
      (arity-test memq 2 3)
      (arity-test memq 2 2))
  (err/rt-test (memq 'a 1) exn:application:mismatch?)
  (err/rt-test (memq 'a '(1 . 2)) exn:application:mismatch?))

(test-mem memq 'memq)
(test-mem memv 'memv)
(test-mem member 'member)

(test '("apple") memq "apple" '("apple")) ; literals are interned
(test '(#"apple") memq #"apple" '(#"apple")) ; literals are interned
(test #f memq (list->string (string->list "apple")) '("apple"))
(test #f memq (list->bytes (bytes->list #"apple")) '(#"apple"))
(test #f memv (list->string (string->list "apple")) '("apple"))
(test '("apple") member "apple" '("apple"))

; (test #f memq 1/2 '(1/2)) ; rationals are immutable and we may want to optimize
(test '(1/2) memv 1/2 '(1/2))
(test '(1/2) member 1/2 '(1/2))

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
(test-ass assoc 'assoc)

(test #f assq '(a) '(((a)) ((b)) ((c))))
(test #f assv '(a) '(((a)) ((b)) ((c))))
(test '((b) 1) assoc '(b) '(((a)) ((b) 1) ((c))))

; (test #f assq '1/2 '(((a)) (1/2) ((c)))) ; rationals are immutable and we may want to optimize
(test '(1/2) assv '1/2 '(((a)) (1/2) ((c))))
(test '(1/2) assoc '1/2 '(((a)) (1/2) ((c))))

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
(test #f immutable? (make-hasheq))
(test #f immutable? (make-hasheqv))
(test #f immutable? (make-hash))
(test #f immutable? (make-weak-hasheq))
(test #f immutable? (make-weak-hash))

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
(test #t keyword<? '#:a '#:b)
(test #f keyword<? '#:b '#:b)
(test #t keyword<? '#:b '#:bb)
(test #f keyword<? '#:b '#:)
(test #t keyword<? (string->keyword "a") (string->keyword "\uA0"))
(test #t keyword<? (string->keyword "a") (string->keyword "\uFF"))
(test #f keyword<? (string->keyword "\uA0") (string->keyword "a"))
(test #f keyword<? (string->keyword "\uFF") (string->keyword "a"))
(test #t keyword<? (string->keyword "\uA0") (string->keyword "\uFF"))
(test #f keyword<? (string->keyword "\uFF") (string->keyword "\uA0"))
(test #f keyword<? (string->keyword "\uA0") (string->keyword "\uA0"))

(arity-test keyword? 1 1)
(arity-test keyword<? 2 -1)

(define (char-tests)
  (test #t eqv? '#\  #\Space)
  (test #t eqv? #\space '#\Space)
  (test #t char? #\a)
  (test #t char? #\()
  (test #t char? #\ )
  (test #t char? '#\newline)
  (arity-test char? 1 1)

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
  (arity-test char=? 2 -1)
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
  (arity-test char<? 2 -1)
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
  (arity-test char>? 2 -1)
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
  (arity-test char<=? 2 -1)
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
  (arity-test char>=? 2 -1)
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
  (arity-test char-ci=? 2 -1)
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
  (arity-test char-ci<? 2 -1)
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
  (arity-test char-ci>? 2 -1)
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
  (arity-test char-ci<=? 2 -1)
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
  (arity-test char-ci>=? 2 -1)
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
(arity-test integer->char 1 1)
(arity-test char->integer 1 1)
(err/rt-test (integer->char 5.0))
(err/rt-test (integer->char 'a))
(err/rt-test (integer->char -1))
(err/rt-test (integer->char (expt 2 32)))
(err/rt-test (integer->char 10000000000000000))
(err/rt-test (char->integer 5))

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

(test #t string? "The word \"recursion\\\" has many meanings.")
(test #t string? "")
(arity-test string? 1 1)
(test 3 'make-string (string-length (make-string 3)))
(test "" make-string 0)
(arity-test make-string 1 2)
(err/rt-test (make-string "hello"))
(err/rt-test (make-string 5 "hello"))
(err/rt-test (make-string 5.0 #\b))
(err/rt-test (make-string 5.2 #\a))
(err/rt-test (make-string -5 #\f))
(define 64-bit-machine? (eq? (expt 2 40) (eq-hash-code (expt 2 40))))
(unless 64-bit-machine?
  (err/rt-test (make-string 500000000000000 #\f) exn:fail:out-of-memory?)) ;; bignum on 32-bit machines
(err/rt-test (make-string 50000000000000000000 #\f) exn:fail:out-of-memory?)  ;; bignum on 64-bit machines

(unless 64-bit-machine?
  (err/rt-test (make-vector 1234567890 #\f) exn:fail:out-of-memory?)
  (err/rt-test (read (open-input-string "#1234567890(0)")) exn:fail:out-of-memory?))
(test #t vector? (make-vector 0))

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
(err/rt-test (string-ref "" 0) exn:application:mismatch?)
(err/rt-test (string-ref "" (expt 2 100)) exn:application:mismatch?)
(err/rt-test (string-ref "apple" -1))
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

  (test #t string<? "A" "B")
  (test #t string<? "a" "b")
  (test #f string<? "9" "0")
  (test #f string<? "A" "A")
  (test #t string<? "A" "AB")
  (test #f string<? "AB" "A")
  (test #f string<? ax ax2)
  (test #t string<? ax ay)
  (test #f string<? ay ax)

  (test #f string>? "A" "B")
  (test #f string>? "a" "b")
  (test #t string>? "9" "0")
  (test #f string>? "A" "A")
  (test #f string>? "A" "AB")
  (test #t string>? "AB" "A")
  (test #f string>? ax ax2)
  (test #f string>? ax ay)
  (test #t string>? ay ax)

  (test #t string<=? "A" "B")
  (test #t string<=? "a" "b")
  (test #f string<=? "9" "0")
  (test #t string<=? "A" "A")
  (test #t string<=? "A" "AB")
  (test #f string<=? "AB" "A")
  (test #t string<=? ax ax2)
  (test #t string<=? ax ay)
  (test #f string<=? ay ax)

  (test #f string>=? "A" "B")
  (test #f string>=? "a" "b")
  (test #t string>=? "9" "0")
  (test #t string>=? "A" "A")
  (test #f string>=? "A" "AB")
  (test #t string>=? "AB" "A")
  (test #t string>=? ax ax2)
  (test #f string>=? ax ay)
  (test #t string>=? ay ax)

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
       (arity-test pred 2 -1)
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
(err/rt-test (make-bytes #"hello"))
(err/rt-test (make-bytes 5 #"hello"))
(err/rt-test (make-bytes 5.0 98))
(err/rt-test (make-bytes 5.2 97))
(err/rt-test (make-bytes -5 98))
(err/rt-test (make-bytes 50000000000000000000 #\f))
(unless 64-bit-machine?
  (err/rt-test (make-bytes 500000000000000 45) exn:fail:out-of-memory?)) ;; bignum on 32-bit machines
(err/rt-test (make-bytes 50000000000000000000 45) exn:fail:out-of-memory?)  ;; bignum on 64-bit machines


(define f (make-bytes 3 (char->integer #\*)))
(test #"?**" 'bytes-set! (begin (bytes-set! f 0 (char->integer #\?)) f))
(arity-test bytes-set! 3 3)
(err/rt-test (bytes-set! #"hello" 0 #\a)) ; immutable bytes constant
(define hello-bytes (bytes-copy #"hello"))
(err/rt-test (bytes-set! hello-bytes 'a 97))
(err/rt-test (bytes-set! 'hello 4 97))
(err/rt-test (bytes-set! hello-bytes 4 'a))
(err/rt-test (bytes-set! hello-bytes 4.0 'a))
(err/rt-test (bytes-set! hello-bytes 5 97) exn:application:mismatch?)
(err/rt-test (bytes-set! hello-bytes -1 97))
(err/rt-test (bytes-set! hello-bytes (expt 2 100) 97) exn:application:mismatch?)
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
(err/rt-test (bytes-ref #"" 0) exn:application:mismatch?)
(err/rt-test (bytes-ref #"" (expt 2 100)) exn:application:mismatch?)
(err/rt-test (bytes-ref #"apple" -1))
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
(arity-test bytes-copy 1 1)
(arity-test bytes-fill! 2 2)
(err/rt-test (bytes-copy 'blah))
(err/rt-test (bytes-fill! 'sym 1))
(err/rt-test (bytes-fill! #"static" 1))
(err/rt-test (bytes-fill! (bytes-copy #"oops") #\5))

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
(for ([succeed? '(#f #t)])
  (for ([N '(1 100 1000 1023 1024 10000)])
    (for ([M (list 0 (quotient N 2))])
      (define o (open-output-bytes))
      (void (regexp-match-positions #rx"y" 
                                    (string-append
                                     (make-string N #\x) 
                                     (if succeed? "y" ""))
                                    M
                                    (+ N (if succeed? 1 0))
                                    o))
    (test (- N M) bytes-length (get-output-bytes o)))))

(arity-test regexp 1 1)
(arity-test regexp? 1 1)
(arity-test regexp-match 2 6)
(arity-test regexp-match-positions 2 6)
(arity-test regexp-match-peek 2 6)
(arity-test regexp-match-peek-positions 2 6)
(arity-test regexp-replace 3 4)
(arity-test regexp-replace* 3 6)

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

(test '(b e h) map cadr '((a b) (d e) (g h)))
(test '(5 7 9) map + '(1 2 3) '(4 5 6))
(test '#(0 1 4 9 16) 'for-each
	(let ((v (make-vector 5)))
		(for-each (lambda (i) (vector-set! v i (* i i)))
			'(0 1 2 3 4))
		v))


(define (map-tests map)
  (let ([size? exn:application:mismatch?]
	[non-list? type?])
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
    (err/rt-test (map (lambda (x) 10) '(1 2) '(3 4)) exn:application:mismatch?)))
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
(test '((10)) 'exns
      (with-handlers ([void (lambda (x) (list x))])
        (with-handlers ([integer? (lambda (x) (raise (list x)))])
          (raise 10))))
(test '((10)) 'exns
      (let/ec esc
        (parameterize ([uncaught-exception-handler (lambda (x) (esc (list x)))])
          (with-handlers ([integer? (lambda (x) (raise (list x)))])
            (raise 10)))))
(test '#((10)) 'exns
      (let/ec esc
        (with-handlers ([void (lambda (x) (vector x))])
          (parameterize ([uncaught-exception-handler (lambda (x) (esc (list x)))])
            (with-handlers ([integer? (lambda (x) (raise (list x)))])
              (raise 10))))))

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
(test (make-arity-at-least 2) procedure-arity >)
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


(test-cont)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; hash tables

(arity-test make-hash 0 1)
(arity-test make-hasheq 0 1)
(arity-test make-hasheqv 0 1)
(arity-test make-weak-hash 0 1)
(arity-test make-weak-hasheq 0 1)
(arity-test make-weak-hasheqv 0 1)

(define (hash-tests make-hash make-hasheq make-hasheqv
                    make-weak-hash make-weak-hasheq make-weak-hasheqv
                    hash-ref hash-set! hash-ref! hash-update! hash-has-key?
                    hash-remove! hash-count
                    hash-map hash-for-each
                    hash-iterate-first hash-iterate-next
                    hash-iterate-value hash-iterate-key
                    hash-copy
                    hash-clear! hash-clear
                    hash-empty?)
  (define-struct ax (b c)) ; opaque
  (define-struct a (b c) #:inspector (make-inspector))

  (define save
    (let ([x null]) (case-lambda [() x] [(a) (set! x (cons a x)) a])))
  (define an-ax (make-ax 1 2))

  (define (check-hash-tables weak? reorder?)
    (let ([h1 (if weak? (make-weak-hasheq) (make-hasheq))]
          [l (list 1 2 3)])
      (test #t eq? (eq-hash-code l) (eq-hash-code l))
      (test #t eq? (eqv-hash-code l) (eqv-hash-code l))
      (test #t eq? (equal-hash-code l) (equal-hash-code l))
      (test #t eq? (equal-hash-code l) (equal-hash-code (list 1 2 3)))
      (hash-set! h1 l 'ok)
      (test 'ok hash-ref h1 l)
      (err/rt-test (hash-ref h1 'nonesuch (lambda (x) 'bad-proc)) exn:fail:contract:arity?)
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
      (hash-remove! h1 l)
      (test 'nope hash-ref h1 l (lambda () 'nope))
      (err/rt-test (hash-update! h1 l add1))
      (hash-update! h1 l add1 0)
      (test 1 hash-ref h1 l)
      (hash-remove! h1 l))

    (let ([h1 (if weak? (make-weak-hasheqv) (make-hasheqv))]
          [n (expt 2 500)]
          [q (/ 1 2)]
          [s (make-string 2 #\q)])
      (hash-set! h1 n 'power)
      (hash-set! h1 q 'half)
      (hash-set! h1 s 'string)
      (test 'power hash-ref h1 (expt (read (open-input-string "2")) 500))
      (test 'half hash-ref h1 (/ 1 (read (open-input-string "2"))))
      (test #f hash-ref h1 (make-string (read (open-input-string "2")) #\q) #f))

    (let ([h1 (if weak? (make-weak-hash) (make-hash))]
          [l (list 1 2 3)]
          [v (vector 5 6 7)]
          [a (make-a 1 (make-a 2 3))]
          [b (box (list 1 2 3))]
          [fl (flvector 1.0 +nan.0 0.0)])

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
                     (hash-set! h1 (save fl) 'flvector))]
            [puts2 (lambda ()
                     (hash-set! h1 (save (list 5 7)) 'another-list)
                     (hash-set! h1 (save 3+0.0i) 'izi-complex)
                     (hash-set! h1 (save v) 'vector)
                     (hash-set! h1 (save a) 'struct)
                     (hash-set! h1 (save an-ax) 'structx)
                     (hash-set! h1 (save b) 'box))])
        (if reorder?
          (begin
            (puts2)
            (test 6 hash-count h1)
            (puts1))
          (begin
            (puts1)
            (test 8 hash-count h1)
            (puts2))))

      (when reorder?
        ;; Add 1000 things and take them back out in an effort to
        ;; trigger GCs that somehow affect hashing:
        (let loop ([i 0.0])
          (unless (= i 1000.0)
            (hash-set! h1 i #t)
            (loop (add1 i))
            (hash-remove! h1 i))))

      (test 14 hash-count h1)
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
              (,(flvector 1.0 +nan.0 0.0) . flvector)))
      (hash-remove! h1 (list 1 2 3))
      (test 13 hash-count h1)
      (test 'not-there hash-ref h1 l (lambda () 'not-there))
      (let ([c 0])
        (hash-for-each h1 (lambda (k v) (set! c (add1 c))))
        (test 13 'count c))
      ;; return the hash table:
      h1))

  (define (check-tables-equal mode t1 t2 weak?)
    (test #t equal? t1 t2)
    (test (equal-hash-code t1) equal-hash-code t2)
    (test #t equal? t1 (hash-copy t1))
    (let ([again (if weak? (make-weak-hash) (make-hash))])
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
                        (check-hash-tables #t #f)
                        (check-hash-tables #t #t)
                        #t))
  
  ;; Make sure copy doesn't share:
  (for ([make-hash (list make-hash
                         make-weak-hash)])
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
                         make-weak-hash)])
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

(hash-tests make-hash make-hasheq make-hasheqv
            make-weak-hash make-weak-hasheq make-weak-hasheqv
            hash-ref hash-set! hash-ref! hash-update! hash-has-key?
            hash-remove! hash-count
            hash-map hash-for-each
            hash-iterate-first hash-iterate-next
            hash-iterate-value hash-iterate-key
            hash-copy
            hash-clear! hash-clear
            hash-empty?)
(let ([ub-wrap (lambda (proc)
                 (lambda (ht . args)
                   (apply proc (unbox ht) args)))])
  (hash-tests (lambda () (box #hash()))
              (lambda () (box #hasheq()))
              (lambda () (box #hasheqv()))
              #f #f #f
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
              (ub-wrap hash-empty?)))

(test #f hash? 5)
(test #t hash? (make-hasheq))
(test #t hash? (make-hasheqv))
(test #t hash-eq? (make-hasheq))
(test #f hash-eq? (make-hash))
(test #f hash-eq? (make-hasheqv))
(test #t hash-eq? (make-weak-hasheq))
(test #f hash-eq? (make-weak-hash))
(test #f hash-eq? (make-weak-hasheqv))
(test #f hash-eqv? (make-hasheq))
(test #f hash-eqv? (make-hash))
(test #t hash-eqv? (make-hasheqv))
(test #f hash-eqv? (make-weak-hasheq))
(test #f hash-eqv? (make-weak-hash))
(test #t hash-eqv? (make-weak-hasheqv))
(test #f hash-weak? (make-hasheq))
(test #f hash-weak? (make-hash))
(test #f hash-weak? (make-hasheqv))
(test #t hash-weak? (make-weak-hasheq))
(test #t hash-weak? (make-weak-hash))
(test #t hash-weak? (make-weak-hasheqv))

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
(err/rt-test (hash-weak? 5))

(let ([a (expt 2 500)]
      [b (expt (read (open-input-string "2")) 500)])
  (test #t equal? (eqv-hash-code a) (eqv-hash-code b))
  (test #t equal? (equal-hash-code a) (equal-hash-code b)))

;; Check for proper clearing of weak hash tables
;; (internally, value should get cleared along with key):
(let ([ht (make-weak-hasheq)])
  (let loop ([n 10])
    (unless (zero? n)
      (hash-set! ht (make-string 10) #f)
      (loop (sub1 n))))
  (collect-garbage)
  (map (lambda (i) (format "~a" i)) (hash-map ht cons)))

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
  (test-del-eq make-weak-hash))

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
  (test #f equal? (mk make-hash) (mk make-hasheq))
  (test #f equal? (mk make-hash) (mk make-hasheqv))
  (test #f equal? (mk make-hasheq) (mk make-hasheqv))
  (test #f equal? (mk make-hash) (mk make-weak-hash))
  (test #f equal? (mk make-hasheq) (mk make-weak-hasheq))
  (test #f equal? (mk make-hasheqv) (mk make-weak-hasheqv)))
(let ([mk (lambda (mk)
            (mk `((1 . 2))))])
  (test #t equal? (mk make-immutable-hash) (mk make-immutable-hash))
  (test #t equal? (mk make-immutable-hasheq) (mk make-immutable-hasheq))
  (test #t equal? (mk make-immutable-hasheqv) (mk make-immutable-hasheqv))
  (test #f equal? (mk make-immutable-hash) (mk make-immutable-hasheq))
  (test #f equal? (mk make-immutable-hash) (mk make-immutable-hasheqv))
  (test #f equal? (mk make-immutable-hasheq) (mk make-immutable-hasheqv)))

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
(err/rt-test (hash-iterate-next (make-hasheq) 0))
(err/rt-test (hash-iterate-next (make-weak-hasheq) 0))

(let ([check-all-bad
       (lambda (op)
         (err/rt-test (op #f 0))
         (err/rt-test (op (make-hasheq) -1))
         (err/rt-test (op (make-hasheq) (- (expt 2 100))))
         (err/rt-test (op (make-hasheq) 1.0)))])
  (check-all-bad hash-iterate-next)
  (check-all-bad hash-iterate-key)
  (check-all-bad hash-iterate-value))

(test (list 1 2 3) sort (hash-keys #hasheq((1 . a) (2 . b) (3 . c))) <)
(test (list 'a 'b 'c) 
      sort (hash-values #hasheq((1 . a) (2 . b) (3 . c))) string<? #:key symbol->string)
(test (list (cons 1 'a) (cons 2 'b) (cons 3 'c)) 
      sort (hash->list #hasheq((1 . a) (2 . b) (3 . c))) < #:key car)

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
(arity-test hash-keys 1 1)
(arity-test hash-values 1 1)
(arity-test hash-count 1 1)
(arity-test hash-ref 2 3)
(arity-test hash-set! 3 3)
(arity-test hash-set 3 3)
(arity-test hash-remove! 2 2)
(arity-test hash-remove 2 2)
(arity-test hash-map 2 2)
(arity-test hash-for-each 2 2)
(arity-test hash? 1 1)
(arity-test hash-eq? 1 1)
(arity-test hash-weak? 1 1)

;; Ensure that hash-table hashing is not sensitive to the
;; order of key+value additions
(let ()
  (define ht (make-hash))
  (define ht2 (make-hash))
  (define wht (make-weak-hash))
  (define wht2 (make-weak-hash))
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
  
  (test (equal-hash-code ht) values (equal-hash-code ht2))
  (test (equal-hash-code wht) values (equal-hash-code wht2))
  (test (equal-secondary-hash-code ht) values (equal-secondary-hash-code ht2))
  (test (equal-secondary-hash-code wht) values (equal-secondary-hash-code wht2))

  (let ([ht (for/hash ([i (in-list l)])
              (values (a i) (a (a i))))]
        [ht2 (for/hash ([i (in-list l2)])
               (values (a i) (a (a i))))])
    (test (equal-hash-code ht) values (equal-hash-code ht2))
    (test (equal-secondary-hash-code ht) values (equal-secondary-hash-code ht2)))

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
  (equal-secondary-hash-code ht))

;; Check that an equal hash code on an
;;  mutable, opaque structure does not
;;  see mutation.
(let ()
  (struct a (x [y #:mutable]))
  (define an-a (a 1 2))
  (define v (equal-hash-code an-a))
  (set-a-y! an-a 8)
  (test v equal-hash-code an-a))


;; Try to build a hash table whose indexes fonr't fit in 32 bits:
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

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Misc

(test #t string? (version))
(test #t string? (banner))
(test #t symbol? (system-type))
(test (system-type) system-type 'os)
(test #t string? (system-type 'machine))
(test #t symbol? (system-type 'link))
(test #t relative-path? (system-library-subpath))

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
          (list proc (procedure-reduce-arity proc ar))))])
  (let ([check-all-but-one
         (lambda (+)
           (check-ok + 0 '(0) '(1))
           (check-ok + 2 '(2) '(0 1 3 4))
           (check-ok + 10 '(10) (list 0 11 (expt 2 70)))
           (check-ok + (expt 2 70) (list (expt 2 70)) (list 0 10  (add1 (expt 2 70))))
           (check-ok + (make-arity-at-least 2) (list 2 5 (expt 2 70)) (list 0 1))
           (check-ok + (list 2 4) '(2 4) '(0 3))
           (check-ok + (list 2 4) '(4 2) '(0 3))
           (check-ok + (list 0 (make-arity-at-least 2)) (list 0 2 5 (expt 2 70)) (list 1))
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

(report-errs)

"last item in file"
