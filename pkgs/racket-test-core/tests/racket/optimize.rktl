
(load-relative "loadtest.rktl")

(Section 'optimization)

(require racket/flonum
         racket/extflonum
         racket/fixnum
         racket/unsafe/undefined
         racket/unsafe/ops
         compiler/zo-parse
         compiler/zo-marshal
         (prefix-in k: '#%kernel))
         ;; Some primitives like `random` are shadowed by Racket functions in
         ;; `racket/base` and other modules. Using the primitive makes the
         ;; compilation more predictable and removes the reference to the
         ;; external modules in the functions.

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (comp=? c1 c2 want-same?)
  (let ([s1 (open-output-bytes)]
	[s2 (open-output-bytes)])
    (write c1 s1)
    (write c2 s2)
    (let ([t1 (get-output-bytes s1)]
	  [t2 (get-output-bytes s2)])
      (define same? (bytes=? t1 t2))
      (when (and (not same?) want-same?)
        (pretty-write (zo-parse (open-input-bytes t1)))
        (pretty-write (zo-parse (open-input-bytes t2))))
      (unless (equal? same? want-same?)
        ;; Unquote to cause a failure to stop
        'stop)
      same?)))

(define test-comp
  (case-lambda
   [(expr1 expr2) (test-comp expr1 expr2 #t)]
   [(expr1 expr2 same?)
    (define (->stx s)
      ;; Give `s` a minimal location, so that other macro locations
      ;; don't bleed through:
      (datum->syntax #f s (vector 'here #f #f #f #f)))
    (test same? `(compile ,same? (,expr1 => ,expr2)) (comp=? (compile (->stx expr1)) (compile (->stx expr2)) same?))]))

(let ([x (compile '(lambda (x) x))])
  (test #t 'fixpt (eq? x (compile x))))

(test-comp 5 '(if #t 5 (cons 1 2)))
(test-comp 5 '(if #f (cons 1 2) 5))

(test-comp 5 '(begin0 5 'hi "apple" 1.5))
(test-comp 5 '(begin0 5 (begin0 'hi "apple" 1.5)))
(test-comp 5 '(begin0 5 (begin0 'hi "apple") 1.5))
(test-comp 5 '(begin0 5 (begin 'hi "apple" 1.5)))
(test-comp 5 '(begin0 5 (begin 'hi "apple") 1.5))
(test-comp 5 '(begin0 (begin0 5 'hi "apple" 1.5)))
(test-comp 5 '(begin0 (begin0 5 'hi "apple") 1.5))

; Can't drop `begin0' if the first expression may change a continuation marks:
(test-comp '(lambda () 3)
           '(lambda () (begin0 (begin0 (+ 1 2) 'hi "apple") 1.5)))
(test-comp '(lambda () (let ([sum +]) (begin0 (begin0 (+ 1 2) 'hi "apple") 1.5)))
           '(lambda () (let ([sum +]) (begin0 (begin0 (sum 1 2) 'hi "apple") 1.5))))
(test-comp '(lambda (f) (begin0 (begin0 (f 1 2) #f) #f))
           '(lambda (f) (begin0 (begin0 (f 1 2) 'hi "apple") 1.5)))
(test-comp '(lambda (f) (f 1 2))
           '(lambda (f) (begin0 (begin0 (f 1 2) 'hi "apple") 1.5))
           #f)

(test-comp 5 '(begin 'hi "apple" 1.5 5))
(test-comp 5 '(begin (begin 'hi "apple" 1.5) 5))
(test-comp 5 '(begin (begin 'hi "apple") 1.5 5))
(test-comp 5 '(begin (begin0 'hi "apple" 1.5) 5))
(test-comp 5 '(begin (begin0 'hi "apple") 1.5 5))
(test-comp 5 '(begin (begin 'hi "apple" 1.5 5)))
(test-comp 5 '(begin 'hi (begin "apple" 1.5 5)))

(test-comp '(lambda () (begin (random) 5))
           '(lambda () (begin0 5 (random))))
(test-comp '(lambda () (begin (read) 5))
           '(lambda () (begin0 5 (read))))
(test-comp '(lambda () (begin (random) (cons 1 2)))
           '(lambda () (begin0 (cons 1 2) (random))))
(test-comp '(lambda () (begin (read) (cons 1 2)))
           '(lambda () (begin0 (cons 1 2) (read)))
           #f)

(test-comp '(lambda () (random))
           '(lambda () (begin0 (random) #f)))
(test-comp '(lambda (f) (f))
           '(lambda (f) (begin0 (f) #f))
           #f)

(test-comp '(lambda (f) (begin (random) (begin0 (f) 7)))
           '(lambda (f) (begin0 (begin (random) (f)) 7)))
(test-comp '(lambda () (begin (random) (random) (cons 1 2)))
           '(lambda () (begin0 (begin (random) (cons 1 2)) (random))))



(test-comp '(let ([x 8][y 9]) (lambda () x))
	   '(let ([x 8][y 9]) (lambda () (if #f y x))))
(test-comp '(let ([x 8][y 9]) (lambda () (+ x y)))
	   '(let ([x 8][y 9]) (lambda () (if #f y (+ x y)))))

;; Don't optimize away use before definition:
(test-comp '(letrec ([x (begin x 5)]) x) '5 #f)
(test-comp '(letrec ([x (letrec ([y 5]) x 6)]) x) '6 #f)

(test-comp '(let ([x 5]) (set! x 2)) '(let ([x 5]) (set! x x) (set! x 2)))

(test-comp '(let* () (f 5))
	   '(f 5))
(test-comp '(letrec () (f 5))
	   '(f 5))
(test-comp '(with-handlers () (f 5))
	   '(f 5))
(test-comp '(parameterize () (f 5))
	   '(f 5))

(test-comp '(let ([i (cons 0 1)]) (let ([j i]) j))
	   '(let ([i (cons 0 1)]) i))

(define (normalize-depth s)
  `(let ([a ,s]
	 [b (let-values ([(a b c d e f) (values 1 2 3 4 5 6)])
	      (list a b c d e f))])
     10))

;; We use nonsense `display' and `write' where we used to use `cons' and
;; `list', because the old ones now get optimized away:
(test-comp (normalize-depth '(let* ([i (display 0 1)][j i]) j))
	   (normalize-depth '(let* ([i (display 0 1)]) i)))

(test-comp (normalize-depth '(let* ([i (display 0 1)][j (write 2)][k (write 3)][g i]) g))
	   (normalize-depth '(let* ([i (display 0 1)][j (write 2)][k (write 3)]) i)))

(test-comp (normalize-depth '(let* ([i (display 0 1)][j (write 2)][k (write 3)][g i][h g]) h))
	   (normalize-depth '(let* ([i (display 0 1)][j (write 2)][k (write 3)]) i)))

(test-comp (normalize-depth '(let* ([i (display 0 1)][g i][h (car g)][m h]) m))
	   (normalize-depth '(let* ([i (display 0 1)][h (car i)]) h)))

(test-comp (void) '(void))
(test-comp 3 '(+ 1 2))
(test-comp 65 '(char->integer #\A))
(test-comp (expt 5 30)
           '(expt 5 (* 5 6)))
(test-comp 88
           '(if (pair? null) 89 88))
(test-comp 89
           '(if (list? null) 89 88))

(test-comp '(lambda (x) (if x 2 1))
           '(lambda (x) (if (not x) 1 2)))
(test-comp '(lambda (x) (if x 2 1))
           '(lambda (x) (if (not (not (not x))) 1 2)))
(test-comp '(lambda (x) (not x))
           '(lambda (x) (if x #f #t)))

(let ([test-equal-reduction
       (lambda (val)
        (test-comp `(lambda (x) (equal? x ,val))
                   `(lambda (x) (eq? x ,val)))
        (test-comp `(lambda (x) (equal? ,val x))
                   `(lambda (x) (eq? ,val x)))
        (test-comp `(lambda (x) (eqv? x ,val))
                   `(lambda (x) (eq? x ,val)))
        (test-comp `(lambda (x) (eqv? ,val x))
                   `(lambda (x) (eq? ,val x))))]
       [test-equal-reduction/only-eqv
        (lambda (val)
         (test-comp `(lambda (x) (equal? x ,val))
                    `(lambda (x) (eqv? x ,val)))
         (test-comp `(lambda (x) (equal? ,val x))
                    `(lambda (x) (eqv? ,val x)))
         (test-comp `(lambda (x) (equal? x ,val))
                    `(lambda (x) (eq? x ,val))
                    #f)
         (test-comp `(lambda (x) (equal? ,val x))
                    `(lambda (x) (eq? ,val x))
                    #f)
         (test-comp `(lambda (x) (eqv? x ,val))
                    `(lambda (x) (eq? x ,val))
                    #f)
         (test-comp `(lambda (x) (eqv? ,val x))
                   `(lambda (x) (eq? ,val x))
                   #f))])
  (test-equal-reduction 7)
  (test-equal-reduction/only-eqv 7.0)
  (test-equal-reduction/only-eqv '(expt 2 100))
  (test-equal-reduction #\a)
  (test-equal-reduction/only-eqv #\u100)
  (test-equal-reduction ''a)
  (test-equal-reduction ''#:a)
  (test-equal-reduction '(exact-positive-integer? (random 2))))
  
  
(test-comp '(lambda (x) (eq? 7 x))
           '(lambda (x) (equal? 7 x)))
(test-comp '(lambda (x) (eq? x 7))
           '(lambda (x) (eqv? x 7)))
(test-comp '(lambda (x) (eq? 7 x))
           '(lambda (x) (eqv? 7 x)))

; car is a primitive, map is required from another module
(let ([test-equal?
       (lambda (e?)
         (test-comp #t
                    `(,e? 7 7))
         (test-comp #f
                    `(,e? 9 6))
         (test-comp #t
                    `(,e? (values 1 2) (values 1 2))
                    #f)
         (test-comp '(lambda (x) #t)
                    `(lambda (x) (,e? x x)))
         (test-comp '(lambda (x) #t)
                    `(lambda (x) (,e? car car)))
         (test-comp '(lambda (x) (list map #t))
                    `(lambda (x) (list map (,e? map map))))
         (test-comp '(module ? racket/base
                       (define x (if (zero? (random 2)) '() '(1)))
                       #t)
                    `(module ? racket/base
                       (define x (if (zero? (random 2)) '() '(1)))
                       (,e? x x)))
         (test-comp '(letrec ([x #t]
                              [y (random)])
                       (list x x y y))
                    `(letrec ([x (,e? y y)]
                              [y (random)])
                       (list x x y y))
                    #f)
         (test-comp `(lambda (x y) (when (and (pair? x) (box? y)) (,e? x y)))
                    `(lambda (x y) (when (and (pair? x) (box? y)) #f)))
         (test-comp `(lambda (x y) (car x) (unbox y) (,e? x y))
                    `(lambda (x y) (car x) (unbox y) #f))
         (test-comp `(lambda (x) (car x) (,e? x (box 0)))
                    `(lambda (x) (car x) #f))
         ;Ensure that the reduction doesn't eliminate side effects
         (test-comp `(lambda (x) (car x) (,e? (begin (newline) x) (box 0)))
                    `(lambda (x) (car x) #f)
                    #f)
         (test-comp `(lambda (x) (car x) (,e? x (begin (newline) (box 0))))
                    `(lambda (x) (car x) #f)
                    #f)
         (test-comp `(lambda () (,e? (box 0) (begin (newline) 7)))
                    `(lambda () (begin (newline) 7))
                    #f)
         (test-comp `(lambda () (,e? (begin (newline) 7) (box 0)))
                    `(lambda () (begin (newline) 7))
                    #f)
         (test-comp `(lambda (x) (if (,e? x '(0)) (pair? x) 0))
                    `(lambda (x) (if (,e? x '(0)) #t 0)))
         (test-comp `(lambda (x) (if (,e? x (list 0)) (pair? x) 0))
                    `(lambda (x) (if (,e? x (list 0)) #t 0)))
         (test-comp `(lambda (x y) (car y) (if (,e? x y) (pair? x) 0))
                    `(lambda (x y) (car y) (if (,e? x y) #t 0)))
         (test-comp `(lambda (x y) (boolean? (,e? x y)))
                    `(lambda (x y) (,e? x y) #t)))])
  (test-equal? 'eq?)
  (test-equal? 'eqv?)
  (test-equal? 'equal?))

(test-comp '(let ([x 3]) x)
	   '((lambda (x) x) 3))
(test-comp '(let ([x 3][y 4]) (+ x y))
	   '((lambda (x y) (+ x y)) 3 4))
(test-comp '5
	   '((lambda ignored 5) 3 4))
(test-comp '5
	   '(let ([f (lambda ignored 5)])
              (f 3 4)))
(test-comp '5
	   '(let ([f (lambda (a . ignored) a)])
              (f 5 3 4)))
(test-comp '(let ([x (list 3 4)]) x)
	   '(let ([f (lambda (a . b) b)])
              (f 5 3 4)))
(test-comp '(lambda (g)
              ((let ([r (read)])
                 (lambda () (+ r r)))))
           '(lambda (g)
              (let ([r (read)])
                (+ r r))))
(test-comp '(lambda (g)
              ((let ([r (read)])
                 (lambda (x) (+ r r)))
               g))
           '(lambda (g)
              (let ([r (read)])
                (+ r r))))
(test-comp '(lambda (g z)
              ((begin 
                 (read)
                 (lambda () (+ z z)))))
           '(lambda (g z)
              (begin 
                (read)
                (+ z z))))
(test-comp '(lambda (g z)
              ((begin 
                 (read)
                 (lambda (x) (+ z z)))
               g))
           '(lambda (g z)
              (begin 
                (read)
                (+ z z))))
(test-comp '(lambda (g z)
              (let ([get (lambda ()
                           (begin
                             (read)
                             (lambda (x) (+ z z))))])
                ((get) g)))
           '(lambda (g z)
              (begin
                (read)
                (+ z z))))
(test-comp '(lambda ()
              (let ([a (display "a")]
                    [g (lambda (x) x)])
                (list
                 ((begin
                    (display "b")
                    g)
                  a)
                 g
                 g)))
           '(lambda ()
              (let ([a (display "a")]
                    [g (lambda (x) x)])
                (list
                 (begin
                   (display "b")
                   a)
                 g
                 g))))

;; Check reduction of single-use lambdas
;; this test uses that a lambda with a '(1) can't be duplicated
(test-comp '((lambda (x) '(1)) 5)
           ''(1))
(test-comp '((case-lambda [(x) '(1)] [(x y) 0]) 5)
           ''(1))
(test-comp '(let ([f (lambda (x) '(1))])
              (f 5))
           ''(1))
(test-comp '(let ([f (case-lambda [(x) '(1)] [(x y) 0])])
              (f 5))
           ''(1))
(test #t (lambda () (let ([f (lambda (x) '(1))])
                      (eq? (f 5) (f 5)))))
(test #t (lambda () (let ([f (case-lambda [(x) '(1)] [(x y) 0])])
                      (eq? (f 5) (f 5)))))

;; Check that lambdas are marked as single valed and mark preserving
(test-comp '(let ([f (lambda () '(1))])
              (display (list f f))
              (values (f))) 
           '(let ([f (lambda () '(1))])
              (display (list f f))
              (f)))
(test-comp '(let ([f (lambda (x) '(1))])
              (display (list f f))
              (values (f 0))) 
           '(let ([f (lambda (x) '(1))])
              (display (list f f))
              (f 0)))
(test-comp '(let ([f (lambda (x y) '(1))])
              (display (list f f))
              (values (f 0 0))) 
           '(let ([f (lambda (x y) '(1))])
              (display (list f f))
              (f 0 0)))
(test-comp '(let ([f (lambda (x y z) '(1))])
              (display (list f f))
              (values (f 0 0 0))) 
           '(let ([f (lambda (x y z) '(1))])
              (display (list f f))
              (f 0 0 0)))
(test-comp '(letrec ([even (lambda (x) (if (= x 0) #t (not (odd (sub1 x)))))]
                     [odd (lambda (x) (if (= x 1) #t (not (even (sub1 x)))))])
              (display (list even even odd odd))
              (values (even 1000))) 
           '(letrec ([even (lambda (x) (if (= x 0) #t (not (odd (sub1 x)))))]
                     [odd (lambda (x) (if (= x 1) #t (not (even (sub1 x)))))])
              (display (list even even odd odd))
              (even 1000))) 
(test-comp '(letrec ([f (lambda (x) (g '(1)))]
                     [g (lambda (x) (display x) (if (zero? (random 2)) '(1 2) (values 1 2)))])
              (display (list f f g g))
              (values (f 0))) 
           '(letrec ([f (lambda (x) (g '(1)))]
                     [g (lambda (x) (display x) (if (zero? (random 2)) '(1 2) (values 1 2)))])
              (display (list f f g g))
              (f 0))
           #f)
(test-comp '(letrec ([g (lambda (x) (display x) (if (zero? (random 2)) '(1 2) (values 1 2)))]
                     [f (lambda (x) (g '(1)))])
              (display (list f f g g))
              (values (f 0))) 
           '(letrec ([g (lambda (x) (display x) (if (zero? (random 2)) '(1 2) (values 1 2)))]
                     [f (lambda (x) (g '(1)))])
              (display (list f f g g))
              (f 0))
           #f)


(test-comp '(lambda (w z)
              (let ([x (cons w z)])
                (car x)))
           '(lambda (w z) w))

(test-comp '(lambda (w z)
              (let ([x (cons w z)])
                (cdr x)))
           '(lambda (w z) z))
(test-comp '(lambda (w z)
              (let ([x (list w z)])
                (car x)))
           '(lambda (w z) w))
(test-comp '(lambda (w z)
              (let ([x (list* w z)])
                (car x)))
           '(lambda (w z) w))
(test-comp '(lambda (w z)
              (let ([x (list w z)])
                (cadr x)))
           '(lambda (w z) z))
(test-comp '(lambda (w z)
              (let ([x (list (cons 1 (cons w z)))])
                (car (cdr (car x)))))
           '(lambda (w z) w))
(test-comp '(lambda (w z)
              (car (list w z)))
           '(lambda (w z) w))
(test-comp '(lambda (w z)
              (car (list* w z)))
           '(lambda (w z) w))
(test-comp '(lambda (w z)
              (car (list* w z w)))
           '(lambda (w z) w))
(test-comp '(lambda (w z)
              (cdr (list w)))
           '(lambda (w z) null))
(test-comp '(lambda (w z)
              (cdr (list w z)))
           '(lambda (w z) (list z)))
(test-comp '(lambda (w z)
              (cdr (list w z w)))
           '(lambda (w z) (list z w)))
(test-comp '(lambda (w z)
              (cdr (list* w z)))
           '(lambda (w z) z))
(test-comp '(lambda (w z)
              (cdr (list* w z w)))
           '(lambda (w z) (list* z w)))

(test-comp '(lambda (w z)
              (car (list w (random))))
           '(lambda (w z) w)
           #f)
(test-comp '(lambda (w z)
              (car (list w (random) (random))))
           '(lambda (w z) w)
           #f)
(test-comp '(lambda (w z)
              (cdr (list (random))))
           '(lambda (w z) null)
           #f)
(test-comp '(lambda (w z)
              (cdr (list (random) w)))
           '(lambda (w z) (list w))
           #f)
(test-comp '(lambda (w z)
              (cdr (list (random) w z)))
           '(lambda (w z) (list w z))
           #f)

(test-comp '(lambda (u v) (car (cons u v)))
           '(lambda (u v) u))
(test-comp '(lambda (u v) (unsafe-car (cons u v)))
           '(lambda (u v) u))
(test-comp '(lambda (u v) (car (unsafe-cons-list u v)))
           '(lambda (u v) u))

(test-comp '(lambda (u v) (cdr (cons u v)))
           '(lambda (u v) v))
(test-comp '(lambda (u v) (unsafe-cdr (cons u v)))
           '(lambda (u v) v))
(test-comp '(lambda (u v) (cdr (unsafe-cons-list u v)))
           '(lambda (u v) v))

(test-comp '(lambda (v) (unbox (box v)))
           '(lambda (v) v))
(test-comp '(lambda (v) (unsafe-unbox (box v)))
           '(lambda (v) v))
(test-comp '(lambda (v) (unsafe-unbox* (box v)))
           '(lambda (v) v))

(test-comp '(lambda () (car (cons (random 2) (random 3))))
           '(lambda () (begin0 (random 2) (random 3))))
(test-comp '(lambda () (car (cons (random 2) (begin (random 3) (lambda (x) x)))))
           '(lambda () (begin0 (random 2) (random 3))))
(test-comp '(lambda () (cdr (cons (random 2) (random 3))))
           '(lambda () (begin (random 2) (random 3))))
(test-comp '(lambda () (cdr (cons (begin (random 2) (lambda (x) x)) (random 3))))
           '(lambda () (begin (random 2) (random 3))))
(test-comp '(lambda () (cdr (cons (begin (random 1) (random 2) (lambda (x) x)) (random 3))))
           '(lambda () (begin (random 1) (random 2) (random 3))))

(test-comp '(lambda (w z) (pair? (list)))
           '(lambda (w z) #f))
(test-comp '(lambda (w z) (null? (list)))
           '(lambda (w z) #t))
(test-comp '(lambda (w z) (pair? (cons z w)))
           '(lambda (w z) #t))
(test-comp '(lambda (w z) (pair? (unsafe-cons-list z w)))
           '(lambda (w z) #t))
(test-comp '(lambda (w z) (pair? (list w)))
           '(lambda (w z) #t))
(test-comp '(lambda (w z) (pair? (list w z)))
           '(lambda (w z) #t))
(test-comp '(lambda (w z) (pair? (list w z w)))
           '(lambda (w z) #t))
(test-comp '(lambda (w z) (pair? (list w (random) w)))
           '(lambda (w z) (random) #t))
(test-comp '(lambda (w z) (pair? (list (read) (random) w)))
           '(lambda (w z) (read) (random) #t))
(test-comp '(lambda (w z) (pair? (list z (random) (read))))
           '(lambda (w z) (random) (read) #t))
(test-comp '(lambda (w z) (pair? (list (if z (random) (error 'e)) (read))))
           '(lambda (w z) (if z (random) (error 'e)) (read) #t))
(test-comp '(lambda (w z) (pair? (list (with-continuation-mark 'k 'v (read)) (random))))
           '(lambda (w z) (with-continuation-mark 'k 'v (read)) (random) #t))
(test-comp '(lambda (w z) (vector? (vector w z)))
           '(lambda (w z) #t))
(test-comp '(lambda (w z) (vector? (vector-immutable w z)))
           '(lambda (w z) #t))
(test-comp '(lambda (w z) (vector? (list 1)))
           '(lambda (w z) #f))
(test-comp '(lambda (w z) (mpair? (mcons 1 2)))
           '(lambda (w z) #t))
(test-comp '(lambda (w z) (box? (box 1)))
           '(lambda (w z) #t))
(test-comp '(lambda (w z) (box? (box-immutable 1)))
           '(lambda (w z) #t))

(test-comp '(lambda (w z) (pair? (cons w)))
           '(lambda (w z) #f)
           #f)
(test-comp '(lambda (w z) (pair? (list* w)))
           '(lambda (w z) #t)
           #f)
(test-comp '(lambda (w z) (pair? (list* w)))
           '(lambda (w z) #f)
           #f)
(test-comp '(lambda (w z) (box? (box 1 2)))
           '(lambda (w z) #t)
           #f)
(test-comp '(lambda (w z) (box? (box-immutable 1 2)))
           '(lambda (w z) #t)
           #f)

(test-comp '(lambda (w z) (list? (begin (random) null)))
           '(lambda (w z) (random) #t))
(test-comp '(lambda (w z) (list? (begin (random) (void))))
           '(lambda (w z) (random) #f))
(test-comp '(lambda (w z) (list? (cons w z)))
           '(lambda (w z) #t)
           #f)

(test-comp '(lambda (x) (car x) #t)
           '(lambda (x) (car x) (pair? x)))
(test-comp '(lambda (x) (cdr x) #t)
           '(lambda (x) (cdr x) (pair? x)))
(test-comp '(lambda (x) (cadr x) #t)
           '(lambda (x) (cadr x) (pair? x)))
(test-comp '(lambda (f) (procedure-arity-includes? f 5) #t)
           '(lambda (f) (procedure-arity-includes? f 5) (procedure? f)))
(test-comp '(lambda (f l) (f l) #t)
           '(lambda (f l) (f l) (procedure? f)))

(test-comp '(lambda (z) (let ([o #f]) (car z)) #t)
           '(lambda (z) (let ([o #f]) (car z)) (pair? z)))
(test-comp '(lambda (z) (let ([o (random)]) (car z)) #t)
           '(lambda (z) (let ([o (random)]) (car z)) (pair? z)))
(test-comp '(lambda (z) (let ([o z]) (list (car o) o o)) #t)
           '(lambda (z) (let ([o z]) (list (car o) o o)) (pair? z)))
(test-comp '(lambda (z) (let ([o z] [x (random)]) (list (car o) x x)) #t)
           '(lambda (z) (let ([o z] [x (random)]) (list (car o) x x)) (pair? z)))
(test-comp '(lambda (z) (let ([f (lambda () (car z))]) (f) #t))
           '(lambda (z) (let ([f (lambda () (car z))]) (f) (pair? z))))
(test-comp '(lambda (z) (let ([f (lambda () (car z))]) (f)) #t)
           '(lambda (z) (let ([f (lambda () (car z))]) (f)) (pair? z)))
(test-comp '(lambda (z) (let ([f (lambda (i) (car z))]) (f 0) #t))
           '(lambda (z) (let ([f (lambda (i) (car z))]) (f 0) (pair? z))))
(test-comp '(lambda (z) (let ([f (lambda (i) (car z))]) (f 0)) #t)
           '(lambda (z) (let ([f (lambda (i) (car z))]) (f 0)) (pair? z)))
(test-comp '(lambda (z) (let ([f (lambda (i) (car i))]) (f z) #t))
           '(lambda (z) (let ([f (lambda (i) (car i))]) (f z) (pair? z))))
(test-comp '(lambda (z) (let ([f (lambda (i) (car i))]) (f z)) #t)
           '(lambda (z) (let ([f (lambda (i) (car i))]) (f z)) (pair? z)))

; Test that the optimizer infers correctly the type of all the arguments
; and the type of the return value. Use #f in case the type is unknown.
(define (test-arg-types proc/args? val? 
                        (omit-on-good-args? #f)
                        (dont-infer-type-for-args? #f))
  (define proc (car proc/args?))
  (define args? (cdr proc/args?))
  (define vars (for/list ([i (in-range (length args?))])
                 (string->symbol (string-append "arg" (number->string i)))))
  (define vars/true (for/list ([i (in-list args?)]) #t))
  (define test/vars (for/list ([test? (in-list args?)]
                               [var (in-list vars)])
                      (if test? `(,test? ,var) #t)))
  (when val?
    (test-comp `(lambda ,vars (when (and ,@test/vars) (,val? (,proc ,@vars))))
               `(lambda ,vars (when (and ,@test/vars) (,proc ,@vars) #t))))
  (when omit-on-good-args?
    (test-comp `(lambda ,vars (when (and ,@test/vars) (,proc ,@vars)) (void))
               `(lambda ,vars (void))))
  (when (not dont-infer-type-for-args?)
    (test-comp `(lambda ,vars (,proc ,@vars) (list ,@test/vars))
               `(lambda ,vars (,proc ,@vars) (list ,@vars/true)))))

;Test types inference for vector?
(test-arg-types '(vector-length vector?) 'fixnum? 'may-omit)
(test-arg-types '(vector->values vector?) #f)
(test-arg-types '(vector-ref vector? fixnum?) #f)
(test-arg-types '(vector-set! vector? fixnum? #f) 'void?)
(test-arg-types '(vector->list vector?) 'list?)
(test-arg-types '(list->vector list?) 'vector?)
(test-arg-types '(struct->vector #f) 'vector?)
(test-arg-types '(struct->vector #f #f) 'vector?)
(test-arg-types '(vector->immutable-vector vector?) 'vector?)

;Test special cases of make-vector
(test-arg-types '(make-vector fixnum?) 'vector?)
(test-arg-types '(make-vector fixnum? #f) 'vector?)
(test-comp '(lambda (w z) (vector? (make-vector (w) (z))))
           '(lambda (w z) (make-vector (w) (z)) #t))
(test-comp '(lambda (w z) (vector? (make-vector (w))))
           '(lambda (w z) (make-vector (w)) #t))
(test-comp '(lambda (w z) (vector? (make-vector 5 (z))))
           '(lambda (w z) (values (z)) #t))
(test-comp '(lambda (w z) (vector? (make-vector 5 w)))
           '(lambda (w z) #t))
(test-comp '(lambda (w z) (vector? (make-vector 5)))
           '(lambda (w z) #t))
(test-comp '(lambda (w z) (vector? (make-vector -1)))
           '(lambda (w z) #t)
           #f)
(test-comp '(lambda (w z) (vector? (make-vector #f)))
           '(lambda (w z) #t)
           #f)

;Test types inference for box?
(test-arg-types '(box #f) 'box?)
(test-arg-types '(box-immutable #f) 'box?)
(test-arg-types '(unbox box?) #f)
(test-arg-types '(set-box! box? #f) 'void?)

;Test types inference for string?
(test-arg-types '(string-length string?) 'fixnum? 'may-omit)
(test-arg-types '(string-ref string? fixnum?) 'char?)
(test-arg-types '(string-set! string? fixnum? char?) 'void?)
(test-arg-types '(string->immutable-string string?) 'string? 'may-omit)
(test-arg-types '(string-append) string? 'may-omit)
(test-arg-types '(string-append string?) 'string? 'may-omit)
(test-arg-types '(string-append string? string?) 'string? 'may-omit)
(test-arg-types '(string-append string? string? string?) 'string? 'may-omit)
(test-arg-types '(string-append string? string? string? string?) 'string? 'may-omit)

;Test types inference for bytes?
(test-arg-types '(bytes-length bytes?) 'fixnum? 'may-omit)
(test-arg-types '(bytes-ref bytes? fixnum?) 'fixnum?)
(test-arg-types '(bytes-set! bytes? fixnum? fixnum?) 'void?)
(test-arg-types '(bytes->immutable-bytes bytes?) 'bytes? 'may-omit)
(test-arg-types '(bytes-append) bytes? 'may-omit)
(test-arg-types '(bytes-append bytes?) 'bytes? 'may-omit)
(test-arg-types '(bytes-append bytes? bytes?) 'bytes? 'may-omit)
(test-arg-types '(bytes-append bytes? bytes? bytes?) 'bytes? 'may-omit)
(test-arg-types '(bytes-append bytes? bytes? bytes? bytes?) 'bytes? 'may-omit)

;Test types inference for list?
(test-arg-types '(length list?) 'fixnum? 'may-omit)
(test-arg-types '(list-ref pair? fixnum?) #f)
(test-arg-types '(append) #f 'may-omit)
(test-arg-types '(append #f) #f 'may-omit)
(test-arg-types '(append list? #f) #f 'may-omit)
(test-arg-types '(append list? list? #f) #f 'may-omit)
(test-arg-types '(append list? list? list? #f) #f 'may-omit)
(test-arg-types '(append list?) list? 'may-omit 'dont-infer)
(test-arg-types '(append list? list?) list? 'may-omit 'dont-infer)
(test-arg-types '(append list? list? list?) list? 'may-omit 'dont-infer)
(test-arg-types '(append list? list? list? list?) list? 'may-omit 'dont-infer)

;Test types inference for symbol? and keyword?
(test-arg-types '(symbol->string symbol?) 'string? 'may-omit)
(test-arg-types '(string->symbol string?) 'symbol? 'may-omit)
(test-arg-types '(keyword->string keyword?) 'string? 'may-omit)
(test-arg-types '(string->keyword string?) 'keyword? 'may-omit)
(test-arg-types '(gensym) 'symbol?)
(test-arg-types '(gensym #f) 'symbol?)

;Test the map primitive and the map version defined in private/map.rkt
;The optimizer is not capable of figuring out that the result of map is a list?
(test-arg-types '(k:map procedure? list?) 'list?)
(test-arg-types '(k:map procedure? list? list?) 'list?)
(test-arg-types '(map procedure? list?) #f) ;should be list?
(test-arg-types '(map procedure? list? list?) #f) ;should be list? 

(test-comp '(lambda (w z)
              (let ([x (list* w z)]
                    [y (list* z w)])
                (error "bad")
                (equal? x y)))
           '(lambda (w z)
              (error "bad")
              (equal? (list* w z) (list* z w))))

(test-comp '(lambda (x) (when (list x) (append x (values 1 2))) (void))
           '(lambda (x) (void))
           #f)
(err/rt-test (pair? (list (values 1 2) 0)) exn:fail:contract:arity?)
(test-comp '(lambda (w z)
              (pair? (list (values 1 2) 0)))
           '(lambda (w z)
              (values (values 1 2))
              #t))

(test-comp '(lambda (w z)
              (let ([l '(1 2)]
                    [l2 (list w z)]
                    [m (mcons 1 2)]
                    [v (vector w w w)]
                    [v2 (vector-immutable w w w)])
                (list (car l)
                      (cdr l)
                      (mpair? l)
                      (pair? l)
                      (pair? l2)
                      (mpair? m)
                      (vector? v)
                      (vector? v2)
                      (null? v)
                      v v v2 v2)))
           '(lambda (w z)
              (let ([l '(1 2)]
                    [l2 (list w z)]
                    [m (mcons 1 2)]
                    [v (vector w w w)]
                    [v2 (vector-immutable w w w)])
                (list (unsafe-car l)
                      (unsafe-cdr l)
                      #f
                      #t
                      #t
                      #t
                      #t
                      #t
                      #f
                      v v v2 v2))))

(test-comp '(lambda (w z)
              (if (list w z (random 7))
                  (let ([l (list (random))])
                    (if l
                        (list (car l) (cdr l))
                        'oops))
                  "bad"))
           '(lambda (w z)
              (begin
                (list w z (random 7))
                (let ([l (list (random))])
                  (list (unsafe-car l) (unsafe-cdr l))))))

(test-comp '(lambda (w z)
              (let ([l (if w
                           (lambda () w)
                           (lambda () z))])
                (if (procedure? l)
                    (list l l)
                    2)))
           '(lambda (w z)
              (let ([l (if w
                           (lambda () w)
                           (lambda () z))])
                (list l l))))

(test-comp '(lambda (w z)
              (list (if (pair? w) (car w) (car z))
                    (cdr w)))
           '(lambda (w z)
              (list (if (pair? w) (car w) (car z))
                    (unsafe-cdr w)))
           #f)

(test-comp '(lambda (w z)
              (list (if z (car z) (car w))
                    (cdr w)))
           '(lambda (w z)
              (list (if z (car z) (car w))
                    (unsafe-cdr w)))
           #f)

(test-comp '(lambda (w z)
              (list (if (pair? w) (car z) (car w))
                    (cdr w)))
           '(lambda (w z)
              (list (if (pair? w) (car z) (car w))
                    (unsafe-cdr w))))

(test-comp '(lambda (w z)
              (list (if z (car w) (cdr w))
                    (cdr w)))
           '(lambda (w z)
              (list (if z (car w) (cdr w))
                    (unsafe-cdr w))))

(test-comp '(lambda (w z x)
              (list (car x) (if z (car w) (cdr w)) (car x)))
           '(lambda (w z x)
              (list (car x) (if z (car w) (cdr w)) (unsafe-car x))))

(test-comp '(lambda (w z x)
              (list (car x) (if z (car w) 2) (car x)))
           '(lambda (w z x)
              (list (car x) (if z (car w) 2) (unsafe-car x))))

(test-comp '(lambda (w z x)
              (list (car x) (if z 1 (cdr w)) (car x)))
           '(lambda (w z x)
              (list (car x) (if z 1 (cdr w)) (unsafe-car x))))

(test-comp '(lambda (w z x)
              (list (car x) (if z 1 2) (car x)))
           '(lambda (w z x)
              (list (car x) (if z 1 2) (unsafe-car x))))

(test-comp '(lambda (w)
              (list
                (car (begin (random) w))
                (cdr (begin (random) w))
                (pair? (begin (random) w))
                (null? (begin (random) w)))) 
           '(lambda (w)
              (list
                (car (begin (random) w))
                (unsafe-cdr (begin (random) w))
                (begin (random) #t)
                (begin (random) #f))))

(test-comp '(lambda (w) (car w) #t)
           '(lambda (w) (car w) (pair? w)))
(test-comp '(lambda (w) (cadr w) #t)
           '(lambda (w) (cadr w) (pair? w)))

(test-comp '(lambda (w f)
              (list
                (car (let ([x (random)]) (f x x) w))
                (cdr (let ([x (random)]) (f x x) w))
                (pair? (let ([x (random)]) (f x x) w))
                (null? (let ([x (random)]) (f x x) w))))
           '(lambda (w f)
              (list
                (car (let ([x (random)]) (f x x) w))
                (unsafe-cdr (let ([x (random)]) (f x x) w))
                (let ([x (random)]) (f x x) #t)
                (let ([x (random)]) (f x x) #f))))

(test-comp '(lambda ()
              (car (let ([y (random)])
                     (list y (set! y 5)))))
           '(lambda ()
              (let ([y (random)])
                (begin0 y (set! y 5)))))

; It's necessary to use the random from #%kernel because otherwise
; the function will keep an unnecessary reference for the module that
; defines the random visible from racket/base.
(test-comp '(lambda (w) (car w) (mcar w))
           '(lambda (w) (car w) (mcar w) (k:random)))
(test-comp '(lambda (w) (car w w))
           '(lambda (w) (car w w) (k:random)))
(test-comp '(lambda (w) (car w w w))
           '(lambda (w) (car w w w) (k:random)))
(test-comp '(lambda (w) (cons w))
           '(lambda (w) (cons w) (k:random)))
(test-comp '(lambda (w) (cons))
           '(lambda (w) (cons) (k:random)))

; test for unary aplications
(test-comp -1
           '(- 1))
(test-comp '(lambda (f) (begin (f) -1))
           '(lambda (f) (- (begin (f) 1))))
(test-comp '(letrec ([x (lambda (t) x)]) (x x) -1)
           '(- (letrec ([x (lambda (t) x)]) (x x) 1)))
(test-comp 1
           '(car (cons 1 2)))
(test-comp '(lambda (f) (begin (f) 1))
           '(lambda (f) (car (begin (f) (cons 1 2)))))
(test-comp '(letrec ([x (lambda (t) x)]) (x x) 1)
           '(car (letrec ([x (lambda (t) x)]) (x x) (cons 1 2))))
           
(test-comp '(lambda (w z) (box? (list (cons (random w) z))))
           '(lambda (w z) (random w) #f))

(test-comp '(lambda () (begin0 (random 1) (random 2)))
           '(lambda () (car (cons (random 1) (random 2)))))
(test-comp '(lambda () (begin (random 1) (random 2)))
           '(lambda () (cdr (cons (random 1) (random 2)))))

(test-comp '(lambda () (begin (random 1) (random 2) (random 3) (random 4)))
           '(lambda () (begin (car (cons (random 1) (random 2))) (random 3) (random 4)))) ;
(test-comp '(lambda () (begin (random 1) (random 2) (random 3) (random 4)))
           '(lambda () (begin (cdr (cons (random 1) (random 2))) (random 3) (random 4))))
(test-comp '(lambda () (begin (random 1) (random 2) (random 3) (random 4)))
           '(lambda () (begin (random 1) (car (cons (random 2) (random 3))) (random 4)))) ;
(test-comp '(lambda () (begin (random 1) (random 2) (random 3) (random 4)))
           '(lambda () (begin (random 1) (cdr (cons (random 2) (random 3))) (random 4))))
(test-comp '(lambda () (begin (random 1) (random 2) (begin0 (random 3) (random 4))))
           '(lambda () (begin (random 1) (random 2) (car (cons (random 3) (random 4))))))
(test-comp '(lambda () (begin (random 1) (random 2) (random 3) (random 4)))
           '(lambda () (begin (random 1) (random 2) (cdr (cons (random 3) (random 4))))))

(test-comp '(lambda () (begin0 (random 1) (random 2) (random 3) (random 4)))
           '(lambda () (begin0 (car (cons (random 1) (random 2))) (random 3) (random 4))))
(test-comp '(lambda () (begin0 (begin (random 1) (random 2)) (random 3) (random 4)))
           '(lambda () (begin0 (cdr (cons (random 1) (random 2))) (random 3) (random 4))))
(test-comp '(lambda () (begin0 (random 1) (random 2) (random 3) (random 4)))
           '(lambda () (begin0 (random 1) (car (cons (random 2) (random 3))) (random 4)))) ;
(test-comp '(lambda () (begin0 (random 1) (random 2) (random 3) (random 4)))
           '(lambda () (begin0 (random 1) (cdr (cons (random 2) (random 3))) (random 4))))
(test-comp '(lambda () (begin0 (random 1) (random 2) (random 3) (random 4)))
           '(lambda () (begin0 (random 1) (random 2) (car (cons (random 3) (random 4)))))) ;
(test-comp '(lambda () (begin0 (random 1) (random 2) (random 3) (random 4)))
           '(lambda () (begin0 (random 1) (random 2) (cdr (cons (random 3) (random 4))))))

(test-comp '(lambda (w)
              (begin (random) w))
           '(lambda (w)
              (car (cons w (random)))))
;don't exchange if it may capture a continuation
(test-comp '(lambda (f) 
              (begin (values (f)) (list 7)))
           '(lambda (f) 
              (car (cons (list 7) (values (f)))))
           #f)
;don't exchange if it may be not safe for space
(test-comp '(lambda (f n)
              (let ([big (cons (f) (make-vector 10))])
                (display big) 
                (begin (make-vector n) (car big))))
           '(lambda (f n)
              (let ([big (cons (f) (make-vector 10))])
                (display big) 
                (car (cons (car big) (make-vector n)))))
           #f)

(test-comp '(lambda (w)
              (begin
                (list (random) w)
                17))
           '(lambda (w)
              (begin (random) 17)))
(test-comp '(lambda (w)
              (begin0
               17
               (list (random) w)))
           '(lambda (w)
              (begin0 17 (random))))

(test-comp '(lambda (w) (not (list w)))
           '(lambda (w) #f))

(test-comp '(lambda (a b)
              (not (if a b (list 1 2))))
           '(lambda (a b)
              (not (if a b #t))))

;ensure that variable p is not marked as used in the lambda
(test-comp '(let ([p (if (zero? (random 2)) 1 2)])
              (list p p (lambda () (not p))))
           '(let ([p (if (zero? (random 2)) 1 2)])
              (list p p (lambda () #f))))
(test-comp '(let ([p (lambda () 0)])
              (list p p (lambda () (not p))))
           '(let ([p (lambda () 0)])
              (list p p (lambda () #f))))
;this still doesn't work without the additional p
#;(test-comp '(let ([p (lambda () 0)])
                (list p p (lambda () (procedure? p))))
             '(let ([p (lambda () 0)])
                (list p p (lambda () #t))))
(test-comp '(let ([p (lambda () 0)])
              (list p p (lambda () (procedure? p))))
           '(let ([p (lambda () 0)])
              (list p p (lambda () p #t))))

(test-comp '(lambda (w) (if (void (list w)) 1 2))
           '(lambda (w) 1))

; Different number of arguments use different code paths
(test-comp '(lambda (f x) (void))
           '(lambda (f x) (void (list))))
(test-comp '(lambda (f x) (begin (values (f x)) (void)))
           '(lambda (f x) (void (list (f x)))))
(test-comp '(lambda (f x) (begin (values (f x)) (values (f x)) (void)))
           '(lambda (f x) (void (list (f x) (f x)))))
(test-comp '(lambda (f x) (begin (values (f x)) (values (f x)) (values (f x)) (void)))
           '(lambda (f x) (void (list (f x) (f x) (f x)))))


(test null
      call-with-values (lambda () (with-continuation-mark 'a 'b (values))) list)

;; Ok to move `box' past a side effect (that can't capture a
;; resumable continuation):
(test-comp '(let ([h (box 0.0)])
              (list (random) h))
           '(list (random) (box 0.0)))

;; Don't move `box' past a `lambda':
(test-comp '(let ([h (box 0.0)])
              (lambda () h))
           '(lambda () (box 0.0))
           #f)

;; Make sure that a mutable top-level isn't copy-propagated
;; across another effect:
(test-comp '(module m racket/base
              (define x 10)
              (define (f y)
                (let ([old x])
                  (set! x y)
                  (set! x old))))
           '(module m racket/base
              (define x 10)
              (define (f y)
                (let ([old x])
                   (set! x y)
                   (set! x x))))
            #f)

;; Do copy-propagate a reference to a mutable top-level 
;; across non-effects:
(test-comp '(module m racket/base
              (define x 10)
              (define (f y)
                (let ([old x])
                  (list (cons y y)
                        (set! x old)))))
           '(module m racket/base
              (define x 10)
              (define (f y)
                (list (cons y y)
                      (set! x x)))))

;; Treat access to a mutable top-level as an effect:
(test-comp '(module m racket/base
              (define x 10)
              (define (f y)
                (let ([old x])
                  (list (cons y x)
                        (set! x old)))))
           '(module m racket/base
              (define x 10)
              (define (f y)
                (list (cons y x)
                      (set! x x))))
            #f)

;; Don't move a side-effecting experssion past an unsafe operation
;; that observes effects:
(test-comp '(lambda (b f)
             (let* ([x (f (lambda () b))])
               (cons (unsafe-unbox b) x)))
           '(lambda (b f)
             (cons (unsafe-unbox b) (f (lambda () b))))
           #f)

(test-comp '(module m racket/base
             (define (true) #t)
             (define no (if (true) (lambda (x) (cons x 'no)) display))
             (newline) ; <--- just to break the simultaneous group
             (define yes (if (true) (lambda (x) (cons x 'yes)) display))
             (no 5)
             (yes 5))
           '(module m racket/base
             (define (true) #t)
             (define no (lambda (x) (cons x 'no)))
             (newline)
             (define yes (lambda (x) (cons x 'yes)))
             (cons 5 'no)
             (cons 5 'yes)))

(test-comp '(let ([x 1][y 2]) x)
	   '1)
(test-comp '(let ([x 1][y 2]) (+ y x))
	   '3)
(test-comp '(let ([x 1][y 2]) (cons x y))
	   '(cons 1 2))
(test-comp '(let* ([x (cons 1 1)][y x]) (cons x y))
	   '(let* ([x (cons 1 1)]) (cons x x)))
(test-comp '(let* ([x 1][y (add1 x)]) (+ y x))
	   '3)
(test-comp '(letrec ([x (cons 1 1)][y x]) (cons x y))
	   '(letrec ([x (cons 1 1)][y x]) (cons x x)))

;; Remove unnecessary bindings 
(test-comp '(let ([f (lambda (x) x)]) f)
           (syntax-property (datum->syntax #f '(lambda (x) x) (vector 'here #f #f #f #f))
                            'inferred-name
                            'f))
(test-comp '(let ([f (lambda (x) x)]) f f)
           (syntax-property (datum->syntax #f '(lambda (x) x) (vector 'here #f #f #f #f))
                            'inferred-name
                            'f))
(test-comp '(lambda (g) (let ([f (g)]) f))
           '(lambda (g) (values (g))))
(test-comp '(lambda (g) (let ([f (g)]) f f))
           '(lambda (g) (values (g))))
(test-comp '(lambda (x) (let ([f (car x)]) f))
           '(lambda (x) (car x)))
(test-comp '(lambda (x) (let ([f (car x)]) f f))
           '(lambda (x) (car x)))
(test-comp '(lambda (g) (let* ([i (g 0)] [f (g i)]) f))
           '(lambda (g) (let* ([i (g 0)]) (values (g i)))))
(test-comp '(lambda (g) (let* ([i (g 0)] [f (g i)]) f f))
           '(lambda (g) (let* ([i (g 0)]) (values (g i)))))
(test-comp '(lambda (g) (let* ([i (g 0)] [f (car i)]) f))
           '(lambda (g) (let* ([i (g 0)]) (car i))))
(test-comp '(lambda (g) (let* ([i (g 0)] [f (car i)]) f f))
           '(lambda (g) (let* ([i (g 0)]) (car i))))
(test-comp '(let ([f (lambda (x) x)]) 7)
           7)
(test-comp '(let ([f (lambda (x) x)]) f 7)
           7)
(test-comp '(lambda (g) (let* ([i (g 0)] [f (car i)]) 7))
           '(lambda (g) (let* ([i (g 0)]) (begin (car i) 7))))
(test-comp '(lambda (g) (let* ([i (g 0)] [f (car i)]) f 7))
           '(lambda (g) (let* ([i (g 0)]) (begin (car i) 7))))
(test-comp '(lambda (g) (let* ([i (g 0)] [f (box i)]) 7))
           '(lambda (g) (let* ([i (g 0)]) 7)))
(test-comp '(lambda (g) (let* ([i (g 0)] [f (box i)]) f 7))
           '(lambda (g) (let* ([i (g 0)]) 7)))
(test-comp '(lambda (g) (let* ([i (g 0)] [f (g i)]) 7))
           '(lambda (g) (let* ([i (g 0)]) (begin (values (g i)) 7))))
(test-comp '(lambda (g) (let* ([i (g 0)] [f (g i)]) f 7))
           '(lambda (g) (let* ([i (g 0)]) (begin (values (g i)) 7))))


(test-comp '(letrec ([f (lambda (x) x)])
	      (f 10)
	      f)
	   '(letrec ([f (lambda (x) x)])
	      f))
(test-comp '(let ([f (lambda (x) x)])
	      (f 10))
	   10)
(test-comp '(let ([f (lambda (x) (add1 x))]
		  [y 10])
	      (f y))
	   '11)

(test-comp '(module m racket/base
              (define (f x) (+ x 1))
              (f 8))
	   '(module m racket/base
              (define (f x) (+ x 1))
              9))

(test-comp '(let ([f (lambda (x) 10)])
              3)
           '3)

(test-comp '(let ([x (#%expression
                      (begin (quote-syntax foo) 3))])
              x)
           '3)
(test-comp '(if (lambda () 10)
                'ok
                (quote-syntax no!))
           ''ok)

(test-comp '(lambda (x) (not (if x #f 2)))
           '(lambda (x) (not (if x #f #t))))
(test-comp '(lambda (x) (let ([z 2]) (not (if x #f z))))
           '(lambda (x) (let ([z 2]) (not (if x #f #t)))))
(test-comp '(lambda (z) (when (pair? z) #f))
           '(lambda (z) (when (pair? z) (not z))))
(test-comp '(lambda (z) (when (pair? z) (set! z #f) #f))
           '(lambda (z) (when (pair? z) (set! z #f) (not z)))
           #f)

(test-comp '(lambda (x) (if x x #f))
           '(lambda (x) x))
(test-comp '(lambda (x y) (set! x y) (if x x #f))
           '(lambda (x y) (set! x y) x))
(test-comp '(lambda (x) (if (cons 1 x) 78 78))
           '(lambda (x) 78))
(test-comp '(lambda (x) (if (null? x) 78 78))
           '(lambda (x) 78))
(test-comp '(lambda (x) (if (values 1 2) 78 78))
           '(lambda (x) (values 1 2) 78)
           #f)
(test-comp '(if (values 1 2) (values 1 2) #f)
           '(values 1 2)
           #f)
; car is a primitive, map is required from another module
(test-comp '(lambda (x) (if (null? x) car car))
           '(lambda (x) car))
(test-comp '(lambda (x) (if (null? x) map map))
           '(lambda (x) map))
(test-comp '(module ? racket/base
              (define x (if (zero? (random 2)) '() '(1)))
              (if (null? x) x x))
           '(module ? racket/base
              (define x (if (zero? (random 2)) '() '(1)))
              x))
(test-comp '(lambda (x) (if (null? x) x x))
           '(lambda (x) x))
(test-comp '(lambda (x) (if (null? x) null x))
           '(lambda (x) x))
(test-comp '(lambda (x) (not (if (null? x) #t x)))
           '(lambda (x) (not x)))

;reduce ignored `if`s
(test-comp '(lambda (x) (void (if x 1 2)))
           '(lambda (x) (void)))
(test-comp '(lambda (f) (void (if (f) 1 2)))
           '(lambda (f) (void (values (f)))))
(test-comp '(lambda () (void (if (eq? (random 2) 0) 1 2)))
           '(lambda () (void (random 2))))
(test-comp '(lambda (x) (void (if (eq? (random 2) 0) (box x) (list x))))
           '(lambda (x) (void (random 2))))
(test-comp '(lambda (x) (void (if x (random) 1)))
           '(lambda (x) (void (if x (random) 2))))
(test-comp '(lambda (x) (void (if x 1 (random))))
           '(lambda (x) (void (if x 2 (random)))))
(test-comp '(lambda (x) (void (if x (random) 1)))
           '(lambda (x) (void))
           #f)
(test-comp '(lambda (x) (void (if x 1 (random))))
           '(lambda (x) (void))
           #f)

(test-comp '(lambda (x) (let ([n (list 1)])
                          (list n n (not (if x #t n)))))
           '(lambda (x) (let ([n (list 1)])
                          (list n n #f))))
(test-comp '(lambda (x) (let ([n (if (zero? (random 2)) 1 -1)])
                          (list n n (not (if x #t n)))))
           '(lambda (x) (let ([n (if (zero? (random 2)) 1 -1)])
                          (list n n #f))))
(test-comp '(lambda (x) (let ([n (if (zero? (random 2)) 1 -1)])
                          (list n n (not (if x #t n)))))
           '(lambda (x) (let ([n (if (zero? (random 2)) 1 -1)])
                          (list n n #f))))

; Test reductions in expressions that are similar to the expansion of `or`
(test-comp '(lambda (z)
              (when (boolean? z)
                (if z z 0)))
           '(lambda (z)
              (when (boolean? z)
                (if z #t 0))))
(test-comp '(lambda (z)
              (let ([r (boolean? z)])
                (if r r 0)))
           '(lambda (z)
              (if (boolean? z) #t 0)))
(test-comp '(lambda (x) (if (let ([r (something)])
                              (if r r (something-else)))
                            (a1)
                            (a2)))
           '(lambda (x) (if (if (something) #t (something-else))
                            (a1)
                            (a2))))

(test-comp '(lambda (x) (if (if x x (something-else))
                            (a1)
                            (a2)))
           '(lambda (x) (if (if x #t (something-else))
                             (a1)
                             (a2))))

(test-comp '(lambda (x) (if x (something-else) x))
           '(lambda (x) (if x (something-else) #f)))
                             
(test-comp '(lambda (x) (if (if x #t #f)
                            (a1)
                            (a2)))
           '(lambda (x) (if x
                            (a1)
                            (a2))))

(test-comp '(lambda (x) (if (if x x x)
                            (a1)
                            (a2)))
           '(lambda (x) (if x
                            (a1)
                            (a2))))

(test-comp '(lambda (x) (if (pair? x) #t #f))
           '(lambda (x) (pair? x)))

(test-comp '(lambda (x) (let ([r (something)])
                          (if r #t (something-else))))
           '(lambda (x) (if (something) #t (something-else))))

(let ([test-if-if-reduction
       (lambda (dup)
         (test-comp `(lambda (x y z) (if (if x y #f) z ,dup))
                    `(lambda (x y z) (if x (if y z ,dup) ,dup)))
         (test-comp `(lambda (x y z) (if (if x #f y) z ,dup))
                    `(lambda (x y z) (if x ,dup (if y z ,dup))))
         (test-comp `(lambda (x y z) (if (if x y #t) ,dup z))
                    `(lambda (x y z) (if x (if y ,dup z) ,dup)))
         (test-comp `(lambda (x y z) (if (if x #t y) ,dup z))
                    `(lambda (x y z) (if x ,dup (if y ,dup z)))))])
  (test-if-if-reduction 1)
  (test-if-if-reduction ''x)
  (test-if-if-reduction "x")
  (test-if-if-reduction #"x")
  (test-if-if-reduction #t)
  (test-if-if-reduction #f)
  (test-if-if-reduction 'car)
  (test-if-if-reduction 'map))

(let ([test-pred-implies-val
       (lambda (pred? val)
         (test-comp `(lambda (x) (if (,pred? x) ,val 0))
                    `(lambda (x) (if (,pred? x) x 0)))
         (test-comp `(lambda (x) (eq? x ,val))
                    `(lambda (x) (,pred? x)))
         (test-comp `(lambda (x) (eq? ,val x))
                    `(lambda (x) (,pred? x))))])
  (test-pred-implies-val 'null? 'null)
  (test-pred-implies-val 'void? '(void))
  (test-pred-implies-val 'eof-object? 'eof)
  (test-pred-implies-val 'k:true-object? '#t)  
  (test-pred-implies-val 'not '#f))
(test-comp '(lambda (f) (eq? (f) (begin (newline) null)))
           '(lambda (x) (begin (newline) null) (null? (f)))
           #f)
(test-comp '(lambda (x) (if (null? x) 1 0) null)
           '(lambda (x) (if (null? x) 1 0) x)
           #f)
(test-comp '(lambda (x) (if x 1 (list #f)))
           '(lambda (x) (if x 1 (list x))))


(test-comp '(lambda (x) (let ([r (something)])
                          (r)))
           '(lambda (x) ((something))))
(test-comp '(lambda (x) (let ([r (something)])
                          (r (something-else))))
           '(lambda (x) ((something) (something-else))))
(test-comp '(lambda (x z) (let ([r (something)])
                            (z r)))
           '(lambda (x z) (z (something))))
(test-comp '(lambda (x) (let ([r (something)])
                          (r (something-else) 1 2)))
           '(lambda (x) ((something) (something-else) 1 2)))
(test-comp '(lambda (x z) (let ([r (something)])
                            (with-continuation-mark r z (something-else))))
           '(lambda (x z) (with-continuation-mark (something) z (something-else))))
(test-comp '(lambda (x z) (let ([r (something)])
                            (with-continuation-mark z r (something-else))))
           '(lambda (x z) (with-continuation-mark z (something) (something-else))))
(test-comp '(lambda (x z) (let ([r (something)])
                            (set! z r)))
           '(lambda (x z) (set! z (something))))
(test-comp '(lambda (x z) (let ([r (something)])
                            (call-with-values (lambda () (z)) r)))
           '(lambda (x z) (call-with-values (lambda () (z)) (something))))

;; Don't move closure allocation:
(test-comp '(lambda (z) (let ([r (lambda () z)])
                          (lambda () r)))
           '(lambda (z) (lambda ()
                          (lambda () z)))
           #f)
;; Don't move omittable expressions that keep a reference:
(test-comp '(lambda (z) (let ([r (pair? z)])
                          (lambda () r)))
           '(lambda (z) (lambda ()
                          (lambda () (pair? z))))
           #f)
(test-comp '(lambda (z) (when (list? z)
                          (let ([r (list->vector z)])
                            (lambda () r))))
           '(lambda (z) (when (list? z)
                          (lambda () (list->vector z))))
           #f)


(test-comp '(if (let ([z (random)]) null) 1 2)
           '(if (let ([z (random)]) #t) 1 2))

(test-comp '(if (if (list? (cons 1 null)) null (void)) 1 2)
           '1)

(test-comp '(if (if (list? (cons 1 null)) 7 8) 1 2)
           '1)

(test-comp '(if (if (list? (cons 1 null)) #t #t) 1 2)
           '1)

(test-comp '(let ([x '(7)])
              (list x x (if (if (list? (cons 1 null)) x 3) 0 1)))
           '(let ([x '(7)])
              (list x x 0)))
(test-comp '(lambda (x)
              (cons (car x)
                    (if (let ([y (random)]) (pair? x)) 1 2)))
           '(lambda (x)
              (cons (car x)
                    (let ([y (random)]) 1))))
(test-comp '(lambda (x)
              (cons (car x)
                    (if (begin (random) (random) (pair? x)) 1 2)))
           '(lambda (x)
              (cons (car x)
                    (begin (random) (random) 1))))
(test-comp '(lambda (x)
              (cons (car x)
                    (if (begin (random) (random) (box? x)) 1 2)))
           '(lambda (x)
              (cons (car x)
                    (begin (random) (random) 2))))
(test-comp '(lambda (x)
              (if (begin (random) (random) (cons x x)) 1 2))
           '(lambda (x)
              (begin (random) (random) 1)))
(test-comp '(lambda (x)
              (if (begin (random) (random) (not (cons x x))) 1 2))
           '(lambda (x)
              (begin (random) (random) 2)))
(test-comp '(lambda (x)
              (if (let ([n (random 9)]) (random n) (random n) (cons (car x) x)) 1 2))
           '(lambda (x)
              (let ([n (random 9)]) (random n) (random n) (car x) 1)))
(test-comp '(lambda (x)
              (if (let ([n (random 9)]) (random n) (random n) (not (cons (car x) x))) 1 2))
           '(lambda (x)
              (let ([n (random 9)]) (random n) (random n) (car x) 2)))

(test-comp '(lambda (x)
              (if (let ([n (random 9)]) (random n) (random n) (cons (car x) x)) (cons x 1) (cons x 2)))
           '(lambda (x)
              (let ([n (random 9)]) (random n) (random n) (car x) (cons x 1))))
(test-comp '(lambda (x)
              (if (let ([n (random 9)]) (random n) (random n) (not (cons (car x) x))) (cons x 1) (cons x 2)))
           '(lambda (x)
              (let ([n (random 9)]) (random n) (random n) (car x) (cons x 2))))

(test-comp '(lambda (x)
              (if (begin (random) (not (begin (random) x))) 1 2))
           '(lambda (x)
              (if (begin (random) (random) x) 2 1)))


(test-comp '(lambda (y)
              (let ([f (lambda (x) x)])
                (if f
                    (+ y 1)
                    (- y 1))))
           '(lambda (y)
              (+ y 1)))

(test-comp '(module m racket/base
              (define (f x) x)
              (define (g y)
                (if f
                    (+ y 1)
                    (- y 1))))
           '(module m racket/base
              (define (f x) x)
              (define (g y)
                (+ y 1))))

(test-comp '(module m racket/base
              (struct p (x y) #:omit-define-syntaxes)
              (define (g y)
                (if p-x
                    (+ y 1)
                    (- y 1))))
           '(module m racket/base
              (struct p (x y) #:omit-define-syntaxes)
              (define (g y)
                (+ y 1))))


(test-comp '(let ()
             (define (f x)
               (procedure-specialize
                (lambda (y) (+ x y))))
             ((f 10) 12))
           '22)

(test-comp '(let ()
             (define (f x)
               (procedure-specialize
                (lambda (y) (+ x y))))
             (procedure? (f 10)))
           '#t)

(test-comp '(let ([f (procedure-specialize
                      (lambda (y) (+ 1 y)))])
             (list f (procedure-arity-includes? f 1)))
           '(let ([f (procedure-specialize
                      (lambda (y) (+ 1 y)))])
             (list f #t)))

(test-comp '(values 10)
           10)
(test-comp '(let ([x (values 10)])
              (values x))
           10)
(test-comp '(let ([x (random)])
              (values x))
           '(let ([x (random)])
              x))
(test-comp '(let ([x (+ (cons 1 2) 0)])
              (values x))
           '(let ([x (+ (cons 1 2) 0)])
              x))
(test-comp '(let ([x (+ (random) 3)])
              (values x))
           '(let ([x (+ (random) 3)])
              x))
(test-comp '(lambda (x)
              (begin (random) x))
           '(lambda (x)
              (values (begin (random) x))))
(test-comp '(lambda (x f)
              (letrec ([z (lambda () z)]) (f z) x))
           '(lambda (x f)
              (values (letrec ([z (lambda () z)]) (f z) x))))
(test-comp '(lambda (x f)
              (letrec ([z (lambda () z)]) (f z) z))
           '(lambda (x f)
              (values (letrec ([z (lambda () z)]) (f z) z))))
(test-comp '(lambda (f)
              (let ([x (f)]) (list x x)))
           '(lambda (f)
              (let ([x (values (f))]) (list x x))))
(test-comp '(lambda (f)
              (if (f) 0 1))
           '(lambda (f)
              (if (values (f)) 0 1)))

(test-comp '(let ([x (+ (cons 1 2) 3)])
              (- x 8))
           '(- (+ (cons 1 2) 3) 8))
(test-comp '(let ([x (+ (random) 3)])
              (- x 8))
           '(- (+ (random) 3) 8))

(test-comp '(let ([x (peek-char)])
              (cons x 10))
           '(cons (peek-char) 10))

(test-comp '(let ([x (peek-char)])
              (let ([y x])
                (cons y 10)))
           '(cons (peek-char) 10))

(test-comp '(lambda (x)
              (let ([y x])
                (cons y 10)))
           '(lambda (x) (cons x 10)))

(test-comp '(lambda (x)
              (let ([y x])
                (cons y y)))
           '(lambda (x) (cons x x)))

(test-comp '(let ([f (lambda (x)
                       (let ([y x])
                         (cons y y)))])
              (f (peek-char)))
           '(let ([y (peek-char)])
              (cons y y)))

(test-comp '(let ([g (lambda (f)
                       ;; Try to get uses of `z' replaced by `x',
                       ;; but before `x' and `y' are split apart.
                       ;; Single-use tracking of `x' can go wrong.
                       (let-values ([(x y) (f (cons 1 2)
                                              (cons 3 4))])
                         (let ([z x])
                           (list z z y))))])
              (g values))
           '(let ([x (cons 1 2)]
                  [y (cons 3 4)])
              (list x x y)))

(test-comp '(let ([g (lambda (f)
                       (letrec-values ([(x y) (f (cons 1 2)
                                                 (cons 3 4))])
                         (let ([z x])
                           (list z z y))))])
              (g values))
           '(let ([g (lambda (f)
                       (letrec-values ([(x y) (f (cons 1 2)
                                                 (cons 3 4))])
                         (list x x y)))])
              (g values)))

(test-comp '(letrec-values ([(g f) (values
                                    ;; The `let`s provide names:
                                    (let ([g (lambda () (f))]) g)
                                    (let ([f (lambda () (g))]) f))])
              (g))
           '(letrec ([g (lambda () (f))]
                     [f (lambda () (g))])
              (g)))

;; Since `list` is effect-free, `f` should not be checked for
;; undefined. I don't see a way to test that, though.
#;
(letrec ([f (list
             (let ([f (lambda () f)]) f))])
  (car f))

;; Make sure `values` splitting doesn't reorder expressions
(let ([f (lambda (z)
           (let-values ([(a b) (values (list (z 1)) (list (z 2)))])
             (list a b)))])
  (set! f f)
  (let ([v 0])
    (test '((1) (2)) f (lambda (n) (set! v n) n))
    (test 2 values v)))

;; Make sure `values` splitting doesn't use wrong clock values
;; leading to reordering:
(test-comp '(lambda (p)
             (define-values (x y) (values (car p) (cdr p)))
             (values y x))
           '(lambda (p)
             (values (unsafe-cdr p) (car p)))
           #f)
(test-comp '(lambda (p)
             (define-values (x y) (values (car p) (cdr p)))
             (values y x))
           '(lambda (p)
             (let ([x (car p)])
               (values (unsafe-cdr p) x))))

(test-comp '(lambda (z)
             ;; Moving `(list z)` after `(list (z 2))` is not allowed
             ;; in case `(z 2)` captures a continuation:
             (let-values ([(a b) (values (list z) (list (z 2)))])
               (list b a)))
           '(lambda (z)
              (list (list (z 2)) (list z)))
           #f)
(test-comp '(lambda (z)
              (let-values ([(a b) (values (list (z 2)) (list z))])
                (list a a b)))
           '(lambda (z)
             (let ([a (list (z 2))])
               (list a a (list z)))))

;; It would be nice if the optimizer could do these two, but because it
;; involves temporarily reordering `(list z)` and `(list (z 2))`
;; (which is not allowed in case `(z 2)` captures a continuation),
;; the optimizer currently cannot manage it:
#;
(test-comp '(lambda (z)
              (let-values ([(a b) (values (list (z 2)) (list z))])
                (list a b)))
           '(lambda (z)
             (list (list (z 2)) (list z))))
#;
(test-comp '(lambda (z)
              (let-values ([(a b) (values (list z) (list (z 2)))])
                (list a b)))
           '(lambda (z)
             (list (list z) (list (z 2)))))

(test-comp '(module m racket/base
             ;; Reference to a ready module-level variable shouldn't
             ;; prevent let-values splitting
             (#%plain-module-begin
              (define z (random))
              (define (f)
                (let-values ([(a b) (values (cons 1 z) (cons 2 z))])
                  (list a b)))))
           '(module m racket/base
             ;; Reference to a ready module-level variable shouldn't
             ;; prevent let-values splitting
             (#%plain-module-begin
              (define z (random))
              (define (f)
                (list (cons 1 z) (cons 2 z))))))

(test-comp '(module m racket/base
             ;; Don't reorder references to a mutable variable
             (#%plain-module-begin
              (define z (random))
              (define (f)
                (let-values ([(a b) (values (cons 1 z) (cons 2 z))])
                  (list b a)))
              (set! z 5)))
           '(module m racket/base
             ;; Reference to a ready module-level variable shouldn't
             ;; prevent let-values splitting
             (#%plain-module-begin
              (define z (random))
              (define (f)
                (list (cons 2 z) (cons 1 z)))
              (set! z 5)))
           #f)

(test-comp '(lambda (z)
             ;; It's ok to reorder unsafe operations relative
             ;; to each other:
             (let ([x (unsafe-fx+ z z)]
                   [y (unsafe-fx- z z)])
               (- y x)))
           '(lambda (z)
             (- (unsafe-fx- z z) (unsafe-fx+ z z))))

(test-comp '(lambda (z)
             ;; It's not ok to move a safe operation past an
             ;; unsafe one:
             (let ([x (car z)])
               (+ (unsafe-car z) x)))
           '(lambda (z)
             (+ (unsafe-car z) (car z)))
           #f)

(test-comp '(lambda (z v)
             ;; It's ok to move an unsafe operation past a
             ;; safe one:
             (let ([x (unsafe-car v)])
               (+ (car z) x)))
           '(lambda (z v)
             (+ (car z) (unsafe-car v))))

;; Ok to reorder arithmetic that will not raise an error:
(test-comp '(lambda (x y)
             (if (and (real? x) (real? y))
                 (let ([w (+ x y)]
                       [z (- y x)])
                   (+ z w))
                 (void)))
           '(lambda (x y)
             (if (and (real? x) (real? y))
                 (+ (- y x) (+ x y))
                 (void))))

(parameterize ([compile-context-preservation-enabled
                ;; Avoid different amounts of unrolling
                #t])
  ;; Inference of loop variable as number should allow
  ;; additions to be reordered:
  (test-comp '(lambda ()
               (let loop ([n 0] [m 9])
                 (let ([a (+ n 9)]
                       [b (+ m 10)])
                   (loop b a))))
             '(lambda ()
               (let loop ([n 0] [m 9])
                 (loop (+ m 10) (+ n 9))))))

;; Don't reorder pass a `values` with the wrong nunmber of arguments.
;; `values` is the only primitive that is omitable and may return multiple values.
(test-comp '(lambda (b)
              (let ([x (unbox b)])
                (+ (values 2 2) x)))
           '(lambda (b)
              (+ (values 2 2) (unbox b)))
           #f)

(test-comp '(lambda (z)
              (let-values ([(x y)
                            (if z
                                (values z (list z))
                                (values z (box z)))])
                (list x y)))
           '(lambda (z)
              (list z (if z (list z) (box z)))))

(test-comp '(lambda (z)
              (let-values ([(x y)
                            (if z
                                (values 1 1)
                                (let ([more (list z z)])
                                  (values 4 more)))])
                (list x y)))
           '(lambda (z)
              (let ([r (if z 1 (list z z))])
                (list (if z 1 4) r))))

(test-comp '(lambda (a b c f)
              (let ((d (if a
                           a
                           b)))
                (let ((e
                       (if b
                           c
                           (if (= f 90000)
                               #f
                               (add1 c)))))
                  (values d e))))
           '(lambda (a b c f)
              (values (if a a b)
                      (if b c (if (= f 90000)
                                  #f
                                  (add1 c))))))

;; Don't move a branch that selects a variable past an
;; expression that can inspect space consumption:
(test-comp '(lambda (a b c f)
              (let ((d (if a b c)))
                (f)
                d))
           '(lambda (a b c f)
             (f)
             (if a b c))
           #f)

(test-comp '(lambda (x y q)
              (let ([z (+ x y)])
                (list (if q x y) z)))
           '(lambda (x y q)
              (list (if q x y) (+ x y))))

(test-comp '(lambda (x y)
              (let ([z (car y)])
                (if x x z)))
           '(lambda (x y)
              (if x x (car y)))
           #f)

(test-comp '(let-values ([(x y) (values 1 2)])
              (+ x y))
           3)

(test-comp '(let-values ([() (values)])
              5)
           5)

(test-comp '(let-values ([() (values)])
              (lambda () x))
           '(lambda () x))

(test-comp '(letrec-values ([() (values)])
              5)
           5)

(test-comp '(let-values ([() (values)]
                         [(x) 10])
              x)
           10)

(test-comp '(letrec-values ([() (values)]
                            [(x) 10])
              x)
           10)

(test-comp '(letrec-values ([(x) 10]
                            [() (values)])
              x)
           10)

(test-comp '(let-values ([(x) 10]
                         [() (values)])
              x)
           10)

(test-comp '(letrec ([x 3]
                     [f (lambda (y) x)])
              f)
           '(letrec ([f (lambda (y) 3)])
              f))

(test-comp '(letrec ([x 3]
                     [f (lambda (y) x)])
              (f 10))
           3)

(test-comp '(letrec ([f (lambda (y) (f y))])
              3)
           3)

(test-comp '(letrec ([len (lambda (l)
                            (if (null? l)
                                0
                                (len (cdr l))))])
              (len null))
           0)

(test-comp '(letrec ([foo (lambda ()
                            (set! foo 10))])
              0)
           0)

(test-comp '(letrec ([foo (lambda () 12)]
                     [goo (lambda () foo)])
              goo)
           '(let* ([foo (lambda () 12)]
                   [goo (lambda () foo)])
              goo))

(test-comp '(let* ([foo (lambda () 12)]
                   [goo (lambda () foo)])
              11)
           11)

(test-comp '(letrec ([foo (lambda () 12)]
                     [goo (lambda () foo)])
              11)
           11)

(test-comp '(letrec ([goo (lambda () foo)]
                     [foo (lambda () goo)])
              15)
           15)

(test-comp '(letrec ((c (λ () T))
                     (T (λ () c))
                     (E (λ () T)))
             5)
           5)

(parameterize ([compile-context-preservation-enabled 
                ;; Avoid different amounts of unrolling
                #t])
  (test-comp '(letrec ((even
                        (let ([unused 6])
                          (let ([even (lambda (x) (if (zero? x) #t (even (sub1 x))))])
                            (values even)))))
                (even 10000))
             '(letrec ((even (lambda (x) (if (zero? x) #t (even (sub1 x))))))
                (even 10000))))

(test-comp '(lambda (a)
              (define (x) (x))
              (displayln a)
              (define (y) (y))
              (list (x) (y)))
           '(lambda (a)
              (letrec ([x (lambda () (x))])
                (displayln a)
                (letrec ([y (lambda () (y))])
                  (list (x) (y))))))

(test-comp '(lambda (a)
              (define (x) (x))
              (define (y) (y))
              (list x y))
           '(lambda (a)
              (letrec ([x (lambda () (x))])
                (letrec ([y (lambda () (y))])
                  (list x y)))))

(test-comp '(lambda (a)
              (define (x) (x))
              (displayln x)
              (define (y) (y))
              (list x y))
           '(lambda (a)
              (letrec ([x (lambda () (x))])
                (displayln x)
                (letrec ([y (lambda () (y))])
                  (list x y)))))

(parameterize ([compile-context-preservation-enabled 
                ;; Avoid different amounts of unrolling
                #t])
  (test-comp '(lambda (a)
                (define (x) (y))
                (define h (+ a a))
                (define (y) (x))
                (list (x) (y) h))
             '(lambda (a)
                (define h (+ a a))
                (letrec ([x (lambda () (y))]
                         [y (lambda () (x))])
                  (list (x) (y) h)))))

(test-comp '(lambda (f a)
              (define x (f y))
              (define y (m))
              (define-syntax-rule (m) 10)
              (f "hi!\n")
              (define z (f (lambda () (+ x y a))))
              (define q (p))
              (define p (q))
              (list x y z))
           '(lambda (f a)
              (letrec ([x (f y)]
                       [y 10])
                (f "hi!\n")
                (let ([z (f (lambda () (+ x y a)))])
                  (letrec ([q (p)]
                           [p (q)])
                    (list x y z))))))

(test-comp '(lambda (f a)
              (#%stratified-body
               (define x (f y))
               (define y (m))
               (define-syntax-rule (m) 10)
               (define z (f (lambda () (+ x y a))))
               (define q (p))
               (define p (q))
               (list x y z)))
           '(lambda (f a)
              (letrec-values ([(x) (f y)]
                              [(y) 10]
                              [(z) (f (lambda () (+ x y a)))]
                              [(q) (p)]
                              [(p) (q)])
                (list x y z))))

(test-comp '(lambda (f a)
              (letrec ([y (if (zero? a)
                              (error "no")
                              8)]
                       [f (lambda (x) (f x))])
                f))
           '(lambda (f a)
              (let ([y (if (zero? a)
                           (error "no")
                           8)])
                (letrec ([f (lambda (x) (f x))])
                  f))))

(test-comp #t
           '(procedure? add1))
(test-comp '(lambda () #t)
           '(lambda () (procedure? add1)))
(test-comp #t
           '(procedure? (lambda (x) x)))
(test-comp '(lambda () #t)
           '(lambda () (procedure? (lambda (x) x))))
(test-comp #f
           '(pair? (lambda (x) x)))
(test-comp '(lambda () #f)
           '(lambda () (pair? (lambda (x) x))))
(test-comp '(let ([f (lambda (x) x)])
              (if (procedure? f)
                  (list f)
                  88))
           '(let ([f (lambda (x) x)])
              (list f)))
(test-comp '(let ([f (lambda (x) x)])
              (list
               f
               f
               (procedure? f)
               (procedure? (begin (random) f))
               (procedure? (letrec ([x (lambda (t) x)]) (x x) f))))
           '(let ([f (lambda (x) x)])
              (list
               f
               f
               #t
               (begin (random) #t)
               (letrec ([x (lambda (t) x)]) (x x) #t))))

(test-comp '(letrec ([f (case-lambda 
                         [(x) x]
                         [(x y) (f (+ x y))])])
	      (f 10))
	   '10)

(test-comp '(lambda (x) #f)
           '(lambda (x) (pair? (if x car cdr))))
(test-comp '(lambda (x) #t)
           '(lambda (x) (procedure? (if x car cdr))))
(test-comp '(lambda (x) #t)
           '(lambda (x) (fixnum? (if x 2 3))))
(test-comp '(lambda (x) #f)
           '(lambda (x) (procedure? (if x 2 3))))

(test-comp '(lambda ()
              (let ([is3 (lambda () 3)])
                (letrec ([g (lambda () 3)]
                         [is0 (lambda () 0)])
                  g)))
           '(lambda ()
              (let ([is3 (lambda () 3)])
                (letrec ([g (lambda () (is3))]
                         [is0 (lambda () 0)])
                  g))))

(test-comp '(lambda (f)
              (let ([x (f)])
                (letrec ([g (lambda () (list x #t))]
                         [dummy (lambda () 0)])
                  g)))
           '(lambda (f)
              (let ([x (f)])
                (letrec ([g (lambda () (list x (procedure? x)))]
                         [dummy (lambda () 0)])
                  g)))
           #f)

(test-comp '(lambda (f)
              (let ([x (f)])
                (car x)
                (letrec ([g (lambda () (list x #t))]
                         [dummy (lambda () 0)])
                  g)))
           '(lambda (f)
              (let ([x (f)])
                (car x)
                (letrec ([g (lambda () (list x (pair? x)))]
                         [dummy (lambda () 0)])
                  g))))

(test-comp '(lambda (f)
              (let ([x (f)])
                (letrec ([g (lambda () (car x) (list x #t))]
                         [dummy (lambda () 0)])
                  g)))
           '(lambda (f)
              (let ([x (f)])
                (letrec ([g (lambda () (car x) (list x (pair? x)))]
                         [dummy (lambda () 0)])
                  g))))

(test-comp '(module m racket/base
              (void 10))
           '(module m racket/base))

(test-comp '(module m racket/base
              (void (quote-syntax unused!)))
           '(module m racket/base))

(test-comp '(module m '#%kernel
              (values 1 2))
           '(module m '#%kernel))

(test-comp '(module m racket/base
              (printf "pre\n")
              (void 10))
           '(module m racket/base
              (printf "pre\n")))

(test-comp '(module out racket/base
              (module in racket/base
                (provide inlinable-function)
                (define inlinable-function (lambda (x) (list 1 x 3))))
              (require 'in)
              (lambda () (display (inlinable-function 2)) (list 1 2 3)))
           '(module out racket/base  
              (module in racket/base
                (provide inlinable-function)
                (define inlinable-function (lambda (x) (list 1 x 3))))
              (require 'in)
              (lambda () (display (inlinable-function 2)) (inlinable-function 2))))

(test-comp '(module out racket/base
              (module in racket/base
                (provide inlinable-function)
                (define inlinable-function (lambda (x) (list 1 x 3))))
              (require 'in)
              (lambda () (display (procedure? inlinable-function)) #t))
           '(module out racket/base  
              (module in racket/base
                (provide inlinable-function)
                (define inlinable-function (lambda (x) (list 1 x 3))))
              (require 'in)
              (lambda () (display (procedure? inlinable-function)) (procedure? inlinable-function))))

(let ([try-equiv
       (lambda (extras)
         (lambda (a b)
           (test-comp `(module m racket/base
                         (define (f x)
                           (apply x ,@extras ,a)))
                      `(module m racket/base
                         (define (f x)
                           (x ,@extras ,@b))))))])
  (map (lambda (try-equiv)
         (try-equiv '(list) '())
         (try-equiv '(quote ()) '())
         (try-equiv '(list 1) '(1))
         (try-equiv '(quote (1)) '(1))
         (try-equiv '(list 1 2) '(1 2))
         (try-equiv '(quote (1 2)) '(1 2))
         (try-equiv '(list 1 2 3) '(1 2 3))
         (try-equiv '(quote (1 2 3)) '(1 2 3))
         (try-equiv '(list 1 2 3 4 5 6) '(1 2 3 4 5 6))
         (try-equiv '(quote (1 2 3 4 5 6)) '(1 2 3 4 5 6)))
       (list
        (try-equiv null)
        (try-equiv '(0))
        (try-equiv '(0 1)))))
         
(test-comp '(module m racket/base
              (define (q x)
                ;; Single-use bindings should be inlined always:
                (let* ([a (lambda (x) (+ x 10))]
                       [b (lambda (x) (+ 1 (a x)))]
                       [c (lambda (x) (+ 1 (b x)))]
                       [d (lambda (x) (+ 1 (c x)))]
                       [e (lambda (x) (+ 1 (d x)))]
                       [f (lambda (x) (+ 1 (e x)))]
                       [g (lambda (x) (+ 1 (f x)))]
                       [h (lambda (x) (+ 1 (g x)))]
                       [i (lambda (x) (+ 1 (h x)))]
                       [j (lambda (x) (+ 1 (i x)))]
                       [k (lambda (x) (+ 1 (j x)))])
                  (k x))))
           '(module m racket/base
              (define (q x)
                (+ 1 (+ 1 (+ 1 (+ 1 (+ 1 (+ 1 (+ 1 (+ 1 (+ 1 (+ 1 (+ x 10))))))))))))))

(let ([check (lambda (proc arities non-arities)
               (test-comp `(procedure? ,proc)
                            #t)
               (test-comp `(module m racket/base
                             (define f ,proc)
                             (print (procedure? f)))
                          `(module m racket/base
                             (define f ,proc)
                             (print #t)))
               (test-comp `(procedure-arity-includes? ,proc -1)
                          #t
                          #f)
               (test-comp `(procedure-arity-includes? ,proc -1)
                          #f
                          #f)
               (for-each
                (lambda (a)
                  (test-comp `(procedure-arity-includes? ,proc ,a)
                             #t)
                  (test-comp `(module m racket/base
                                (define f ,proc)
                                (print (procedure-arity-includes? f ,a)))
                             `(module m racket/base
                                (define f ,proc)
                                (print #t))))
                arities)
               (for-each
                (lambda (a)
                  (test-comp `(procedure-arity-includes? ,proc ,a)
                             #f)
                  (test-comp `(module m racket/base
                                (define f ,proc)
                                (print (procedure-arity-includes? f ,a)))
                             `(module m racket/base
                                (define f ,proc)
                                (print #f))))
                non-arities))])
  (check '(lambda (x) x) '(1) '(0 2 3))
  (check '(lambda (x y) x) '(2) '(0 1 3))
  (check '(lambda (x . y) x) '(1 2 3) '(0))
  (check '(case-lambda [() 1] [(x y) x]) '(0 2) '(1 3))
  (check '(lambda (x [y #f]) y) '(1 2) '(0 3))
  (check 'integer? '(1) '(0 2 3))
  (check 'cons '(2) '(0 1 3))
  (check 'list '(0 1 2 3) '()))

(test-comp '(lambda () (primitive? car))
           '(lambda () #t))
(test-comp '(lambda () (procedure-arity-includes? car 1))
           '(lambda () #t))
(test-comp '(lambda () (procedure-arity-includes? car 2))
           '(lambda () #f))
(test-comp '(lambda () (procedure-arity-includes? (begin (random) car) 1))
           '(lambda () (random) #t))
(test-comp '(lambda () (procedure-arity-includes? (begin (random) car) 2))
           '(lambda () (random) #f))
(test-comp '(lambda () (procedure-arity-includes? (begin (random) car) 1))
           '(lambda () #t)
           #f)
(test-comp '(lambda () (procedure-arity-includes? (begin (random) car) 2))
           '(lambda () #f)
           #f)

(test-comp '(lambda ()
              (let ([l '(1 2)])
                (car l)))
           '(lambda () 1))

(let ([test-dropped
       (lambda (cons-name . args)
         (test-comp `(let ([x 5])
                       (let ([y (,cons-name ,@args)])
                         x))
                    5))])
  (test-dropped 'cons 1 2)
  (test-dropped 'mcons 1 2)
  (test-dropped 'box 1)
  (let ([test-multi
         (lambda (cons-name)
           (test-dropped cons-name 1 2)
           (test-dropped cons-name 1 2 3)
           (test-dropped cons-name 1)
           (unless (eq? cons-name 'list*)
             (test-dropped cons-name)))])
    (test-multi 'list)
    (test-multi 'list*)
    (test-multi 'vector)
    (test-multi 'vector-immutable)))
(test-comp `(let ([x 5])
              (let ([y (list*)])
                x))
           5
           #f)

(let ([test-pred
       (lambda (pred-name)
         (test-comp `(lambda (z)
                       (let ([x ',pred-name])
                         (let ([y (,pred-name z)])
                           x)))
                    `(lambda (z) ',pred-name))
         (test-comp `(lambda (z)
                       (boolean? (,pred-name z)))
                    `(lambda (z) (,pred-name z) #t)))])
  (test-pred 'pair?)
  (test-pred 'mpair?)
  (test-pred 'list?)
  (test-pred 'k:list-pair?)
  (test-pred 'vector?)
  (test-pred 'box?)
  (test-pred 'number?)
  (test-pred 'real?)
  (test-pred 'complex?)
  (test-pred 'rational?)
  (test-pred 'integer?)
  (test-pred 'exact-integer?)
  (test-pred 'exact-nonnegative-integer?)
  (test-pred 'exact-positive-integer?)
  (test-pred 'inexact-real?)
  (test-pred 'fixnum?)
  (test-pred 'flonum?)
  (test-pred 'single-flonum?)
  (test-pred 'null?)
  (test-pred 'void?)
  (test-pred 'symbol?)
  (test-pred 'keyword?)
  (test-pred 'string?)
  (test-pred 'bytes?)
  (test-pred 'path?)
  (test-pred 'char?)
  (test-pred 'k:interned-char?)
  (test-pred 'boolean?)
  (test-pred 'chaperone?)
  (test-pred 'impersonator?)
  (test-pred 'procedure?)
  (test-pred 'eof-object?)
  (test-pred 'immutable?)
  (test-pred 'not)
  (test-pred 'k:true-object?))

(let ([test-implies
       (lambda (pred1 pred2 [val '=>])
         (cond
           [(eq? val '=>) 
            (test-comp `(lambda (z) (when (,pred1 z) (,pred2 z)))
                       `(lambda (z) (when (,pred1 z) #t)))
            (test-comp `(lambda (z) (when (,pred2 z) (,pred1 z)))
                       `(lambda (z) (when (,pred2 z) #t))
                       #f)
            (test-comp `(lambda (z) (when (,pred2 z) (,pred1 z)))
                       `(lambda (z) (when (,pred2 z) #f))
                       #f)]
           [(eq? val '!=) 
            (test-comp `(lambda (z) (when (,pred1 z) (,pred2 z)))
                       `(lambda (z) (when (,pred1 z) #f)))
            (test-comp `(lambda (z) (when (,pred2 z) (,pred1 z)))
                       `(lambda (z) (when (,pred2 z) #f)))]
            [(eq? val '?) 
            (test-comp `(lambda (z) (when (,pred1 z) (,pred2 z)))
                       `(lambda (z) (when (,pred1 z) #t))
                       #f)
            (test-comp `(lambda (z) (when (,pred1 z) (,pred2 z)))
                       `(lambda (z) (when (,pred1 z) #f))
                       #f)
            (test-comp `(lambda (z) (when (,pred2 z) (,pred1 z)))
                       `(lambda (z) (when (,pred2 z) #t))
                       #f)
            (test-comp `(lambda (z) (when (,pred2 z) (,pred1 z)))
                       `(lambda (z) (when (,pred2 z) #f))
                       #f)]
            [else
              (test '= (list pred1 pred2 val) 'bad-option)]))])

  (test-implies 'null? 'k:list-pair? '!=)
  (test-implies 'null? 'pair? '!=)
  (test-implies 'null? 'list?)
  (test-implies 'k:list-pair? 'pair?)
  (test-implies 'k:list-pair? 'list?)
  (test-implies 'list? 'pair? '?)
  (test-implies 'k:interned-char? 'char?)
  (test-implies 'not 'boolean?)
  (test-implies 'k:true-object? 'boolean?)
)

(test-comp '(lambda (z)
              (when (and (list? z)
                         (pair? z))
                (k:list-pair? z)))
           '(lambda (z)
              (when (and (list? z)
                         (pair? z))
                #t)))
(test-comp '(lambda (z)
              (when (and (list? z)
                         (not (null? z)))
                (k:list-pair? z)))
           '(lambda (z)
              (when (and (list? z)
                         (not (null? z)))
                #t)))
(test-comp '(lambda (z)
              (when (and (list? z)
                         (not (pair? z)))
                (null? z)))
           '(lambda (z)
              (when (and (list? z)
                         (not (pair? z)))
                #t)))
(test-comp '(lambda (z)
              (when (and (list? z)
                         (not (k:list-pair? z)))
                (null? z)))
           '(lambda (z)
              (when (and (list? z)
                         (not (k:list-pair? z)))
                #t)))
(test-comp '(lambda (z)
              (when (and (boolean? z)
                         (not (k:true-object? z)))
                (not z)))
           '(lambda (z)
              (when (and (boolean? z)
                         (not (k:true-object? z)))
                #t)))
(test-comp '(lambda (z)
              (when (and (boolean? z)
                         (not (not z)))
                (k:true-object? z)))
           '(lambda (z)
              (when (and (boolean? z)
                         (not (not z)))
                #t)))


(let ([test-reduce
       (lambda (pred-name expr [val #t])
         (test-comp `(list ',pred-name (,pred-name ,expr))
                    `(list ',pred-name ,val))
         (test-comp `(let ([e ,expr])
                       (list ',pred-name e e (,pred-name e)))
                    `(let ([e ,expr])
                       (list ',pred-name e e ,val))))])
  (test-reduce 'list? 0 #f)
  (test-reduce 'list? ''())
  (test-reduce 'list? ''(1))
  (test-reduce 'list? ''(1 2))
  #;(test-reduce 'list? ''(1 . 2) #f)
  (test-reduce 'list? '(list))
  (test-reduce 'list? '(list 1))
  (test-reduce 'list? '(list 1 2))
  #;(test-reduce 'list? '(cons 1 2) #f)
  (test-reduce 'list? '(cons 1 null))
  (test-reduce 'list? '(cons 1 (list 2 3)))
  (test-reduce 'list? '(cdr (list 1 2)))
  (test-reduce 'list? '(cdr (list 1)))

  (test-reduce 'null? 0 #f)
  (test-reduce 'null? ''())
  (test-reduce 'null? ''(1) #f)
  (test-reduce 'null? ''(1 2) #f)
  (test-reduce 'null? ''(1 . 2) #f)
  (test-reduce 'null? '(list))
  (test-reduce 'null? '(list 1) #f)
  (test-reduce 'null? '(list 1 2) #f)
  (test-reduce 'null? '(cons 1 2) #f)
  (test-reduce 'null? '(cons 1 null) #f)
  (test-reduce 'null? '(cons 1 (list 2 3)) #f)
  (test-reduce 'null? '(cdr (list 1 2)) #f)
  (test-reduce 'null? '(cdr (list 1)))

  (test-reduce 'pair? 0 #f)
  (test-reduce 'pair? ''() #f)
  (test-reduce 'pair? ''(1))
  (test-reduce 'pair? ''(1 2))
  (test-reduce 'pair? ''(1 . 2))
  (test-reduce 'pair? '(list) #f)
  (test-reduce 'pair? '(list 1))
  (test-reduce 'pair? '(list 1 2))
  (test-reduce 'pair? '(cons 1 2))
  (test-reduce 'pair? '(cons 1 null))
  (test-reduce 'pair? '(cons 1 (list 2 3)))
  (test-reduce 'pair? '(cdr (list 1 2)))
  (test-reduce 'pair? '(cdr (list 1)) #f)

  (test-reduce 'k:list-pair? 0 #f)
  (test-reduce 'k:list-pair? ''() #f)
  (test-reduce 'k:list-pair? ''(1))
  (test-reduce 'k:list-pair? ''(1 2))
  #;(test-reduce 'k:list-pair? ''(1 . 2) #f)
  (test-reduce 'k:list-pair? '(list) #f)
  (test-reduce 'k:list-pair? '(list 1))
  (test-reduce 'k:list-pair? '(list 1 2))
  #;(test-reduce 'k:list-pair? '(cons 1 2) #f)
  (test-reduce 'k:list-pair? '(cons 1 null))
  (test-reduce 'k:list-pair? '(cons 1 (list 2 3)))
  (test-reduce 'k:list-pair? '(cdr (list 1 2)))
  (test-reduce 'k:list-pair? '(cdr (list 1)) #f)
)

(let ([test-bin
       (lambda (bin-name)
         (test-comp `(lambda (z)
                       (let ([x ',bin-name])
                         (let ([y (,bin-name z z)])
                           x)))
                    `(lambda (z) ',bin-name)))])
  (test-bin 'eq?)
  (test-bin 'eqv?))

(for ([middle (in-list (list '(random) ; known immediate
                             '(read)))] ; could capture continuation?
      [default-same? (in-list (list #t
                                    #f))])
  (let ([test-move
         (lambda (expr [same? default-same?])
           (test-comp `(lambda (z)
                         (let ([x ,expr])
                           (let ([y ,middle])
                             (list y x))))
                      `(lambda (z)
                         (list ,middle ,expr))
                      same?))])
    (test-move '(cons 1 2))
    (test-move '(mcons 1 2))
    (test-move '(list 1))
    (test-move '(list 1 2))
    (test-move '(list 1 2 3))
    (test-move '(list* 1 2))
    (test-move '(list* 1 2 3))
    (test-move '(vector 1))
    (test-move '(vector 1 2))
    (test-move '(vector 1 2 3))
    (test-move '(box 2))
    (test-move '(box-immutable 2))
    (test-move '(cons 1 2 3) #f)
    (test-move '(mcons 1 2 3) #f)
    (test-move '(box 1 2) #f)
    (test-move '(box-immutable 1 2) #f)
    (test-move '(quote (1 2)) #t)))

;; Check move in to `else` branch where `then`
;; branch might capture a continuation
(test-comp `(lambda (z)
              (let ([x (cons 1 2)])
                (if z
                    (read)
                    x)))
           `(lambda (z)
              (if z
                  (read)
                  (cons 1 2))))
;; But not after the merge:
(test-comp `(lambda (z)
              (let ([x (cons 1 2)])
                (if z
                    (read)
                    (void))
                x))
           `(lambda (z)
              (if z
                  (read)
                  (void))
              (cons 1 2))
           #f)

(let* ([test-use-unsafe/savetype
        (lambda (pred op unsafe-op savetype)
          (test-comp `(lambda (x)
                        (if (,pred x)
                            (,op x)
                            (cdr x)))
                     `(lambda (x)
                        (if (,pred x)
                            (,unsafe-op x)
                            (cdr x))))
          (test-comp `(lambda (x)
                        (list (,op x) (,op x)))
                     `(lambda (x)
                        (list (,op x) (,unsafe-op x)))
                     savetype)
          (test-comp `(lambda (x)
                        (if (and (,pred x)
                                 (zero? (random 2)))
                              (,op x)
                              (cdr x)))
                     `(lambda (x)
                        (if (and (,pred x)
                                 (zero? (random 2)))
                            (,unsafe-op x)
                            (cdr x)))))]
       [test-use-unsafe
        (lambda (pred op unsafe-op)
          (test-use-unsafe/savetype pred op unsafe-op #t))])
  (test-use-unsafe 'pair? 'car 'unsafe-car)
  (test-use-unsafe 'pair? 'cdr 'unsafe-cdr)
  (test-use-unsafe 'mpair? 'mcar 'unsafe-mcar)
  (test-use-unsafe 'mpair? 'mcdr 'unsafe-mcdr)
  (test-use-unsafe 'box? 'unbox 'unsafe-unbox)
  (test-use-unsafe 'vector? 'vector-length 'unsafe-vector-length)
  (test-use-unsafe 'string? 'string-length 'unsafe-string-length)
  (test-use-unsafe 'bytes? 'bytes-length 'unsafe-bytes-length)
  (test-use-unsafe/savetype 'fixnum? 'bitwise-not 'unsafe-fxnot #f)
  (test-use-unsafe/savetype 'fixnum? 'fxnot 'unsafe-fxnot #f))

(let ([test-use-unsafe-fxbinary
       (lambda (op unsafe-op)
         (test-comp `(lambda (vx vy)
                       (let ([x (vector-length vx)]
                             [y (vector-length vy)])
                         (,op x y)))
                    `(lambda (vx vy)
                       (let ([x (vector-length vx)]
                             [y (vector-length vy)])
                         (,unsafe-op x y))))
         (test-comp `(lambda (x y)
                       (when (and (fixnum? x) (fixnum? y))
                         (,op x y)))
                    `(lambda (x y)
                       (when (and (fixnum? x) (fixnum? y))
                         (,unsafe-op x y))))
         (test-comp `(lambda (x y)
                       (when (and (fixnum? x) (fixnum? y) (zero? (random 2)))
                         (,op x y)))
                    `(lambda (x y)
                       (when (and (fixnum? x) (fixnum? y) (zero? (random 2)))
                         (,unsafe-op x y)))))])
  (test-use-unsafe-fxbinary 'bitwise-and 'unsafe-fxand)
  (test-use-unsafe-fxbinary 'bitwise-ior 'unsafe-fxior)
  (test-use-unsafe-fxbinary 'bitwise-xor 'unsafe-fxxor)
  (test-use-unsafe-fxbinary 'fxand 'unsafe-fxand)
  (test-use-unsafe-fxbinary 'fxior 'unsafe-fxior)
  (test-use-unsafe-fxbinary 'fxxor 'unsafe-fxxor)

  (test-use-unsafe-fxbinary '= 'unsafe-fx=)
  (test-use-unsafe-fxbinary '< 'unsafe-fx<)
  (test-use-unsafe-fxbinary '> 'unsafe-fx>)
  (test-use-unsafe-fxbinary '<= 'unsafe-fx<=)
  (test-use-unsafe-fxbinary '>= 'unsafe-fx>=)
  (test-use-unsafe-fxbinary 'min 'unsafe-fxmin)
  (test-use-unsafe-fxbinary 'max 'unsafe-fxmax)

  (test-use-unsafe-fxbinary 'fx= 'unsafe-fx=)
  (test-use-unsafe-fxbinary 'fx< 'unsafe-fx<)
  (test-use-unsafe-fxbinary 'fx> 'unsafe-fx>)
  (test-use-unsafe-fxbinary 'fx<= 'unsafe-fx<=)
  (test-use-unsafe-fxbinary 'fx>= 'unsafe-fx>=)
  (test-use-unsafe-fxbinary 'fxmin 'unsafe-fxmin)
  (test-use-unsafe-fxbinary 'fxmax 'unsafe-fxmax))

(test-comp '(lambda (vx)
              (let ([x (vector-length vx)])
                  (zero? x)))
           '(lambda (vx)
              (let ([x (vector-length vx)])
                (unsafe-fx= x 0))))
(test-comp '(lambda (x)
              (when (fixnum? x)
                (zero? x)))
           '(lambda (x)
              (when  (fixnum? x)
                (unsafe-fx= x 0))))
(test-comp '(lambda (x)
              (when (and (fixnum? x) (zero? (random 2)))
                (zero? x)))
           '(lambda (x)
              (when (and (fixnum? x) (zero? (random 2)))
                (unsafe-fx= x 0))))

;test special case for bitwise-and and fixnum?
(test-comp '(lambda (x)
              (let ([y (bitwise-and x 2)])
                (list y y (fixnum? y))))
           '(lambda (x)
              (let ([y (bitwise-and x 2)])
                (list y y #t))))
(test-comp '(lambda (x)
              (let ([y (bitwise-and x 2)])
                (fixnum? x)))
           '(lambda (x)
              (let ([y (bitwise-and x 2)])
                #t))
           #f)

;; Make sure that `bitwise-and` is known to return a fixnum for non-negative
;; fixnum arguments but not for a negative one

(test-comp '(lambda (x)
             (bitwise-ior (bitwise-and x 7) 1))
           '(lambda (x)
             (unsafe-fxior (bitwise-and x 7) 1)))
(test-comp '(lambda (x)
             (bitwise-ior (bitwise-and x -7) 1))
           '(lambda (x)
             (unsafe-fxior (bitwise-and x -7) 1))
           #f)


(test-comp `(lambda (x)
              (thread (lambda () (set! x 5)))
              (if (pair? x)
                  (car x)
                  (cdr x)))
           `(lambda (x)
              (thread (lambda () (set! x 5)))
              (if (pair? x)
                  (unsafe-car x)
                  (cdr x)))
           #f)

;; + fold to fixnum overflow, fx+ doesn't
(test-comp `(module m racket/base
              (+ (sub1 (expt 2 30)) (sub1 (expt 2 30))))
           `(module m racket/base
              (- (expt 2 31) 2)))
(test-comp `(module m racket/base
              (require racket/fixnum)
              (fx+ (sub1 (expt 2 30)) (sub1 (expt 2 30))))
           `(module m racket/base
              (require racket/fixnum)
              (- (expt 2 31) 2))
           #f)

;; Propagate type implications from RHS:
(test-comp '(lambda (x)
              (let ([y (car x)])
                (list (cdr x) y (car x) y)))
           '(lambda (x)
             (let ([y (car x)])
               (list (unsafe-cdr x) y (unsafe-car x) y))))

;; don't duplicate an operation by moving it into a lambda':
(test-comp '(lambda (x)
              (let ([y (unsafe-flvector-length x)])
                (let ([f (lambda () y)])
                  (+ (f) (f)))))
           '(lambda (x)
              (+ (unsafe-flvector-length x) (unsafe-flvector-length x)))
           #f)

(when (extflonum-available?)
  (test-comp '(lambda (x)
              (let ([y (unsafe-extflvector-length x)])
                (let ([f (lambda () y)])
                  (+ (f) (f)))))
           '(lambda (x)
              (+ (unsafe-extflvector-length x) (unsafe-extflvector-length x)))
           #f))

;; don't delay an unsafe car, because it might be space-unsafe
(test-comp '(lambda (f x)
              (let ([y (unsafe-car x)])
                (f)
                y))
           '(lambda (f x)
              (f)
              (unsafe-car x))
           #f)

;; It would be ok to delay `list', because there's no space-safety issue
;; ... except that an arbitrary function might capture a continuation:
(test-comp '(lambda (f x)
              (let ([y (list x)])
                (f)
                y))
           '(lambda (f x)
              (f)
              (list x))
           #f)
(test-comp '(lambda (f x)
              (let ([y (list x)])
                (random)
                y))
           '(lambda (f x)
              (random)
              (list x)))

;; don't duplicate formerly once-used variable due to inlining
(test-comp '(lambda (y)
              (let ([q (unsafe-fl* y y)]) ; => q is known flonum
                (let ([x (unsafe-fl* q q)]) ; can delay (but don't duplicate)
                  (define (f z) (unsafe-fl+ z x))
                  (if y
                      (f 10)
                      f))))
           '(lambda (y)
              (let ([q (unsafe-fl* y y)])
                (let ([x (unsafe-fl* q q)])
                  (define (f z) (unsafe-fl+ z x))
                  (if y
                      (unsafe-fl+ 10 x)
                      f)))))
;; double-check that previous test doesn't succeed due to copying
(test-comp '(lambda (y)
              (let ([q (unsafe-fl* y y)])
                (let ([x (unsafe-fl* q q)])
                  (define (f z) (unsafe-fl+ z x))
                  (if y
                      (unsafe-fl+ 10 x)
                      f))))
           '(lambda (y)
              (let ([q (unsafe-fl* y y)])
                (define (f z) (unsafe-fl+ z (unsafe-fl* q q)))
                (if y
                    (unsafe-fl+ 10 (unsafe-fl* q q))
                    f)))
           #f)

(when (extflonum-available?)
  ;; don't duplicate formerly once-used variable due to inlining
  (test-comp '(lambda (y)
                (let ([q (unsafe-extfl* y y)]) ; => q is known flonum
                  (let ([x (unsafe-extfl* q q)]) ; can delay (but don't duplicate)
                    (define (f z) (unsafe-extfl+ z x))
                    (if y
                        (f 10)
                        f))))
             '(lambda (y)
                (let ([q (unsafe-extfl* y y)])
                  (let ([x (unsafe-extfl* q q)])
                    (define (f z) (unsafe-extfl+ z x))
                    (if y
                        (unsafe-extfl+ 10 x)
                        f)))))
  ;; double-check that previous test doesn't succeed due to copying
  (test-comp '(lambda (y)
                (let ([q (unsafe-extfl* y y)])
                  (let ([x (unsafe-extfl* q q)])
                    (define (f z) (unsafe-extfl+ z x))
                    (if y
                        (unsafe-extfl+ 10 x)
                        f))))
             '(lambda (y)
                (let ([q (unsafe-extfl* y y)])
                  (define (f z) (unsafe-extfl+ z (unsafe-extfl* q q)))
                  (if y
                      (unsafe-extfl+ 10 (unsafe-extfl* q q))
                      f)))
             #f))

;; check move through an intermediate variable:
(test-comp '(lambda (n)
              (let ([p (+ n n)])
                (if n
                    (let ([m (unsafe-fx- p 1)]
                          [t (- p p)])
                      (let ([q (- p p)]
                            [s m])
                        (+ p s q t)))
                    'ok)))
           '(lambda (n)
              (let ([p (+ n n)])
                (if n
                    (let ([m (unsafe-fx- p 1)]
                          [t (- p p)])
                      (+ p m (- p p) t))
                    'ok))))

(test-comp '(lambda (n)
              (let ([p (fx+ n n)])
                (if n
                    (let ([m (unsafe-fx- p 1)]
                          [t (- p p)])
                      (let ([q (- p p)]
                            [s m])
                        (+ p s q t)))
                    'ok)))
           '(lambda (n)
              (let ([p (fx+ n n)])
                (if n
                    (let ([t (- p p)])
                      (+ p (unsafe-fx- p 1) (- p p) t))
                    'ok))))

;; eliminate unneeded tests:
(test-comp '(lambda (n)
              (let ([p (fl+ n n)])
                (if (flonum? p)
                    (fl+ p p)
                    'bad)))
           '(lambda (n)
              (let ([p (fl+ n n)])
                (fl+ p p))))
(test-comp '(lambda (n)
              (let ([p (fx+ n n)])
                (if (fixnum? p)
                    (fx+ p p)
                    'bad)))
           '(lambda (n)
              (let ([p (fx+ n n)])
                (fx+ p p))))
(test-comp '(lambda (n)
              (let ([p (extfl+ n n)])
                (if (extflonum? p)
                    (extfl+ p p)
                    'bad)))
           '(lambda (n)
              (let ([p (extfl+ n n)])
                (extfl+ p p))))

(test-comp '(lambda (n)
              (let ([p (fl+ n n)])
                (list
                  p p
                  (flonum? p)
                  (flonum? (begin (random) p))
                  (flonum? (letrec ([x (lambda (t) x)]) (x x) p)))))
           '(lambda (n)
              (let ([p (fl+ n n)])
                (list
                  p p
                  #t 
                  (begin (random) #t)
                  (letrec ([x (lambda (t) x)]) (x x) #t)))))

(test-comp '(lambda (n)
              (let ([p (fx+ n n)])
                (list
                  p p
                  (fixnum? p)
                  (fixnum? (begin (random) p))
                  (fixnum? (letrec ([x (lambda (t) x)]) (x x) p)))))
           '(lambda (n)
              (let ([p (fx+ n n)])
                (list
                  p p
                  #t  
                  (begin (random) #t)
                  (letrec ([x (lambda (t) x)]) (x x) #t)))))
(test-comp '(lambda (n)
              (let ([p (extfl+ n n)])
                (list
                  p p
                  (extflonum? p)
                  (extflonum? (begin (random) p))
                  (extflonum? (letrec ([x (lambda (t) x)]) (x x) p)))))
           '(lambda (n)
              (let ([p (extfl+ n n)])
                (list
                  p p
                  #t
                  (begin (random) #t)
                  (letrec ([x (lambda (t) x)]) (x x) #t)))))

;; simple cross-module inlining
(test-comp `(module m racket/base 
              (require racket/bool)
              (list true))
           `(module m racket/base 
              (require racket/bool)
              (list #t)))

(test-comp `(module m racket/base 
              (require racket/list)
              empty?
              (empty? 10))
           `(module m racket/base 
              (require racket/list)
              empty? ; so that it counts as imported
              (null? 10)))

(test-comp `(module m racket/base
             (module a racket/base
               (provide b c)
               (define c #f)
               (set! c c)
               (define (b) (c)))
             (module d racket/base
               (require (submod ".." a))
               (list b c (b))))
           `(module m racket/base
             (module a racket/base
               (provide b c)
               (define c #f)
               (set! c c)
               (define (b) (c)))
             (module d racket/base
               (require (submod ".." a))
               (list b c (c)))))

(test-comp `(module m racket/base
             (module a racket/base
               (provide b c)
               (define c
                 (let ([x 0])
                   (lambda (y)
                     (begin0
                      x
                      (set! x y)))))
               (define (b z)
                 (c z)))
             (module d racket/base
               (require (submod ".." a))
               (list b c (b 1))))
           `(module m racket/base
             (module a racket/base
               (provide b c)
               (define c
                 (let ([x 0])
                   (lambda (y)
                     (begin0
                      x
                      (set! x y)))))
               (define (b z)
                 (c z)))
             (module d racket/base
               (require (submod ".." a))
               (list b c (c 1)))))

(module check-inline-request racket/base
  (require racket/performance-hint)
  (provide loop)
  (begin-encourage-inline
   (define loop
     ;; large enough that the compiler wouldn't infer inlining:
     (lambda (f n)
       (let loop ([i n])
         (if (zero? i)
             10
             (cons (f i) (loop (sub1 n)))))))))

(test-comp `(module m racket/base 
              (require 'check-inline-request)
              loop
              (loop list 1)) ; 1 is small enough to fully unroll
           `(module m racket/base 
              (require 'check-inline-request)
              loop ; so that it counts as imported
              (let ([f list]
                    [n 1])
                (let loop ([i n])
                  (if (zero? i)
                      10
                      (cons (f i) (loop (sub1 n))))))))

(test-comp `(module m racket/base
              (require racket/unsafe/ops)
              (define (f x)
                (let-values ([(a b) (values x (unsafe-fx+ x x))])
                  (list a b))))
           `(module m racket/base
              (require racket/unsafe/ops)
              (define (f x)
                (let ([a x]
                      [b (unsafe-fx+ x x)])
                  (list a b)))))

(test-comp `(module m racket/base
              (define (f x)
                (let-values ([(a b) (values x (+ x x))])
                  (list a b))))
           `(module m racket/base
              (define (f x)
                (let ([a x]
                      [b (+ x x)])
                  (list a b)))))

(test-comp `(module m racket/base
              (define (f x)
                (let*-values ([(a b) (values x (+ x x))])
                  (list a b))))
           `(module m racket/base
              (define (f x)
                (let* ([a x]
                       [b (+ x x)])
                  (list a b)))))

(test-comp `(module m racket/base
              (define (f x)
                (let*-values ([(a b) (values x (+ x x))])
                  (set! a 5)
                  (/ a b))))
           `(module m racket/base
              (define (f x)
                ;; Not equivalent if a continuation capture
                ;; during `+' somehow exposes the shared `a'?
                (let* ([a x]
                       [b (+ x x)])
                  (set! a 5)
                  (/ a b))))
           #f)

;; check omit & reorder possibilities for unsafe
;; operations on mutable values:
(let ()
  (define (check-omit-ok expr [yes? #t])
    (displayln (list expr 1 '!))
    ;; can omit:
    (test-comp `(module m racket/base
                  (require racket/unsafe/ops)
                  (define (f x y z)
                    (f x y z)))
               `(module m racket/base
                  (require racket/unsafe/ops)
                  (define (f x y z)
                    ,expr
                    (f x y z)))
               yes?)
    (displayln (list expr 2 '!))
    ;; cannot reorder:
    (test-comp `(module m racket/base
                  (require racket/unsafe/ops)
                  (define (f x y z w)
                    (display w)
                    (let ([temp ,expr])
                      (vector-ref x y)
                      (f x temp))))
               `(module m racket/base
                  (require racket/unsafe/ops)
                  (define (f x y z w)
                    (display w)
                    (vector-ref x y)
                    (f x y z ,expr)))
               #f)
     (displayln (list expr 3 '!))
  )
  (map check-omit-ok
       '((unsafe-vector-ref x y)
         (unsafe-vector*-ref x y)
         (unsafe-struct-ref x y)
         (unsafe-struct*-ref x y)
         (unsafe-mcar x)
         (unsafe-mcdr x)
         (unsafe-unbox y)
         (unsafe-unbox* x)
         (unsafe-bytes-ref x y)
         (unsafe-string-ref x y)
         (unsafe-flvector-ref x y)
         (unsafe-fxvector-ref x y)
         (unsafe-f64vector-ref x y)
         (unsafe-s16vector-ref x y)
         (unsafe-u16vector-ref x y)))
  (map (lambda (x) (check-omit-ok x #f))
       '((unsafe-vector-set! x y z)
         (unsafe-vector*-set! x y z)
         (unsafe-struct-set! x y z)
         (unsafe-struct*-set! x y z)
         (unsafe-set-mcar! x y)
         (unsafe-set-mcdr! x y)
         (unsafe-set-box! x y)
         (unsafe-set-box*! x y)
         (unsafe-bytes-set! x y z)
         (unsafe-string-set! x y z)
         (unsafe-flvector-set! x y z)
         (unsafe-fxvector-set! x y z)
         (unsafe-f64vector-set! x y z)
         (unsafe-s16vector-set! x y z)
         (unsafe-u16vector-set! x y z)))

  (when (extflonum-available?)
    (map check-omit-ok
         '((unsafe-extflvector-ref x y)
           (unsafe-f80vector-ref x y)))

    (map (lambda (x) (check-omit-ok x #f))
         '((unsafe-extflvector-set! x y z)
           (unsafe-f80vector-set! x y z)
           ))
    ))

(test-comp '(lambda (x)
              (hash-ref '#hash((x . y)) x (lambda () 10)))
           '(lambda (x)
              (hash-ref '#hash((x . y)) x 10)))
(test-comp '(lambda (x)
              (hash-ref x x (lambda () 10)))
           '(lambda (x)
              (hash-ref x x 10))
           #f)
(test-comp '(lambda (x)
              (hash-ref '#hash((x . y)) x (lambda () add1)))
           '(lambda (x)
              (hash-ref '#hash((x . y)) x add1))
           #f)

(test-comp '(lambda ()
             (hash-ref #hash()
                       'missing
                       (λ ()
                         'UNEXPECTED!)))
           '(lambda ()
             (hash-ref #hash()
                       'missing
                       'UNEXPECTED!)))
(test-comp '(lambda ()
             (hash-ref #hash()
                       'missing
                       (λ (required-arg)
                         'UNEXPECTED!)))
           '(lambda ()
             (hash-ref #hash()
                       'missing
                       'UNEXPECTED!))
           #f)

;; Check elimination of ignored structure predicate
;; and constructor applications:

(test-comp '(module m racket/base
              (define-values (struct:a a a? a-ref a-set!)
                (make-struct-type 'a #f 2 0))
              (begin0
               (a? (a-ref (a 1 2) 1))
               a?
               a
               a-ref
               (a? 7)
               (a 1 2)
               5))
           '(module m racket/base
              (define-values (struct:a a a? a-ref a-set!)
                (make-struct-type 'a #f 2 0))
              (begin0
               (a? (a-ref (a 1 2) 1))
               5)))

(test-comp '(module m racket/base
              (define-values (struct:a a a? a-x a-y)
                (let-values ([(struct:a a a? a-ref a-set!)
                              (make-struct-type 'a #f 2 0)])
                  (values struct:a a a?
                          (make-struct-field-accessor a-ref 0)
                          (make-struct-field-accessor a-ref 1))))
              (begin0
               (a? (a-x (a 1 2)))
               a?
               a
               a-x
               (a? 7)
               (a 1 2)
               5))
           '(module m racket/base
              (define-values (struct:a a a? a-x a-y)
                (let-values ([(struct:a a a? a-ref a-set!)
                              (make-struct-type 'a #f 2 0)])
                  (values struct:a a a?
                          (make-struct-field-accessor a-ref 0)
                          (make-struct-field-accessor a-ref 1))))
              (begin0
               (a? (a-x (a 1 2)))
               5)))

(test-comp '(module m racket/base
              (struct a (x y) #:omit-define-syntaxes)
              (begin0
               (a? (a-x (a 1 2)))
               a?
               a
               a-x
               (a? 7)
               (a 1 2)
               5))
           '(module m racket/base
              (struct a (x y) #:omit-define-syntaxes)
              (begin0
               (a? (a-x (a 1 2)))
               5)))

(test-comp '(module m racket/base
              (struct a (x y) #:omit-define-syntaxes #:prefab)
              (begin0
               (a? (a-x (a 1 2)))
               a?
               a
               a-x
               (a? 7)
               (a 1 2)
               5))
           '(module m racket/base
              (struct a (x y) #:omit-define-syntaxes #:prefab)
              (begin0
               (a? (a-x (a 1 2)))
               5)))

(test-comp '(module m racket/base
              (struct a (x y) #:omit-define-syntaxes #:mutable)
              (begin0
               (a? (set-a-x! (a 1 2) 5))
               a?
               a
               a-x
               set-a-x!
               (a? 7)
               (a 1 2)
               5))
           '(module m racket/base
              (struct a (x y) #:omit-define-syntaxes #:mutable)
              (begin0
               (a? (set-a-x! (a 1 2) 5))
               5)))

(test-comp '(module m racket/base
              (struct a (x y) #:omit-define-syntaxes)
              (struct b (z) #:super struct:a #:omit-define-syntaxes)
              (begin0
               (list (a? (a-x (a 1 2)))
                     (b? (b-z (b 1 2 3))))
               a?
               a
               a-x
               (a? 7)
               (a 1 2)
               b?
               b
               b-z
               (b 1 2 3)
               5))
           '(module m racket/base
              (struct a (x y) #:omit-define-syntaxes)
              (struct b (z) #:super struct:a #:omit-define-syntaxes)
              (begin0
               (list (a? (a-x (a 1 2)))
                     (b? (b-z (b 1 2 3))))
               5)))

(test-comp '(module m racket/base
              (struct a (x y) #:omit-define-syntaxes
                      #:property prop:custom-write (lambda (v port mode)
                                                     (write-string "#<a>" port))
                      #:property prop:equal+hash (list (lambda (a b eql?) (eq? a b))
                                                       (lambda (a hash-code) 0)
                                                       (lambda (a hash-code) 1)))
              (begin0
               (a? (a-x (a 1 2)))
               a?
               a
               a-x
               (a? 7)
               (a 1 2)
               5))
           '(module m racket/base
              (struct a (x y) #:omit-define-syntaxes
                      #:property prop:custom-write (lambda (v port mode)
                                                     (write-string "#<a>" port))
                      #:property prop:equal+hash (list (lambda (a b eql?) (eq? a b))
                                                       (lambda (a hash-code) 0)
                                                       (lambda (a hash-code) 1)))
              (begin0
               (a? (a-x (a 1 2)))
               5)))

(module struct-a-for-optimize racket/base
  (provide (struct-out a)
           (struct-out b))
  (struct a (x y))
  (struct b a (z)))

(module struct-c-for-optimize racket/base
  (require 'struct-a-for-optimize)
  (provide (struct-out c))
  (struct c a (q)))

(test-comp '(module m racket/base
              (require 'struct-a-for-optimize)
              (begin0
               (list (a? (a-x (a 1 2)))
                     (b? (b-z (b 1 2 3))))
               a?
               a
               a-x
               (a? 7)
               (a 1 2)
               b?
               b
               b-z
               (b 1 2 3)
               5))
           '(module m racket/base
              (require 'struct-a-for-optimize)
              (begin0
               (list (a? (a-x (a 1 2)))
                     (b? (b-z (b 1 2 3))))
               5)))

(test-comp '(module m racket/base
              (require 'struct-c-for-optimize)
              (begin0
               (list (c? (c-q (c 1 2 3))))
               c?
               c
               c-q
               (c 1 2 3)
               5))
           '(module m racket/base
              (require 'struct-c-for-optimize)
              (begin0
               (list (c? (c-q (c 1 2 3))))
               5)))

(test-comp '(module m racket/base
             (require racket/unsafe/ops)
             (struct a (x y))
             (define (f v)
               (if (a? v)
                   (list (a-x v) (a-y v))
                   (void))))
           '(module m racket/base
             (require racket/unsafe/ops)
             (struct a (x y))
             (define (f v)
               (if (a? v)
                   (list (unsafe-struct-ref v 0)
                         (unsafe-struct-ref v 1))
                   (void)))))

(test-comp '(module m racket/base
             (require racket/unsafe/ops)
             (struct a (x y) #:authentic)
             (define (f v)
               (if (a? v)
                   (list (a-x v) (a-y v))
                   (void))))
           '(module m racket/base
             (require racket/unsafe/ops)
             (struct a (x y) #:authentic)
             (define (f v)
               (if (a? v)
                   (list (unsafe-struct*-ref v 0)
                         (unsafe-struct*-ref v 1))
                   (void)))))

(test-comp '(module m racket/base
             (require racket/unsafe/ops)
             (struct a (x y))
             (define (f v)
               (list (a-x v) (a-y v))))
           '(module m racket/base
             (require racket/unsafe/ops)
             (struct a (x y))
             (define (f v)
               (list (a-x v)
                     (unsafe-struct-ref v 1)))))

(test-comp '(module m racket/base
             (require racket/unsafe/ops)
             (struct a (x y))
             (define (f v)
               (list (a-x v) (a? v))))
           '(module m racket/base
             (require racket/unsafe/ops)
             (struct a (x y))
             (define (f v)
               (list (a-x v) #t))))

(test-comp '(module m racket/base
             (require racket/unsafe/ops)
             (struct a (x y))
             (struct b a (z))
             (define (f v)
               (and (b? v) (b-z v))))
           '(module m racket/base
             (require racket/unsafe/ops)
             (struct a (x y))
             (struct b a (z))
             (define (f v)
               (and (b? v) (unsafe-struct-ref v 2)))))

(test-comp '(module m racket/base
             (require racket/unsafe/ops)
             (struct a (x y))
             (struct b a (z))
             (define (f v)
               (list (b-z v) (a? v))))
           '(module m racket/base
             (require racket/unsafe/ops)
             (struct a (x y))
             (struct b a (z))
             (define (f v)
               (list (b-z v) #t))))

(test-comp '(module m racket/base
             (require 'struct-a-for-optimize
                      racket/unsafe/ops)
             (struct c b (m))
             (define (f v)
               (and (c? v) (c-m v))))
           '(module m racket/base
             (require 'struct-a-for-optimize
                      racket/unsafe/ops)
             (struct c b (m))
             (define (f v)
               (and (c? v) (unsafe-struct-ref v 3)))))

(test-comp '(module m racket/base
              (struct a (x y) #:omit-define-syntaxes
                      #:property prop:procedure 0)
              (begin0
               (a? (a-x (a 1 2)))
               a?
               a
               a-x
               (a? 7)
               (a 1 2)
               5))
           '(module m racket/base
              (struct a (x y) #:omit-define-syntaxes
                      #:property prop:procedure 0)
              (begin0
               (a? (a-x (a 1 2)))
               5)))

(test-comp '(module m racket/base
              (struct a (x) #:omit-define-syntaxes #:mutable)

              (procedure? a)
              (lambda (x) (values (a x)))
              (lambda (x) (void (a x)))

              (procedure? a?)
              (lambda (x) (values (a? x)))
              (lambda (x) (void (a? x)))
              (lambda (x) (boolean? (a? x)))
              (lambda (x) (when (a? x) (a? x)))

              (procedure? a-x)
              (lambda (x) (values (a-x x)))
              (lambda (x) (when (a? x) (void (a-x x))))

              (procedure? set-a-x!)
              (lambda (x) (values (set-a-x! x 5))))
           '(module m racket/base
              (struct a (x) #:omit-define-syntaxes #:mutable)

              #t
              (lambda (x) (a x))
              (lambda (x) a (void))

              #t
              (lambda (x) (a? x))
              (lambda (x) a (void))
              (lambda (x) a #t)
              (lambda (x) (when (a? x) #t))

              #t
              (lambda (x) (a-x x))
              (lambda (x) (when (a? x) (void)))

              #t
              (lambda (x) (set-a-x! x 5))))

(test-comp '(lambda ()
             (make-struct-type 'a #f 0 0 #f)
             10)
           '(lambda ()
             10))

(test-comp '(lambda ()
             (make-struct-type-property 'a)
             10)
           '(lambda ()
             10))

(test-comp '(module m racket/base
             (define-values (prop:a a? a-ref) (make-struct-type-property 'a))
             (lambda (x)
               (a? x)
               (if a? (if a-ref x 11) 10)))
           '(module m racket/base
             (define-values (prop:a a? a-ref) (make-struct-type-property 'a))
             (lambda (x)
               x)))
(test-comp '(module m racket/base
              (define-values (prop:a a? a-ref) (make-struct-type-property 'a))

              (procedure? a?)
              (lambda (x) (values (a? x)))
              (lambda (x) (void (a? x)))
              (lambda (x) (boolean? (a? x)))
              #;(lambda (x) (when (a? x) (a? x)))

              (procedure? a-ref)
              (lambda (x) (values (a-ref x)))
              #;(lambda (x) (when (a? x) (void (a-ref x)))))
           '(module m racket/base
              (define-values (prop:a a? a-ref) (make-struct-type-property 'a))

              #t
              (lambda (x) (a? x))
              (lambda (x) a? (void))
              (lambda (x) a? #t)
              #;(lambda (x) (when (a? x) #t))

              #t
              (lambda (x) (a-ref x))
              #;(lambda (x) (when (a? x) (void)))))

(test-comp '(module m racket/base
             (define (f x) (list (g x) g))
             ;; Defining and using a property doesn't interrupt a sequence
             ;; of simultaneous definitions, so `g` above can be inlined
             (define-values (prop:a a? a-ref) (make-struct-type-property 'a))
             (struct b () #:property prop:a 'a)
             (define (g y) (list y)))
           '(module m racket/base
             (define (f x) (list (list x) g))
             (define-values (prop:a a? a-ref) (make-struct-type-property 'a))
             (struct b () #:property prop:a 'a)
             (define (g y) (list y))))

(test-comp '(module m racket/base
             (define (f x) (list (g x) g))
             ;; A property type with a guard inhibits inlining, because the
             ;; guard might raise an error
             (define-values (prop:a a? a-ref) (make-struct-type-property 'a error))
             (struct b () #:property prop:a 'a)
             (define (g y) (list y)))
           '(module m racket/base
             (define (f x) (list (list x) g))
             (define-values (prop:a a? a-ref) (make-struct-type-property 'a error))
             (struct b () #:property prop:a 'a)
             (define (g y) (list y)))
           #f)

(module struct-type-property-a racket/base
  (provide prop:a)
  (define-values (prop:a a? a-ref) (make-struct-type-property 'a)))

(test-comp '(module m racket/base
             (require 'struct-type-property-a)
             (define (f x) (list (g x) g))
             (struct b () #:property prop:a 'a)
             (define (g y) (list y)))
           '(module m racket/base
             (require 'struct-type-property-a)
             (define (f x) (list (list x) g))
             (struct b () #:property prop:a 'a)
             (define (g y) (list y))))

(module struct-type-property-a-with-guard racket/base
  (provide prop:a)
  (define-values (prop:a a? a-ref) (make-struct-type-property 'a error)))

(test-comp '(module m racket/base
             (require 'struct-type-property-a-with-guard)
             (define (f x) (list (g x) g))
             (struct b () #:property prop:a 'a)
             (define (g y) (list y)))
           '(module m racket/base
             (require 'struct-type-property-a-with-guard)
             (define (f x) (list (list x) g))
             (struct b () #:property prop:a 'a)
             (define (g y) (list y)))
           #f)

;; A function with a required optional argument creates a pattern like
;; the ones above, but intermediate points include extra references
;; that make it difficult to check with `test-comp`
#;
(test-comp '(module m racket/base
             (define (f x) (list (g #:x x)))
             (define (g #:x y) (list y)))
           '(module m racket/base
             (define (f x) (list (list x)))
             (define (g #:x y) (list y))))

(test-comp `(lambda (b)
              (let ([v (unbox b)])
                (with-continuation-mark 'x 'y (unbox v))))
           `(lambda (b)
              (with-continuation-mark 'x 'y (unbox (unbox b))))
           #f)
(test-comp `(lambda (b)
              (let ([v (box b)])
                (with-continuation-mark 'x 'y (box v))))
           `(lambda (b)
              (with-continuation-mark 'x 'y (box (box b)))))

(test-comp `(lambda () (list 1))
           `(lambda ()
              (with-continuation-mark 'x 'y (list 1))))
(test-comp `(lambda () (random) (list 1))
           `(lambda ()
              (with-continuation-mark 'x (random) (list 1))))
(test-comp `(lambda (f) (values (f)) (list 1))
           `(lambda (f)
              (with-continuation-mark 'x (f) (list 1))))
(test-comp `(lambda () (values 1 2) (list 1))
           `(lambda ()
              (with-continuation-mark 'x (values 1 2) (list 1)))
           #f)
(test-comp `(lambda (x)
             (with-continuation-mark
               x 1
               (with-continuation-mark
                x 2
                (x))))
            `(lambda (x)
              (with-continuation-mark
                x 2
                (x))))
(test-comp `(lambda (x)
             (with-continuation-mark
               x (display x)
               (with-continuation-mark
                x 2
                (x))))
            `(lambda (x)
              (display x)
              (with-continuation-mark
                x 2
                (x))))
(test-comp `(lambda (x)
             (with-continuation-mark
               x 1
               (with-continuation-mark
                x (current-continuation-marks)
                (x))))
            `(lambda (x)
              (with-continuation-mark
                x (current-continuation-marks)
               (x)))
            #f)
(test-comp '(lambda (v)
             (let ([x (with-continuation-mark
                          'x 10
                          (+ v v))])
               x))
           '(lambda (v)
             (values
              (with-continuation-mark
                  'x 10
                  (+ v v)))))

(test-comp `(lambda (x y f)
              (set! x 5)
              (list
                (#%variable-reference x)
                (#%variable-reference y)
                (variable-reference-constant? (#%variable-reference x))
                (variable-reference-constant? (#%variable-reference y))
                (variable-reference-constant? (letrec ([z (lambda () z)]) (f z) (#%variable-reference x)))
                (variable-reference-constant? (letrec ([z (lambda () z)]) (f z) (#%variable-reference y)))))
           `(lambda (x y f)
              (set! x 5)
              (list
                (#%variable-reference x)
                (#%variable-reference y)
                #f
                #t
                (letrec ([z (lambda () z)]) (f z) #f)
                (letrec ([z (lambda () z)]) (f z) #t))))

(test-comp `(module m racket/base
             (define f (random))
             (define g (random))
             (list (variable-reference-constant? (#%variable-reference f))
                   (#%variable-reference g)))
           `(module m racket/base
             (define f (random))
             (define g (random))
             (list #t
                   (#%variable-reference g))))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Types related to arithmetic

(let ()
  (define (check-real-op op [can-omit? #t] [can-multi? #t])
    (test-comp `(lambda (x y)
                 (list (,op x y)
                       (real? x)
                       (real? y)
                       (number? x)
                       (number? y)))
               `(lambda (x y)
                 (list (,op x y)
                       #t
                       #t
                       #t
                       #t)))
    (when can-multi?
      (test-comp `(lambda (x y z w)
                   (list (,op x y z w)
                         (real? x)
                         (real? y)
                         (real? z)
                         (real? w)))
                 `(lambda (x y z w)
                   (list (,op x y z w)
                         #t
                         #t
                         #t
                         #t))))
    (when can-omit?
      (test-comp `(lambda (x y)
                   (if (and (real? x) (real? y))
                       (with-continuation-mark
                           'x 'y
                         (,op x y))
                       (error "bad")))
                 `(lambda (x y)
                   (if (and (real? x) (real? y))
                       (,op x y)
                       (error "bad"))))))
  (check-real-op 'quotient #f #f)
  (check-real-op 'remainder #f #f)
  (check-real-op 'modulo #f #f)
  (check-real-op 'max)
  (check-real-op 'min)
  (check-real-op '<)
  (check-real-op '>)
  (check-real-op '<=)
  (check-real-op '>=)

  (define (check-number-op op [closed-under-reals? #t])
    (test-comp `(lambda (x y)
                 (list (,op x y)
                       (number? x)
                       (number? y)))
               `(lambda (x y)
                 (list (,op x y)
                       #t
                       #t)))
    (test-comp `(lambda (x y z w)
                 (list (,op x y z w)
                       (number? x)
                       (number? y)
                       (number? z)
                       (number? w)))
               `(lambda (x y z w)
                 (list (,op x y z w)
                       #t
                       #t
                       #t
                       #t)))
    (test-comp `(lambda (x y)
                 (list (,op x y)
                       (real? x)))
               `(lambda (x y)
                 (list (,op x y)
                       #t))
               ;; cannot assume `real?`
               #f)
    (when closed-under-reals?
      (test-comp `(lambda (x y)
                   (if (and (real? x) (real? y))
                       (let ([v (,op x y)])
                         (with-continuation-mark
                             'x 'y
                             ;; No error possible from `<`:
                             (list (< 2 v) (< 1 v))))
                       (error "bad")))
                 `(lambda (x y)
                   (if (and (real? x) (real? y))
                       (let ([v (,op x y)])
                         (list (< 2 v) (< 1 v)))
                       (error "bad"))))))
  (check-number-op '+)
  (check-number-op '-)
  (check-number-op '*)
  (check-number-op '/)
  (check-number-op '+)
  (check-number-op '= #f)
  
  (define (check-number-op-unary op)
    (test-comp `(lambda (x y)
                 (list (,op x)
                       (number? x)))
               `(lambda (x y)
                 (list (,op x)
                       #t)))
    ;; Check closed under reals:
    (test-comp `(lambda (x y)
                 (if (real? x)
                     (with-continuation-mark
                       'x 'y
                       ;; No error possible from `<`:
                       (< 1 (,op x)))
                     (error "bad")))
               `(lambda (x y)
                 (if (real? x)
                     (< 1 (,op x))
                     (error "bad")))))
  
  (check-number-op-unary 'add1)
  (check-number-op-unary 'sub1)
  (check-number-op-unary 'abs))

(test-comp '(lambda () (-) (void))
           '(lambda () (void))
           #f)
(test-comp '(lambda () (/) (void))
           '(lambda () (void))
           #f)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Check elimination of dead code after error
(test-comp '(lambda () (random) (error 'error))
           '(lambda () (random) (error 'error) 5))
(test-comp '(lambda () (random) (error 'error))
           '(lambda () (random) (error 'error) (k:random) 5))
(test-comp '(lambda () (error 'error))
           '(lambda () 5 (error 'error) 5))
(test-comp '(lambda (f) (f) (f) (error 'error))
           '(lambda (f) (f) (f) (error 'error) (f)))

(test-comp '(lambda (f) (begin0 (f) (random) (error 'error)))
           '(lambda (f) (begin0 (f) (random) (error 'error) (k:random) (f))))
(test-comp '(lambda (f) (error 'error))
           '(lambda (f) (begin0 (error 'error) (k:random) (f))))
(test-comp '(lambda (f) (error 'error))
           '(lambda (f) (begin0 7 (error 'error) (k:random) (f))))

(test-comp '(lambda (n)
              (let ([p (begin (error 'error) (fl+ n n))])
                (if (flonum? p)
                    (fl+ p p)
                    'bad)))
           '(lambda (n)
              (let ([p (begin (error 'error) (fl- n n))])
                (if (flonum? p)
                    (fl+ p p)
                    'bad))))

(test-comp '(lambda () (if (error 'error) 1 2))
           '(lambda () (if (error 'error) 1 2) 5))
(test-comp '(lambda () (error 'error))
           '(lambda () (if (error 'error) 1 2) 5))
(test-comp '(lambda (x) (if x (error 'error) 0) 3)
           '(lambda (x) (if x (error 'error) 0) 4)
           #f)
(test-comp '(lambda (x) (if x 0 (error 'error)) 3)
           '(lambda (x) (if x 0 (error 'error)) 4)
           #f)
(test-comp '(lambda (x) (if x (error 'error 1) (error 'error 2)))
           '(lambda (x) (if x (error 'error 1) (error 'error 2)) 5))

(test-comp '(lambda (x) (if x (error 'error) (car x)) (unsafe-car x))
           '(lambda (x) (if x (error 'error) (car x)) (car x)))
(test-comp '(lambda (x) (if x (car x) (error 'error)) (unsafe-car x))
           '(lambda (x) (if x (car x) (error 'error)) (car x)))
(test-comp '(lambda (x) (if x (begin (car x) (error 'error)) 0) (unsafe-car x))
           '(lambda (x) (if x (begin (car x) (error 'error)) 0) (car x))
           #f)
(test-comp '(lambda (x) (if x 0 (begin (car x) (error 'error))) (unsafe-car x))
           '(lambda (x) (if x 0 (begin (car x) (error 'error))) (car x))
           #f)

(test-comp '(lambda (x) (if (car x) (error 'error) 0) (unsafe-car x))
           '(lambda (x) (if (car x) (error 'error) 0) (car x)))
(test-comp '(lambda (x) (if (car x) 0 (error 'error)) (unsafe-car x))
           '(lambda (x) (if (car x) 0 (error 'error)) (car x)))

(test-comp '(lambda (f) (error 'error))
           '(lambda (f) (with-continuation-mark (error 'error) 'v (f))))
(test-comp '(lambda (f) (values (f)) (error 'error))
           '(lambda (f) (with-continuation-mark (f) (error 'error) (f))))

(test-comp '(lambda (f x) (f x x) (set! x 3) (error 'error))
           '(lambda (f x) (f x x) (set! x 3) (set! x (error 'error)) 5))
(test-comp '(lambda (f x) (error 'error))
           '(lambda (f x) (set! x (error 'error)) 5))
(test-comp '(lambda (f) (let ([x (random)]) (f x x) (set! x 3) (error 'error)))
           '(lambda (f) (let ([x (random)]) (f x x) (set! x 3) (set! x (error 'error)) 5)))
(test-comp '(lambda (f) (let ([x (random)]) (error 'error)))
           '(lambda (f) (let ([x (random)]) (set! x (error 'error)) 5)))

#;(test-comp '(lambda (f) (error 'error))
           '(lambda (f) (call-with-values (error 'error) (f))))
#;(test-comp '(lambda (g) (g) (error 'error))
           '(lambda (g) (call-with-values (g) (error 'error))))

(test-comp '(lambda () (error 'error))
           '(lambda () ((error 'error) 0) 5))
(test-comp '(lambda () (error 'error))
           '(lambda () (car (error 'error)) 5))
(test-comp '(lambda () (error 'error))
           '(lambda () (not (error 'error)) 5))
(test-comp '(lambda (f) (values (f)) (error 'error))
           '(lambda (f) ((f) (error 'error)) 5))
           
(test-comp '(lambda () (error 'error))
           '(lambda () ((error 'error) 0 1) 5))
(test-comp '(lambda () (error 'error))
           '(lambda () (cons (error 'error) 1) 5))
(test-comp '(lambda () (error 'error))
           '(lambda () (cons 0 (error 'error)) 5))
(test-comp '(lambda (f) (f) (error 'error))
           '(lambda (f) (f) (cons (error 'error) (f)) 5))
(test-comp '(lambda (f) (values (f)) (error 'error))
           '(lambda (f) (cons (f) (error 'error)) 5))
(test-comp '(lambda (f) (values (f)) (error 'error))
           '(lambda (f) ((f) (error 'error) (f)) 5))
(test-comp '(lambda (f g) (values (f)) (values (g)) (error 'error))
           '(lambda (f g) ((f) (g) (error 'error)) 5))

(test-comp '(lambda (f) (error 'error))
           '(lambda (f) ((error 'error) (f) (f) (f)) 5))
(test-comp '(lambda (f) (values (f)) (error 'error))
           '(lambda (f) ((f) (error 'error) (f) (f)) 5))
(test-comp '(lambda (f) (values (f)) (values (f)) (error 'error))
           '(lambda (f) ((f) (f) (error 'error) (f)) 5))
(test-comp '(lambda (f) (values (f)) (values (f)) (values (f)) (error 'error))
           '(lambda (f) ((f) (f) (f) (error 'error)) 5))

(test-comp '(lambda (f) (let ([x (error 'error)]) #f))
           '(lambda (f) (let ([x (error 'error)]) (f x x)) 5))
(test-comp '(lambda (f) (let ([x (error 'error)] [y #f]) #f))
           '(lambda (f) (let ([x (error 'error)] [y (k:random)]) (f x x y y)) 5))
(test-comp '(lambda (f) (let ([x (random)] [y (random)]) (f x x y y) (error 'error)))
           '(lambda (f) (let ([x (random)] [y (random)]) (f x x y y) (error 'error)) 5))
(test-comp '(lambda (f) (let-values ([(x) (error 'error)] [(y) #f] [(z) #f] ) #f))
           '(lambda (f) (let-values ([(x) (error 'error)] [(y z) (f)]) (f x x y y z z)) 5))
(test-comp '(lambda (f) (let-values ([(x) (error 'error)] [(y) #f] [(z) #f]) #f))
           '(lambda (f) (let-values ([(x y) (values (error 'error) (k:random))] [(z) (f)]) (f x x y y z z)) 5))
(test-comp '(lambda (f) (let-values ([(x) (begin (random) (error 'error))] [(y) #f] [(z) #f]) #f))
           '(lambda (f) (let-values ([(x y) (values (random) (error 'error))] [(z) (f)]) (f x x y y z z)) 5))
;alternative reduction:
#;(test-comp '(lambda (f) (let-values ([(x) (random)] [(y) (error 'error)] [(z) #f]) #f))
             '(lambda (f) (let-values ([(x y) (values (random) (error 'error))] [(z) (f)]) (f x x y y z z)) 5))

(test-comp '(lambda (f) (letrec ([x (lambda() y)] [y (lambda () x)]) (f x y) (error 'error)))
           '(lambda (f) (letrec ([x (lambda() y)] [y (lambda () x)]) (f x y) (error 'error)) 5))
(test-comp '(lambda (f) (letrec ([x (lambda() y)] [y (lambda () x)] [z (error 'error)]) #f))
           '(lambda (f) (letrec ([x (lambda() y)] [y (lambda () x)] [z (error 'error)]) (f x y z)) 5))
(test-comp '(lambda (f) (letrec ([x (lambda() y)] [z (error 'error)] [y #f]) #f))
           '(lambda (f) (letrec ([x (lambda() y)] [z (error 'error)] [y (lambda () x)]) (f x y z)) 5)
           #f) ; letrec-check pass determines that the body of `x` is dead
(test-comp '(lambda (f) (letrec ([z (error 'error)] [x #f] [y #f]) #f))
           '(lambda (f) (letrec ([z (error 'error)] [x (lambda() y)] [y (lambda () x)]) (f x y z)) 5))

(test-comp '(let-values ([() (error "oops")]) 11)
           '(error "oops"))
(test-comp '(let-values ([(x y) (error "oops")]) 11)
           '(error "oops"))
(test-comp '(letrec-values ([() (error "oops")]) 11)
           '(error "oops"))
(test-comp '(letrec-values ([(x y) (error "oops")]) 11)
           '(error "oops"))
(test-comp '(let-values (((y) (read)) (() (error "oops"))) 11)
           '(let () (begin (read) (error "oops"))))
(test-comp '(let-values (((y) (read)) (() (error "oops"))) 11)
           '(let () (begin (read) (error "oops"))))
(test-comp '(let-values ((() (error "oops")) ((x) 9)) 11)
           '(error "oops"))
(test-comp '(let-values ((() (error "oops")) (() (values))) 11)
           '(error "oops"))
(test-comp '(let-values (((y) (read)) (() (error "oops")) ((x) 9)) 11)
           '(let () (begin (read) (error "oops"))))
(test-comp '(let-values (((y) (read)) (() (error "oops")) (() (values))) 11)
           '(let () (begin (read) (error "oops"))))
(test-comp '(error "oops")
           '(let () (begin (read) (error "oops")))
           #f)

(test-comp '(with-continuation-mark
             'x 'y
             (let-values ([() (with-continuation-mark
                                  'x 'z
                                  (error "oops"))])
               11))
           '(with-continuation-mark
             'x 'y
             (begin0
              (with-continuation-mark
                  'x 'z
                  (error "oops"))
              (void))))

(test-comp `(module m racket/base
              (define x 5)
              (set! x 3)
              (error 'error))
           `(module m racket/base
              (define x 5)
              (set! x 3)
              (set! x (error 'error))))

(test-comp `(module m racket/base
              (module bad racket/base
                (error 'error))
              (random)
              5)
           `(module m racket/base
              (module bad racket/base
                (error 'error))
              (random))
           #f)

#;(test-comp `(module m racket/base
              f
              (error 'error))
           `(module m racket/base
              f
              (error 'error)
              (define f 5))
           #f)
           
(test-comp `(module m racket/base
              (define f 5)
              (error 'error))
           `(module m racket/base
              (define f 5)
              (error 'error)
              (set! f 0))
           #f)

;; Error simplifications must not break `with-continuation-mark`:
(let ([f (lambda ()
           (with-continuation-mark
               'contrast-dye 1
               (begin0
                 (with-continuation-mark
                     'contrast-dye 2
                     (+ 1 #f))
                 (void))))])
  (set! f f)
  (test '(2 1)
        'contrast-dye
        (with-handlers ([exn:fail? (lambda (exn)
                                     (continuation-mark-set->list (exn-continuation-marks exn)
                                                                  'contrast-dye))])
          (f))))
(let ([check-escape-position
       (lambda (nontail-wrap)
         (test-comp `(lambda ()
                      (with-continuation-mark
                          'contrast-dye 1
                          ,(nontail-wrap `(with-continuation-mark
                                           'contrast-dye 2
                                           (+ 1 #f)))))
                    `(lambda ()
                      (with-continuation-mark
                          'contrast-dye 1
                          (begin0
                            (with-continuation-mark
                                'contrast-dye 2
                                (+ 1 #f))
                            (void))))))])
  (check-escape-position (lambda (e)
                           `(+ 1 ,e)))
  (check-escape-position (lambda (e)
                           `(values ,e)))
  (check-escape-position (lambda (e)
                           `(let ([x ,e])
                             x)))
  (check-escape-position (lambda (e)
                           `(if ,e 1 2)))
  (check-escape-position (lambda (e)
                           `(begin ,e 1)))
  (check-escape-position (lambda (e)
                           `(begin0 ,e 1))))

;; Aritmethic simplifications must not break `with-continuation-mark`:
(let ([f (lambda ()
           (define retval #f)
           (with-continuation-mark
               'contrast-dye 1
               (unsafe-fx+
                 0
                 (with-continuation-mark
                     'contrast-dye 2
                     (begin
                       (set! retval (continuation-mark-set->list
                                     (current-continuation-marks)
                                     'contrast-dye))
                       7))))
           retval)])
  (set! f f)
  (test '(2 1)
        'contrast-dye
        (f)))

(let ([check-wcm-wrap
       (lambda (nontail-wrap)
         (test-comp `(lambda (p)
                      (with-continuation-mark
                          'contrast-dye 1
                          ,(nontail-wrap `(with-continuation-mark
                                              'contrast-dye 2
                                              (p)))))
                    `(lambda (p)
                      (with-continuation-mark
                          'contrast-dye 1
                          (unsafe-fx+
                            0
                            (with-continuation-mark
                                'contrast-dye 2
                                (p)))))))])
  (check-wcm-wrap (lambda (e)
                    `(unsafe-fx+ 0 ,e)))
  (check-wcm-wrap (lambda (e)
                    `(unsafe-fx+ ,e 0)))
  (check-wcm-wrap (lambda (e)
                    `(unsafe-fx- ,e 0)))
  (check-wcm-wrap (lambda (e)
                    `(unsafe-fx* 1 ,e)))
  (check-wcm-wrap (lambda (e)
                    `(unsafe-fx* ,e 1)))
  (check-wcm-wrap (lambda (e)
                    `(unsafe-fxquotient ,e 1)))
  (check-wcm-wrap (lambda (e)
                    `(unsafe-fl+ 0.0 ,e)))
  (check-wcm-wrap (lambda (e)
                    `(unsafe-fl+ ,e 0.0)))
  (check-wcm-wrap (lambda (e)
                    `(unsafe-fl- ,e 0.0)))
  (check-wcm-wrap (lambda (e)
                    `(unsafe-fl* 1.0 ,e)))
  (check-wcm-wrap (lambda (e)
                    `(unsafe-fl* ,e 1.0)))
  (check-wcm-wrap (lambda (e)
                    `(unsafe-fl/ ,e 1.0))))

;; Check `if` reduction in a boolen context:
(let ([f (lambda (x)
           (define retval #f)
           (not 
            (with-continuation-mark
                'contrast-dye 1
                (if (with-continuation-mark
                        'contrast-dye 2
                        (begin
                          (set! retval (continuation-mark-set->list
                                        (current-continuation-marks)
                                        'contrast-dye))
                           x))
                  #t
                  #f)))
           retval)])
  (set! f f)
  (test '(2 1)
        'contrast-dye
        (f 'x)))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Test that the `if` is not confused by the
;; predicates that recognize #f.

(test-comp '(lambda (x) (when (boolean? x)
                          (if x 1 2)))
           '(lambda (x) (when (boolean? x)
                          1))
           #f)

(test-comp '(lambda (x) (when (not x)
                          (if x 1 2)))
           '(lambda (x) (when (not x)
                          1))
           #f)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Special case of folding for string=? and bytes=?

(test-comp '(lambda () (string=? "123" "123"))
           '(lambda () #t))
(test-comp '(lambda () (string=? "123" "123456"))
           '(lambda () #f))
(test-comp '(lambda () (string=? "123" "456"))
           '(lambda () #f))
(test-comp '(lambda () (bytes=? #"123" #"123"))
           '(lambda () #t))
(test-comp '(lambda () (bytes=? #"123" #"123456"))
           '(lambda () #f))
(test-comp '(lambda () (bytes=? #"123" #"456"))
           '(lambda () #f))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Check that the type information is shifted in the
;; right direction while inlining.
;; The first example triggered a bug in 6.3.

(test-comp '(let ([zz (lambda (x) (lambda (y) 0))])
              (lambda (a b c)
                ((zz (let ([loop (lambda () 0)]) loop)) (car a))
                (list c (pair? c))))
           '(let ([zz (lambda (x) (lambda (y) 0))])
              (lambda (a b c)
                ((zz (let ([loop (lambda () 0)]) loop)) (car a))
                (list c #t)))
           #f)

(test-comp '(let ([zz (lambda (x) (lambda (y) 0))])
              (lambda (a b c)
                ((zz (let ([loop (lambda () 0)]) loop)) (car a))
                (list a (pair? a))))
           '(let ([zz (lambda (x) (lambda (y) 0))])
              (lambda (a b c)
                ((zz (let ([loop (lambda () 0)]) loop)) (car a))
                (list a #t))))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Check that the unused continuations are removed

(test-comp '(call-with-current-continuation (lambda (ignored) 5))
           5)
(test-comp '(call-with-composable-continuation (lambda (ignored) 5))
           5)
(test-comp '(call-with-escape-continuation (lambda (ignored) 5))
           5)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Check splitting of definitions
(test-comp `(module m racket/base
              (define-values (x y) (values 1 2)))
           `(module m racket/base
              (define x 1)
              (define y 2)))
(test-comp `(module m racket/base
              (define-values (x y z w) (values 1 2 4 5)))
           `(module m racket/base
              (define x 1)
              (define y 2)
              (define z 4)
              (define w 5)))
(test-comp `(module m racket/base
              (define-values (x y)
                (let ([x (lambda (x) x)]
                      [y (lambda (x y) y)])
                  (values x y))))
           `(module m racket/base
              (define x (lambda (x) x))
              (define y (lambda (x y) y))))
(test-comp `(module m racket/base
              (define-values (x y z)
                (let ([x (lambda (x) x)]
                      [y (lambda (x y) y)]
                      [z (lambda (x y z) z)])
                  (values x y z))))
           `(module m racket/base
              (define x (lambda (x) x))
              (define y (lambda (x y) y))
              (define z (lambda (x y z) z))))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Check bytecode verification of lifted functions

(let ([check
       (lambda (expr)
         (let-values ([(r w) (make-pipe)])
           (write (compile expr) w)
           (parameterize ([read-accept-compiled #t])
             (read r))))])
  (check '(module m racket/base
            (provide f)
            (define (f x)
              (let loop ([n 0])
                (set! x (+ n 1)) ; close over mutated variable
                (loop n #f)
                (loop n)))))
  (check '(module m racket/base
            (provide f)
            (define s (make-string 10))
            (define (f x)
              (let loop ([n 0])
                (set! x (+ n 1)) ; close over mutated variable
                (loop n s) ; and refer to global
                (loop n))))))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Make sure "mutated?" flag isn't confused with "ready" flag:
(module bad-order racket/base
  (define (f) (printf "~a\n" i))
  (f)
  (define i 9)
  (set! i 10))
(err/rt-test (dynamic-require ''bad-order #f))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Check lifting of a function with only an unused rest arg:

(test 1 'continue
      (let/ec foo
        (let ([continue (lambda extras
                          (foo 1))])
          (+ 1 (continue)))))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; call-with-values optimization

;; should get converted to let:
(module cwv-1 racket/base
  (define (cwv-1-f x)
    (call-with-values (lambda () (+ x 3))
      (lambda (y) (+ y 2))))
  (provide cwv-1-f))
(require 'cwv-1)
(test 15 cwv-1-f 10)

;; known function doesn't expect 1 argument
(module cwv-2 racket/base
  (define (cwv-2-f x)
    (call-with-values (lambda () (+ x 3))
      (lambda (y z) (+ y 2))))
  (provide cwv-2-f))
(require 'cwv-2)
(err/rt-test (cwv-2-f 10) exn:fail:contract:arity?)

;; known function, unknown number of results:
(module cwv-3 racket/base
  (define (cwv-3-f g)
    (call-with-values (lambda () (g))
      (lambda (y) (+ y 2))))
  (provide cwv-3-f))
(require 'cwv-3)
(test 12 cwv-3-f (lambda () 10))
(err/rt-test (cwv-3-f (lambda () (values 1 2))) exn:fail:contract:arity?)

;; ditto, need 2 results:
(module cwv-4 racket/base
  (define (cwv-4-f g)
    (call-with-values (lambda () (g))
      (lambda (y z) (+ y z 2))))
  (provide cwv-4-f))
(require 'cwv-4)
(test 12 cwv-4-f (lambda () (values 4 6)))
(err/rt-test (cwv-4-f (lambda () 10)) exn:fail:contract:arity?)
(err/rt-test (cwv-4-f (lambda () (values 1 2 10))) exn:fail:contract:arity?)

;; unknown first function:
(module cwv-5 racket/base
  (define (cwv-5-f g)
    (call-with-values g
      (lambda (y) (+ y 2))))
  (provide cwv-5-f))
(require 'cwv-5)
(test 12 cwv-5-f (lambda () 10))
(err/rt-test (cwv-5-f (lambda () (values 1 2))) exn:fail:contract:arity?)

;; ditto, need 2 results:
(module cwv-6 racket/base
  (define (cwv-6-f g)
    (call-with-values g
      (lambda (y z) (+ y z 2))))
  (provide cwv-6-f))
(require 'cwv-6)
(test 12 cwv-6-f (lambda () (values 4 6)))
(err/rt-test (cwv-6-f (lambda () 10)) exn:fail:contract:arity?)
(err/rt-test (cwv-6-f (lambda () (values 1 2 10))) exn:fail:contract:arity?)

;; unknown second function:
(module cwv-2-1 racket/base
  (define (cwv-2-1-f x h)
    (call-with-values (lambda () (+ x 3))
      h))
  (provide cwv-2-1-f))
(require 'cwv-2-1)
(test 15 cwv-2-1-f 10 (lambda (y) (+ y 2)))

;; unknown function doesn't expect 1 argument
(module cwv-2-2 racket/base
  (define (cwv-2-2-f x h)
    (call-with-values (lambda () (+ x 3))
      h))
  (provide cwv-2-2-f))
(require 'cwv-2-2)
(err/rt-test (cwv-2-2-f 10 (lambda (y z) (+ y 2))) exn:fail:contract:arity?)

;; known function, unknown number of results:
(module cwv-2-3 racket/base
  (define (cwv-2-3-f g h)
    (call-with-values (lambda () (g))
      h))
  (provide cwv-2-3-f))
(require 'cwv-2-3)
(test 12 cwv-2-3-f (lambda () 10) (lambda (y) (+ y 2)))
(test 23 cwv-2-3-f (lambda () (values 10 11)) (lambda (y z) (+ y z 2)))
(err/rt-test (cwv-2-3-f (lambda () (values 1 2)) (lambda (y) (+ y 2))) exn:fail:contract:arity?)
(err/rt-test (cwv-2-3-f (lambda () 10) (lambda (y z) (+ y 2))) exn:fail:contract:arity?)
(err/rt-test (cwv-2-3-f (lambda () (values 1 2 3)) (lambda (y z) (+ y 2))) exn:fail:contract:arity?)

;; unknown first function:
(module cwv-2-5 racket/base
  (define (cwv-2-5-f g h)
    (call-with-values g h))
  (provide cwv-2-5-f))
(require 'cwv-2-5)
(test 12 cwv-2-5-f (lambda () 10) (lambda (y) (+ y 2)))
(err/rt-test (cwv-2-5-f (lambda () (values 1 2)) (lambda (y) (+ y 2))) exn:fail:contract:arity?)
(err/rt-test (cwv-2-5-f (lambda () 1) (lambda (y z) (+ y 2))) exn:fail:contract:arity?)
(err/rt-test (cwv-2-5-f (lambda () (values 1 2 3)) (lambda (y z) (+ y 2))) exn:fail:contract:arity?)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Transform call-with-values to direct aplication:
(test-comp '(lambda (f) (f 7))
           '(lambda (f) (call-with-values (lambda () 7) (lambda (x) (f x)))))
(test-comp '(lambda () (car 7))
           '(lambda () (call-with-values (lambda () 7) car)))
(test-comp '(lambda () ('not-a-procedure 7))
           '(lambda () (call-with-values (lambda () 7) 'not-a-procedure))
           #f)
(test-comp '(module ? racket/base
              (define f (lambda (x) (list x 0)))
              (lambda () (display f) (f 7)))
           '(module ? racket/base
              (define f (lambda (x) (list x 0)))
              (lambda () (display f) (call-with-values (lambda () 7) f))))
(test-comp '(module ? racket/base
              (define f (let ([tmp (list 0)]) (lambda (x) (list x tmp))))
              (lambda () (f 7)))
           '(module ? racket/base
              (define f (let ([tmp (list 0)]) (lambda (x) (list x tmp))))
              (lambda () (call-with-values (lambda () 7) f))))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Inlining with higher-order functions:

(test 0 'ho1 (let ([x (random 1)])
               ((let ([fn (add1 (random 1))])
                  (lambda (c) c))
                x)))
(test 0 'ho2 (let ([x (random 1)]
                   [id (lambda (c) c)])
               ((let ([fn (add1 (random 1))])
                  id)
                x)))
(test 0 'ho3 (let ([proc (lambda (q)
                           (let ([fn (add1 (random 1))])
                             (lambda (c) c)))])
               (let ([x (random 1)])
                 ((proc 99) x))))
(test '(2 0) 'ho4 (let ([y (+ 2 (random 1))])
                    (let ([x (random 1)])
                      ((let ([fn (add1 (random 1))])
                         (lambda (c) (list y c)))
                       x))))
(test '(2 0) 'ho5 (let ([y (+ 2 (random 1))])
                    (let ([x (random 1)]
                          [id (lambda (c) (list y c))])
                      ((let ([fn (add1 (random 1))])
                         id)
                       x))))
(test '(2 0) 'ho6 (let ([y (+ 2 (random 1))])
                    (let ([proc (lambda (q)
                                  (let ([fn (add1 (random 1))])
                                    (lambda (c) (list y c))))])
                      (let ([x (random 1)])
                        ((proc 98)
                         x)))))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Check that an unboxable flonum argument 
;; is not incorrectly inferred:

(test '(done)
      'unboxing-inference-test
      (let ()
        (define (f x y)
          (if (zero? y)
              ;; prevents inlining:
              '(done)
              (if (zero? y)
                  ;; incorrectly triggered unboxing, 
                  ;; once upon a time:
                  (fl+ x 1.0) 
                  ;; not a float argument => no unboxing of x:
                  (f y (sub1 y)))))
        (f 1.0 100)))

(when (extflonum-available?)
  (test '(done)
      'unboxing-inference-test
      (let ()
        (define (f x y)
          (if (zero? y)
              ;; prevents inlining:
              '(done)
              (if (zero? y)
                  ;; incorrectly triggered unboxing, 
                  ;; once upon a time:
                  (extfl+ x 1.0t0) 
                  ;; not a float argument => no unboxing of x:
                  (f y (sub1 y)))))
        (f 1.0t0 100))))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Test against letrec-splitting bug:

(err/rt-test (eval `(begin
                      (define (T x) 'v)
                      (let ([A (lambda (x) 'v)]) 
                        (define (B x) (F))
                        (define (C x) (A)) ; turns into constant
                        (define (D x) (D))
                        (define (E x) (A) (T))
                        (define (F x) 'v)
                        (list (C) (E) (D)))))
             exn:fail:contract:arity?)

(err/rt-test (eval `(begin
                      (define (T x) 'v)
                      (let ()
                        (define (A x) 'v)
                        (define (B x) 'v)
                        (define (C x) 'v)
                        (define (D x) (B))
                        (define (E x) (H) (E))
                        (define (F x) (C))
                        (define (G x) (T))
                        (define (H x) (A) (T))
                        (define (I x) 'v)
                        (H)
                        (F))))
             exn:fail:contract:arity?)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Make sure the compiler doesn't reorder past a mutable variable:

(let ()
  (define (example-1 lst)
    (define x 0)
    (define (doit)
    (reverse (foldl (lambda (v store) (set! x (add1 x)) (cons v store))
                    '() lst)))
    (let ([results (doit)])
      (list x results)))
  (test '(3 (a b c)) example-1 '(a b c)))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Make sure JIT-implemented `apply-values' recognizes chaperones:

(test 99 (lambda ()
           (call-with-values 
               (lambda () (apply values (make-list (add1 (random 1)) '(99))))
             (chaperone-procedure car (lambda (v) v)))))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; check `fl->fx' on unboxed argument:

(test 1 (lambda (x) (fl->fx (fl/ (fl- x 0.0) 1.0))) 1.0)
(test 1 (lambda (x) (inexact->exact (fl/ (fl- x 0.0) 1.0))) 1.0)
(err/rt-test (let ([f (lambda (x) (fl->fx (fl/ (fl- x 0.0) 1.0)))])
               (set! f f)
               (f 1e100))
             ;; make sure that exception reports actual bad argument, and
             ;; not some bad argument due to the fact that the original
             ;; was unboxed:
             (lambda (exn)
               (regexp-match #rx"1e[+]?100" (exn-message exn))))
(test (inexact->exact 1e100) (lambda (x) (inexact->exact (fl/ (fl- x 0.0) 1.0))) 1e100)

(when (extflonum-available?)
  (test 1 (lambda (x) (extfl->fx (extfl/ (extfl- x 0.0t0) 1.0t0))) 1.0t0)
  (test 1 (lambda (x) (extfl->exact (extfl/ (extfl- x 0.0t0) 1.0t0))) 1.0t0)
  (err/rt-test (let ([f (lambda (x) (extfl->fx (extfl/ (extfl- x 0.0t0) 1.0t0)))])
                 (set! f f)
                 (f 1t100))
               ;; make sure that exception reports actual bad argument, and
               ;; not some bad argument due to the fact that the original
               ;; was unboxed:
               (lambda (exn)
                 (regexp-match #rx"1t[+]?100" (exn-message exn))))
  (test (extfl->exact 1t100) (lambda (x) (extfl->exact (extfl/ (extfl- x 0.0t0) 1.0t0))) 1t100))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Check that compiler handles shifting `#%variable-reference'

(test #f
      'varref-shift
      (let ()
        (define (f #:x [x #f]) #f)
        (define (g #:y [y #f])
          (begin (f) #f))
        #f))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Make sure the compiler doesn't end up in an infinite inling loop:

(module unc-small-self-call racket/base
  (define unc1
    (let ([x 1])
      (lambda ()
        (unc1))))
  (unc1))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Regression test related to the `let'-resolve pass:

(module check-against-problem-in-let-resolver racket/base
  (let-values (((fail2) 12))
    (let ([debugger-local-bindings
           (lambda ()
             (case-lambda ((v) (set! fail2 v))))])
      (let ([f3 (lambda ()
                  (let ([debugger-local-bindings
                         (lambda ()
                           (debugger-local-bindings))])
                    '3))])
        (let ([debugger-local-bindings
               (lambda ()
                 (case-lambda ((v) (set! f3 v))))])
          (f3))))))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Make sure that certain lifting operations
;;  do not lose track of flonum-ness of a variable:

(let ([e '(let ([f (random)])
            (define (s t)
              (cons
               (lambda () (s (fl+ t 1.0)))
               (lambda () f)))
            (s 0.0))]
      [ns (make-base-namespace)]
      [o (open-output-bytes)])
  (parameterize ([current-namespace ns])
    (namespace-require 'racket/flonum)
    (write (compile e) o))
  ;; bytecode validation can catch the relevant mistake:
  (parameterize ([read-accept-compiled #t])
    (read (open-input-bytes (get-output-bytes o)))))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Check compilation of an example that triggers
;; shifting of a closure's coordinates during
;; optimization without reoptimization:

(let ([ns (make-base-namespace)])
  (parameterize ([current-namespace ns])
    (namespace-require 'racket/unsafe/ops)
    (compile '(lambda (a)
                (unsafe-fl- a
                            (lambda ()
                              (set! a 'v)))))))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Check compilation of an n-ary `/' that isn't
;; constant folded due to a divide-by-zero:

(err/rt-test (call/cc (lambda (k) (/ 1 2 0))))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Check slow path on `list-tail', where
;; index is > 10000:

(test 4.8
      'list-ref-test
      (let loop ((line 0))
        (let* ((numlist (build-list 20004 (lambda (x) 2.4)))
               (n (length numlist)))
          (let* ((mid (/ n 2))
                 (n1 (car numlist))
                 (n2 (list-ref numlist mid)))
            (for-each values numlist)
            (+ n1 n2)))))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Check JIT handling of unboxed arguments in loops,
;;   including a loop starts in tail and non-tail positions.

(let ()
  (define N 100000)

  (define (non-tail)
    (define-values (a b)
      (let loop ([n N] [x -1.0] [y 1.0])
        (cond
         [(zero? n) (values x y)]
         [else (loop (sub1 n)
                     (fl+ x -1.0)
                     (fl+ y 1.0))])))
    (values a b))

  (define (non-tail2)
    (for/fold ([v 0.0]) ([i (in-range N)])
      (define-values (a b)
        (let loop ([n 10] [x -1.0] [y 1.0])
          (cond
           [(zero? n) (values x y)]
           [else (loop (sub1 n)
                       (fl+ x -1.0)
                       (fl+ y 1.0))])))
      (fl+ v (fl- a b))))

  (define (tail)
    (let loop ([n N] [x -1.0] [y 1.0])
      (cond
       [(zero? n) (values x y)]
       [else (loop (sub1 n)
                   (fl+ x -1.0)
                   (fl+ y 1.0))])))

  (define x-tail #f)
  (define x-non-tail #f)
  (define x-non-tail2 #f)
  (set! x-tail tail)
  (set! x-non-tail non-tail)
  (set! x-non-tail2 non-tail2)

  (test-values '(-100001.0 100001.0) non-tail)
  (test -2200000.0 non-tail2)
  (test-values '(-100001.0 100001.0) tail))


(when (extflonum-available?)
  (let ()
    (define N 100000)
    
    (define (non-tail)
      (define-values (a b)
        (let loop ([n N] [x -1.0t0] [y 1.0t0])
          (cond
           [(zero? n) (values x y)]
           [else (loop (sub1 n)
                       (extfl+ x -1.0t0)
                       (extfl+ y 1.0t0))])))
      (values a b))
    
    (define (non-tail2ext)
      (for/fold ([v 0.0t0]) ([i (in-range N)])
        (define-values (a b)
          (let loop ([n 10] [x -1.0t0] [y 1.0t0])
            (cond
             [(zero? n) (values x y)]
             [else (loop (sub1 n)
                         (extfl+ x -1.0t0)
                         (extfl+ y 1.0t0))])))
        (extfl+ v (extfl- a b))))
    
    (define (tail)
      (let loop ([n N] [x -1.0t0] [y 1.0t0])
        (cond
         [(zero? n) (values x y)]
         [else (loop (sub1 n)
                     (extfl+ x -1.0t0)
                     (extfl+ y 1.0t0))])))

    (define x-tail #f)
    (define x-non-tail #f)
    (define x-non-tail2ext #f)
    (set! x-tail tail)
    (set! x-non-tail non-tail)
    (set! x-non-tail2ext non-tail2ext)

    (test-values '(-100001.0t0 100001.0t0) non-tail)
    (test -2200000.0t0 non-tail2ext)
    (test-values '(-100001.0t0 100001.0t0) tail)))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Check for correct fixpoint calculation when lifting

;; This test is especially fragile. It's a minimized(?) variant
;; of PR 12910, where just enbought `with-continuation-mark's
;; are needed to thwart inlining, and enough functions are 
;; present in the right order to require enough fixpoint
;; iterations.

(define a-top-level-variable 5)
(define (do-test-of-lift-fixpoint)
  (define-syntax-rule (wcm e) (with-continuation-mark a-top-level-variable 'e e))
  (define (parse-string input-string)

    (let* ((nextTokenIsReady #f)

           (nextCharacter #\space)
           (nextCharacterIsReady #f)
           (count 0)

           (input-index 0)

           (input-length (string-length input-string)))
      
      (define (scanner0)
        (state0 (wcm (scanchar))))
      
      (define (state0 c)
        (if (eq? c #\()
            (begin
              (consumechar)
              'lparen)
            (if (eq? c #\,)
                (wcm (state1 (scanchar)))
                (void))))
      (define (state1 c)
        (wcm (consumechar)))

      (define (parse-datum)
        (let ([t (next-token)])
          (if (eq? t 'lparen)
              (parse-compound-datum)
              (wcm (parse-simple-datum)))))
      
      (define (parse-simple-datum)
        (wcm (next-token)))
      
      (define (parse-compound-datum)
        (wcm
         (begin
           (consume-token!)
           (parse-datum))))

      (define (next-token)
        (wcm (scanner0)))
      
      (define (consume-token!)
        (set! nextTokenIsReady #f))
      
      (define (scanchar)
        (when (= count 4) (error "looped correctly"))
        (begin
          (set! count (add1 count))
          (if nextCharacterIsReady
              nextCharacter
              (begin
                (if (< input-index input-length)
                    (set! nextCharacter
                          (wcm (string-ref input-string input-index)))
                    (set! nextCharacter #\~))
                (set! nextCharacterIsReady #t)
                (scanchar)))))
      
      (define (consumechar)
        (when (wcm (not nextCharacterIsReady))
          (scanchar)))

      (parse-datum)))
  (set! parse-string parse-string)
  (parse-string "()"))
(err/rt-test (do-test-of-lift-fixpoint) exn:fail?)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; generate byecode with a lifted function that has
;; a boxed argument and rest args, to test that case
;; of the validator

(parameterize ([current-namespace (make-base-namespace)])
  (define o (open-output-bytes))
  (write
   (compile
    '(lambda (x)
       (define (g . y) (if (zero? (random 1))
                           (reverse (cons x y))
                           (g y y y y y y y y y)))
       (set! x x)
       (g 12 13)))
   o)
  (test '(13 12 10)
        (parameterize ([read-accept-compiled #t])
          (eval (read (open-input-bytes (get-output-bytes o)))))
        10))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(module check-tail-call-by-jit-for-struct-predicate racket/base
  (provide go)

  (struct s (x))
  
  (define f #f)
  (set! f (lambda (v)
            (if (zero? v)
                (let ([vec (make-vector 6)])
                  (vector-set-performance-stats! vec (current-thread))
                  (vector-ref vec 3))
                (s? (sub1 v)))))
  
  (void (f 5)) ; JIT decides that `s?' is a struct predicate
  (set! s? f) ; break the JIT's optimistic assumption
  
  (define (go)
    (define init-size
      (let ([vec (make-vector 6)])
        (vector-set-performance-stats! vec (current-thread))
        (vector-ref vec 3)))
    (define size (f 500000)) ; make sure that this still leads to a tail loop
    ((- size init-size) . < . 20000)))

(test #t (dynamic-require ''check-tail-call-by-jit-for-struct-predicate 'go))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Test bytecode validator's checking of constantness

(let ()
  (define c1
    '(module c1 racket/kernel
       ((if (zero? (random 1))
            (lambda (f) (display (f)))
            #f)
        (lambda ()
          ;; This access of i should raise an exception:
          i))
       (define-values (i) (random 1))))

  (define o (open-output-bytes))

  (parameterize ([current-namespace (make-base-namespace)])
    (write (compile c1) o))

  (define m (zo-parse (open-input-bytes (get-output-bytes o))))

  (define o2 (open-output-bytes))

  ;; construct bytecode that is broken by claiming that `i' is constant
  ;; in the too-early reference:
  (void
   (write-bytes
    (zo-marshal
     (match m
       [(compilation-top max-let-depth binding-namess prefix code)
        (compilation-top max-let-depth binding-namess prefix 
                         (let ([body (mod-body code)])
                           (struct-copy mod code [body
                                                  (match body 
                                                    [(list a b)
                                                     (list (match a
                                                             [(application rator (list rand))
                                                              (application
                                                               rator
                                                               (list
                                                                (struct-copy 
                                                                 lam rand
                                                                 [body
                                                                  (match (lam-body rand)
                                                                    [(toplevel depth pos const? ready?)
                                                                     (toplevel depth pos #t #t)])])))])
                                                           b)])])))]))
    o2))

  ;; validator should reject this at read or eval time (depending on how lazy validation is):
  (err/rt-test (parameterize ([current-namespace (make-base-namespace)]
                              [read-accept-compiled #t])
                 (eval (read (open-input-bytes (get-output-bytes o2)))))
               exn:fail:read?))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; make sure sfs pass doesn't add a nested begin0
;; to clear the variables used in the first expression

(let ()
  (define c
    '(module c racket/base
       (define z (let ([result (random)])
                   (begin0 (lambda () result) (newline))))))

  (define o (open-output-bytes))

  (parameterize ([current-namespace (make-base-namespace)])
    (write (compile c) o))

  (define m (zo-parse (open-input-bytes (get-output-bytes o))))

  ; extract the content of the begin0 expression
  (define (analyze-beg0 m)
    (define def-z (car (mod-body (compilation-top-code m))))
    (define body-z (let-one-body (def-values-rhs def-z)))
    (define expr-z (car (beg0-seq body-z)))
    (cond
      [(lam? expr-z) 'ok]
      [(beg0? expr-z) 'not-reduced-beg0-in-sfs]
      [else 'unexpected]))

  (test 'ok (lambda () (analyze-beg0 m))))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; make sure `begin0' propertly propagates "multiple results" flags

(test '(1 2 3) (lambda ()
                 (call-with-values
                     (lambda () (begin0
                                 (values 1 2 3)
                                 (newline)))
                   list)))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Make sure compiler isn't too agressive for the validator
;;  in terms of typed arguments:

(let ([m '(module m racket/base
            (require racket/flonum)
            (define (f x)
              (letrec ([z (if x (other 1) 'none)]
                       [collect (lambda (x)
                                  (lambda (n)
                                    (list '(1 2 3)
                                          (fl+ n x))))]
                       [a (collect 0.0)]
                       [other 6])
                (values a z))))])
  (define o (open-output-bytes))
  (write (compile m) o)
  (parameterize ([read-accept-compiled #t])
    ;; too-aggressive compilation produces a validator failure here
    (read (open-input-bytes (get-output-bytes o)))))

(when (extflonum-available?)
  (let ([m '(module m racket/base
              (require racket/extflonum)
              (define (f x)
                (letrec ([z (if x (other 1) 'none)]
                         [collect (lambda (x)
                                    (lambda (n)
                                      (list '(1 2 3)
                                            (extfl+ n x))))]
                         [a (collect 0.0t0)]
                         [other 6])
                  (values a z))))])
    (define o (open-output-bytes))
    (write (compile m) o)
    (parameterize ([read-accept-compiled #t])
      ;; too-aggressive compilation produces a validator failure here
      (read (open-input-bytes (get-output-bytes o))))))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; check error checking of JITted `continuation-mark-set-first'

(err/rt-test (let ([f #f])
               (set! f (lambda ()
                         (continuation-mark-set-first 5 #f)))
               (f)))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; check a `case-lambda' that closes over flonums

(let ()
  (define f #f)
  (set! f
        (lambda (x)
          (let ([x (fl+ x x)])
            (case-lambda
             [() (fl+ x x)]
             [(y) (fl+ x y)]))))
  
  (test 4.0 (f 1.0) 2.0))

(when (extflonum-available?)
  (let ()
    (define f #f)
    (set! f
          (lambda (x)
            (let ([x (extfl+ x x)])
              (case-lambda
                [() (extfl+ x x)]
                [(y) (extfl+ x y)]))))
    
    (test 4.0t0 (f 1.0t0) 2.0t0)
    ))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; define-inline

(require (only-in racket/performance-hint define-inline))
(let ([O (open-output-string)])
  ;; Compares output to make sure that things are evaluated the right number of
  ;; times, and in the right order.
  (define-syntax-rule (show x r) (begin (display x O) r))
  (define-syntax-rule (test/output E result output)
    (begin (test result (lambda () E))
           (test #t equal? output
                 (bytes->string/utf-8 (get-output-bytes O #t)))))
  ;;
  (define-inline (f x) (+ x x))
  (test/output (f (show 'arg1 1))
               2 "arg1")
  ;;
  (define-inline (f2 x y) (+ x y))
  (test/output (f2 (show 'arg1 1) (show 'arg2 2))
               3 "arg1arg2")
  ;;
  (define-inline (g #:x [x 0]) (f x))
  (test/output (g #:x (show 'arg1 1))
               2 "arg1")
  (test/output (g)
               0 "")
  ;;
  (define-inline (h #:x x) (f x))
  (test/output (h #:x (show 'arg1 1))
               2 "arg1")
  ;;
  (define-inline (i [x 0]) (f x))
  (test/output (i (show 'arg1 1))
               2 "arg1")
  (test/output (i)
               0 "")
  ;;
  (define-inline (j x #:y [y 0]) (+ x y))
  (test/output (j (show 'arg1 1) #:y (show 'arg2 2))
               3 "arg1arg2")
  (test/output (j #:y (show 'arg1 2) (show 'arg2 1))
               3 "arg1arg2")
  (test/output (j (show 'arg1 1))
               1 "arg1")
  ;;
  (define-inline (k x [y x]) (+ x y))
  (test/output (k (show 'arg1 1) (show 'arg2 2))
               3 "arg1arg2")
  (test/output (k (show 'arg1 1))
               2 "arg1")
  ;;
  (define-inline (l . x) (+ (apply + x) (apply + x)))
  (test/output (l (show 'arg1 1) (show 'arg2 2))
               6 "arg1arg2")
  ;;
  (define-inline (l2 y . x) (+ y y (apply + x) (apply + x)))
  (test/output (l2 (show 'arg0 3) (show 'arg1 1) (show 'arg2 2))
               12 "arg0arg1arg2")
  ;;
  (define-inline (l3 y [z 0] . x) (+ y y z z z (apply + x) (apply + x)))
  (test/output (l3 (show 'arg0 3) (show 'arg1 1) (show 'arg2 2))
               13 "arg0arg1arg2")
  (test/output (l3 (show 'arg0 3))
               6 "arg0")
  ;;
  (define-inline (l4 #:x [x 0] . y) (+ x x x (apply + y) (apply + y)))
  (test/output (l4 #:x (show 'arg1 1))
               3 "arg1")
  (test/output (l4 #:x (show 'arg1 1) (show 'arg2 2) (show 'arg3 3))
               13 "arg1arg2arg3")
  (test/output (l4 (show 'arg2 2) (show 'arg3 3))
               10 "arg2arg3")
  ;; test for function fallback (recursion)
  (define-inline (sum . l) (if (null? l) 0 (+ (car l) (apply sum (cdr l)))))
  (test/output (sum 1 2 3 4)
               10 "")
  ;; higher-order use
  (define-inline (add2 x) (+ x 2))
  (test/output (add2 3)
               5 "")
  (test/output (map add2 '(1 2 3))
               '(3 4 5) "")
  ;; currying syntax
  (define-inline ((adder x) y) (+ x y))
  (test/output (let ([add2 (adder (show 'arg1 2))])
                 (+ (add2 (show 'arg2 1)) (add2 (show 'arg2 2))))
               7 "arg1arg2arg2")
  (define-inline (even? x) (if (zero? x) #t (odd?  (sub1 x))))
  (define-inline (odd? x)  (if (zero? x) #f (even? (sub1 x))))
  (test/output (odd? 2)
               #f "")

  ;; multiple keyword arguments that have to be sorted:
  (define-inline (sub #:a a #:b b)
    (- a b))
  (test/output (sub #:a 2 #:b 1)
               1 "")
  (test/output (sub #:b 1 #:a 2)
               1 "")
  )


;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Make sure the compiler unboxes the `v'
;; argument in the loop below:

(let ()
  (define l '(module m racket/base
               (require racket/flonum)
               (define (f)
                 (let loop ([n 1000][v 0.0])
                   (if (zero? n)
                       v
                       (loop (- n 1)
                             (fl+ v 2.0)))))))
  (define b
    (let ([o (open-output-bytes)])
      (write (compile l) o)
      (parameterize ([read-accept-compiled #t])
        (zo-parse (open-input-bytes (get-output-bytes o))))))
  (let* ([m (compilation-top-code b)]
         [d (car (mod-body m))]
         [b (closure-code (def-values-rhs d))]
         [c (application-rator (lam-body b))]
         [l (closure-code c)]
         [ts (lam-param-types l)])
    (test 'flonum cadr ts)))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Make sure the compiler doesn't add a check for whether
;; `later` is defined in the body of `kw-proc`:

(let ()
  (define l '(module m racket/base
               (define (kw-proc x #:optional [optional 0])
                 (later))
              (define (later) '(1 2 3))))
  (define b
    (let ([o (open-output-bytes)])
      (write (compile l) o)
      (parameterize ([read-accept-compiled #t])
        (zo-parse (open-input-bytes (get-output-bytes o))))))
  (let* ([m (compilation-top-code b)]
         [d (car (mod-body m))]
         [rhs (def-values-rhs d)]
         [b (inline-variant-direct rhs)]
         [v (application-rator (lam-body b))])
    (test #t toplevel-const? v)))

(let ()
  (define l '(module m racket/base
              (struct s (x))
              (define (kw-proc x #:optional [optional 0])
                (later))
              (define an-s (s 10))
              (define (later) '(1 2 3))))
  (define b
    (let ([o (open-output-bytes)])
      (write (compile l) o)
      (parameterize ([read-accept-compiled #t])
        (zo-parse (open-input-bytes (get-output-bytes o))))))
  (let* ([m (compilation-top-code b)]
         [d (cadr (mod-body m))]
         [rhs (def-values-rhs d)]
         [b (inline-variant-direct rhs)]
         [v (application-rator (lam-body b))])
    (test #t toplevel-const? v)))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The validator should understand that a structure
;; constructor always succeeds:

(let ()
  (define (go sub)
    (let ([e `(module m racket/base
                (provide bar)
                (struct foo (x))
                (define empty
                  (let ((t ,sub))
                    (lambda () t)))
                (define (bar)
                  (empty)))]
          [o (open-output-bytes)])
      (write (compile e) o)
      (parameterize ([current-namespace (make-base-namespace)])
        (eval
         (parameterize ([read-accept-compiled #t])
           (read (open-input-bytes (get-output-bytes o)))))
        ((dynamic-require ''m 'bar)))))
  (go '(foo 1))
  (go '(foo? (list 1 2 3)))
  ;; No optimization here for this one:
  (go '(foo-x (foo 1))))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The JIT should check an inlined-constructor guess
;; to make sure that it's "simple" (e.g., no guards)

(let ([f #f])
  (set! f (lambda (f x) (f x)))
  (struct a (x))
  (struct b (y) #:property prop:procedure 0)
  (test 1 a-x (f a 1))
  (test 2 (f b (lambda () 2))))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Make sure an already-in-place loop argument
;; isn't cleared for space safety:

(test '((1 2 3 4 5 6)
        (- 4 6 8 10 12)
        (- - 9 12 15 18)
        (- - - 16 20 24)
        (- - - - 25 30)
        (- - - - - 36))
      values
      (for/list ([y (in-range 1 7)])
        ;; `y' is the already in place argument for the
        ;; following loop:
        (for/list ([x (in-range 1 7)])
          (if (<= y x) (* x y) '-))))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Make sure that a procedure used in a first-order
;; way bound by `letrec' can have a typed closure element:

(let ([e `(module m racket/base
            (provide f)
            (define (f v)
              (let ([y (vector-length v)])
                (letrec ([foo (lambda (r)
                                (if (zero? r)
                                    y
                                    (foo (sub1 r))))])
                  foo))))]
      [o (open-output-bytes)])
  (write (compile e) o)
  (parameterize ([current-namespace (make-base-namespace)])
    (eval
     (parameterize ([read-accept-compiled #t])
       (read (open-input-bytes (get-output-bytes o)))))
    (((dynamic-require ''m 'f) (vector 1)) 0)))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Make sure that constant folding doesn't try to
;; use too much memory, where a run-time limit would
;; catch the problem:

(module uses-too-much-memory-for-shift racket/base
  (define c (make-custodian))
  (custodian-limit-memory c (* 1024 1024 10))
  (parameterize ([current-custodian c])
    (sync
     (thread
      (lambda ()
        (with-handlers ([exn:fail:out-of-memory? void])
          (arithmetic-shift 1 30070458541082)))))))
(when (eq? '3m (system-type 'gc))
  (void (dynamic-require ''uses-too-much-memory-for-shift #f)))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Make sure that closure fields are correctly type-tagged
;; when a function has an unused rest arg:

(parameterize ([current-namespace (make-base-namespace)]
               [read-on-demand-source #f]
               [read-accept-compiled #t])
  (let ([o (open-output-bytes)])
    (write (compile '(module m racket/base
                       (require racket/fixnum)
                       (define (test l)
                         (define n (fxmax (length l) 1))
                         (lambda _ n))))
           o)
    ;; Should succeed, as opposed to a validation error:
    (eval (read (open-input-bytes (get-output-bytes o))))))

(parameterize ([current-namespace (make-base-namespace)]
               [read-on-demand-source #f]
               [read-accept-compiled #t])
  (let ([o (open-output-bytes)])
    (write (compile '(module m racket/base
                       (require racket/fixnum)
                       (define ident (lambda (x) x))
                       (set! ident ident)
                       (define app (lambda (f) (f)))
                       (set! app app)
                       (let ([n (fxmax (length '()) 1)])
                         (app (lambda _ (ident n))))))
           o)
    ;; Should succeed, as opposed to a validation error:
    (eval (read (open-input-bytes (get-output-bytes o))))))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Check interaciton of 3-D macros, non-empty closures, JIT, and bytecode:

(let ([o (open-output-bytes)])
  (write (compile
          #'(module 3D-eval-macro racket/base
              (require (for-syntax racket/base syntax/parse))
              (provide phase1-eval)
              
              (define-syntax (phase1-eval stx)
                #'((let-syntax ([go (lambda (stx) #`#,(lambda () #'(void)))])
                     (go))))))
         o)
  (eval (parameterize ([read-accept-compiled #t])
          (read (open-input-bytes (get-output-bytes o))))))

(require '3D-eval-macro)
(define f (lambda ()
            (phase1-eval)))
(test #t syntax? (f))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Check unboxing through conditionals

(let ()
  (define (check pred t1 e1)
    (define v (* 2.0 (if (eval (pred 7.0))
                         (eval (t1 7.0))
                         (eval (e1 7.0)))))
    (test v (eval `(lambda (arg)
                     (let ([x (if ,(pred 'arg)
                                  ,(t1 'arg)
                                  ,(e1 'arg))])
                       (fl+ x x))))
          7.0)
    (test v (eval `(lambda (arg)
                     (fl* 2.0 (if ,(pred 'arg)
                                  ,(t1 'arg)
                                  ,(e1 'arg)))))
          7.0))
  (for ([pred (in-list (list
                        (lambda (arg) `(negative? ,arg))
                        (lambda (arg) `(positive? ,arg))
                        (lambda (arg) `(even? (fl* ,arg ,arg)))))])
    (for ([t1 (in-list (list
                        (lambda (arg) `(fl+ ,arg 8.0))
                        (lambda (arg) `(fl- (fl+ ,arg 8.0) 1.0))))])
      (for ([e1 (in-list (list (lambda (arg) `(fl* 8.0 ,arg))
                               (lambda (arg) `(begin
                                                (display "")
                                                (fl* 8.0 ,arg)))))])
        (check pred t1 e1)))))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Check unboxing with mutual recusion:

(let ()
  ;; Literal lists thwart inlining:
  (define (odd n acc)
    (if (fl= n 0.0)
        '(nope)
        (even (fl- n 1.0)
              (fl+ acc 0.5))))
  (define (even n acc)
    (if (fl= n 0.0)
        (cons acc '(yep))
        (odd (fl- n 1.0)
             (fl+ acc 0.5))))
  (test '(0.5e7 yep) (lambda () (even 1e7 0.0))))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Double-check that allocation is not moved
;; across continuation capture:

(let ()
  (define g #f)
  (define p (make-continuation-prompt-tag))
  (define (t)
    (let ([a (cons 1 2)])
      (call-with-composable-continuation (λ (k) (set! g k)) p)
      a))
  (call-with-continuation-prompt t p)
  (test #t eq?
        (call-with-continuation-prompt g p)
        (call-with-continuation-prompt g p)))

(let ()
  (define g #f)
  (define p (make-continuation-prompt-tag))
  (define (t)
    (let ([a (fl+ (random) 1.0)])
      (call-with-composable-continuation (λ (k) (set! g k)) p)
      (if (fl= a 0.0) ; encourage unboxing of `a` (if not for the continuation capture)
          #f
          a))) ; must not delay flnonum allocation to here
  (call-with-continuation-prompt t p)
  (test #t eq?
        (call-with-continuation-prompt g p)
        (call-with-continuation-prompt g p)))
  
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Make sure the compiler isn't confused by
;; throwaway internal-definition expansion:

(test #t procedure?
      (eval '(lambda (db)
               (if #f
                   (let ()
                     (define matches
                       (let loop ()
                         (define s db)
                         null))
                     5)
                   (void)))))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Check that bytecode validator is consistent with respect to the
;; optimizer and special-casing of bitwise operators:

(let ([o (open-output-bytes)])
  (write (compile
          #'(module function racket/base
              (lambda (x)
                (let ([v (bitwise-xor 0
                                      (let ([v (random)])
                                        (begin
                                          (bitwise-and x 2))))])
                  (list (bitwise-and v 2) v)))))
         o)
  (eval (parameterize ([read-accept-compiled #t])
          (read (open-input-bytes (get-output-bytes o))))))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Check that an unsafe opertion's argument is
;; not "optimized" away if it's a use of
;; a variable before definition:

(err/rt-test (let ()
               (unsafe-fx+ x 1)
               (define x 3)
               x)
             exn:fail:contract:variable?)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Try to check that JIT's register shortcuts
;; (tracking when a value is ready in a register)
;; works ok. The example here failed once upon
;; a time, at least.

;; Example by Dale Vaillancourt

(module check-jit-registers-shortcut racket/base
  (require racket/fixnum racket/match) 

  (struct bral-empty () #:transparent)
  (struct bral-node (weight tree rest) #:transparent)

  (struct node (val even odd) #:transparent)

  (struct var (name idx) #:transparent)


  (define (half n) (fxrshift n 1))

  (define (lookup-tree w i t)
    (if (node? t)
        (if (zero? i) 
            (node-val t)
            (let [(w/2 (half w))]
              (if (<= i w/2)
                  (lookup-tree w/2 (fx- i 1) (node-even t))
                  (lookup-tree w/2 (- (- i 1) w/2) (node-odd t)))))
        (if (zero? i) t #f)))

  (define (lookup i ls)
    (match ls 
      [(bral-empty) #f]
      [(bral-node weight tree ls*)
       (if (< i weight)
           (lookup-tree weight i tree)
           (lookup (- i weight) ls*))]))

  (define a-node
    (bral-node
     15
     (node
      'a
      (node 'b (node 'c 'd 'e) (node 'f 'g 'h))
      (node 'i (node 'j 'k 'l) (node 'm 'n 'o)))
     (bral-empty)))

  (define result (lookup 2 a-node))
  (provide result))

(test 'c dynamic-require ''check-jit-registers-shortcut 'result)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Check (non-)inference of flonums with `if` branches:

(let ()
  (define f (lambda (x)
              (let ([v (if x 1 2.0)])
                (fl+ v v))))
  (set! f f)
  (err/rt-test (f #t)))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Make sure compilation doesn't try to inline forever:

(module cfg-extract-test racket/base
  (define (report-answer-all k)
    (k (list (random 10))))

  (lambda ()
    (let loop ([success-k 0]
               [fail-k 1]
               [k 0])
      (let ([new-got-k
             (lambda (val stream depth tasks next-k)
               (let ([next-k (lambda (x y tasks)
                               (loop (random)
                                     1
                                     (lambda (end tasks success-k fail-k)
                                       (next-k success-k fail-k 8))))])
                 (report-answer-all (lambda (tasks)
                                      (success-k 0 1 2 3 next-k)))))])
        (k 5 5 new-got-k
           (lambda (tasks)
             (report-answer-all 8)))))))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Check that cross-module inlining decompiles a function
;; with correct use counts on arguments (specifically: B is used
;; twice, so the argument expression can't be inlined for two uses)

(module module-with-cross-module-inlining racket/base
  (require racket/function)

  (module bad racket
    (provide evil-func)
    (define (evil-func A B)
      (A B)
      B))

  (require (submod "." bad))

  (define n 0)

  (define (bar) (set! n (add1 n)) (void))

  (evil-func (curry void) (bar))

  (provide n))

(test 1 dynamic-require ''module-with-cross-module-inlining 'n)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Check that moving `(car x)` doesn't assume
;; the pairness invariant established by `(car x)`.

(define (f x)
  (define (g x)
    (let* ([z (random)]
           [y (car x)])
      (+ (random y) z)))
  (g x))

(err/rt-test (f 10))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Check that JIT-inlined `apply` doesn't overflow the runstack

(define (f n done? . args)
  (cond
   [(positive? n)
    (or (f (sub1 n) done?) #t)]
   [done? #t]
   [(= 50 (length args))
    100]
   [(apply f 0 #t 1 2 3 4 5 6 7 8 9 10 args)
    (apply f 0 #f (cons 1 args))]))

(for/fold ([v 0]) ([i 2])
  (+ v
     (for/fold ([v2 0]) ([i (in-range 100 512)])
       (f i #f))))
(collect-garbage)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Check that `#%variable-reference` inlines ok:

(let ([go
       (lambda ()
         (define foo 3)
         (#%variable-reference foo))])
  (define v (list (go) (go)))
  (set! v v)
  (test '(#t #t) map variable-reference? v))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Check correct use of shift in reduction of non-#f variables in Boolean contexts 

; Due to a bad coordinates use, the optimizer confused f-two with unused-pair, 
; and in a Boolean context reduced f-two to #t. See 16ce8fd90d.
; The right number of let's is difficult to calculate, so we generate
; many variations. Before the fix, this test produced an error when n was 16.

(for ([n (in-range 30)])
  (define many-lets (for/fold ([many-lets '(void)]) ([i (in-range n)])
                      `(let ([f 0]) ,many-lets)))
  (test-comp `(let ()
                (define ignored (lambda () ,many-lets))
                (let ([f-two (not (zero? (random 1)))]
                      [unused-pair (cons 0 0)])
                  (if (let ([f-one #f])
                        (if f-one f-one f-two))
                    (displayln (list 'yes f-two ,n))
                    111111)))
             `(let ()
                (define ignored (lambda () ,many-lets))
                (let ([f-two (not (zero? (random 1)))]
                      [unused-pair (cons 0 0)])
                  (if (let ([f-one #f])
                        (if f-one f-one f-two))
                    (displayln (list 'yes f-two ,n))
                    222222)))
             #f))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Make sure the compiler doesn't try to inline forever,
;; due to bad single-use tracking:

(module check-inline-single-use-tracking racket/base
  (define dup (lambda (f) (f f)))
  (lambda ()
    ;; Initially, `rep` is used only once, but inlining
    ;; followed by other optimizations changes that:
    (let ([rep (lambda (f) (f f))])
      (dup rep))))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Check specialization with a capturing lambda:

(let ()
  (define (f x)
    (procedure-specialize
     (lambda (y)
       (lambda () (+ x y)))))
  (set! f f)
  (test 11 ((f 10) 1)))


(let ()
  (define (f x)
    (set! x (add1 x))
    (procedure-specialize
     (lambda (y)
       (lambda () (+ x y)))))
  (set! f f)  
  (test 12 ((f 10) 1)))

(let ()
  (define (f)
    (procedure-specialize
     (lambda ()
       #'x)))
  (set! f f)
  (test #t syntax? ((f)))
  (test 'x syntax-e ((f))))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Make sure the compiler doesn't try to inline forever a curried
;; version of an infinite loop:

(module curried-module-level-function-calls-itself racket/base
  (define ((proc))
    ((proc))))

(module curried-local-function-calls-itself racket/base
  (let ()
    (define ((proc))
      ((proc)))
    (void proc proc)))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Make sure validation doesn't fail for importing a setter of a
;; structure type that has auto fields:

(module provides-a-mutator-for-a-struct-with-an-auto-field racket/base
  (provide foo set-foo-y!)
  (struct foo (x [y #:mutable] [z #:auto])))

(let ([e `(module uses-mutator-with-an-auto-field racket/base
           (require 'provides-a-mutator-for-a-struct-with-an-auto-field)
           (provide f)
           (define (f x)
             (and x
                  (set-foo-y! x 1))))]
      [o (open-output-bytes)])
  (write (compile e) o)
  (parameterize ([read-accept-compiled #t])
    (eval (read (open-input-bytes (get-output-bytes o)))))
  ((dynamic-require ''uses-mutator-with-an-auto-field 'f) #f))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Make sure that the optimizer doesn't discard a known error on the
;; right-hand side of a `letrec`

(err/rt-test
 (letrec-values ([() (list (3) the-val)]
                 [(the-val) 42])
                777)
 exn:fail:contract?)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The `let` and `with-continuation-mark` wrappers for `b`
;; delay the optimizer's detection of the right-hand side as
;; a closure enough that the resolve pass gets a `letrec`
;; that is being reinterpreted as a `let*`. But make sure 
;; that the location of `a` is allocated before the closure
;; for `b`.

(test (void)
      'call
      (let ([f (letrec ([a 0]
                        [b (let ([t 0])
                             (with-continuation-mark
                                 'x
                               'y
                               (lambda () (set! a 1))))])
                 (list b b))])
        (set! f f)
        ((car f))))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Regression test to check that `values` is
;; handled correctly for estimating clock advances

(module triggers-optimizer-clock-estimation '#%kernel
  
  (define-values (make-sequence) (lambda (_) 3))
  
  (define-values (string>) (lambda (s) s))
  
  (values
   (let-values (((_1 _2) (make-sequence (string>))))
     (void))))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Check that optimizer-clock tracking is ok
;; when a `let-values` split is allowed due to
;; a detected error condition

(let ()
  (define (binop a b)
    'binop)
  (define (bar p)
    (let ([a (car p)])
      (let-values ([(val1 val2) (binop a)])
        'bar)))
  bar)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Check that `string-append` on a known-string argument
;; is not treated consistently by the optimzier and
;; validator

(let ([c (compile
          '(module m racket/base
            (define ill
              (let ((base (string-append "a")))
                (λ (x) (string-append base x))))
            (ill "b")))])
  (define o (open-output-bytes))
  (write c o)
  (parameterize ([read-accept-compiled #t])
    (void (read (open-input-bytes (get-output-bytes o))))))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Check for an optimizer regresssion

(err/rt-test (+ (let-values (((x y) (let-values ((() 9)) 2))) x) (error))
             exn:fail?)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Check that optimizer-clock are updated 
;; after the equal? is reduced to eq?

(test-comp '(lambda (x)
              (let ([m (box 5)])
                (list (equal? x 7) m)))
           '(lambda (x)
              (list (eq? x 7) (box 5))))


;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Regression test to check that the optimizer doesn't
;; get confused in handling a single-use function that
;; is too large to be inlined into multiple uses.

;; The optimizer had a bad interaction between delayed
;; use marking of functions and moving single-use
;; expressions, which somehow is relevant in this
;; module. The fact that the code is at compile time
;; may have been relevant for limiting cross-module inlining.

(module optimizer-single-use-function-test racket/base
  (require (for-syntax racket/base
                       syntax/parse
                       racket/list
                       syntax/stx
                       racket/syntax))
  
  (define-syntax (define-mongo-struct-field stx)
    (syntax-parse stx
      [#:ref
       (list 'x
             'mongo-dict-ref)]
      [#:set!
       (list 'x
             'mongo-dict-set!)]
      [#:inc
       (list (format-id 'struct "inc-~a-~a!" 'struct 'field)
             'mongo-dict-inc!)]
      [#:null
       (list (format-id 'struct "null-~a-~a!" 'struct 'field)
             'mongo-dict-remove!)]
      [#:push
       (list (format-id 'struct "push-~a-~a!" 'struct 'field)
             'mongo-dict-push!)]
      [#:append
       (list (format-id 'struct "append-~a-~a!" 'struct 'field)
             'mongo-dict-append!)]
      [#:set-add
       (format-id 'struct "set-add-~a-~a!" 'struct 'field)]
      [#:set-add*
       (format-id 'struct "set-add*-~a-~a!" 'struct 'field)]
      [#:pop
       (list (format-id 'struct "pop-~a-~a!" 'struct 'field)
             'mongo-dict-pop!)]
      [#:shift
       (list (format-id 'struct "shift-~a-~a!" 'struct 'field)
             'mongo-dict-shift!)]
      [#:pull
       (list (format-id 'struct "pull-~a-~a!" 'struct 'field)
             'mongo-dict-pull!)]
      [#:pull* 'pull]
      [_ 'err])))


;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; At the time of its addition, this example causes the
;; optimizer to initially use the `random-configuration`
;; variable, instead of substituting `(unknown)`, because it
;; can't provide that the substitution is ok --- but later it
;; learns enough to decide the the substitution is ok after
;; all

(module optimizer-decides-to-inline-once-use-after-all racket/base
  (define unknown #f)
  (set! unknown unknown)
  (define (generate-samples)
    (define random-configuration (unknown))
    (for ([i 0])
      (for ([s (in-list 'obviously-not-a-list)])
        (unknown random-configuration)))))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Make sure the expander and compiler don't go quadratic
;; for
;;  (lambda (arg-id ...) (define def-id _rhs) ... (arg-id def-id) ...)

(let ()
  (define (gensym-n n)
    (let loop ([i n])
      (if (zero? i)
          '()
          (cons (gensym) (loop (sub1 i))))))

  (define (time-it n)
    (let ([start (current-process-milliseconds)])
      (let* ([args (gensym-n n)]
             [defns (gensym-n n)])
        (eval
         `(lambda ,args
            ,@(map (lambda (defn) `(define ,defn ',defn)) defns)
            ,@(map (lambda (arg defn) `(,arg ,defn)) args defns))))
      (- (current-process-milliseconds) start)))

  (let loop ([tries 3])
    (let ([a (time-it 100)]
          [b (time-it 1000)])
      ;; n lg(n) is ok, n^2 is not
      (when (b . > . (* 50 a))
        (if (zero? tries)
            (test 'fail "compilation took too long" (/ b a 1.0))
            (loop (sub1 tries)))))))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(report-errs)
