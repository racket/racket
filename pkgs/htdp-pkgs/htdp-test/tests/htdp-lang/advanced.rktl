
;; Basic checks for the advanced language. See also
;;  beginner.rkt

(load-relative (collection-file-path "loadtest.rktl" "tests/racket"))

;; Don't need these:
(define no-extra-if-tests? #t)

;; Check export names:
;; based on new docs: 
(require (submod lang/htdp-advanced procedures))
(for ((s (docs)))
  (for ((rows (cdr s)))
    (for ((r rows))
      (define sy (syntax-e (car r)))
      (define vv (dynamic-require 'lang/htdp-advanced sy))
      (when (and (procedure? vv) (not (eq? vv call/cc)))
	(test sy object-name vv)))))

;; based on old docs: 

#;
(require syntax/docprovide)
#;
(let ([docs (lookup-documentation '(lib "htdp-advanced.rkt" "lang") 'procedures)])
  (for-each
   (lambda (row)
     (for-each
      (lambda (doc)
	(let ([v (dynamic-require '(lib "htdp-advanced.rkt" "lang") (car doc))])
	  (when (and (procedure? v)
		     (not (eq? v call/cc)))
	    (test (car doc) object-name v))))
      (cdr row)))
   docs))

(define current-htdp-lang 'lang/htdp-advanced)
(load-relative "htdp-test.rktl")

(require (lib "htdp-advanced.rkt" "lang"))

(load-relative "beg-adv.rktl")
(load-relative "bega-adv.rktl")
(load-relative "intm-adv.rktl")

(define (f6 a) (a))
(test (void) f6 void)

(define (x7) 10)
(test 10 x7)
(define x8 (lambda () 11))
(test 11 x8)

(htdp-syntax-test #'begin "begin: expected an open parenthesis before begin, but found none")
(htdp-syntax-test #'(begin) "begin: expected at least one expression after begin, but nothing's there")
(htdp-syntax-test #'(begin (define x 10)) "define: found a definition that is not at the top level")
(htdp-syntax-test #'(begin (define x 10) x) "define: found a definition that is not at the top level")
(htdp-syntax-test #'(let () (begin (define x 10) x)) "define: found a definition that is not at the top level")
(htdp-syntax-test #'(+ 1 (begin)) "begin: expected at least one expression after begin, but nothing's there")

(test 1 'begin (begin 1))
(test 2 'begin (begin 1 2))
(test 3 'begin (begin 1 2 3))

(htdp-top (define ex 12))
(htdp-test 13 'begin+set! (begin (set! ex 13) ex))
(htdp-test 12 'begin+set! (begin 12 ex))
(htdp-top-pop 1)

(htdp-syntax-test #'begin0 "begin0: expected an open parenthesis before begin0, but found none")
(htdp-syntax-test #'(begin0) "begin0: expected at least one expression after begin0, but nothing's there")

(htdp-test 1 'begin0 (begin0 1))
(htdp-test 2 'begin0 (begin0 2 1))
(htdp-test 3 'begin0 (begin0 3 2 1))


(htdp-syntax-test #'set! "set!: expected an open parenthesis before set!, but found none")
(htdp-syntax-test #'(set!) "set!: expected a variable after set!, but nothing's there")
(htdp-syntax-test #'(set! x) "set!: expected an expression for the new value, but nothing's there")
(htdp-syntax-test #'(set! 1 2) "set!: expected a variable after set!, but found a number")
(htdp-syntax-test #'(set! x 2 3) "set!: expected only one expression for the new value, but found 1 extra part")
(htdp-syntax-test #'(set! set! 2) "set!: expected a variable after set!, but found a set!")
(htdp-syntax-test #'(set! x 1) "x: this variable is not defined")
(htdp-syntax-test #'(lambda (x) (set! x 2)) "set!: expected a mutable variable after set!, but found a variable that cannot be modified: x")
(htdp-syntax-test #'(let ([x 5]) (lambda (x) (set! x 2))) "set!: expected a mutable variable after set!, but found a variable that cannot be modified")

(htdp-top (set! x 5))
(htdp-err/rt-test (define x 10) "set!: cannot set variable before its definition: x")
(htdp-top-pop 1)

(htdp-top (define x 5))
(htdp-top (set! x 'hello))
(htdp-test 'hello 'access-x x)
(htdp-test 18 'set! (local [(define x 12)]
		 (begin
		   (set! x 18)
		   x)))
(htdp-test 19 (lambda (x)
	   (local [(define x 12)]
		  (begin
		    (set! x 19)
		    x)))
      45)

(htdp-syntax-test #'delay "delay: expected an open parenthesis before delay, but found none")
(htdp-syntax-test #'(delay) "delay: expected an expression after delay, but nothing's there")
(htdp-syntax-test #'(delay 1 2) "delay: expected only one expression after delay, but found 1 extra part")

(htdp-top (define d (delay (begin (set! x 89) 12))))
(htdp-test #t promise? d)
(htdp-test 12 force d)
(htdp-top (force d))
(htdp-test 89 'access-x x)
(htdp-top (set! x 13))
(htdp-test 12 force d)
(htdp-test 13 'access-x x)
(htdp-top-pop 4)

(htdp-syntax-test #'(let name) "let: expected at least one binding (in parentheses) after let, but nothing's there")
(htdp-syntax-test #'(let name 10) "let: expected at least one binding (in parentheses) after let, but found a number")
(htdp-syntax-test #'(let name ()) "let: expected an expression after the bindings, but nothing's there")
(htdp-syntax-test #'(let name ([x]) 1) "let: expected an expression after the name x, but nothing's there")
(htdp-syntax-test #'(let name ([x 10] 2) 1) "let: expected a binding with a variable and an expression, but found a number")
(htdp-syntax-test #'(let name ([11 10]) 1) "let: expected a variable for the binding, but found a number")
(htdp-syntax-test #'(let name ([x 10]) 1 2) "let: expected only one expression after the bindings, but found 1 extra part")
(htdp-syntax-test #'(let name ([x 10][x 11]) 1) "let: x was defined locally more than once")
(htdp-test 10 'lookup (let name () 10))
(htdp-test 1024 'loop (let loop ([n 10]) (if (zero? n) 1 (* 2 (loop (sub1 n))))))

(htdp-test 19 'lookup (recur empty-f () 19))

(htdp-syntax-test #'case "case: expected an open parenthesis before case, but found none")
(htdp-syntax-test #'(case) "case: expected an expression after case, but nothing's there")
(htdp-syntax-test #'(case 5) "expected a clause with at least one choice (in parentheses) and an answer after the expression, but nothing's there")
(htdp-syntax-test #'(case 5 12) "case: expected a clause with at least one choice (in parentheses) and an answer, but found a number")
(htdp-syntax-test #'(case 5 []) "case: expected a clause with at least one choice (in parentheses) and an answer, but found an empty part")
(htdp-syntax-test #'(case 5 [5 10]) "case: expected at least one choice (in parentheses), but found a number")
(htdp-syntax-test #'(case 5 [(5) 10] 12) "case: expected a clause with at least one choice (in parentheses) and an answer, but found a number")
(htdp-syntax-test #'(case 5 [(5)]) "case: expected an expression for the answer in the case clause, but nothing's there")
(htdp-syntax-test #'(case 5 [(5) 12 13]) "case: expected only one expression for the answer in the case clause, but found 1 extra part")
(htdp-syntax-test #'(case 5 [("a") 10]) "case: expected a symbol (without its quote) or a number as a choice, but found a string")
(htdp-syntax-test #'(case 5 [() 10]) "expected a symbol (without its quote) or a number as a choice, but nothing's there")
(htdp-syntax-test #'(case 5 [(5 "a") 10]) "case: expected a symbol (without its quote) or a number as a choice, but found a string")
(htdp-syntax-test #'(case 5 [else 12][(5) 10]) "case: found an else clause that isn't the last clause in its case expression")
(htdp-syntax-test #'(case 5 [(5) 10][else 12][else 13]) "case: found an else clause that isn't the last clause in its case expression")

(htdp-test 'a 'case (case 5 [(5) 'a]))
(htdp-test 'b 'case (case 5 [(6) 'a][else 'b]))
(htdp-test 'c 'case (case 5 [(6 5) 'c][else 'b]))
(htdp-test 'd 'case (case 'hello [(6 5 hello) 'd][else 'b]))
(htdp-test 'd 'case (case 'hello [(no) 10][(6 5 hello) 'd][else 'b]))
(htdp-test 'cc 'case (case (+ 2 3) [(6 5) 'cc][else 'b]))

(htdp-syntax-test #'when "when: expected an open parenthesis before when, but found none")
(htdp-syntax-test #'(when) "when: expected a question and an answer, but nothing's there")
(htdp-syntax-test #'(when 10) "when: expected a question and an answer, but found only one part")
(htdp-syntax-test #'(when 10 12 13) "when: expected a question and an answer, but found 3 parts")

(htdp-err/rt-test (when 1 2) rx:not-true-or-false)

(htdp-test (void) 'when (when false 1))
(htdp-test 11 'when (when true 11))

(htdp-syntax-test #'unless "unless: expected an open parenthesis before unless, but found none")
(htdp-syntax-test #'(unless) "unless: expected a question and an answer, but nothing's there")
(htdp-syntax-test #'(unless 10) "unless: expected a question and an answer, but found only one part")
(htdp-syntax-test #'(unless 10 12 13) "unless: expected a question and an answer, but found 3 parts")

(htdp-err/rt-test (unless 1 2) rx:not-true-or-false)

(htdp-test (void) 'unless (unless true 1))
(htdp-test 11 'unless (unless false 11))

(htdp-syntax-test #'shared "shared: expected an open parenthesis before shared, but found none")
(htdp-syntax-test #'(shared) "shared: expected at least one binding (in parentheses) after shared, but nothing's there")
(htdp-syntax-test #'(shared ()) "shared: expected an expression after the bindings, but nothing's there")
(htdp-syntax-test #'(shared 1 2) "shared: expected at least one binding (in parentheses) after shared, but found a number")
(htdp-syntax-test #'(shared () 1 2) "shared: expected only one expression after the bindings, but found 1 extra part")
(htdp-syntax-test #'(shared (x) 2) "shared: expected a binding with a variable and an expression, but found something else")
(htdp-syntax-test #'(shared ([]) 2) "shared: expected a variable for a binding, but nothing's there")
(htdp-syntax-test #'(shared ([x]) 2) "shared: expected an expression after the binding name, but nothing's there")
(htdp-syntax-test #'(shared ([x 1 3]) 2) "shared: expected only one expression after the binding name, but found 1 extra part")
(htdp-syntax-test #'(shared ([1 3]) 2) "shared: expected a variable for the binding, but found a number")
(htdp-syntax-test #'(shared ([x 1][x 2]) 2) "shared: found a variable that is used more than once: x")

(htdp-test 1 'shared (shared () 1))
(htdp-test 1 'shared (shared ([x 1]) x))
(htdp-test '(1) 'shared (shared ([x (cons 1 null)]) x))
(htdp-test 1 car (shared ([x (cons 1 x)]) x))
(htdp-test 1 cadr (shared ([x (cons 1 x)][y (cons 2 x)]) y))
(htdp-test 1 cadddr (shared ([x (cons 1 x)][y (cons 2 x)]) y))
(htdp-test 1 first (shared ([x (cons 1 x)]) x))
(htdp-test 1 second (shared ([x (cons 1 x)]) x))
(htdp-test 1 third (shared ([x (cons 1 x)]) x))
(htdp-test 1 fourth (shared ([x (cons 1 x)]) x))
(htdp-test 1 fifth (shared ([x (cons 1 x)]) x))
(htdp-test 1 sixth (shared ([x (cons 1 x)]) x))
(htdp-test 1 seventh (shared ([x (cons 1 x)]) x))
(htdp-test 1 eighth (shared ([x (cons 1 x)]) x))
(htdp-test #t (lambda (l) (eq? l (cdr l))) (shared ([x (cons 1 x)]) x))
(htdp-test #t (lambda (l) (eq? l (car l))) (shared ([x (list x x)]) x))
(htdp-test #t (lambda (l) (eq? l (cadr l))) (shared ([x (list x x)]) x))
(htdp-err/rt-test (shared ([x (cons 1 y)][y 5]) x))

(htdp-syntax-test #'recur "recur: expected an open parenthesis before recur, but found none")
(htdp-syntax-test #'(recur) "recur: expected a function name after recur, but nothing's there")
(htdp-syntax-test #'(recur 10) "recur: expected a function name after recur, but found a number")
(htdp-syntax-test #'(recur name) "recur: expected at least one binding (in parentheses) after recur, but nothing's there")
(htdp-syntax-test #'(recur name 10) "recur: expected at least one binding (in parentheses) after recur, but found a number")
(htdp-syntax-test #'(recur name ([x 1])) "recur: expected an expression after the bindings, but nothing's there")
(htdp-syntax-test #'(recur name ([x]) 1) "recur: expected an expression after the name x, but nothing's there")
(htdp-syntax-test #'(recur name ([x 10] 2) 1) "recur: expected a binding with a variable and an expression, but found a number")
(htdp-syntax-test #'(recur name ([11 10]) 1) "recur: expected a variable for the binding, but found a number")
(htdp-syntax-test #'(recur name ([x 10]) 1 2) "recur: expected only one expression after the bindings, but found 1 extra part")
(htdp-syntax-test #'(recur name ([x 10][x 11]) 1) "recur: x was defined locally more than once")
(htdp-test 18 'lookup (recur name ([x 18]) x))
(htdp-test 1024 'loop (recur loop ([n 10]) (if (zero? n) 1 (* 2 (loop (sub1 n))))))
(htdp-test 13 'loop (recur f ([f 13]) f))
(htdp-test 14 'loop (let ([f 14]) (recur f ([f f]) f)))

(load (collection-file-path "shared-tests.rktl" "tests" "racket"))

(htdp-err/rt-test (cons 1 2) "cons: second argument must be a list, but received 1 and 2")
(htdp-err/rt-test (append (list 1) 2) "append: last argument must be a list, but received 2")

(htdp-err/rt-test (first 1) "first: expects a non-empty list; given: 1")
(htdp-err/rt-test (rest 1) "rest: expects a non-empty list; given: 1")


(htdp-test #t 'equal? (equal? (vector (list 10) 'apple) (vector (list 10) 'apple)))
(htdp-test #t 'equal? (equal?  (shared ([x (cons 10 x)]) x) (shared ([x (cons 10 x)]) x)))
(htdp-test #t 'equal? (equal?  (shared ([x (cons (vector x) x)]) x) (shared ([x (cons (vector x) x)]) x)))
(htdp-test #f 'equal? (equal?  (shared ([x (cons 10 x)]) x) (shared ([x (cons 10 (cons 11 x))]) x)))
(htdp-test #f 'equal? (equal?  (shared ([x (cons (vector x) x)]) x) (shared ([x (cons (box x) x)]) x)))

(htdp-test #t 'equal~? (equal~? (vector (list 10) 'apple) (vector (list 10) 'apple) 0.1))
(htdp-test #t 'equal~? (equal~? (vector (list 10) 'apple) (vector (list 10.02) 'apple) 0.1))
(htdp-test #f 'equal~? (equal~? (vector (list 10) 'apple) (vector (list 10.2) 'apple) 0.1))
(htdp-test #t 'equal? (equal? (box (list 10)) (box (list 10))))
(htdp-test #t 'equal~? (equal~? (box (list 10)) (box (list 10)) 0.1))
(htdp-test #t 'equal~? (equal~? (box (list 10)) (box (list 10.02)) 0.1))

(htdp-test #t 'equal~? (equal~?  (shared ([x (cons 10 x)]) x) (shared ([x (cons 10.02 x)]) x) 0.1))
(htdp-test #f 'equal~? (equal~?  (shared ([x (cons 10 x)]) x) (shared ([x (cons 10.2 x)]) x) 0.1))

(htdp-test 1 'hash-copy
           (local [(define ht (make-hash (list (list 'a 1))))
                   (define htp (hash-copy ht))]
             (begin (hash-set! htp 'a 2)
                    (hash-ref ht 'a))))
(htdp-test 1 'hash-count (hash-count (make-hash (list (list 'a 1)))))
(htdp-test 42 'hash-for-each 
           (local [(define x 0)
                   (define (f k v) (set! x 42))]
             (begin (hash-for-each (make-hash (list (list 1 2))) f)
                    x)))
(htdp-test #t 'hash-has-key? (hash-has-key? (make-hash (list (list 1 2))) 1))
(htdp-test #f 'hash-has-key? (hash-has-key? (make-hash (list (list 1 2))) 2))
(htdp-test (list #f #f) 'hash-map
           (hash-map (make-hash (list (list 1 #t) (list 2 #t)))
                     (lambda (k v) (not v))))
(htdp-test 1 'hash-ref (hash-ref (make-hash (list (list 'a 1))) 'a))
(htdp-test 2 'hash-ref (hash-ref (make-hash (list (list 'a 1))) 'b 2))
(htdp-test 2 'hash-ref (hash-ref (make-hash (list (list 'a 1))) 'b (lambda () 2)))
(htdp-test 1 'hash-ref!
           (local [(define ht (make-hash (list (list 'a 1))))]
             (hash-ref! ht 'a 2)))
(htdp-test 2 'hash-ref!
           (local [(define ht (make-hash (list (list 'a 1))))]
             (hash-ref! ht 'b 2)))
(htdp-test 2 'hash-ref!
           (local [(define ht (make-hash (list (list 'a 1))))]
             (begin (hash-ref! ht 'b 2)
                    (hash-ref ht 'b))))
(htdp-test (list #t #f) 'hash-remove! 
           (local [(define ht (make-hash (list (list 'a 1))))]
             (list (hash-has-key? ht 'a)
                   (begin (hash-remove! ht 'a)
                          (hash-has-key? ht 'a)))))
(htdp-err/rt-test
           (local [(define ht (make-hash (list (list 'a 1))))]
             (list (hash-has-key? ht 'a)
                   (begin (hash-remove ht 'a)
                          (hash-has-key? ht 'a)))))
(htdp-test 2 'hash-set!
           (local [(define ht (make-hash (list (list 'a 1))))]
             (begin (hash-set! ht 'a 2)
                    (hash-ref ht 'a))))
(htdp-err/rt-test
           (local [(define ht (make-hash (list (list 'a 1))))]
             (begin (hash-set ht 'a 2)
                    (hash-ref ht 'a))))
(htdp-err/rt-test
           (local [(define ht (make-hash (list (list 'a 1))))]
             (begin (hash-update ht 'a add1)
                    (hash-ref ht 'a))))
(htdp-test 2 'hash-update!
           (local [(define ht (make-hash (list (list 'a 1))))]
             (begin (hash-update! ht 'a add1)
                    (hash-ref ht 'a))))
(htdp-test 2 'hash-update!
           (local [(define ht (make-hash (list (list 'a 1))))]
             (begin (hash-update! ht 'b add1 1)
                    (hash-ref ht 'b))))
(htdp-test 2 'hash-update!
           (local [(define ht (make-hash (list (list 'a 1))))]
             (begin (hash-update! ht 'b add1 (lambda () 1))
                    (hash-ref ht 'b))))
(htdp-test #t 'hash?
           (hash? (make-hash)))
(htdp-test #t 'hash?
           (hash? (make-hasheq)))
(htdp-test #t 'hash?
           (hash? (make-hasheqv)))
(htdp-test #t 'hash?
           (hash? (make-hash (list (list 'a 1)))))
(htdp-test #t 'hash?
           (hash? (make-hasheq (list (list 'a 1)))))
(htdp-test #t 'hash?
           (hash? (make-hasheqv (list (list 'a 1)))))
(htdp-test #f 'hash?
           (hash? 1))
(htdp-test #t 'hash-equal?
           (hash-equal? (make-hash (list (list 'a 1)))))
(htdp-test #f 'hash-equal?
           (hash-equal? (make-hasheq (list (list 'a 1)))))
(htdp-test #f 'hash-equal?
           (hash-equal? (make-hasheqv (list (list 'a 1)))))
(htdp-test #f 'hash-eq?
           (hash-eq? (make-hash (list (list 'a 1)))))
(htdp-test #t 'hash-eq?
           (hash-eq? (make-hasheq (list (list 'a 1)))))
(htdp-test #f 'hash-eq?
           (hash-eq? (make-hasheqv (list (list 'a 1)))))
(htdp-test #f 'hash-eqv?
           (hash-eqv? (make-hash (list (list 'a 1)))))
(htdp-test #f 'hash-eqv?
           (hash-eqv? (make-hasheq (list (list 'a 1)))))
(htdp-test #t 'hash-eqv?
           (hash-eqv? (make-hasheqv (list (list 'a 1)))))

;; immutable tests
(htdp-test 1 'hash-copy
           (local [(define ht (make-immutable-hash (list (list 'a 1))))
                   (define htp (hash-copy ht))]
             (hash-ref htp 'a)))
(htdp-test 1 'hash-count (hash-count (make-immutable-hash (list (list 'a 1)))))
(htdp-test 42 'hash-for-each 
           (local [(define x 0)
                   (define (f k v) (set! x 42))]
             (begin (hash-for-each (make-immutable-hash (list (list 1 2))) f)
                    x)))
(htdp-test #t 'hash-has-key? (hash-has-key? (make-immutable-hash (list (list 1 2))) 1))
(htdp-test #f 'hash-has-key? (hash-has-key? (make-immutable-hash (list (list 1 2))) 2))
(htdp-test (list #f #f) 'hash-map
           (hash-map (make-immutable-hash (list (list 1 #t) (list 2 #t)))
                     (lambda (k v) (not v))))
(htdp-test 1 'hash-ref (hash-ref (make-immutable-hash (list (list 'a 1))) 'a))
(htdp-test 2 'hash-ref (hash-ref (make-immutable-hash (list (list 'a 1))) 'b 2))
(htdp-test 2 'hash-ref (hash-ref (make-immutable-hash (list (list 'a 1))) 'b (lambda () 2)))
(htdp-err/rt-test
             (local [(define ht (make-immutable-hash (list (list 'a 1))))]
               (hash-ref! ht 'a 2)))
(htdp-err/rt-test 
           (local [(define ht (make-immutable-hash (list (list 'a 1))))]
             (list (hash-has-key? ht 'a)
                   (begin (hash-remove! ht 'a)
                          (hash-has-key? ht 'a)))))
(htdp-test (list #t #f) 'hash-remove
           (local [(define ht (make-immutable-hash (list (list 'a 1))))]
             (list (hash-has-key? ht 'a)
                   (hash-has-key? (hash-remove ht 'a) 'a))))
(htdp-err/rt-test
           (local [(define ht (make-immutable-hash (list (list 'a 1))))]
             (begin (hash-set! ht 'a 2)
                    (hash-ref ht 'a))))
(htdp-test 2 'hash-set
           (local [(define ht (make-immutable-hash (list (list 'a 1))))]
             (hash-ref (hash-set ht 'a 2) 'a)))
(htdp-err/rt-test
           (local [(define ht (make-immutable-hash (list (list 'a 1))))]
             (begin (hash-update! ht 'a add1)
                    (hash-ref ht 'a))))
(htdp-test 2 'hash-update
           (local [(define ht (make-immutable-hash (list (list 'a 1))))]
             (hash-ref (hash-update ht 'a add1) 'a)))
(htdp-test 2 'hash-update
           (local [(define ht (make-immutable-hash (list (list 'a 1))))]
             (hash-ref (hash-update ht 'b add1 1) 'b)))
(htdp-test 2 'hash-update
           (local [(define ht (make-immutable-hash (list (list 'a 1))))]
             (hash-ref (hash-update ht 'b add1 (lambda () 1)) 'b)))
(htdp-test #t 'hash?
           (hash? (make-immutable-hash)))
(htdp-test #t 'hash?
           (hash? (make-immutable-hasheq)))
(htdp-test #t 'hash?
           (hash? (make-immutable-hasheqv)))
(htdp-test #t 'hash?
           (hash? (make-immutable-hash (list (list 'a 1)))))
(htdp-test #t 'hash?
           (hash? (make-immutable-hasheq (list (list 'a 1)))))
(htdp-test #t 'hash?
           (hash? (make-immutable-hasheqv (list (list 'a 1)))))
(htdp-test #f 'hash?
           (hash? 1))
(htdp-test #t 'hash-equal?
           (hash-equal? (make-immutable-hash (list (list 'a 1)))))
(htdp-test #f 'hash-equal?
           (hash-equal? (make-immutable-hasheq (list (list 'a 1)))))
(htdp-test #f 'hash-equal?
           (hash-equal? (make-immutable-hasheqv (list (list 'a 1)))))
(htdp-test #f 'hash-eq?
           (hash-eq? (make-immutable-hash (list (list 'a 1)))))
(htdp-test #t 'hash-eq?
           (hash-eq? (make-immutable-hasheq (list (list 'a 1)))))
(htdp-test #f 'hash-eq?
           (hash-eq? (make-immutable-hasheqv (list (list 'a 1)))))
(htdp-test #f 'hash-eqv?
           (hash-eqv? (make-immutable-hash (list (list 'a 1)))))
(htdp-test #f 'hash-eqv?
           (hash-eqv? (make-immutable-hasheq (list (list 'a 1)))))
(htdp-test #t 'hash-eqv?
           (hash-eqv? (make-immutable-hasheqv (list (list 'a 1)))))

;; Check set...! error message:
(htdp-top (define-struct a1 (b)))
(htdp-err/rt-test (set-a1-b! 1 2) #rx"set-a1-b!")
(htdp-top-pop 1)

;; Simulate set! in the repl
(module my-advanced-module (lib "htdp-advanced.rkt" "lang")
  (define x 10)
  (define (f y) f)
  (define-struct s (x y)))
(mz-require 'my-advanced-module)
(parameterize ([current-namespace (module->namespace ''my-advanced-module)])
  (eval #'(set! x 12))
  (eval #'(set! f 12))
  (eval #'(set! make-s 12))
  (eval #'(set! s-x 12))
  (eval #'(set! s? 12))
  (eval #'(set! set-s-x! 12)))

;; define-datatype

(htdp-syntax-test #'define-datatype #rx"define-datatype: expected an open parenthesis before define-datatype, but found none")
(htdp-syntax-test #'(define-datatype) #rx"define-datatype: expected a datatype type name after `define-datatype', but nothing's there")
(htdp-syntax-test #'(define-datatype dt 10) #rx"define-datatype: expected a variant after the datatype type name in `define-datatype', but found a number")
(htdp-syntax-test #'(define-datatype dt [v1] 10) #rx"define-datatype: expected a variant after the datatype type name in `define-datatype', but found a number")
(htdp-syntax-test #'(define-datatype dt v1) #rx"define-datatype: expected a variant after the datatype type name in `define-datatype', but found something else")
(htdp-syntax-test #'(define-datatype dt [v1 f1 f1]) #rx"define-datatype: in variant `v1': found a field name that is used more than once: f1")
(htdp-syntax-test #'(define-datatype dt [10]) #rx"define-datatype: expected a variant name, found a number")
(htdp-syntax-test #'(define-datatype dt [(v1)]) #rx"define-datatype: expected a variant name, found a part")
(htdp-syntax-test #'(define-datatype dt [v1 10]) #rx"define-datatype: in variant `v1': expected a field name, found a number")
(htdp-syntax-test #'(define-datatype dt [v1] [v1]) #rx"define-datatype: found a variant name that is used more than once: v1")
(htdp-syntax-test #'(define-datatype posn [v1]) "posn?: this name was defined previously and cannot be re-defined")
(htdp-syntax-test #'(define-datatype dt [posn]) "posn: this name was defined previously and cannot be re-defined")
(htdp-syntax-test #'(define-datatype lambda [v1]) #rx"define-datatype: expected a datatype type name after `define-datatype', but found a keyword")
(htdp-syntax-test #'(define-datatype dt [lambda]) #rx"define-datatype: expected a variant name, found a keyword")
(htdp-syntax-test #'(define-datatype (dt)) #rx"define-datatype: expected a datatype type name after `define-datatype', but found a part")
(htdp-syntax-test #'(+ 1 (define-datatype dt [v1])) #rx"define-datatype: found a definition that is not at the top level")

(htdp-top (define-datatype dt))
(htdp-test #f 'dt? (dt? 1))
(htdp-top-pop 1)

(htdp-top (define x 5))
(htdp-syntax-test #'(define-datatype x [v1]) #rx"x: this name was defined previously and cannot be re-defined")
(htdp-syntax-test #'(define-datatype dt [x]) #rx"x: this name was defined previously and cannot be re-defined")
(htdp-top-pop 1)

(htdp-top (define-datatype a
            [a0]
            [a1 b]
            [a3 b c d]))
(htdp-test #t 'a0? (a0? (make-a0)))
(htdp-test #t 'a? (a? (make-a0)))
(htdp-test #t 'a1? (a1? (make-a1 1)))
(htdp-test #t 'a? (a? (make-a1 1)))
(htdp-test #t 'a3? (a3? (make-a3 1 2 3)))
(htdp-test #t 'a? (a? (make-a3 1 2 3)))
(htdp-test #f 'a1? (a1? (make-a3 1 2 3)))
(htdp-test #f 'a3? (a3? (make-a1 1)))
(htdp-test #f 'a? (a? 1))
(htdp-top-pop 1)

;; match

(htdp-syntax-test #'match #rx"match: expected an open parenthesis before match, but found none")
(htdp-syntax-test #'(match) #rx"match: expected an expression after `match', but nothing's there")
(htdp-syntax-test #'(match 1) #rx"match: expected a pattern--answer clause after the expression following `match', but nothing's there")

(htdp-syntax-test #'(match 1 10) #rx"match: expected a pattern--answer clause, but found a number")
(htdp-syntax-test #'(match 1 x) #rx"match: expected a pattern--answer clause, but found something else")
(htdp-syntax-test #'(match 1 []) #rx"match: expected a pattern--answer clause, but found an empty clause")
(htdp-syntax-test #'(match 1 [x]) #rx"expected an expression for the answer in a `match' clause, but nothing's there")
(htdp-syntax-test #'(match 1 [x 10 10]) #rx"expected only one expression for the answer in a `match' clause, but found 1 extra part")
(htdp-syntax-test #'(match 1 [x 10 x]) #rx"expected only one expression for the answer in a `match' clause, but found 1 extra part")

(htdp-syntax-test #'(match 1 [x 10] 10) #rx"match: expected a pattern--answer clause, but found a number")
(htdp-syntax-test #'(match 1 [x 10] x) #rx"match: expected a pattern--answer clause, but found something else")
(htdp-syntax-test #'(match 1 [x 10] []) #rx"match: expected a pattern--answer clause, but found an empty clause")
(htdp-syntax-test #'(match 1 [x 10] [x]) #rx"expected an expression for the answer in a `match' clause, but nothing's there")
(htdp-syntax-test #'(match 1 [x 10] [x 10 10]) #rx"expected only one expression for the answer in a `match' clause, but found 1 extra part")
(htdp-syntax-test #'(match 1 [x 10] [x 10 x]) #rx"expected only one expression for the answer in a `match' clause, but found 1 extra part")

(define-syntax-rule (htdp-match/v res pat expr val)
  (htdp-test res 'pat (match expr [pat val] [else #f])))
(define-syntax-rule (htdp-match res pat expr)
  (htdp-match/v res pat expr #t))

(htdp-match #t true true)
(htdp-match #f true false)
(htdp-match #f true 1)

(htdp-match #f false true)
(htdp-match #t false false)
(htdp-match #f false 1)

(htdp-match #t empty empty)
(htdp-match #f empty 1)

(htdp-match #t 1 1)
(htdp-match #t '1 1)
(htdp-match #t `1 1)
(htdp-match #f 1 2)

(htdp-match #t "foo" "foo")
(htdp-match #t '"foo" "foo")
(htdp-match #t `"foo" "foo")
(htdp-match #f "foo" "bar")

(htdp-match #t #\a #\a)
(htdp-match #t '#\a #\a)
(htdp-match #t `#\a #\a)
(htdp-match #f #\a #\b)

(htdp-match #t 'a 'a)
(htdp-match #f 'a 'b)

(htdp-match #t '(a b) (list 'a 'b))
(htdp-match #t ''a ''a)
(htdp-match #t '`a '`a)
(htdp-match #t ',a ',a)
(htdp-match #t ',@a ',@a)

(htdp-match #t `(a b) (list 'a 'b))
(htdp-match #t `'a ''a)
(htdp-match #t ``a '`a)

(htdp-match #t (cons a b) (list 1))
(htdp-match #f (cons 1 2) 1)
(htdp-match #t (list a b) (list 1 2))
(htdp-match #f (list a b) (list 1))
(htdp-match #t (list* a b) (list 1))
(htdp-match #f (list* a b) empty)

(htdp-match #t (vector x y) (vector 1 2))
(htdp-match #f (vector x x) (vector 1 2))
(htdp-match #t (vector _ _) (vector 1 2))
(htdp-match #f (vector x y) (vector 1))

(htdp-match #t (box x) (box 1))
(htdp-match #f (box x) 1)

(htdp-match/v 1 a 1 a)

(htdp-top (define-struct my-posn (x y)))
(htdp-match/v 3 (struct my-posn (x y)) (make-my-posn 1 2) (+ x y))
(htdp-top-pop 1)

(htdp-match/v 3 (struct posn (x y)) (make-posn 1 2) (+ x y))
(htdp-match/v 3 (cons (struct posn (x y)) empty) (cons (make-posn 1 2) empty) (+ x y))
(htdp-match/v 3 (list* (struct posn (x y)) empty) (list* (make-posn 1 2) empty) (+ x y))
(htdp-match/v 3 (list (struct posn (x y))) (list (make-posn 1 2)) (+ x y))
(htdp-match/v 3 (vector (struct posn (x y))) (vector (make-posn 1 2)) (+ x y))
(htdp-match/v 3 (box (struct posn (x y))) (box (make-posn 1 2)) (+ x y))

(htdp-match/v 3 `,(struct posn (x y)) (make-posn 1 2) (+ x y))
(htdp-match/v 1 `(a ,b) (list 'a 1) b)
(htdp-match/v 1 `(a ,@(list b)) (list 'a 1) b)

;; ----------------------------------------

(report-errs)
