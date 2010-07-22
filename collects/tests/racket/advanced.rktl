
;; Basic checks for the advanced language. See also
;;  beginner.ss

(load-relative "loadtest.rktl")

;; Don't need these:
(define no-extra-if-tests? #t)

;; Check export names:
(require syntax/docprovide)
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

(htdp-syntax-test #'begin)
(htdp-syntax-test #'(begin))
(htdp-syntax-test #'(begin (define x 10)))
(htdp-syntax-test #'(begin (define x 10) x))
(htdp-syntax-test #'(let () (begin (define x 10) x)))
(htdp-syntax-test #'(+ 1 (begin)))

(test 1 'begin (begin 1))
(test 2 'begin (begin 1 2))
(test 3 'begin (begin 1 2 3))

(htdp-top (define ex 12))
(htdp-test 13 'begin+set! (begin (set! ex 13) ex))
(htdp-test 12 'begin+set! (begin 12 ex))
(htdp-top-pop 1)

(htdp-syntax-test #'begin0)
(htdp-syntax-test #'(begin0))

(htdp-test 1 'begin0 (begin0 1))
(htdp-test 2 'begin0 (begin0 2 1))
(htdp-test 3 'begin0 (begin0 3 2 1))

(htdp-syntax-test #'set!)
(htdp-syntax-test #'(set!))
(htdp-syntax-test #'(set! x))
(htdp-syntax-test #'(set! 1 2))
(htdp-syntax-test #'(set! x 2 3))
(htdp-syntax-test #'(set! set! 2))
(htdp-syntax-test #'(lambda (x) (set! x 2)))
(htdp-syntax-test #'(let ([x 5]) (lambda (x) (set! x 2))))

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

(htdp-syntax-test #'delay)
(htdp-syntax-test #'(delay))
(htdp-syntax-test #'(delay 1 2))

(htdp-top (define d (delay (begin (set! x 89) 12))))
(htdp-test #t promise? d)
(htdp-test 12 force d)
(htdp-top (force d))
(htdp-test 89 'access-x x)
(htdp-top (set! x 13))
(htdp-test 12 force d)
(htdp-test 13 'access-x x)

(htdp-syntax-test #'(let name))
(htdp-syntax-test #'(let name 10))
(htdp-syntax-test #'(let name ()))
(htdp-syntax-test #'(let name ([x]) 1))
(htdp-syntax-test #'(let name ([x 10] 2) 1))
(htdp-syntax-test #'(let name ([11 10]) 1))
(htdp-syntax-test #'(let name ([x 10]) 1 2))
(htdp-syntax-test #'(let name ([x 10][x 11]) 1))
(htdp-test 10 'lookup (let name () 10))
(htdp-test 1024 'loop (let loop ([n 10]) (if (zero? n) 1 (* 2 (loop (sub1 n))))))

(htdp-test 19 'lookup (recur empty-f () 19))

(htdp-syntax-test #'case)
(htdp-syntax-test #'(case))
(htdp-syntax-test #'(case 5))
(htdp-syntax-test #'(case 5 12))
(htdp-syntax-test #'(case 5 []))
(htdp-syntax-test #'(case 5 [5 10]))
(htdp-syntax-test #'(case 5 [(5) 10] 12))
(htdp-syntax-test #'(case 5 [(5)]))
(htdp-syntax-test #'(case 5 [(5) 12 13]))
(htdp-syntax-test #'(case 5 [("a") 10]))
(htdp-syntax-test #'(case 5 [() 10]))
(htdp-syntax-test #'(case 5 [(5 "a") 10]))
(htdp-syntax-test #'(case 5 [else 12][(5) 10]))
(htdp-syntax-test #'(case 5 [(5) 10][else 12][else 13]))

(htdp-test 'a 'case (case 5 [(5) 'a]))
(htdp-test 'b 'case (case 5 [(6) 'a][else 'b]))
(htdp-test 'c 'case (case 5 [(6 5) 'c][else 'b]))
(htdp-test 'd 'case (case 'hello [(6 5 hello) 'd][else 'b]))
(htdp-test 'd 'case (case 'hello [(no) 10][(6 5 hello) 'd][else 'b]))
(htdp-test 'cc 'case (case (+ 2 3) [(6 5) 'cc][else 'b]))

(htdp-syntax-test #'when)
(htdp-syntax-test #'(when))
(htdp-syntax-test #'(when 10))
(htdp-syntax-test #'(when 10 12 13))

(htdp-err/rt-test (when 1 2))

(htdp-test (void) 'when (when false 1))
(htdp-test 11 'when (when true 11))

(htdp-syntax-test #'unless)
(htdp-syntax-test #'(unless))
(htdp-syntax-test #'(unless 10))
(htdp-syntax-test #'(unless 10 12 13))

(htdp-err/rt-test (unless 1 2))

(htdp-test (void) 'unless (unless true 1))
(htdp-test 11 'unless (unless false 11))

(htdp-syntax-test #'shared)
(htdp-syntax-test #'(shared))
(htdp-syntax-test #'(shared ()))
(htdp-syntax-test #'(shared 1 2))
(htdp-syntax-test #'(shared () 1 2))
(htdp-syntax-test #'(shared (x) 2))
(htdp-syntax-test #'(shared ([]) 2))
(htdp-syntax-test #'(shared ([x]) 2))
(htdp-syntax-test #'(shared ([x 1 3]) 2))
(htdp-syntax-test #'(shared ([1 3]) 2))
(htdp-syntax-test #'(shared ([x 1][x 2]) 2))

(htdp-test 1 'shared (shared () 1))
(htdp-test 1 'shared (shared ([x 1]) x))
(htdp-test '(1) 'shared (shared ([x (cons 1 null)]) x))
(htdp-test 1 car (shared ([x (cons 1 x)]) x))
(htdp-test 1 cadr (shared ([x (cons 1 x)][y (cons 2 x)]) y))
(htdp-test 1 cadddr (shared ([x (cons 1 x)][y (cons 2 x)]) y))
(htdp-test #t (lambda (l) (eq? l (cdr l))) (shared ([x (cons 1 x)]) x))
(htdp-test #t (lambda (l) (eq? l (car l))) (shared ([x (list x x)]) x))
(htdp-test #t (lambda (l) (eq? l (cadr l))) (shared ([x (list x x)]) x))
(htdp-err/rt-test (shared ([x (cons 1 y)][y 5]) x))

(htdp-syntax-test #'recur)
(htdp-syntax-test #'(recur))
(htdp-syntax-test #'(recur 10))
(htdp-syntax-test #'(recur name))
(htdp-syntax-test #'(recur name 10))
(htdp-syntax-test #'(recur name ([x 1])))
(htdp-syntax-test #'(recur name ([x]) 1))
(htdp-syntax-test #'(recur name ([x 10] 2) 1))
(htdp-syntax-test #'(recur name ([11 10]) 1))
(htdp-syntax-test #'(recur name ([x 10]) 1 2))
(htdp-syntax-test #'(recur name ([x 10][x 11]) 1))
(htdp-test 18 'lookup (recur name ([x 18]) x))
(htdp-test 1024 'loop (recur loop ([n 10]) (if (zero? n) 1 (* 2 (loop (sub1 n))))))
(htdp-test 13 'loop (recur f ([f 13]) f))
(htdp-test 14 'loop (let ([f 14]) (recur f ([f f]) f)))

(load (build-path (collection-path "tests" "racket") "shared-tests.rktl"))

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
(htdp-test 2 'hash-set!
           (local [(define ht (make-hash (list (list 'a 1))))]
             (begin (hash-set! ht 'a 2)
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

(htdp-syntax-test #'define-datatype #rx"define-datatype: found a use of `define-datatype' that does not follow an open parenthesis")
(htdp-syntax-test #'(define-datatype) #rx"define-datatype: expected a datatype type name after `define-datatype', but nothing's there")
(htdp-syntax-test #'(define-datatype dt 10) #rx"define-datatype: expected a variant after the datatype type name in `define-datatype', but found a number")
(htdp-syntax-test #'(define-datatype dt [v1] 10) #rx"define-datatype: expected a variant after the datatype type name in `define-datatype', but found a number")
(htdp-syntax-test #'(define-datatype dt v1) #rx"define-datatype: expected a variant after the datatype type name in `define-datatype', but found something else")
(htdp-syntax-test #'(define-datatype dt [v1 f1 f1]) #rx"define-datatype: in variant `v1': found a field name that was used more than once: f1")
(htdp-syntax-test #'(define-datatype dt [10]) #rx"define-datatype: expected a variant name, found a number")
(htdp-syntax-test #'(define-datatype dt [(v1)]) #rx"define-datatype: expected a variant name, found something else")
(htdp-syntax-test #'(define-datatype dt [v1 10]) #rx"define-datatype: in variant `v1': expected a field name, found a number")
(htdp-syntax-test #'(define-datatype dt [v1] [v1]) #rx"define-datatype: found a variant name that was used more than once: v1")
(htdp-syntax-test #'(define-datatype posn [v1]) #rx"posn\\?: this name has a built-in meaning and cannot be re-defined")
(htdp-syntax-test #'(define-datatype dt [posn]) #rx"posn: this name has a built-in meaning and cannot be re-defined")
(htdp-syntax-test #'(define-datatype lambda [v1]) #rx"define-datatype: expected a datatype type name after `define-datatype', but found a keyword")
(htdp-syntax-test #'(define-datatype dt [lambda]) #rx"define-datatype: expected a variant name, found a keyword")
(htdp-syntax-test #'(define-datatype (dt)) #rx"define-datatype: expected a datatype type name after `define-datatype', but found something else")
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

(htdp-syntax-test #'match #rx"match: found a use of `match' that does not follow an open parenthesis")
(htdp-syntax-test #'(match) #rx"match: expected an expression after `match', but nothing's there")
(htdp-syntax-test #'(match 1) #rx"match: expected a pattern--answer clause after the expression following `match', but nothing's there")

(htdp-syntax-test #'(match 1 10) #rx"match: expected a pattern--answer clause, but found a number")
(htdp-syntax-test #'(match 1 x) #rx"match: expected a pattern--answer clause, but found something else")
(htdp-syntax-test #'(match 1 []) #rx"match: expected a pattern--answer clause, but found an empty clause")
(htdp-syntax-test #'(match 1 [x]) #rx"expected an expression for the answer in a `match' clause, but nothing's there")
(htdp-syntax-test #'(match 1 [x 10 10]) #rx"expected only one expression for the answer in a `match' clause, but found one extra part")
(htdp-syntax-test #'(match 1 [x 10 x]) #rx"expected only one expression for the answer in a `match' clause, but found one extra part")

(htdp-syntax-test #'(match 1 [x 10] 10) #rx"match: expected a pattern--answer clause, but found a number")
(htdp-syntax-test #'(match 1 [x 10] x) #rx"match: expected a pattern--answer clause, but found something else")
(htdp-syntax-test #'(match 1 [x 10] []) #rx"match: expected a pattern--answer clause, but found an empty clause")
(htdp-syntax-test #'(match 1 [x 10] [x]) #rx"expected an expression for the answer in a `match' clause, but nothing's there")
(htdp-syntax-test #'(match 1 [x 10] [x 10 10]) #rx"expected only one expression for the answer in a `match' clause, but found one extra part")
(htdp-syntax-test #'(match 1 [x 10] [x 10 x]) #rx"expected only one expression for the answer in a `match' clause, but found one extra part")

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
