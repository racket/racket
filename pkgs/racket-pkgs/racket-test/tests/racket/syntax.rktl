
(load-relative "loadtest.rktl")

(Section 'syntax)

;; ----------------------------------------

(test 0 'with-handlers (with-handlers () 0))
(test 1 'with-handlers (with-handlers ([void void]) 1))
(test 2 'with-handlers (with-handlers ([void void]) 1 2))
(test 'zero 'zero
      (with-handlers ((zero? (lambda (x) 'zero)))
		     (raise 0)))
(test 'zero 'zero
      (with-handlers ((zero? (lambda (x) 'zero))
		      (positive? (lambda (x) 'positive)))
		     (raise 0)))
(test 'positive 'positive
      (with-handlers ((zero? (lambda (x) 'zero))
		      (positive? (lambda (x) 'positive)))
		     (raise 1)))
(test 5 'with-handlers
      (with-handlers ([void (lambda (x) 5)])
	(with-handlers ((zero? (lambda (x) 'zero)))
	  (/ 0))))

(error-test #'(with-handlers ()
	         (/ 0))
	    exn:fail:contract:divide-by-zero?)
(error-test #'(with-handlers ((zero? (lambda (x) 'zero)))
		 (/ 0))
	    exn:application:type?)
(error-test #'(with-handlers ((zero? (lambda (x) 'zero))
			     (boolean? (lambda (x) 'boolean)))
		 (/ 0))
	    exn:application:type?)

(syntax-test #'with-handlers)
(syntax-test #'(with-handlers))
(syntax-test #'(with-handlers . 1))
(syntax-test #'(with-handlers ((zero? (lambda (x) 'zero)))))
(syntax-test #'(with-handlers ((zero? (lambda (x) 'zero))) . 1))
(syntax-test #'(with-handlers (zero?) 1))
(syntax-test #'(with-handlers ((zero?)) 1))
(syntax-test #'(with-handlers ((zero? . zero?)) 1))
(syntax-test #'(with-handlers ((zero? zero?) . 2) 1))
(syntax-test #'(with-handlers ((zero? zero?) zero?) 1))
(syntax-test #'(with-handlers ((zero? zero?) (zero?)) 1))
(syntax-test #'(with-handlers ((zero? zero?) (zero?)) 1))
(syntax-test #'(with-handlers ((zero? zero? zero?)) 1))
(syntax-test #'(with-handlers ((zero? zero? . zero?)) 1))
(syntax-test #'(with-handlers ((zero? zero?)) 1 . 2))

(error-test #'(with-handlers ((0 void)) (/ 0)) 
	    exn:application:type?)
(error-test #'(with-handlers ((void 0)) (/ 0))
	    exn:application:type?)
(error-test #'(with-handlers ((unbound-variable void)) 0)
	    exn:fail:contract:variable?)
(error-test #'(with-handlers ((void unbound-variable)) 0)
	    exn:fail:contract:variable?)
(error-test #'(with-handlers (((values 1 2) void)) 0)
	    arity?)
(error-test #'(with-handlers ((void (values 1 2))) 0)
	    arity?)

(test-values '(1 2) (lambda () (with-handlers ([void void])
				 (values 1 2))))

(test 'c (#%plain-lambda () 'a (define-values (x) 'b) 'c))

(test '(quote a) 'quote (quote 'a))
(test '(quote a) 'quote ''a)
(syntax-test #'quote)
(syntax-test #'(quote))
(syntax-test #'(quote 1 2))

(test 12 (if #f + *) 3 4)
(syntax-test #'(+ 3 . 4))
(syntax-test #'(apply + 1 . 2))

(test 8 (lambda (x) (+ x x)) 4)
(define reverse-subtract
  (lambda (x y) (- y x)))
(test 3 reverse-subtract 7 10)
(define add4
  (let ((x 4))
    (lambda (y) (+ x y))))
(test 10 add4 6)
(test (letrec([x x]) x) 'lambda (let ([x (lambda () (define d d) d)]) (x)))
(test (letrec([x x]) x) 'lambda ((lambda () (define d d) d)))
(test '(3 4 5 6) (lambda x x) 3 4 5 6)
(test '(5 6) (lambda (x y . z) z) 3 4 5 6)
(test 'second (lambda () (cons 'first 2) 'second))
(syntax-test #'lambda)
(syntax-test #'(lambda))
(syntax-test #'(lambda x))
(syntax-test #'(lambda ()))
(syntax-test #'(lambda () (begin)))
(syntax-test #'(lambda . x))
(syntax-test #'(lambda x . x))
(syntax-test #'(lambda x . 5))
(syntax-test #'(lambda ((x)) x))
(syntax-test #'(lambda 5 x))
(syntax-test #'(lambda (5) x))
(syntax-test #'(lambda (x (y)) x))
(syntax-test #'(lambda (x . 5) x))
(syntax-test #'(lambda (x) x . 5))

(let ([f
       (case-lambda
	[() 'zero]
	[(x) (cons 1 1) 'one]
	[(x y) 'two]
	[(x y z . rest) 'three+]
	[x 'bad])]
      [g
       (case-lambda
	[(x y z) 'three]
	[(x y) (cons 2 2) 'two]
	[(x) 'one]
	[() 'zero]
	[x (cons 0 'more!) 'more])]
      [h
       (case-lambda
	[(x y) 'two]
	[(x y z w) 'four])])
  (test 'zero f)
  (test 'one f 1)
  (test 'two f 1 2)
  (test 'three+ f 1 2 3)
  (test 'three+ f 1 2 3 4)
  (test 'three+ f 1 2 3 4 5 6 7 8 9 10)

  (test 'zero g)
  (test 'one g 1)
  (test 'two g 1 2)
  (test 'three g 1 2 3)
  (test 'more g 1 2 3 4 5 6 7 8 9 10)

  (test 'two h 1 2)
  (test 'four h 1 2 3 4)
  (let ([h '(case-lambda
	     [(x y) 'two]
	     [(x y z w) 'four])])
    (error-test (datum->syntax #f (list h) #f) arity?)
    (error-test (datum->syntax #f (list* h '(1)) #f) arity?)
    (error-test (datum->syntax #f (list* h '(1 2 3)) #f) arity?)
    (error-test (datum->syntax #f (list* h '(1 2 3 4 5 6)) #f) arity?)))

(error-test #'((case-lambda)) arity?)

(syntax-test #'case-lambda)
(syntax-test #'(case-lambda . 1))
(syntax-test #'(case-lambda []))
(syntax-test #'(case-lambda 1))
(syntax-test #'(case-lambda x))
(syntax-test #'(case-lambda [x]))
(syntax-test #'(case-lambda [x 8][y]))
(syntax-test #'(case-lambda [x][y 9]))
(syntax-test #'(case-lambda [8 8]))
(syntax-test #'(case-lambda [((x)) 8]))
(syntax-test #'(case-lambda [(8) 8]))
(syntax-test #'(case-lambda [(x . 9) 8]))
(syntax-test #'(case-lambda [x . 8]))
(syntax-test #'(case-lambda [(x) . 8]))
(syntax-test #'(case-lambda . [(x) 8]))
(syntax-test #'(case-lambda [(x) 8] . y))
(syntax-test #'(case-lambda [(x) 8] . [y 7]))
(syntax-test #'(case-lambda [(x) 8] [8 7]))
(syntax-test #'(case-lambda [(x) 8] [((y)) 7]))
(syntax-test #'(case-lambda [(x) 8] [(8) 7]))
(syntax-test #'(case-lambda [(x) 8] [(y . 8) 7]))
(syntax-test #'(case-lambda [(x) 8] [y . 7]))
(syntax-test #'(case-lambda [(x) 8] [(y) . 7]))
(syntax-test #'(case-lambda [(x x) 8] [(y) 7]))
(syntax-test #'(case-lambda [(x . x) 8] [(y) 7]))
(syntax-test #'(case-lambda [(y) 7] [(x x) 8]))
(syntax-test #'(case-lambda [(y) 7] [(x . x) 8]))

(test 'yes 'if (if (> 3 2) 'yes 'no))
(test 'no 'if (if (> 2 3) 'yes 'no))
(test '1 'if (if (> 3 2) (- 3 2) (+ 3 2)))
(test-values '(1 2) (lambda () (if (cons 1 2) (values 1 2) 0)))
(test-values '(1 2) (lambda () (if (not (cons 1 2)) 0 (values 1 2))))
(syntax-test #'if)
(syntax-test #'(if))
(syntax-test #'(if . #t))
(syntax-test #'(if #t . 1))
(syntax-test #'(if #t 1 . 2))
(syntax-test #'(if #t))
(syntax-test #'(if #t 1))
(syntax-test #'(if #t 1 2 3))
(syntax-test #'(if #t 1 2 . 3))
(error-test #'(if (values 1 2) 3 4) arity?)

(test (void) 'when (when (> 1 2) 0))
(test (void) 'when (when (> 1 2) (cons 1 2) 0))
(test 0 'when (when (< 1 2) 0))
(test 0 'when (when (< 1 2) (cons 1 2) 0))
(test-values '(0 10) (lambda () (when (< 1 2) (values 0 10))))
(syntax-test #'when)
(syntax-test #'(when))
(syntax-test #'(when . 1))
(syntax-test #'(when 1))
(syntax-test #'(when 1 . 2))
(error-test #'(when (values 1 2) 0) arity?)

(test (void) 'unless (unless (< 1 2) 0))
(test (void) 'unless (unless (< 1 2) (cons 1 2) 0))
(test 0 'unless (unless (> 1 2) 0))
(test 0 'unless (unless (> 1 2) (cons 1 2) 0))
(test-values '(0 10) (lambda () (unless (> 1 2) (values 0 10))))
(syntax-test #'unless)
(syntax-test #'(unless))
(syntax-test #'(unless . 1))
(syntax-test #'(unless 1))
(syntax-test #'(unless 1 . 2))
(error-test #'(unless (values 1 2) 0) arity?)

(define x 2)
(test 3 'define (+ x 1))
(set! x 4)
(test 5 'set! (+ x 1))
(syntax-test #'set!)
(syntax-test #'(set!))
(syntax-test #'(set! x))
(syntax-test #'(set! x 1 2))
(syntax-test #'(set! 1 2))
(syntax-test #'(set! (x) 1))
(syntax-test #'(set! . x))
(syntax-test #'(set! x . 1))
(syntax-test #'(set! x 1 . 2))

(define (set!-not-ever-defined) (set! not-ever-defined (add1 not-ever-defined)))
(err/rt-test (set!-not-ever-defined) exn:fail:contract:variable?)

(set!-values (x) 9)
(test 9 'set!-values x)
(test (void) 'set!-values (set!-values () (values)))
(syntax-test #'set!-values)
(syntax-test #'(set!-values))
(syntax-test #'(set!-values . x))
(syntax-test #'(set!-values x))
(syntax-test #'(set!-values 8))
(syntax-test #'(set!-values (x)))
(syntax-test #'(set!-values (x) . 0))
(syntax-test #'(set!-values x 0))
(syntax-test #'(set!-values (x . y) 0))
(syntax-test #'(set!-values (x . 8) 0))
(syntax-test #'(set!-values (x 8) 0))
(syntax-test #'(set!-values (x) 0 1))
(syntax-test #'(set!-values (x) 0 . 1))
(syntax-test #'(set!-values (x x) 0))
(syntax-test #'(set!-values (x y x) 0))
(syntax-test #'(set!-values (y x x) 0))

(error-test #'(set!-values () 1) arity?)
(error-test #'(set!-values () (values 1 2)) arity?)
(error-test #'(set!-values (x) (values)) arity?)
(error-test #'(set!-values (x) (values 1 2)) arity?)
(error-test #'(set!-values (x y) 1) arity?)
(error-test #'(set!-values (x y) (values 1 2 3)) arity?)

(error-test #'(set! unbound-variable 5) exn:fail:contract:variable?)

(test 'greater 'cond (cond ((> 3 2) 'greater)
			   ((< 3 2) 'less)))
(test 'equal 'cond (cond ((> 3 3) 'greater)
			 ((< 3 3) 'less)
			 (else 'equal)))
(test 2 'cond (cond ((assv 'b '((a 1) (b 2))) => cadr)
		     (else #f)))
(test #f 'cond (cond ((assv 'z '((a 1) (b 2))) => cadr)
		     (else #f)))
(syntax-test #'(cond ((assv 'z '((a 1) (b 2))) => cadr)
		    (else 8)
		    (else #f)))
(test #f 'cond (let ([else #f])
		 (cond ((assv 'z '((a 1) (b 2))) => cadr)
		       (else 8)
		       (#t #f))))
(test 'second 'cond (cond ((< 1 2) (cons 1 2) 'second)))
(test 'second-again 'cond (cond ((> 1 2) 'ok) (else (cons 1 2) 'second-again)))
(test 1 'cond (cond (1)))
(test 1 'cond (cond (#f) (1)))
(test 1 'cond (cond (#f 7) (1)))
(test 2 'cond (cond (#f 7) (1 => add1)))
(test add1 'cond (let ([=> 9]) (cond (#f 7) (1 => add1))))
(non-z '(test 0 'case (case (* 2 3)
		(6 0)
		(else 7))))
(test 'composite 'case (case (* 2 3)
			 ((2 3 5 7) 'prime)
			 ((1 4 6 8 9) 'composite)))
(test 'consonant 'case (case (car '(c d))
			 ((a e i o u) 'vowel)
			 ((w y) 'semivowel)
			 (else 'consonant)))
(test 'second 'case (case 10
		      [(10) (cons 1 2) 'second]
		      [else 5]))
(test 'second-again 'case (case 11
			    [(10) (cons 1 2) 'second]
			    [else (cons 1 2) 'second-again]))
(test-values '(10 9) (lambda ()
		       (cond
			[(positive? 0) 'a]
			[(positive? 10) (values 10 9)]
			[else #f])))
(test-values '(10 9) (lambda ()
		       (case (string->symbol "hello")
			[(bye) 'a]
			[(hello) (values 10 9)]
			[else #f])))
(error-test #'(cond [(values 1 2) 8]) arity?)
(error-test #'(case (values 1 2) [(a) 8]) arity?)
(syntax-test #'(case 1 []) #rx"ill-formed clause")
(syntax-test #'(case 1 [(y) 5] []) #rx"ill-formed clause")
(syntax-test #'(case 1 [x]) #rx"not a datum sequence")
(syntax-test #'(case 1 [(y) 5] [x]) #rx"not a datum sequence")
(syntax-test #'(case 1 [(y) 5] [x x]) #rx"not a datum sequence")
(syntax-test #'(case 1 [x x]) #rx"not a datum sequence")
(syntax-test #'(case 1 [(x)]) #rx"missing expression after datum sequence")
(syntax-test #'(case 1 [(y) 5] [(x)]) #rx"missing expression after datum sequence")
(syntax-test #'(case 1 [(x) . 8]) #rx"illegal use of `.'")
(syntax-test #'(case 1 [(x) 10] . 9) #rx"illegal use of `.'")

;; test larger `case' dispatches to trigger for binary-search
;; and hash-table-based dispatch:
(let ()
  (define (f x)
    (case x
      [(1003) 'even-further]
      [(0 -1 -2) 'low]
      [(1) 'one]
      [(2 3 4 5 6) 'middle]
      [(100) 'super]
      [(7 8 9 10 11) 'upper]
      [(1001) 'youch]
      [(12) 'high]
      [(1002) 'further]
      [(13) 'extreme]
      [(14) 'more]))
  (test 'low f -2)
  (test 'low f -1)
  (test 'low f 0)
  (test 'one f 1)
  (test 'middle f 2)
  (test 'middle f 3)
  (test 'middle f 4)
  (test 'middle f 5)
  (test 'middle f 6)
  (test 'upper f 7)
  (test 'upper f 8)
  (test 'upper f 9)
  (test 'upper f 10)
  (test 'upper f 11)
  (test 'high f 12)
  (test 'extreme f 13)
  (test 'more f 14)
  (test 'super f 100)
  (test 'youch f 1001)
  (test 'further f 1002)
  (test 'even-further f 1003)
  (test (void) f 1004)
  (test (void) f 104)
  (test (void) f -104))

(let ()
  (define (f x)
    (case x
      [(#\u1003) 'even-further]
      [(#\u0) 'low]
      [(#\u1) 'one]
      [(#\u2 #\u3 #\u4 #\u5 #\u6) 'middle]
      [(#\u100) 'super]
      [(#\u7 #\u8 #\u9 #\u10 #\u11) 'upper]
      [(#\u1001) 'youch]
      [(#\u12) 'high]
      [(#\u1002) 'further]
      [(#\u13) 'extreme]
      [(#\u14) 'more]))
  (test 'low f #\u0)
  (test 'one f #\u1)
  (test 'middle f #\u2)
  (test 'middle f #\u3)
  (test 'middle f #\u4)
  (test 'middle f #\u5)
  (test 'middle f #\u6)
  (test 'upper f #\u7)
  (test 'upper f #\u8)
  (test 'upper f #\u9)
  (test 'upper f #\u10)
  (test 'upper f #\u11)
  (test 'high f #\u12)
  (test 'extreme f #\u13)
  (test 'more f #\u14)
  (test 'super f #\u100)
  (test 'youch f #\u1001)
  (test 'further f #\u1002)
  (test 'even-further f #\u1003)
  (test (void) f #\u1004)
  (test (void) f #\u104))

(let ()
  (define (f x)
    (case x
      [(low) 0]
      [(one) 1]
      [(middle) 2]
      [(upper #t) 3]
      [(high big up-there more) 4]
      [(extreme massive huge #f gigantic) 5]))
  (test 0 f 'low)
  (test 1 f 'one)
  (test 2 f 'middle)
  (test 3 f 'upper)
  (test 3 f #t)
  (test 4 f 'high)
  (test 4 f 'big)
  (test 4 f 'up-there)
  (test 4 f 'more)
  (test 5 f 'extreme)
  (test 5 f 'massive)
  (test 5 f 'huge)
  (test 5 f #f)
  (test 5 f 'gigantic)
  (test (void) f 'gigante)
  (test (void) f 0))

(let ()
  ;; This test relies on interning of string literals.
  (define (f x)
    (case x
      [("low") 0]
      [("one") 1]
      [("middle") 2]
      [("upper" #t) 3]
      [("high" "big" "up-there" "more") 4]
      [("extreme" "massive" "huge" "gigantic" #f) 5]))
  (test 0 f "low")
  (test 1 f "one")
  (test 2 f "middle")
  (test 3 f "upper")
  (test 3 f #t)
  (test 4 f "high")
  (test 4 f "big")
  (test 4 f "up-there")
  (test 4 f "more")
  (test 5 f "extreme")
  (test 5 f "massive")
  (test 5 f "huge")
  (test 5 f #f)
  (test 5 f "gigantic")
  (test (void) f "gigante")
  (test (void) f 'gigante)
  (test (void) f 0))

(let ()
  ;; This test uses string-copy to avoid interning string literals.
  (define (f x)
    (define y 
      (if (string? x)
          (string-copy x)
          x))
    (case y
      [("low") 0]
      [("one") 1]
      [("middle") 2]
      [("upper" #t) 3]
      [("high" "big" "up-there" "more") 4]
      [("extreme" "massive" "huge" "gigantic" #f) 5]))
  (test 0 f "low")
  (test 1 f "one")
  (test 2 f "middle")
  (test 3 f "upper")
  (test 3 f #t)
  (test 4 f "high")
  (test 4 f "big")
  (test 4 f "up-there")
  (test 4 f "more")
  (test 5 f "extreme")
  (test 5 f "massive")
  (test 5 f "huge")
  (test 5 f #f)
  (test 5 f "gigantic")
  (test (void) f "gigante")
  (test (void) f 'gigante)
  (test (void) f 0))

(let ()
  (define (f x)
    (case x
      [("zero"  #"zero"  (z . 0) (z e r o)   #(z e r o)   #&zero 
                #hash((z . "z") (e . "e") (r . "r") (o . "o")) 
                #s(z e r o)) 
       0]
      [("one"   #"one"   (o . 1) (o n e)     #(o n e)     #&one 
                #hash((o . "o") (n . "n") (e . "e")) 
                #s(o n e)) 
       1]
      [("two"   #"two"   (t . 2) (t w o)     #(t w o)     #&two 
                #hash((t . "t") (w . "w") (o . "o")) 
                #s(t w o))
       2]
      [("three" #"three" (t . 3) (t h r e e) #(t h r e e) #&three 
                #hash((t . "t") (h . "h") (r . "e") (e . "e") (e . "e")) 
                #s(t h r e e)) 
       3]
      [("four"  #"four"  (f . 4) (f o u r)   #(f o u r)   #&four 
                #hash((f . "f") (o . "o") (u . "u") (r . "r"))
                #s(f o u r))
       4]
      [("five"  #"five"  (f . 5) (f i v e)   #(f i v e)   #&five 
                #hash((f . "f") (i . "i") (v . "v") (e . "e"))
                #s(f i v e)) 
       5]
      [("six"   #"six"   (s . 6) (s i x)     #(s i x)     #&six 
                #hash((s . "s") (i . "i") (x . "x")) 
                #s(s i x)) 
       6]
      [("seven" #"seven" (s . 7) (s e v e n) #(s e v e n) #&seven 
                #hash((s . "s") (e . "e") (v . "v") (e . "e") (n . "n"))
                #s(s e v e n))
       7]
      [("eight" #"eight" (e . 8) (e i g h t) #(e i g h t) #&eight 
                #hash((e . "e") (i . "i") (g . "g") (h . "h") (t . "t"))
                #s(e i g h t))
       8]))
  (test 8 f "eight")
  (test 7 f #"seven")
  (test 6 f (cons 's 6))
  (test 5 f '(f i v e))
  (test 4 f '#(f o u r))
  (test 3 f (box 'three))
  (test 2 f (hash 't "t" 'w "w" 'o "o"))
  (test 1 f #s(o n e))
  (test (void) f #f))

(test #t 'and (and (= 2 2) (> 2 1)))
(test #f 'and (and (= 2 2) (< 2 1)))
(test '(f g) 'and (and 1 2 'c '(f g)))
(test #t 'and (and))
(test-values '(1 12) (lambda () (and (cons 1 2) (values 1 12))))
(test #t 'or (or (= 2 2) (> 2 1)))
(test #t 'or (or (= 2 2) (< 2 1)))
(test #f 'or (or #f #f #f))
(test #f 'or (or))
(test '(b c) 'or (or (memq 'b '(a b c)) (+ 3 0)))
(test-values '(1 12) (lambda () (or (not (cons 1 2)) (values 1 12))))
(syntax-test #'(cond #t))
(syntax-test #'(cond ())  )
(syntax-test #'(cond (1 =>))  )
(syntax-test #'(cond (1 => 3 4))  )
(syntax-test #'(cond . #t))
(syntax-test #'(cond (#t . 1)))
(syntax-test #'(cond (#t 1) #f))
(syntax-test #'(cond (#t 1) . #f))
(error-test #'(cond ((values #t #f) 1)) arity?)
(syntax-test #'case)
(syntax-test #'(case))
(syntax-test #'(case 0 #t))
(syntax-test #'(case . 0))
(syntax-test #'(case 0 . #t))
(syntax-test #'(case 0 (0 #t)))
(syntax-test #'(case 0 ()))
(syntax-test #'(case 0 (0)))
(syntax-test #'(case 0 (0 . 8)))
(syntax-test #'(case 0 ((0 . 1) 8)))
(syntax-test #'(case 0 (0 8) #f))
(syntax-test #'(case 0 (0 8) . #f))
(syntax-test #'(case 0 (else 1) (else 2)))
(syntax-test #'(case 0 ((0) =>)))
(syntax-test #'=>)
(syntax-test #'else)
(syntax-test #'(and . 1))
(syntax-test #'(and 1 . 2))
(syntax-test #'(or . 1))
(syntax-test #'(or 1 . 2))
(error-test #'(and #t (values 1 2) 8) arity?)
(error-test #'(or #f (values 1 2) 8) arity?)

(test 6 'let (let ((x 2) (y 3)) (* x y)))
(test 'second 'let (let ((x 2) (y 3)) (* x y) 'second))
(test 6 'let-values (let-values (((x) 2) ((y) 3)) (* x y)))
(test 6 'let-values (let-values (((x y) (values 2 3))) (* x y)))
(test 35 'let (let ((x 2) (y 3)) (let ((x 7) (z (+ x y))) (* z x))))
(test 35 'let-values (let-values (((x y) (values 2 3))) (let-values (((x) 7) ((z) (+ x y))) (* z x))))
(test 70 'let* (let ((x 2) (y 3)) (let* ((x 7) (z (+ x y))) (* z x))))
(test 70 'let*-values (let ((x 2) (y 3)) (let*-values (((x) 7) ((z) (+ x y))) (* z x))))
(test #t 'letrec (letrec ((-even?
                           (lambda (n) (if (zero? n) #t (-odd? (- n 1)))))
                          (-odd?
                           (lambda (n) (if (zero? n) #f (-even? (- n 1))))))
                   (-even? 88)))
(test #t 'letrec-values (letrec-values (((-even? -odd?)
					 (values
					  (lambda (n) (if (zero? n) #t (-odd? (- n 1))))
					  (lambda (n) (if (zero? n) #f (-even? (- n 1)))))))
				       (-even? 88)))
(define x 34)
(test 5 'let (let ((x 3)) (define x 5) x))
(test 5 'let (let ((x 3)) (define-values (x w) (values 5 8)) x))
(test 34 'let x)
(test 6 'let (let () (define x 6) x))
(test 34 'let x)
(test 7 'let* (let* ((x 3)) (define x 7) x))
(test 34 'let* x)
(test 8 'let* (let* () (define x 8) x))
(test 34 'let* x)
(test 9 'letrec (letrec () (define x 9) x))
(test 34 'letrec x)
(test 10 'letrec (letrec ((x 3)) (define x 10) x))
(test 34 'letrec x)
(teval '(test 5 'letrec (letrec ((x 5)(y x)) y)))
(test 3 'let (let ((y 'apple) (x 3) (z 'banana)) x))
(test 3 'let* (let* ((y 'apple) (x 3) (z 'banana)) x))
(test 3 'letrec (letrec ((y 'apple) (x 3) (z 'banana)) x))
(test 3 'let* (let* ((x 7) (y 'apple) (z (set! x 3))) x))
(test 3 'let* (let* ((x 7) (y 'apple) (z (if (not #f) (set! x 3) #f))) x))
(test 3 'let* (let* ((x 7) (y 'apple) (z (if (not #t) #t (set! x 3)))) x))
(test 3 'let-values (let-values (((y x z) (values 'apple 3 'banana))) x))
(test 3 'let*-values (let*-values (((y x z) (values 'apple 3 'banana))) x))
(test 3 'letrec-values (letrec-values (((y x z) (values 'apple 3 'banana))) x))
(test 3 'let*-values (let*-values (((x y) (values 7 'apple)) ((z) (set! x 3))) x))
(test 3 'let*-values (let*-values (((x y) (values 7 'apple)) ((z) (if (not #f) (set! x 3) #f))) x))
(test 3 'let*-values (let*-values (((x y) (values 7 'apple)) ((z) (if (not #t) #t (set! x 3)))) x))
(test 1 'named-let-scope (let ([f add1]) (let f ([n (f 0)]) n)))

(test-values '(3 4) (lambda () (let ([x 3][y 4]) (values x y))))
(test-values '(3 -4) (lambda () (let loop ([x 3][y -4]) (values x y))))
(test-values '(3 14) (lambda () (let* ([x 3][y 14]) (values x y))))
(test-values '(3 24) (lambda () (letrec ([x 3][y 24]) (values x y))))
(test-values '(3 54) (lambda () (let-values ([(x y) (values 3 54)]) (values x y))))
(test-values '(3 64) (lambda () (let*-values ([(x y) (values 3 64)]) (values x y))))
(test-values '(3 74) (lambda () (letrec-values ([(x y) (values 3 74)]) (values x y))))

(test 'one 'let-values (let-values ([() (values)]) 'one))
(test 'two 'let*-values (let*-values ([() (values)]) 'two))
(test 'three 'letrec-values (letrec-values ([() (values)]) 'three))
(test 'onex 'let-values (let-values ([() (values)][() (values)]) 'onex))
(test 'twox 'let*-values (let*-values ([() (values)][() (values)]) 'twox))
(test 'threex 'letrec-values (letrec-values ([() (values)][() (values)]) 'threex))

(letrec ([undef undef])
  (test (list 1 undef undef) 'no-split-letrec (letrec-values ([(a b c) (values 1 a b)]) (list a b c))))

(test '(10 11) 'letrec-values (letrec-values ([(names kps)
					       (letrec ([oloop 10])
						 (values oloop (add1 oloop)))])
					     (list names kps)))

(define (error-test-let/no-* expr)
  (syntax-test (datum->syntax #f (cons 'let expr) #f))
  (syntax-test (datum->syntax #f (cons 'let (cons 'name expr)) #f))
  (syntax-test (datum->syntax #f (cons 'letrec expr) #f)))
(define (error-test-let expr)
  (error-test-let/no-* expr)
  (syntax-test (datum->syntax #f (cons 'let* expr) #f)))
(error-test-let #'x)
(error-test-let #'(x))
(error-test-let #'(()))
(error-test-let #'(x ()))
(syntax-test #'(let* x () 1))
(syntax-test #'(letrec x () 1))
(error-test-let #'(x . 1))
(error-test-let #'(() . 1))
(error-test-let #'(((x 1))))
(error-test-let #'(((x 1)) . 1))
(error-test-let #'(((x . 1)) 1))
(error-test-let #'(((1 1)) 1))
(error-test-let #'(((x 1) 1)  1))
(error-test-let #'(((x 1) . 1)  1))
(error-test-let #'(((x 1 1)) 1))
(error-test-let #'(((x 1 1)) 1))
(error-test-let #'(((x 1)) 1 . 2))
(error-test-let/no-* #'(((x 1) (x 2)) 1))
(error-test-let/no-* #'(((x 1) (y 3) (x 2)) 1))
(error-test-let/no-* #'(((y 3) (x 1) (x 2)) 1))
(error-test-let/no-* #'(((x 1) (x 2) (y 3)) 1))
(test 5 'let* (let* ([x 4][x 5]) x))
(error-test-let #'(() (define x 10)))
(error-test-let #'(() (define x 10) (define y 20)))

(define (do-error-test-let-values/no-* expr syntax-test)
  (syntax-test (datum->syntax #f (cons 'let-values expr) #f))
  (syntax-test (datum->syntax #f (cons 'letrec-values expr) #f)))
(define (do-error-test-let-values expr syntax-test)
  (do-error-test-let-values/no-* expr syntax-test)
  (syntax-test (datum->syntax #f (cons 'let*-values expr) #f)))
(define (error-test-let-values/no-* expr)
  (do-error-test-let-values/no-* expr syntax-test))
(define (error-test-let-values expr)
  (do-error-test-let-values expr syntax-test))
(error-test-let-values #'x)
(error-test-let-values #'(x))
(error-test-let-values #'(()))
(error-test-let-values #'(x ()))
(syntax-test #'(let*-values x () 1))
(syntax-test #'(letrec-values x () 1))
(error-test-let-values #'(x . 1))
(error-test-let-values #'(() . 1))
(error-test-let-values #'((((x) 1))))
(error-test-let-values #'((((x) 1)) . 1))
(error-test-let-values #'((((x) . 1)) 1))
(error-test-let-values #'((((1) 1)) 1))
(error-test-let-values #'((((x 1) 1)) 1))
(error-test-let-values #'((((1 x) 1)) 1))
(error-test-let-values #'((((x) 1) . 1)  1))
(error-test-let-values #'((((x) 1 1)) 1))
(error-test-let-values #'((((x . y) 1)) 1))
(error-test-let-values #'((((x . 1) 1)) 1))
(error-test-let-values #'((((x) 1)) 1 . 2))
(error-test-let-values #'((((x x) 1)) 1))
(error-test-let-values #'((((y) 0) ((x x) 1)) 1))
(error-test-let-values #'((((x x) 1) ((y) 0)) 1))
(error-test-let-values/no-* #'((((x) 1) ((x) 2)) 1))
(error-test-let-values/no-* #'((((x) 1) ((y) 3) ((x) 2)) 1))
(error-test-let-values/no-* #'((((y) 3) ((x) 1) ((x) 2)) 1))
(error-test-let-values/no-* #'((((x) 1) ((x) 2) ((y) 3)) 1))
(test 5 'let* (let*-values ([(x) 4][(x) 5]) x))

(do-error-test-let-values #'((((x y) 1)) 1) (lambda (x) (error-test x arity?)))
(do-error-test-let-values #'((((x) (values 1 2))) 1) (lambda (x) (error-test x arity?)))
(do-error-test-let-values #'(((() (values 1))) 1) (lambda (x) (error-test x arity?)))
(do-error-test-let-values #'((((x) (values))) 1) (lambda (x) (error-test x arity?)))

(test 5 'embedded (let () (define y (lambda () x)) (define x 5) (y)))

(let ([wrap (lambda (body)
	      (syntax-test (datum->syntax #f `(let () ,@body) #f))
	      (syntax-test (datum->syntax #f `(let () (begin ,@body)) #f)))])
  (wrap '((define x 7) (define x 8) x))
  (wrap '((define 3 8) x))
  (wrap '((define-values x 8) x)))

(let ([wrap
       (lambda (val body)
	 (teval `(test ,val 'let-begin (let () ,@body)))
	 (teval `(test ,val 'let-begin (let ([xyzw 12]) ,@body)))
	 (teval `(test ,val (lambda () ,@body)))
	 (teval `(test ,val 'parameterize-begin
		       (parameterize () ,@body)))
	 (teval `(test ,val 'parameterize-begin
		       (parameterize ([current-directory (current-directory)])
			 ,@body)))
	 (teval `(test ,val 'with-handlers-begin
		       (with-handlers () ,@body)))
	 (teval `(test ,val 'with-handlers-begin
		       (with-handlers ([void void]) ,@body)))
	 (teval `(test ,val 'when-begin (when (positive? 1) ,@body)))
	 (teval `(test ,val 'unless-begin (unless (positive? -1) ,@body)))
	 (teval `(test ,val 'cons-begin (cond [(positive? 1) ,@body][else #f])))
	 (teval `(test ,val 'cons-else-begin (cond [(positive? -1) 0][else ,@body])))
         (teval `(test ,val 'case-begin (case (positive? 1) [(#t) ,@body][else -12])))
	 (teval `(test ,val 'cond-only-begin (cond [#t ,@body])))
	 (syntax-test (datum->syntax #f `(do ((x 1)) (#t ,@body) ,@body) #f))
	 (syntax-test (datum->syntax #f `(begin0 12 ,@body) #f)))])
  (wrap 5 '((begin (define x 5)) x))
  (wrap 5 '((begin (define x 5) x)))
  (wrap 15 '((begin (define x 5)) (begin (define y (+ x 10)) y)))
  (wrap 13 '((begin) 13))
  (wrap 7 '((begin) (begin) (begin (define x 7) (begin) x)))
  (wrap 7 '((begin (begin (begin (define x 7) (begin) x))))))

(define x 0)
(define (test-begin bg nested-bg)
  (let* ([make-args
	  (lambda (bg b)
	    (if (eq? bg 'begin)
		b
		(let* ([len (length b)]
		       [last (list-ref b (sub1 len))])
		  (cons last
			(let loop ([l b])
			  (if (null? (cdr l))
			      null
			      (cons (car l) (loop (cdr l)))))))))]
	 [test-bg
	  (lambda (v b)
	    (let* ([args (make-args bg b)]
		   [expr (cons bg args)])
	      (printf "~s:\n" expr)
	      (teval `(test ,v (quote ,bg) ,expr))))]
	 [make-bg
	  (lambda (b)
	    (cons nested-bg (make-args nested-bg b)))]
	 [make-test-bg-d
	  (lambda (bg)
	    (lambda (v1 v2 b)
	      (test-bg (if (eq? bg 'begin)
			   v1
			   v2)
		       b)))]
	 [test-bg-d (make-test-bg-d bg)]
	 [test-bg-d2 (make-test-bg-d nested-bg)])
  (teval '(set! x 0))
  (test-bg-d 6 1 '((set! x 5) (+ x 1)))
  (test-bg 5 '(5))
  (test-bg 3 '(2 3))
  (test-bg 3 `(2 (,bg 3)))
  (test-bg 3 `(,(make-bg '(2)) ,(make-bg '(3))))
  (test-bg-d 7 6 '((set! x 6) 'a (+ x 1)))
  (test-bg ''w '((set! x 6) 'a (+ x 1) 'w))
  (test-bg-d 8 7 '('b (set! x 7) (+ x 1)))
  (test-bg-d 9 8 '('b (set! x 8) 'a (+ x 1)))
  (test-bg ''z '('b (set! x 8) 'a (+ x 1) 'z))
  (test-bg-d 7 9 `(,(make-bg '((set! x 6) 'a)) (+ x 1)))
  (test-bg 10 `(,(make-bg '((set! x 60) 'a)) 10))
  (teval '(test 60 'x x))
  (test-bg 10 `(,(make-bg '((set! x 65) 'a)) (add1 20) 10))
  (teval '(test 65 'x x))
  (test-bg ''a `(10 ,(make-bg '((set! x 66) 'a))))
  (teval '(test 66 'x x))
  (test-bg ''a `(10 (add1 32) ,(make-bg '((set! x 67) 'a))))
  (teval '(test 67 'x x))
  (teval '(set! x 6))
  (test-bg-d 8 7 `(,(make-bg '('b (set! x 7) 'a)) (+ x 1)))
  (test-bg-d 9 8 `(,(make-bg '('b (set! x 8))) ,(make-bg '('a (+ x 1)))))
  (test-bg-d2 10 9 `(,(make-bg `(,(make-bg `('b (set! x 9) ,(make-bg '('a (+ x 1)))))))))
  (test-bg ''s `(,(make-bg `(,(make-bg `('b (set! x 9) ,(make-bg '('a (+ x 1) 's))))))))
  (test-bg ''t `(,(make-bg `(,(make-bg `('b (set! x 9) ,(make-bg '('a (+ x 1))))))) 't))
  (teval `(test 5 call-with-values (lambda () ,(make-bg '((values 1 2) (values 1 3 1)))) +))
  (syntax-test (datum->syntax #f `(,bg . 1) #f))
  (syntax-test (datum->syntax #f `(,bg 1 . 2) #f))))

(test-begin 'begin 'begin)
(test-begin 'begin0 'begin)
(test-begin 'begin0 'begin0)
(test-begin 'begin 'begin0)

(syntax-test #'(begin0))
(begin) ; must succeed, but we can't wrap it

(test 4 'implicit-begin (let ([x 4][y 7]) 'y x))
(test 4 'implicit-begin (let ([x 4][y 7]) y x))

(test 5 'implicit-begin (let () (begin) 10 5))

(error-test #'(begin (define foo (let/cc k k)) (foo 10)) exn:application:type?) ; not exn:application:continuation?

(define f-check #t)
(define f (delay (begin (set! f-check #f) 5)))
(test #t (lambda () f-check))
(test 5 force f)
(test #f (lambda () f-check))
(test 5 force f)
(define f-check-2 (delay (values 1 5)))
(test-values '(1 5) (lambda () (force f-check-2)))
(values 1 2)
(test-values '(1 5) (lambda () (force f-check-2)))
(syntax-test #'delay)
(syntax-test #'(delay))
(syntax-test #'(delay . 1))
(syntax-test #'(delay 1 . 2))

(let ([p (delay/sync 12)]
      [v #f])
  (thread (lambda () (set! v (force p))))
  (sync (system-idle-evt))
  (test 12 force p)
  (test 12 values v)
  (test (void) sync p)
  (test (list (void)) sync (wrap-evt p list)))

(test '(list 3 4) 'quasiquote `(list ,(+ 1 2) 4))
(test '(list a (quote a)) 'quasiquote (let ((name 'a)) `(list ,name ',name)))
(test '(a 3 4 5 6 b) 'quasiquote `(a ,(+ 1 2) ,@(map abs '(4 -5 6)) b))
(test '((foo 7) . cons)
	'quasiquote
	`((foo ,(- 10 3)) ,@(cdr '(c)) . ,(car '(cons))))
(test '#(10 5 2 4 3 8) 'quasiquote `#(10 5 ,(sqrt 4) ,@(map sqrt '(16 9)) 8))
(test 5 'quasiquote `,(+ 2 3))
(test '(a `(b ,(+ 1 2) ,(foo 4 d) e) f)
      'quasiquote `(a `(b ,(+ 1 2) ,(foo ,(+ 1 3) d) e) f))
(test '(a `(b ,x ,'y d) e) 'quasiquote
	(let ((name1 'x) (name2 'y)) `(a `(b ,,name1 ,',name2 d) e)))
(test '(list 3 4) 'quasiquote (quasiquote (list (unquote (+ 1 2)) 4)))
(test '`(list ,(+ 1 2) 4) 'quasiquote '(quasiquote (list (unquote (+ 1 2)) 4)))
(test '(()) 'qq `((,@'())))
(define x 5)
(test '(quasiquote (unquote x)) 'qq ``,x)
(test '(quasiquote (unquote 5)) 'qq ``,,x)
(test '(quasiquote (unquote-splicing x)) 'qq ``,@x)
(test '(quasiquote (unquote-splicing 5)) 'qq ``,@,x)
(test '(quasiquote (quasiquote (quasiquote (unquote (unquote (unquote x)))))) 'qq ````,,,x)
(test '(quasiquote (quasiquote (quasiquote (unquote (unquote (unquote 5)))))) 'qq ````,,,,x)

(test '#hash() 'qq `#hash())
(test '#hash(("apple" . 1) ("banana" . 2) ("coconut" . 3))
      'qq
      `#hash(("apple" . 1) ("banana" . 2) ("coconut" . 3)))
(test '#hash(("apple" . 1) ("banana" . 2) ("coconut" . 3))
      'qq
      `#hash(("apple" . ,1) ("banana" . ,(add1 1)) ("coconut" . ,(+ 1 2))))
(test '#hash(("foo" . (1 2 3 4 5)))
      'qq
      `#hash(("foo" . (1 2 ,(+ 1 2) 4 5))))
(test '#hash(("foo" . (1 2 (+ 1 2) 4 5)))
      'qq
      `#hash(("foo" . (1 2 (+ 1 2) 4 5))))
(test '#hash(("foo" . (1 2 3 4 5)))
      'qq
      `#hash(("foo" . (1 2 ,@(list 3 4 5)))))
(test '#hash((,(read) . 1) (,(+ 1 2) . 3))
      'qq
      `#hash((,(read) . 1) (,(+ 1 2) . ,(+ 1 2))))
(test '#hash((,(read) . 2))
      'qq
      `#hash((,(read) . 1) (,(read) . 2)))
(test '#hash(("moo" . 3) ("foo" . (1 2)))
      'qq
      `#hash(("moo" . ,(+ 1 2)) ("foo" . (1 2))))
(test '#hash(("moo" . (+ 1 2)) ("foo" . -1))
      'qq
      `#hash(("moo" . (+ 1 2)) ("foo" . ,(- 1 2))))
(syntax-test #'`#hash(("foo" . ,@(list 1 2 3 4 5))))
(error-test #'(read (open-input-string "`#hash((foo ,@(list 1 2 3 4 5)))")) exn:fail:read?)

(test '(quasiquote (unquote result)) 'qq `(quasiquote ,result))
(test (list 'quasiquote car) 'qq `(,'quasiquote ,car))

(syntax-test #'quasiquote)
(syntax-test #'(quasiquote))
(syntax-test #'(quasiquote . 5))
(syntax-test #'(quasiquote 1 . 2))
(syntax-test #'(quasiquote 1 2))
(syntax-test #'(unquote 7))
(syntax-test #'(unquote-splicing 7))

(syntax-test #'`(1 . ,@5))
(test (cons 1 5) 'qq `(1 ,@5))
(error-test #'`(1 ,@5 2))

(define (qq-test e)
  (syntax-test (datum->syntax #f e #f))
  (syntax-test (datum->syntax #f (list 'quasiquote e) #f))
  (syntax-test (datum->syntax #f (list 'quasiquote e) #f))
  (syntax-test (datum->syntax #f (list 'quasiquote (list 'quasiquote e)) #f))
  (syntax-test (datum->syntax #f (list 'quasiquote (list 'quasiquote (list 'unquote e))) #f))
  (syntax-test (datum->syntax #f (list 'quasiquote (list 'quasiquote (list 'unquote-splicing e))) #f)))
(qq-test #'(unquote))
(qq-test #'(unquote 7 8 9))
(qq-test #'(unquote-splicing))
(qq-test #'(unquote-splicing 7 8 9))

(test '(unquote . 5) 'qq (quasiquote (unquote . 5)))
(test '(unquote 1 . 5) 'qq (quasiquote (unquote 1 . 5)))
(test '(unquote 1 2 . 5) 'qq (quasiquote (unquote 1 2 . 5)))

(test '(unquote 1 2 7 . 5) 'qq (quasiquote (unquote 1 2 ,(+ 3 4) . 5)))
(test '(unquote 1 2 (unquote (+ 3 4)) . 5) 'qq (quasiquote (unquote 1 2 ,',(+ 3 4) . 5)))

(test '(1 2 3 4 . 5) 'qq `(1 ,@'(2 3 4) . 5))

(error-test #'`(10 ,(values 1 2)) arity?)
(error-test #'`(10 ,@(values 1 2)) arity?)

(define add3 (lambda (x) (+ x 3)))
(test 6 'define (add3 3))
(define (add3 x) (+ x 3))
(test 6 'define (add3 3))
(define first car)
(test 1 'define (first '(1 2)))
(syntax-test #'define)
(syntax-test #'(define))
(syntax-test #'(define . x))
(syntax-test #'(define x))
(syntax-test #'(define x . 1))
(syntax-test #'(define 1 2))
(syntax-test #'(define (1) 1))
(syntax-test #'(define (x 1) 1))
(syntax-test #'(define (x a a) 1))
(syntax-test #'(define ((x 1) a) 1))
(syntax-test #'(define ((x b b) a) 1))
(syntax-test #'(define x 1 . 2))
(syntax-test #'(define x 1 2))

(let ()
  (define ((f x) y z) (list x y z))
  (test '(1 2 3) (f 1) 2 3))
(let ()
  (define ((g a) a b) (list a b))
  (test '(2 3) (g 1) 2 3))

(define-values (add3) (lambda (x) (+ x 3)))
(test 6 'define (add3 3))
(define-values (add3 another) (values (lambda (x) (+ x 3)) 9))
(test 6 'define (add3 3))
(test 9 'define another)
(define-values (first second third) (values car cadr caddr))
(test 1 'define (first '(1 2)))
(test 2 'define (second '(1 2)))
(test 3 'define (third '(1 2 3)))
(define-values () (values))
(syntax-test #'define-values)
(syntax-test #'(define-values))
(syntax-test #'(define-values . x))
(syntax-test #'(define-values x))
(syntax-test #'(define-values (x)))
(syntax-test #'(define-values x . 1))
(syntax-test #'(define-values (x) . 1))
(syntax-test #'(define-values 1 2))
(syntax-test #'(define-values (1) 2))
(syntax-test #'(define-values (x 1) 1))
(syntax-test #'(define-values (x . y) 1))
(syntax-test #'(define-values (x) 1 . 2))
(syntax-test #'(define-values (x) 1 2))
(syntax-test #'(define-values (x x) 10))
(syntax-test #'(define-values (x y x) 10))

(syntax-test #'((define x 2) 0 1))
(syntax-test #'(+ (define x 2) 1))
(syntax-test #'(if (define x 2) 0 1))
(syntax-test #'(begin0 (define x 2)))
(syntax-test #'(begin0 (define x 2) 0))
(syntax-test #'(begin0 0 (define x 2)))
(syntax-test #'(begin0 0 (define x 2) (define x 12)))
(syntax-test #'(let () (define x 2)))
(syntax-test #'(letrec () (define x 2)))
(syntax-test #'(lambda () (define x 2)))
(syntax-test #'(lambda () (void (define x 2)) 1))
(syntax-test #'(cond [(< 2 3) (define x 2)] [else 5]))
(syntax-test #'(cond [else (define x 2)]))

;; No good way to test in mzc:
(error-test #'(define x (values)) exn:application:arity?)
(error-test #'(define x (values 1 2)) exn:application:arity?)
(error-test #'(define-values () 3) exn:application:arity?)
(error-test #'(define-values () (values 1 3)) exn:application:arity?)
(error-test #'(define-values (x y) (values)) exn:application:arity?)
(error-test #'(define-values (x y) 3) exn:application:arity?)
(error-test #'(define-values (x y) (values 1 2 3)) exn:application:arity?)

(begin (define ed-t1 1) (define ed-t2 2))
(test 1 'begin-define ed-t1)
(test 2 'begin-define ed-t2)
(begin (begin (begin (begin 10 (define ed-t2.5 2.5) 12))))
(test 2.5 'begin-define ed-t2.5)
(syntax-test #'(if (zero? 0) (define ed-t3 3) (define ed-t3 -3)))
(syntax-test #'(if #t (define ed-t3 3) (define ed-t3 -3)))
(syntax-test #'(if #f (define ed-t3 3) (define ed-t3 -3)))

(test 45 'define
	(let ((x 5))
		(define foo (lambda (y) (bar x y)))
		(define bar (lambda (a b) (+ (* a b) a)))
		(foo (+ x 3))))
(define x 34)
(define (foo) (define x 5) x)
(test 5 foo)
(test 34 'define x)
(define foo (lambda () (define x 5) x))
(test 5 foo)
(test 34 'define x)
(define (foo x) ((lambda () (define x 5) x)) x)
(test 88 foo 88)
(test 4 foo 4)
(test 34 'define x)

(test 5 'define
      (let ()
	(define x 5)
	(define define (lambda (a b) (+ a b)))
	8
	(define x 7)
	x))
(test 8 'define ; used to be 6
      (let ([y 8])
	(define (define z w) 5)
	(define y 6)
	y))

(syntax-test #'(let ()
		(define x 5)))
(syntax-test #'(let ()
		(if #t
		    (define x 5))
		5))

; Can shadow syntax/macros with embedded defines
(test 5 'intdef (let ()
		  (define lambda 5)
		  lambda))
(test 5 'intdef (let ()
		  (define define 5)
		  'ok
		  define))

(syntax-test #'(lambda () (define x 10) (begin)))
(syntax-test #'(lambda () (define x 10) (begin) (begin)))
(syntax-test #'(lambda () (#%stratified-syntax (define x 10) (begin) (begin x) (begin))))
(syntax-test #'(lambda () (#%stratified-syntax (define x 10) x (define y 12) y)))
(syntax-test #'(lambda () (define-values (x) . 10) x))
(syntax-test #'(lambda () (define-values (x) 10) (begin 1 . 2) x))
(syntax-test #'(lambda () (begin (define-values (x) 10) . 2) x))
(syntax-test #'(lambda () (begin)))
(syntax-test #'(lambda () (define-values . 10) x))
(syntax-test #'(lambda () (define-values x 10) x))
(syntax-test #'(lambda () (define-values (1) 10) x))

(test '(10 12) apply (lambda () (define x 10) (random 3) (define y 12) (list x y)) null)
(test 10 apply (lambda () (define x 10) (begin) (begin x) (begin)) null)

(test '(11 18) apply (lambda () (define x 11) (values 1 2 3) (define y 18) (list x y)) null)

(test 87 (lambda () (define x 87) (begin) (begin x)))

(test '#(0 1 2 3 4) 'do (do ((vec (make-vector 5))
			     (i 0 (+ i 1)))
			    ((= i 5) vec)
			  (vector-set! vec i i)))
(test 25 'do (let ((x '(1 3 5 7 9)))
	       (do ((x x (cdr x))
		    (sum 0 (+ sum (car x))))
		   ((null? x) sum))))
(test 1 'let (let foo () 1))
(test '((6 1 3) (-5 -2)) 'let
      (let loop ((numbers '(3 -2 1 6 -5))
		 (nonneg '())
		 (neg '()))
	(cond ((null? numbers) (list nonneg neg))
	      ((negative? (car numbers))
	       (loop (cdr numbers)
		     nonneg
		     (cons (car numbers) neg)))
	      (else
	       (loop (cdr numbers)
		     (cons (car numbers) nonneg)
		     neg)))))
(test 5 'do (do ((x 1)) (#t 5)))
(test-values '(10 5) (lambda () (do ((x 1)) (#t (values 10 5)))))
(syntax-test #'do)
(syntax-test #'(do))
(syntax-test #'(do ()) )
(syntax-test #'(do () ()) )
(syntax-test #'(do (1) (#t 5) 5))
(syntax-test #'(do ((1)) (#t 5) 5))
(syntax-test #'(do ((1 7)) (#t 5) 5))
(syntax-test #'(do ((x . 1)) (#t 5) 5))
(syntax-test #'(do ((x 1) 2) (#t 5) 5))
(syntax-test #'(do ((x 1) . 2) (#t 5) 5))
(syntax-test #'(do ((x 1)) (#t . 5) 5))
(syntax-test #'(do ((x 1)) (#t 5) . 5))

(test 0 'let/cc (let/cc k (k 0) 1))
(test 0 'let/cc (let/cc k 0))
(test 1 'let/cc (let/cc k (cons 1 2) 1))
(test-values '(2 1) (lambda () (let/cc k (values 2 1))))
(test-values '(2 1) (lambda () (let/cc k (k 2 1))))
(syntax-test #'(let/cc))
(syntax-test #'(let/cc . k))
(syntax-test #'(let/cc k))
(syntax-test #'(let/cc k . 1))
(syntax-test #'(let/cc 1 1))

(test 0 'let/ec (let/ec k (k 0) 1))
(test 0 'let/ec (let/ec k 0))
(test 1 'let/ec (let/ec k (cons 1 2) 1))
(test-values '(2 1) (lambda () (let/ec k (values 2 1))))
(test-values '(2 1) (lambda () (let/ec k (k 2 1))))
(syntax-test #'(let/ec))
(syntax-test #'(let/ec . k))
(syntax-test #'(let/ec k))
(syntax-test #'(let/ec k . 1))
(syntax-test #'(let/ec 1 1))

(define x 1)
(define y -1)
(define (get-x) x)

(test 5 'parameterize (parameterize () 5))
(test 6 'parameterize (parameterize ([error-print-width 10]) 6))
(test 7 'parameterize (parameterize ([error-print-width 10]
				     [uncaught-exception-handler void]) 
                        7))
(define oepw (error-print-width))
(error-test #'(parameterize ([error-print-width 777]) (error 'bad)) exn:fail?)
(test oepw 'parameterize (error-print-width))
(error-test #'(parameterize ([error-print-width 777]
                             [current-output-port (current-error-port)])
                (error 'bad)) 
	    exn:fail?)
(error-test #'(parameterize ([error-print-width 'a]) 10))

(define p (make-parameter 1))
(define q (make-parameter 2))
(test '1 'pz-order (parameterize ([p 3][q (p)]) (q)))

(error-test #'(parameterize) syntaxe?)
(error-test #'(parameterize ()) syntaxe?)
(error-test #'(parameterize ((x y))) syntaxe?)
(error-test #'(parameterize ((x y)) . 8) syntaxe?)
(error-test #'(parameterize (x) 8) syntaxe?)
(error-test #'(parameterize (9) 8) syntaxe?)
(error-test #'(parameterize ((x z) . y) 8) syntaxe?)
(error-test #'(parameterize ((x . z)) 8) syntaxe?)
(error-test #'(parameterize ((x . 9)) 8) syntaxe?)
(error-test #'(parameterize ((x . 9)) 8) syntaxe?)

(error-test #'(parameterize ([10 10]) 8))
(error-test #'(parameterize ([10 10]) 8) (lambda (exn) (not (regexp-match #rx"argument" (exn-message exn)))))
(error-test #'(parameterize ([(lambda () 10) 10]) 8))
(error-test #'(parameterize ([(lambda (a) 10) 10]) 8))
(error-test #'(parameterize ([(lambda (a b) 10) 10]) 8))

(test 1 'time (time 1))
(test -1 'time (time (cons 1 2) -1))
(test-values '(-1 1) (lambda () (time (values -1 1))))
(syntax-test #'time)
(syntax-test #'(time))
(syntax-test #'(time . 1))
(syntax-test #'(time 1 . 2))

; Tests specifically aimed at the compiler
(error-test #'(let ([x (values 1 2)]) x) exn:application:arity?)
; Known primitive
(error-test #'(let ([x (make-pipe)]) x) exn:application:arity?)
; Known local
(error-test #'(let* ([f (lambda () (values 1 2))][x (f)]) x) exn:application:arity?)

; Known local with global in its closure
(test 15 'known (let ([g (lambda ()
			   (letrec ([f (lambda (x)
					 (+ x 5))])
			     (f 10)))])
		  (g)))
; Known local with a set!
(test 16 'known (let ([g (lambda ()
			   (letrec ([f (lambda (x)
					 (let ([y x])
					   (set! x 7)
					   (+ y 5)))])
			     (f 11)))])
		  (g)))
; Known local non-function
(error-test #'(apply (lambda () (let ([f 12]) (f))) null) exn:application:type?)
; Known local with revsed arguments:
(test 10 (letrec ([f (lambda (a b) (if (zero? a) b (f b a)))]) f) 10 0)

(syntax-test #'#%datum)
(syntax-test #'(let ([#%datum 5])
		 1))
(test '(1) '#%datum (#%datum 1))
(test 1 '#%datum (#%datum . 1))
(test 'a '#%datum (#%datum . a))

(syntax-test #'#%app)
(syntax-test #'(#%app . 1))
(syntax-test #'(#%app 2 . 1))
(syntax-test #'(#%app lambda 1))
(syntax-test #'(let ([#%app 5])
		 (+ 1 2)))

(test 3 '#%app (#%app + 1 2))
(syntax-test #'())
(syntax-test #'(#%app))

(syntax-test #'#%top)
(syntax-test #'(#%top 1))
(syntax-test #'(let ([#%top 5])
		 x))
(err/rt-test (#%top . lambda) exn:fail:contract:variable?)
(define x 5)
(test 5 '#%top (#%top . x))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tests related to bytecode optimizer.
;; The (if (let ([x M]) (if x x N)) ...)
;;   => (if (if M #t N) ...)
;; converter drops the variable `x', which means
;; that other mappings must adjust

(let ([val 0])
  (let ([g (lambda ()
	     (letrec ([f (lambda (z x)
			   (if (let ([w (even? 81)])
				 (if w
				     w
				     (let ([y x])
				       (set! x 7)
				       (set! val (+ y 5)))))
			       'yes
			       'no))])
	       (f 0 11)))])
    (g))
  (test 16 values val))

(let ([val 0])
  (let ([g (lambda ()
	     (letrec ([f (lambda (z x)
			   (if (let ([w (even? 81)])
				 (if w
				     w
				     (let ([y x])
				       (set! val (+ y 5)))))
			       'yes
			       'no))])
	       (f 0 11)))])
    (g))
  (test 16 values val))

;; Function-inline test where (h (g v 10)) involves two inlines:
(letrec ([f (lambda (x) (h (g v 10)))]
	 [h (lambda (x) (list x x))]
	 [g (lambda (a b) a)]
	 [v (list 'hello)]
	 [w (list 'no!)]) 
  (test '((hello) (hello)) f 10))

;; Inlining introduces a let binding that is immediately dropped:
(test '(1 . 2)
      (let ([x (cons 1 2)]) (let ([f (lambda (x) x)]) (f (lambda (y) x))))
      10)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Check #%top-interaction

(module quoting-top-interaction racket/base
  (require (for-syntax racket/base))
  (provide (except-out (all-from-out racket/base) #%top-interaction)
           (rename-out [top-interaction #%top-interaction]))
  (define-syntax top-interaction 
    (syntax-rules ()
      [(_ . e) (quote e)])))

(dynamic-require ''quoting-top-interaction #f)
(let ([ns (make-empty-namespace)])
  (namespace-attach-module (current-namespace) ''quoting-top-interaction ns)
  (parameterize ([current-namespace ns])
    (namespace-require ''quoting-top-interaction))
  (test 3 'non-top
        (parameterize ([current-namespace ns])
          (eval '(+ 1 2))))
  (test ''(+ 1 2) 'repl-top
        (let ([s (open-output-bytes)])
          (parameterize ([current-input-port (open-input-string "(+ 1 2)")]
                         [current-namespace ns]
                         [current-output-port s])
            (read-eval-print-loop))
          (let ([p (open-input-bytes (get-output-bytes s))])
            (read p)
            (read p))))
  (let ([tmp-file (make-temporary-file)])
    (let-values ([(base tmp1 mbd?) (split-path tmp-file)])
    (with-output-to-file tmp-file (lambda () (display '(+ 1 2))) #:exists 'truncate/replace)
    (test '(+ 1 2) 'repl-top
          (parameterize ([current-namespace ns])
            (load tmp-file)))
    (with-output-to-file tmp-file (lambda () (display `(module ,tmp1 racket/base (provide x) (define x 12))))
                         #:exists 'truncate/replace)
    (test 12 'module
          (parameterize ([current-namespace ns])
            (dynamic-require tmp-file 'x)))
    (delete-file tmp-file))))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Check that locations for lambda arguments are created
;; one-by-one --- like `let*', and not like `letrec':

(test '((1 10) (x1 10) (x2 z1))
      'lambda-loc
      (let ()
        (define procs null)
        (define again #f)

        (define (f x 
                   [y (let/cc k
                        (unless again
                          (set! again k))
                        (lambda () 'done))]
                   [z 10])
          (set! procs
                (cons (lambda (xv zv)
                        (begin0
                         (list x z)
                         (set! x xv)
                         (set! z zv)))
                      procs))
          (y))

        (f 1)
        (let/cc esc (again esc))

        (list
         ((cadr procs) 'x1 'z1)
         ((car procs) 'x2 'z2)
         ((cadr procs) 'x10 'z10))))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require racket/splicing)

(define abcdefg 10)
(test 12 'splicing-letrec-syntax (splicing-letrec-syntax ([abcdefg (syntax-rules ()
                                                                     [(_) 12])])
                                                         (abcdefg)))
(test 13 'splicing-letrec-syntax (splicing-letrec-syntax ([abcdefg (syntax-rules ()
                                                                     [(_) (abcdefg 10)]
                                                                     [(_ x) (+ 3 x)])])
                                                         (abcdefg)))
(test 13 'splicing-letrec-syntax (let ([abcdefg 9])
                                   (splicing-letrec-syntax ([abcdefg (syntax-rules ()
                                                                       [(_) (abcdefg 10)]
                                                                       [(_ x) (+ 3 x)])])
                                                           (abcdefg))))
(test 12 'splicing-let-syntax (splicing-let-syntax ([abcdefg (syntax-rules ()
                                                               [(_) 12])])
                                                   (abcdefg)))
(test 12 'splicing-let-syntax (let ([abcdefg (lambda () 9)])
                                (splicing-let-syntax ([abcdefg (syntax-rules ()
                                                                 [(_) 12])])
                                                     (abcdefg))))
(test 11 'splicing-let-syntax (let ([abcdefg (lambda (x) x)])
                                (splicing-let-syntax ([abcdefg (syntax-rules ()
                                                                 [(_) (+ 2 (abcdefg 9))]
                                                                 [(_ ?) 77])])
                                                     (abcdefg))))
(define expand-test-use-toplevel? #t)
(splicing-let-syntax ([abcdefg (syntax-rules ()
                                 [(_) 8])])
                     (define hijklmn (abcdefg)))
(define expand-test-use-toplevel? #f)
(test 8 'hijklmn hijklmn)
(test 30 'local-hijklmn (let ()
                          (splicing-let-syntax ([abcdefg (syntax-rules ()
                                                           [(_) 8])])
                                               (define hijklmn (abcdefg)))
                          (define other 22)
                          (+ other hijklmn)))
(test 8 'local-hijklmn (let ()
                         (splicing-let-syntax ([abcdefg (syntax-rules ()
                                                          [(_) 8])])
                                              (begin
                                                (define hijklmn (abcdefg))
                                                hijklmn))))

(test 9 'splicing-letrec-syntax (let ([abcdefg (lambda () 9)])
                                  (splicing-letrec-syntax ([abcdefg (syntax-rules ()
                                                                      [(_) 0])])
                                                          (define x 10))
                                  (abcdefg)))


;; ----------------------------------------

(test 79 'splicing-let (let ()
                         (splicing-let ([x 79])
                           (define (y) x))
                         (y)))
(test 77 'splicing-let (let ()
                         (define q 77)
                         (splicing-let ([q 8]
                                        [x q])
                           (define (z) x))
                         (z)))
(test 81 'splicing-letrec (let ()
                            (define q 77)
                            (splicing-letrec ([q 81]
                                              [x q])
                              (define (z) x))
                            (z)))
(test 82 'splicing-letrec (let ()
                            (define q 77)
                            (splicing-letrec ([x (lambda () (q))]
                                              [q (lambda () 82)])
                              (define (z) x))
                            ((z))))
(test 81 'splicing-letrec (eval
                            '(begin
                               (define q 77)
                               (splicing-letrec ([q 81]
                                                 [x q])
                                                (define (z) x))
                               (z))))
(test 82 'splicing-letrec (eval
                            '(begin
                               (define q 77)
                               (splicing-letrec ([x (lambda () (q))]
                                                 [q (lambda () 82)])
                                                (define (z) x))
                               ((z)))))
(err/rt-test (eval
              '(begin
                 (splicing-letrec ([x q]
                                   [q 81])
                  x)))
             exn:fail:contract:variable?)

(test 82 'splicing-letrec-syntaxes+values
      (let ()
        (define q 77)
        (splicing-letrec-syntaxes+values
           ([(mx) (lambda (stx) (quote-syntax (x)))]
            [(m) (lambda (stx) (quote-syntax (mx)))])
           ([(x) (lambda () (q))]
            [(q) (lambda () 82)])
          (define (a) (m)))
        (a)))

(test 82 'splicing-letrec-syntaxes+values
      (eval
       '(begin
          (define q 77)
          (splicing-letrec-syntaxes+values
              ([(mx) (lambda (stx) (quote-syntax (x)))]
               [(m) (lambda (stx) (quote-syntax (mx)))])
              ([(x) (lambda () (q))]
               [(q) (lambda () 82)])
            (define (a) (m)))
          (a))))

(test 82 'splicing-local
      (let ()
        (define (x) q)
        (define q 77)
        (define-syntax (m stx) (quote-syntax (x)))
        (splicing-local
            [(define-syntax (m stx) (quote-syntax (mx)))
             (define (x) (q))
             (define-syntax (mx stx) (quote-syntax (x)))
             (define (q) 82)]
          (define (a) (m)))
        (a)))

(test 82 'splicing-local
      (eval
       '(begin
          (define (x) q)
          (define q 77)
          (define-syntax (m stx) (quote-syntax (x)))
          (splicing-local
              [(define-syntax (m stx) (quote-syntax (mx)))
               (define (x) (q))
               (define-syntax (mx stx) (quote-syntax (x)))
               (define (q) 82)]
            (define (a) (m)))
          (a))))

;; local names are not visible outside
(test 77 'splicing-local
      (let ()
        (define q 77)
        (define-syntax (m stx) (quote-syntax (x)))
        (splicing-local
            [(define-syntax (m stx) (quote-syntax (q)))
             (define (q) 82)]
          (define (a) (m)))
        (m)))
(test 77 'splicing-local
      (eval
       '(begin
          (define q 77)
          (define-syntax (m stx) (quote-syntax (x)))
          (splicing-local
              [(define-syntax (m stx) (quote-syntax (q)))
               (define (q) 82)]
            (define (a) (m)))
          (m))))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Check keyword & optionals for define-syntax 
;; and define-syntax-for-values:

(test (list 7 #f)
      'dfs/kw
      (eval
       '(begin
          (define-for-syntax (kw/f #:x a b)
            `(list ,a ,b))
          (define-syntax (kw/g stx #:opt [opt #f])
            (syntax-case stx ()
              [(_ v) (datum->syntax stx (kw/f #:x #'v opt))]))
          (kw/g 7))))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Check mutation of local define-for-syntax in let-syntax:

(module set-local-dfs racket/base
  (require (for-syntax racket/base))
  (provide ten)

  (define-for-syntax tl-var 9)

  (define ten
    (let-syntax ([x1 (lambda (stx)
                       (set! tl-var (add1 tl-var))
                       (datum->syntax stx tl-var))])
      (x1))))

(test 10 dynamic-require ''set-local-dfs 'ten)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Test single-result checking in `begin0':

(let ()
  (define (twice x) (printf "ouch\n") (values x x))
  
  (define (pipeline2 . rfuns)
    (let ([x (begin0 ((car rfuns) 1) 123)])
      x))
  
  (define (try f)
    (call-with-values
        (lambda () (with-handlers ([void values]) (f twice)))
      (lambda xs xs)))
  
  (test #t exn? (caar (map try (list pipeline2)))))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Semantics of internal definitions != R5RS

(test 0 'racket-int-def (call-with-continuation-prompt
                         (lambda ()
                           (let ([v 0]
                                 [k #f]
                                 [q void])
                             (define f (let/cc _k (set! k _k)))
                             (define g v) ; fresh location each evaluation
                             (if f
                                 (begin
                                   (set! q (lambda () g))
                                   (set! v 1)
                                   (k #f))
                                 (q))))))
(test 1 'racket-int-def (call-with-continuation-prompt
                         (lambda ()
                           (let ([v 0]
                                 [k #f]
                                 [q void])
                             (#%stratified-body
                              (define f (let/cc _k (set! k _k)))
                              (define g v) ; same location both evaluations
                              (if f
                                  (begin
                                    (set! q (lambda () g))
                                    (set! v 1)
                                    (k #f))
                                  (q)))))))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; check that the compiler is not too agressive with `letrec' -> `let*'

(test "#<undefined>\nready\n"
      get-output-string
      (let ([p (open-output-string)])
        (parameterize ([current-output-port p])
          (let ([restart void])
            (letrec ([dummy1 (let/cc k (set! restart k))]
                     [dummy2 (displayln maybe-ready)]
                     [maybe-ready 'ready])
              (let ([rs restart])
                (set! restart void)
                (rs #f)))))
        p))


;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Check that `syntax/loc' preserves the 'parent-shape property

(test #\[ syntax-property (syntax/loc #'a [b c]) 'paren-shape)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Check that inlining expansion of keyword-argument calls
;; attaches 'alias-of and 'converted-arguments-variant-of
;; syntax properties:

(parameterize ([current-namespace (make-base-namespace)])
  (eval '(require (for-syntax racket/base
                              racket/keyword-transform)))
  (eval '(module m racket/base (provide f) (define (f #:x [x 2]) x)))
  (eval '(require 'm))
  (eval '(define-syntax (extract stx)
           (syntax-case stx ()
             [(_ form pattern var alias?)
              (with-syntax ([e (local-expand #'form 'top-level '())])
                #'(let-syntax ([m (lambda (stx)
                                    (syntax-case (quote-syntax e) ()
                                      [pattern
                                       #`(quote-syntax (var
                                                        .
                                                        #,((if alias?
                                                               syntax-procedure-alias-property
                                                               syntax-procedure-converted-arguments-property)
                                                           #'var)))]))])
                    (define p (m))
                    (and (free-identifier=? (car (syntax-e p))
                                            (cdr (syntax-e (cdr (syntax-e p)))))
                         (car (syntax-e (cdr (syntax-e p)))))))])))
  (define f-id (eval '(quote-syntax f)))
  (test
   #t
   free-identifier=?
   f-id
   (eval '(extract (f #:x 8)
                   (lv ([(proc) f2] . _) (if const? (app f3 . _) . _))
                   f3
                   #f)))
  (test
   #t
   free-identifier=?
   f-id
   (eval '(extract (f #:x 8)
                   (lv ([(proc) f2] . _) (if const? (app f3 . _) . _))
                   f2
                   #t)))
  (test
   #t
   free-identifier=?
   f-id
   (eval '(extract (f #:y 9)
                   (lv ([(proc) f2] . _) . _)
                   f2
                   #t)))
  (test
   #t
   free-identifier=?
   f-id
   (eval '(extract f f2 f2 #t))))


;; Check that alias & converted-argument information is
;; cross-phase:
(require racket/keyword-transform)
(let ([e (parameterize ([current-namespace (make-base-namespace)])
           (expand '(module m racket/base
                      (define (f #:x [x 10]) x)
                      (f #:x 8))))])
  (define (find get)
    (let loop ([e e])
      (or (and (syntax? e)
               (or (get e)
                   (loop (syntax-e e))))
          (and (pair? e)
               (or (loop (car e))
                   (loop (cdr e)))))))
  (test #t 'cross-phase-alias
        (and (find syntax-procedure-converted-arguments-property)
             (find syntax-procedure-alias-property)
             #t)))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Check interaction of marks, `rename-out', and `free-identifier=?' 

(module check-free-eq-with-rename racket/base
  (require (for-syntax racket/base))
  (provide (rename-out [prefix:quote quote])
           check)
  (define-syntax (check stx)
    (syntax-case stx ()
      [(_ id) #`#,(free-identifier=? #'id #'prefix:quote)]))
  (define-syntax-rule (prefix:quote x) (quote x)))

(module use-rename-checker racket/base
  (define-syntax-rule (body)
    (begin
      (provide v)
      (require 'check-free-eq-with-rename)
      (define v (check quote))))
  (body))

(test #t dynamic-require ''use-rename-checker 'v)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Check `let` error messages

(syntax-test #'(let*) #rx"missing binding")
(syntax-test #'(let* ([x 10])) #rx"missing body")
(syntax-test #'(let) #rx"missing name or")
(syntax-test #'(let x) #rx"missing binding pairs or")
(syntax-test #'(let ([10 10])) #rx"missing binding pairs or")
(syntax-test #'(let x ([10 10])) #rx"missing body")
(syntax-test #'(letrec) #rx"missing binding")
(syntax-test #'(letrec ([x 3])) #rx"missing body")

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Check that expansion generated for internal definitions
;; introduces `values' and `begin' as if by macros:

(let ()
  (define (int-def-check)
    (define (values) (error 'hygiene "is broken"))
    1 ; expansion uses `values' and `begin'
    (define x 2)
    3)
  (test 3 int-def-check)

  (define (int-def-check2)
    (define (begin) (error 'hygiene "is broken"))
    1
    (define x 2)
    30)
  (test 30 int-def-check2))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Make sure `#%variable-reference' can be compiled and expanded

(compile '(#%variable-reference))
(expand '(#%variable-reference))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Check marshal & unmarshal of a syntax object
;; containing a list with a hash table

(let ([v #'(quote-syntax (#hash((1 . 2))))])
  (define-values (i o) (make-pipe))
  (write (compile v) o)
  (close-output-port o)
  (define e
    (parameterize ([read-accept-compiled #t])
      (read i)))
  (test (syntax->datum (eval v)) syntax->datum (eval e)))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(report-errs)
