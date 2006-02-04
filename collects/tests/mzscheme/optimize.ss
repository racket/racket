
(load-relative "loadtest.ss")

(SECTION 'optimization)

;; Check JIT inlining of primitives:
(parameterize ([current-namespace (make-namespace)]
	       [eval-jit-enabled #t])
  (namespace-require 'mzscheme)
  (let* ([un0 (lambda (v op arg)
		;; (printf "Trying ~a ~a\n" op arg);
		(let ([name `(,op ,arg)])
		  (test v name ((eval `(lambda (x) (,op x))) arg))
		  (when (boolean? v)
		    (test (if v 'yes 'no)
			  name 
			  ((eval `(lambda (x) (if (,op x) 'yes 'no))) arg)))))]
	 [un (lambda (v op arg)
	       (un0 v op arg)
	       (when (number? arg)
		 (let ([iv (if (number? v)
			       (exact->inexact v)
			       v)])
		   (un0 iv op (exact->inexact arg)))))]
	 [bin0 (lambda (v op arg1 arg2)
		 ;; (printf "Trying ~a ~a ~a\n" op arg1 arg2);
		 (let ([name `(,op ,arg1 ,arg2)])
		   (test v name ((eval `(lambda (x) (,op x ,arg2))) arg1))
		   (test v name ((eval `(lambda (x) (,op ,arg1 x))) arg2))
		   (test v name ((eval `(lambda (x y) (,op x y))) arg1 arg2))
		   (when (boolean? v)
		     ;; (printf " for branch...\n")
		     (test (if v 'yes 'no) name ((eval `(lambda (x) (if (,op x ,arg2) 'yes 'no))) arg1))
		     (test (if v 'yes 'no) name ((eval `(lambda (x) (if (,op ,arg1 x) 'yes 'no))) arg2)))))]
	 [bin (lambda (v op arg1 arg2)
		(bin0 v op arg1 arg2)
		(let ([iv (if (number? v)
			      (exact->inexact v)
			      v)])
		  (bin0 iv op (exact->inexact arg1) arg2)
		  (bin0 iv op arg1 (exact->inexact arg2))
		  (bin0 iv op (exact->inexact arg1) (exact->inexact arg2))))])

    (un #f 'null? 0)
    (un #f 'pair? 0)

    (bin #f eq? 0 10)

    (un #t 'zero? 0)
    (un #f 'zero? 1)
    (un #f 'zero? -1)

    (un #f 'positive? 0)
    (un #t 'positive? 1)
    (un #f 'positive? -1)

    (un #f 'negative? 0)
    (un #f 'negative? 1)
    (un #t 'negative? -1)

    (un #t 'not #f)
    (un #f 'not #t)
    (un #f 'not 10)

    (bin #t '< 100 200)
    (bin #f '< 200 100)
    (bin #f '< 100 100)

    (bin #t '<= 100 200)
    (bin #f '<= 200 100)
    (bin #t '<= 100 100)

    (bin #f '> 100 200)
    (bin #t '> 200 100)
    (bin #f '> 100 100)

    (bin #f '>= 100 200)
    (bin #t '>= 200 100)
    (bin #t '>= 100 100)

    (bin #f '= 100 200)
    (bin #f '= 200 100)
    (bin #t '= 100 100)

    (un 3 'add1 2)
    (un -3 'add1 -4)

    (un 1 'sub1 2)
    (un -5 'sub1 -4)

    ))

;; For some comparison, ignore the stack-depth
;;  part of the compilation result (since it's
;;  an approximation, anyway).
(define maybe-different-depths? #f)

(define (comp=? c1 c2)
  (let ([s1 (open-output-bytes)]
	[s2 (open-output-bytes)])
    (write c1 s1)
    (write c2 s2)
    (let ([t1 (get-output-bytes s1)]
	  [t2 (get-output-bytes s2)]
	  [skip-byte (+ 2 ; #~
			1 ; version length
			(string-length (version))
			1 ; symtab count
			1 ; length
			1 ; CPT_MARSHALLED for top
			1)])
      (or (bytes=? t1 t2)
	  (and maybe-different-depths?
	       (bytes=? (subbytes t1 0 (sub1 skip-byte))
			(subbytes t2 0 (sub1 skip-byte)))
	       (bytes=? (subbytes t1 skip-byte)
			(subbytes t2 skip-byte)))
	  (begin
	    (printf "~s\n~s\n" t1 t2)
	    #f
	    )))))

(define test-comp
  (case-lambda
   [(expr1 expr2) (test-comp expr1 expr2 #t)]
   [(expr1 expr2 same?)
    (test same? `(compile ,same? ,expr2) (comp=? (compile expr1) (compile expr2)))]))

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

; Can't drop `begin0' if the first expresson is not valueable:
(test-comp '(begin0 (begin0 (+ 1 2) 0) 0) '(begin0 (begin0 (+ 1 2) 'hi "apple") 1.5))

(test-comp 5 '(begin 'hi "apple" 1.5 5))
(test-comp 5 '(begin (begin 'hi "apple" 1.5) 5))
(test-comp 5 '(begin (begin 'hi "apple") 1.5 5))
(test-comp 5 '(begin (begin0 'hi "apple" 1.5) 5))
(test-comp 5 '(begin (begin0 'hi "apple") 1.5 5))
(test-comp 5 '(begin (begin 'hi "apple" 1.5 5)))
(test-comp 5 '(begin 'hi (begin "apple" 1.5 5)))

(test-comp '(let ([x 8][y 9]) (lambda () x))
	   '(let ([x 8][y 9]) (lambda () (if #f y x))))
(test-comp '(let ([x 8][y 9]) (lambda () (+ x y)))
	   '(let ([x 8][y 9]) (lambda () (if #f y (+ x y)))))

(test-comp '(let ([x 5]) (set! x 2)) '(let ([x 5]) (set! x x) (set! x 2)))

(test-comp '(let* () (f 5))
	   '(f 5))
(test-comp '(letrec () (f 5))
	   '(f 5))
(test-comp '(with-handlers () (f 5))
	   '(f 5))
(test-comp '(parameterize () (f 5))
	   '(f 5))
(test-comp '(fluid-let () (f 5))
	   '(f 5))

(test-comp '(let ([i (cons 0 1)]) (let ([j i]) j))
	   '(let ([i (cons 0 1)]) i))

(define (normalize-depth s)
  `(let ([a ,s]
	 [b (let-values ([(a b c d e f) (values 1 2 3 4 5 6)])
	      (list a b c d e f))])
     10))

(test-comp (normalize-depth '(let* ([i (cons 0 1)][j i]) j))
	   (normalize-depth '(let* ([i (cons 0 1)]) i)))

(test-comp (normalize-depth '(let* ([i (cons 0 1)][j (list 2)][k (list 3)][g i]) g))
	   (normalize-depth '(let* ([i (cons 0 1)][j (list 2)][k (list 3)]) i)))

(test-comp (normalize-depth '(let* ([i (cons 0 1)][j (list 2)][k (list 3)][g i][h g]) h))
	   (normalize-depth '(let* ([i (cons 0 1)][j (list 2)][k (list 3)]) i)))

(test-comp (normalize-depth '(let* ([i (cons 0 1)][g i][h (car g)][m h]) m))
	   (normalize-depth '(let* ([i (cons 0 1)][h (car i)]) h)))

(set! maybe-different-depths? #t)

(require #%kernel) ; 

(test-comp (void) '(void))
(test-comp 3 '(+ 1 2))
(test-comp 65 '(char->integer #\A))
(test-comp (expt 5 30)
	   '(expt 5 (* 5 6)))
(test-comp 88
	   '(if (pair? null) 89 88))
(test-comp '(if _x_ 2 1)
	   '(if (not _x_) 1 2))
(test-comp '(if _x_ 2 1)
	   '(if (not (not (not _x_))) 1 2))

(test-comp '(let ([x 3]) x)
	   '((lambda (x) x) 3))
(test-comp '(let ([x 3][y 4]) (+ x y))
	   '((lambda (x y) (+ x y)) 3 4))

(report-errs)
