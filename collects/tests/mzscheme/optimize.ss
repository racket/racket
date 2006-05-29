
(load-relative "loadtest.ss")

(SECTION 'optimization)

;; Check JIT inlining of primitives:
(parameterize ([current-namespace (make-namespace)]
	       [eval-jit-enabled #t])
  (namespace-require 'mzscheme)
  (let* ([check-error-message (lambda (name proc)
				(unless (memq name '(eq? not null? pair?
							 real? number? boolean? 
							 procedure? symbol?
							 string? bytes?
							 vector?
							 eof-object?))
				  (let ([s (with-handlers ([exn? exn-message])
					     (proc 'bad))]
					[name (symbol->string name)])
				    (test name
					  (lambda (v)
					    (and (string? v)
						 (let ([v (regexp-match 
							   (format "^~a"
								   (regexp-replace* #rx"[*?+]" name "\\\\\\0"))
							   v)])
						   (and v (car v)))))
					  s))))]
	 [un0 (lambda (v op arg)
		;; (printf "Trying ~a ~a\n" op arg)
		(let ([name `(,op ,arg)])
		  (test v name ((eval `(lambda (x) (,op x))) arg))
		  (when (boolean? v)
		    (test (if v 'yes 'no)
			  name 
			  ((eval `(lambda (x) (if (,op x) 'yes 'no))) arg)))))]
	 [un-exact (lambda (v op arg)
		     (check-error-message op (eval `(lambda (x) (,op x))))
		     (un0 v op arg))]
	       
	 [un (lambda (v op arg)
	       (un-exact v op arg)
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
	 [bin-exact (lambda (v op arg1 arg2)
		      (check-error-message op (eval `(lambda (x) (,op x ,arg2))))
		      (check-error-message op (eval `(lambda (x) (,op ,arg1 x))))
		      (bin0 v op arg1 arg2))]
	 [bin (lambda (v op arg1 arg2)
		(bin-exact v op arg1 arg2)
		(let ([iv (if (number? v)
			      (exact->inexact v)
			      v)])
		  (bin0 iv op (exact->inexact arg1) arg2)
		  (bin0 iv op arg1 (exact->inexact arg2))
		  (bin0 iv op (exact->inexact arg1) (exact->inexact arg2))))]
	 [tri0 (lambda (v op get-arg1 arg2 arg3 check-effect)
		 ;; (printf "Trying ~a ~a ~a\n" op (get-arg1) arg2 arg3);
		 (let ([name `(,op ,get-arg1 ,arg2, arg3)])
		   (test v name ((eval `(lambda (x) (,op x ,arg2 ,arg3))) (get-arg1)))
		   (check-effect)
		   (test v name ((eval `(lambda (x) (,op (,get-arg1) x ,arg3))) arg2))
		   (check-effect)
		   (test v name ((eval `(lambda (x) (,op (,get-arg1) ,arg2 x))) arg3))
		   (check-effect)
		   (test v name ((eval `(lambda (x y z) (,op x y z))) (get-arg1) arg2 arg3))
		   (check-effect)))]
	 [tri-exact (lambda (v op get-arg1 arg2 arg3 check-effect 3rd-all-ok?)
		      (check-error-message op (eval `(lambda (x) (,op x ,arg2 ,arg3))))
		      (check-error-message op (eval `(lambda (x) (,op (,get-arg1) x ,arg3))))
		      (unless 3rd-all-ok?
			(check-error-message op (eval `(lambda (x) (,op (,get-arg1) ,arg2 x)))))
		      (tri0 v op get-arg1 arg2 arg3 check-effect))])

    (un #f 'null? 0)
    (un #f 'pair? 0)
    (un #f 'boolean? 0)
    (un #t 'boolean? #t)
    (un #t 'boolean? #f)
    (un #f 'eof-object? #f)
    (un #t 'eof-object? eof)
    (un #f 'procedure? #f)
    (un #t 'procedure? procedure?)
    (un #t 'procedure? (lambda (x) 10))
    (un #t 'symbol? 'ok)
    (un #f 'symbol? #f)
    (un #t 'vector? (vector 1 2 3))
    (un #f 'vector? #f)
    (un #t 'string? "apple")
    (un #f 'string? #"apple")
    (un #f 'bytes? "apple")
    (un #t 'bytes? #"apple")

    (bin #f 'eq? 0 10)

    (un #t 'zero? 0)
    (un #f 'zero? 1)
    (un #f 'zero? -1)

    (un #f 'positive? 0)
    (un #t 'positive? 1)
    (un #f 'positive? -1)

    (un #f 'negative? 0)
    (un #f 'negative? 1)
    (un #t 'negative? -1)

    (un #t 'real? 1)
    (un #t 'real? (expt 2 100))
    (un #t 'real? 1.0)
    (un #f 'real? 1+2i)
    (un #f 'real? 'apple)

    (un #t 'number? 1)
    (un #t 'number? (expt 2 100))
    (un #t 'number? 1.0)
    (un #t 'number? 1+2i)
    (un #f 'number? 'apple)

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
    (un (expt 2 30) 'add1 (sub1 (expt 2 30)))

    (un 1 'sub1 2)
    (un -5 'sub1 -4)
    (un (- (expt 2 30)) 'sub1 (- 1 (expt 2 30)))

    (bin 11 '+ 4 7)
    (bin -3 '+ 4 -7)
    (bin (expt 2 30) '+ (expt 2 29) (expt 2 29))
    (bin (- (expt 2 31) 2) '+ (sub1 (expt 2 30)) (sub1 (expt 2 30)))

    (bin 3 '- 7 4)
    (bin 11 '- 7 -4)
    (bin 0 '- (expt 2 29) (expt 2 29))
    (bin (expt 2 30) '- (expt 2 29) (- (expt 2 29)))
    (bin (- (expt 2 30)) '- (- (expt 2 29)) (expt 2 29))
    (bin (- 2 (expt 2 31)) '- (- 1 (expt 2 30)) (sub1 (expt 2 30)))

    (bin-exact 11 'bitwise-and 11 43)
    (bin-exact 0 'bitwise-and 11 32)
    (bin-exact 0 'bitwise-and 11 (expt 2 50))
    (bin-exact 0 'bitwise-and 0 -32)
    (bin-exact 11 'bitwise-and 11 -1)
    (bin-exact -11 'bitwise-and -11 -1)
    (bin-exact (expt 2 50) 'bitwise-and (expt 2 50) (expt 2 50))

    (bin-exact 11 'bitwise-ior 8 3)
    (bin-exact 11 'bitwise-ior 11 3)
    (bin-exact -1 'bitwise-ior 11 -1)
    (bin-exact (sub1 (expt 2 51)) 'bitwise-ior (sub1 (expt 2 50)) (expt 2 50))
    (bin-exact (add1 (expt 2 50)) 'bitwise-ior 1 (expt 2 50))

    (bin-exact 11 'bitwise-xor 8 3)
    (bin-exact 8 'bitwise-xor 11 3)
    (bin-exact -2 'bitwise-xor 1 -1)
    (bin-exact (sub1 (expt 2 51)) 'bitwise-xor (sub1 (expt 2 50)) (expt 2 50))
    (bin-exact (add1 (expt 2 50)) 'bitwise-xor 1 (expt 2 50))

    (bin-exact 4 'arithmetic-shift 2 1)
    (bin-exact 1 'arithmetic-shift 2 -1)
    (bin-exact (expt 2 30) 'arithmetic-shift 2 29)
    (bin-exact (expt 2 31) 'arithmetic-shift 2 30)
    (bin-exact (expt 2 32) 'arithmetic-shift 2 31)
    (bin-exact (expt 2 33) 'arithmetic-shift 2 32)
    (bin-exact -2 'arithmetic-shift -1 1)
    (bin-exact -1 'arithmetic-shift -1 -1)
    (bin-exact 2 'arithmetic-shift (expt 2 33) -32)
    (bin-exact 8 'arithmetic-shift (expt 2 33) -30)

    (un-exact -1 'bitwise-not 0)
    (un-exact 0 'bitwise-not -1)
    (un-exact (- -1 (expt 2 30)) 'bitwise-not (expt 2 30))
    (un-exact (- (expt 2 30)) 'bitwise-not (sub1 (expt 2 30)))
    (un-exact (- -1 (expt 2 32)) 'bitwise-not (expt 2 32))

    (bin-exact 'a 'vector-ref #(a b c) 0)
    (bin-exact 'b 'vector-ref #(a b c) 1)
    (bin-exact 'c 'vector-ref #(a b c) 2)

    (bin-exact #\a 'string-ref "abc\u2001" 0)
    (bin-exact #\b 'string-ref "abc\u2001" 1)
    (bin-exact #\c 'string-ref "abc\u2001" 2)
    (bin-exact #\u2001 'string-ref "abc\u2001" 3)

    (bin-exact 65 'bytes-ref #"Abc\xF7" 0)
    (bin-exact 99 'bytes-ref #"Abc\xF7" 2)
    (bin-exact #xF7 'bytes-ref #"Abc\xF7" 3)

    (let ([test-setter
	   (lambda (make-X def-val set-val set-name set ref)
	     (let ([v (make-X 3 def-val)])
	       (check-error-message set-name (eval `(lambda (x) (,set-name ,v -1 ,set-val))))
	       (check-error-message set-name (eval `(lambda (x) (,set-name ,v 3 ,set-val))))
	       (for-each (lambda (i)
			   (tri-exact (void) set-name (lambda () v) i set-val
				      (lambda ()
					(test set-val ref v i)
					(test def-val ref v (modulo (+ i 1) 3))
					(test def-val ref v (modulo (+ i 2) 3))
					(set v i def-val))
				      #t))
			 '(0 1 2))))])
      (test-setter make-vector #f 7 'vector-set! vector-set! vector-ref)
      (test-setter make-bytes 0 7 'bytes-set! bytes-set! bytes-ref)
      (test-setter make-string #\a #\7 'string-set! string-set! string-ref))

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

;; The current optimizer reset depths correctly:
;; (set! maybe-different-depths? #t)

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

(test-comp '(let ([f (lambda (x) x)]) f)
	   (syntax-property (datum->syntax-object #f '(lambda (x) x)) 'inferred-name 'f))

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

(report-errs)
