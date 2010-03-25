
(load-relative "loadtest.ss")

(Section 'optimization)

(require scheme/flonum
         scheme/fixnum
         compiler/zo-parse)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Check JIT inlining of primitives:
(parameterize ([current-namespace (make-base-namespace)]
	       [eval-jit-enabled #t])
  (namespace-require 'scheme/flonum)
  (namespace-require 'scheme/fixnum)
  (let* ([check-error-message (lambda (name proc)
				(unless (memq name '(eq? not null? pair?
							 real? number? boolean? 
							 procedure? symbol?
							 string? bytes?
							 vector? box?
							 eof-object?
                                                         exact-integer?
                                                         exact-nonnegative-integer?
                                                         exact-positive-integer?))
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
	 [bin-int (lambda (v op arg1 arg2)
                    (bin-exact v op arg1 arg2)
                    (let* ([iv (if (number? v)
                                   (exact->inexact v)
                                   v)]
                           [iv0 (if (and (memq op '(* /)) (zero? iv))
                                    0
                                    iv)])
                      (bin0 iv op (exact->inexact arg1) arg2)
                      (bin0 iv0 op arg1 (exact->inexact arg2))
                      (bin0 iv op (exact->inexact arg1) (exact->inexact arg2))))]
	 [bin (lambda (v op arg1 arg2)
		(bin-int v op arg1 arg2)
                (let ([iv (if (number? v)
                              (if (eq? op '*)
                                  (/ v (* 33333 33333))
                                  (if (eq? op '/)
                                      v
                                      (/ v 33333)))
			      v)])
		  (bin0 iv op (/ arg1 33333) (/ arg2 33333)))
                (let ([iv (if (number? v) +nan.0 #f)])
                  (bin0 iv op (exact->inexact arg1) +nan.0)
                  (bin0 iv op +nan.0 (exact->inexact arg2))
                  (unless (eq? op 'eq?)
                    (bin0 iv op +nan.0 +nan.0))))]
	 [tri0 (lambda (v op get-arg1 arg2 arg3 check-effect #:wrap [wrap values])
		 ;; (printf "Trying ~a ~a ~a\n" op (get-arg1) arg2 arg3);
		 (let ([name `(,op ,get-arg1 ,arg2, arg3)]
                       [get-arg2 (lambda () arg2)]
                       [get-arg3 (lambda () arg3)])
		   (test v name ((eval `(lambda (x) ,(wrap `(,op x ,arg2 ,arg3)))) (get-arg1)))
		   (check-effect)
		   (test v name ((eval `(lambda (x) ,(wrap `(,op (,get-arg1) x ,arg3)))) arg2))
		   (check-effect)
		   (test v name ((eval `(lambda (x) ,(wrap `(,op x (,get-arg2) ,arg3)))) (get-arg1)))
		   (check-effect)
		   (test v name ((eval `(lambda (x) ,(wrap `(,op (,get-arg1) (,get-arg2) x)))) arg3))
		   (check-effect)
		   (test v name ((eval `(lambda () ,(wrap `(,op (,get-arg1) (,get-arg2) (,get-arg3)))))))
		   (check-effect)
		   (test v name ((eval `(lambda (x) ,(wrap `(,op (,get-arg1) ,arg2 x)))) arg3))
		   (check-effect)
		   (test v name ((eval `(lambda (x y) ,(wrap `(,op (,get-arg1) x y)))) arg2 arg3))
		   (check-effect)
		   (eval `(define _arg2 ,arg2))
		   (test v name ((eval `(lambda (y) ,(wrap `(,op (,get-arg1) _arg2 y)))) arg3))
                   (check-effect)
		   (test v name ((eval `(lambda (x y z) ,(wrap `(,op x y z)))) (get-arg1) arg2 arg3))
		   (check-effect)))]
         [tri (lambda (v op get-arg1 arg2 arg3 check-effect #:wrap [wrap values])
                (define (e->i n) (if (number? n) (exact->inexact n) n))
                (tri0 v op get-arg1 arg2 arg3 check-effect #:wrap wrap)
                (tri0 (e->i v) op (lambda () (exact->inexact (get-arg1))) (exact->inexact arg2) (exact->inexact arg3) check-effect
                       #:wrap wrap)
                (tri0 (e->i v) op get-arg1 (exact->inexact arg2) arg3 check-effect
                       #:wrap wrap))]
         [tri-if (lambda (v op get-arg1 arg2 arg3 check-effect)
                   (tri v op get-arg1 arg2 arg3 check-effect)
                   (tri (if v 'true 'false) op get-arg1 arg2 arg3 check-effect
                        #:wrap (lambda (e) `(if ,e 'true 'false))))]
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
    (un #t 'box? (box 10))
    (un #f 'box? #f)
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

    (un-exact #t 'exact-integer? 0)
    (un-exact #t 'exact-integer? 10)
    (un-exact #t 'exact-integer? -10)
    (un-exact #t 'exact-integer? (expt 2 100))
    (un-exact #t 'exact-integer? (- (expt 2 100)))
    (un-exact #f 'exact-integer? 10.0)
    (un-exact #f 'exact-integer? 1/2)

    (un-exact #t 'exact-nonnegative-integer? 0)
    (un-exact #t 'exact-nonnegative-integer? 10)
    (un-exact #f 'exact-nonnegative-integer? -10)
    (un-exact #t 'exact-nonnegative-integer? (expt 2 100))
    (un-exact #f 'exact-nonnegative-integer? (- (expt 2 100)))
    (un-exact #f 'exact-nonnegative-integer? 10.0)
    (un-exact #f 'exact-nonnegative-integer? 1/2)

    (un-exact #f 'exact-positive-integer? 0)
    (un-exact #t 'exact-positive-integer? 10)
    (un-exact #f 'exact-positive-integer? -10)
    (un-exact #t 'exact-positive-integer? (expt 2 100))
    (un-exact #f 'exact-positive-integer? (- (expt 2 100)))
    (un-exact #f 'exact-positive-integer? 10.0)
    (un-exact #f 'exact-positive-integer? 1/2)

    (un #t 'not #f)
    (un #f 'not #t)
    (un #f 'not 10)

    (bin #t '< 100 200)
    (bin #f '< 200 100)
    (bin #f '< 100 100)
    (bin #t '< -200 100)
    (bin #f '< 100 -200)
    (bin #t '< 1 (expt 2 30))
    (tri-if #t '< (lambda () 1) 2 3 void)
    (tri-if #f '< (lambda () 1) 3 3 void)
    (tri-if #f '< (lambda () 1) -1 3 void)
    (bin-exact #t 'fx< 100 200)
    (bin-exact #f 'fx< 200 100)
    (bin-exact #f 'fx< 200 200)
    (bin-exact #t 'fl< 100.0 200.0)
    (bin-exact #f 'fl< 200.0 100.0)
    (bin-exact #f 'fl< 200.0 200.0)

    (bin #t '<= 100 200)
    (bin #f '<= 200 100)
    (bin #t '<= 100 100)
    (bin #t '<= -200 100)
    (bin #f '<= 100 -200)
    (tri-if #t '<= (lambda () 1) 2 3 void)
    (tri-if #t '<= (lambda () 1) 3 3 void)
    (tri-if #f '<= (lambda () 1) -1 3 void)
    (bin-exact #t 'fx<= 100 200)
    (bin-exact #f 'fx<= 200 100)
    (bin-exact #t 'fx<= 200 200)
    (bin-exact #t 'fl<= 100.0 200.0)
    (bin-exact #f 'fl<= 200.0 100.0)
    (bin-exact #t 'fl<= 200.0 200.0)

    (bin #f '> 100 200)
    (bin #t '> 200 100)
    (bin #f '> 100 100)
    (bin #f '> -200 100)
    (bin #t '> 100 -200)
    (bin #f '> 1 (expt 2 30))
    (tri-if #t '> (lambda () 3) 2 1 void)
    (tri-if #f '> (lambda () 3) 3 1 void)
    (tri-if #f '> (lambda () 3) -1 1 void)
    (bin-exact #f 'fx> 100 200)
    (bin-exact #t 'fx> 200 100)
    (bin-exact #f 'fx> 200 200)
    (bin-exact #f 'fl> 100.0 200.0)
    (bin-exact #t 'fl> 200.0 100.0)
    (bin-exact #f 'fl> 200.0 200.0)

    (bin #f '>= 100 200)
    (bin #t '>= 200 100)
    (bin #t '>= 100 100)
    (bin #f '>= -200 100)
    (bin #t '>= 100 -200)
    (tri-if #t '>= (lambda () 3) 2 1 void)
    (tri-if #t '>= (lambda () 3) 3 1 void)
    (tri-if #f '>= (lambda () 3) -1 1 void)
    (bin-exact #f 'fx>= 100 200)
    (bin-exact #t 'fx>= 200 100)
    (bin-exact #t 'fx>= 200 200)
    (bin-exact #f 'fl>= 100.0 200.0)
    (bin-exact #t 'fl>= 200.0 100.0)
    (bin-exact #t 'fl>= 200.0 200.0)

    (bin #f '= 100 200)
    (bin #f '= 200 100)
    (bin #t '= 100 100)
    (bin #f '= -200 100)
    (bin #f '= +nan.0 +nan.0)
    (tri-if #t '= (lambda () 3) 3 3 void)
    (tri-if #f '= (lambda () 3) 3 1 void)
    (tri-if #f '= (lambda () 3) 1 3 void)
    (tri-if #f '= (lambda () 1) 3 3 void)
    (bin-exact #f 'fx= 100 200)
    (bin-exact #t 'fx= 200 200)
    (bin-exact #f 'fl= 100.0 200.0)
    (bin-exact #t 'fl= 200.0 200.0)

    (un 3 'add1 2)
    (un -3 'add1 -4)
    (un (expt 2 30) 'add1 (sub1 (expt 2 30)))

    (un 1 'sub1 2)
    (un -5 'sub1 -4)
    (un (- (expt 2 30)) 'sub1 (- 1 (expt 2 30)))

    (un -1 '- 1)
    (un 1 '- -1)
    (un (- (expt 2 30)) '- (expt 2 30))
    (un (expt 2 30) '- (- (expt 2 30)))
    (un -0.0 '- 0.0)
    (un 0.0 '- -0.0)

    (un 0 'abs 0)
    (un 1 'abs 1)
    (un 1 'abs -1)
    (un (sub1 (expt 2 30)) 'abs (sub1 (expt 2 30)))
    (un (expt 2 30) 'abs (- (expt 2 30)))
    (un (sub1 (expt 2 62)) 'abs (sub1 (expt 2 62)))
    (un (expt 2 62) 'abs (- (expt 2 62)))
    (un-exact 3.0 'flabs -3.0)

    (un-exact 3.0 'flsqrt 9.0)
    (un-exact +nan.0 'flsqrt -9.0)

    (let ([test-trig
           (lambda (trig fltrig)
             (un (trig 1.0) fltrig 1.0)
             (un +nan.0 fltrig +nan.0))])
      (test-trig sin 'flsin)
      (test-trig cos 'flcos)
      (test-trig tan 'fltan)
      (test-trig asin 'flasin)
      (test-trig acos 'flacos)
      (test-trig atan 'flatan)
      (test-trig log 'fllog)
      (test-trig exp 'flexp))
    
    (un 1.0 'exact->inexact 1)
    (un 1073741823.0 'exact->inexact (sub1 (expt 2 30)))
    (un -1073741824.0 'exact->inexact (- (expt 2 30)))
    (un 4611686018427387903.0 'exact->inexact (sub1 (expt 2 62)))
    (un -4611686018427387904.0 'exact->inexact (- (expt 2 62)))

    (un-exact 10.0 '->fl 10)
    (un-exact 10.0 'fx->fl 10)

    (bin 11 '+ 4 7)
    (bin -3 '+ 4 -7)
    (bin (expt 2 30) '+ (expt 2 29) (expt 2 29))
    (bin (- (expt 2 31) 2) '+ (sub1 (expt 2 30)) (sub1 (expt 2 30)))
    (tri 6 '+ (lambda () 1) 2 3 void)
    (tri 13/2 '+ (lambda () 1) 5/2 3 void)
    (bin-exact 25 'fx+ 10 15)
    (bin-exact 3.4 'fl+ 1.1 2.3)

    (bin 3 '- 7 4)
    (bin 11 '- 7 -4)
    (bin 0 '- (expt 2 29) (expt 2 29))
    (bin (expt 2 30) '- (expt 2 29) (- (expt 2 29)))
    (bin (- (expt 2 30)) '- (- (expt 2 29)) (expt 2 29))
    (bin (- 2 (expt 2 31)) '- (- 1 (expt 2 30)) (sub1 (expt 2 30)))
    (tri 6 '- (lambda () 10) 3 1 void)
    (tri 13/2 '- (lambda () 10) 3 1/2 void)
    (bin-exact 13 'fx- 5 -8)
    (bin-exact -0.75 'fl- 1.5 2.25)

    (bin 4 '* 1 4)
    (bin 0 '* 0 4)
    (bin 12 '* 3 4)
    (bin -12 '* -3 4)
    (bin -12 '* 3 -4)
    (bin 12 '* -3 -4)
    (bin (expt 2 70) '* 2 (expt 2 69))
    (bin (expt 2 30) '* 2 (expt 2 29))
    (bin (expt 2 31) '* 2 (expt 2 30))
    (bin (- (expt 2 30)) '* 2 (- (expt 2 29)))
    (tri 30 '* (lambda () 2) 3 5 void)
    (tri 5 '* (lambda () 2) 3 5/6 void)
    (bin-exact 253 'fx* 11 23)
    (bin-exact 2.53 'fl* 1.1 2.3)

    (bin 0 '/ 0 4)
    (bin 1/4 '/ 1 4)
    (bin 4 '/ 4 1)
    (bin 4 '/ 16 4)
    (bin -4 '/ -16 4)
    (bin -4 '/ 16 -4)
    (bin 4 '/ -16 -4)
    (tri 3 '/ (lambda () 30) 5 2 void)
    (tri 12 '/ (lambda () 30) 5 1/2 void)
    (bin-exact (/ 1.1 2.3) 'fl/ 1.1 2.3)

    (bin-int 3 'quotient 10 3)
    (bin-int -3 'quotient 10 -3)
    (bin-int 3 'quotient -10 -3)
    (bin-int -3 'quotient -10 3)
    (bin-exact 7 'quotient (* 7 (expt 2 100)) (expt 2 100))
    (bin-exact 3 'fxquotient 10 3)
    (bin-exact -3 'fxquotient 10 -3)
    (bin-exact (expt 2 30) 'quotient (- (expt 2 30)) -1)

    (bin-int 1 'remainder 10 3)
    (bin-int 1 'remainder 10 -3)
    (bin-int -1 'remainder -10 -3)
    (bin-int -1 'remainder -10 3)
    (bin-exact 7 'remainder (+ 7 (expt 2 100)) (expt 2 100))
    (bin-exact 1 'fxremainder 10 3)
    (bin-exact 1 'fxremainder 10 -3)
    (bin-exact -1 'fxremainder -10 3)
    (bin-exact -1 'fxremainder -10 -3)

    (bin-int 1 'modulo 10 3)
    (bin-int -2 'modulo 10 -3)
    (bin-int -1 'modulo -10 -3)
    (bin-int 2 'modulo -10 3)
    (bin-exact 7 'modulo (+ 7 (expt 2 100)) (expt 2 100))
    (bin-exact 1 'fxmodulo 10 3)
    (bin-exact -2 'fxmodulo 10 -3)
    (bin-exact -1 'fxmodulo -10 -3)
    (bin-exact 2 'fxmodulo -10 3)

    (bin 3 'min 3 300)
    (bin -300 'min 3 -300)
    (bin -400 'min -400 -300)
    (tri 5 'min (lambda () 10) 5 20 void)
    (tri 5 'min (lambda () 5) 10 20 void)
    (tri 5 'min (lambda () 20) 10 5 void)
    (bin-exact 3.0 'flmin 3.0 4.5)
    (bin-exact 2.5 'flmin 3.0 2.5)
    (bin0 3.5 '(lambda (x y) (fl+ 1.0 (flmin x y))) 3.0 2.5)
    (bin0 4.0 '(lambda (x y) (fl+ 1.0 (flmin x y))) 3.0 4.5)
    (bin-exact 30 'fxmin 30 45)
    (bin-exact 25 'fxmin 30 25)

    (bin 300 'max 3 300)
    (bin 3 'max 3 -300)
    (bin -3 'max -3 -300)
    (tri 50 'max (lambda () 10) 50 20 void)
    (tri 50 'max (lambda () 50) 10 20 void)
    (tri 50 'max (lambda () 20) 10 50 void)
    (bin-exact 4.5 'flmax 3.0 4.5)
    (bin-exact 3.0 'flmax 3.0 2.5)
    (bin0 5.5 '(lambda (x y) (fl+ 1.0 (flmax x y))) 3.0 4.5)
    (bin0 4.0 '(lambda (x y) (fl+ 1.0 (flmax x y))) 3.0 2.5)
    (bin-exact 45 'fxmax 30 45)
    (bin-exact 30 'fxmax 30 25)

    (bin-exact 11 'bitwise-and 11 43)
    (bin-exact 0 'bitwise-and 11 32)
    (bin-exact 0 'bitwise-and 11 (expt 2 50))
    (bin-exact 0 'bitwise-and 0 -32)
    (bin-exact 11 'bitwise-and 11 -1)
    (bin-exact -11 'bitwise-and -11 -1)
    (bin-exact (expt 2 50) 'bitwise-and (expt 2 50) (expt 2 50))
    (tri-exact #x10101 'bitwise-and (lambda () #x11111) #x10111 #x110101 void #f)
    (bin-exact 11 'fxand 11 43)

    (bin-exact 11 'bitwise-ior 8 3)
    (bin-exact 11 'bitwise-ior 11 3)
    (bin-exact -1 'bitwise-ior 11 -1)
    (bin-exact (sub1 (expt 2 51)) 'bitwise-ior (sub1 (expt 2 50)) (expt 2 50))
    (bin-exact (add1 (expt 2 50)) 'bitwise-ior 1 (expt 2 50))
    (tri-exact #x10101 'bitwise-ior (lambda () #x1) #x100 #x10000 void #f)
    (bin-exact 11 'fxior 8 3)

    (bin-exact 11 'bitwise-xor 8 3)
    (bin-exact 8 'bitwise-xor 11 3)
    (bin-exact -2 'bitwise-xor 1 -1)
    (bin-exact (sub1 (expt 2 51)) 'bitwise-xor (sub1 (expt 2 50)) (expt 2 50))
    (bin-exact (add1 (expt 2 50)) 'bitwise-xor 1 (expt 2 50))
    (tri-exact #x10101 'bitwise-xor (lambda () #x1) #x110 #x10010 void #f)
    (bin-exact 11 'fxxor 8 3)

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
    (bin-exact 4 'fxlshift 2 1)
    (bin-exact 1 'fxrshift 2 1)

    (un-exact -1 'bitwise-not 0)
    (un-exact 0 'bitwise-not -1)
    (un-exact (- -1 (expt 2 30)) 'bitwise-not (expt 2 30))
    (un-exact (- (expt 2 30)) 'bitwise-not (sub1 (expt 2 30)))
    (un-exact (- -1 (expt 2 32)) 'bitwise-not (expt 2 32))
    (un-exact -1 'fxnot 0)
    (un-exact 0 'fxnot -1)

    (bin-exact #t 'bitwise-bit-set? 1 0)
    (bin-exact #f 'bitwise-bit-set? 1 1)
    (bin-exact #t 'bitwise-bit-set? 2 1)
    (bin-exact #t 'bitwise-bit-set? 200 7)
    (bin-exact #f 'bitwise-bit-set? 127 7)
    (bin-exact #f 'bitwise-bit-set? 383 7)
    (bin-exact #f 'bitwise-bit-set? 10 128)
    (bin-exact #t 'bitwise-bit-set? -10 128)
    (bin-exact #t 'bitwise-bit-set? (expt 2 30) 30)
    (bin-exact #t 'bitwise-bit-set? (expt 2 40) 40)
    (bin-exact #f 'bitwise-bit-set? (expt 2 40) 41)
    (bin-exact #t 'bitwise-bit-set? (- (expt 2 40)) 41)

    (bin-exact #t 'char=? #\a #\a)
    (bin-exact #t 'char=? #\u1034 #\u1034)
    (bin-exact #f 'char=? #\a #\b)
    (bin-exact #f 'char=? #\u1034 #\a)

    (bin-exact 'a 'vector-ref #(a b c) 0)
    (bin-exact 'b 'vector-ref #(a b c) 1)
    (bin-exact 'c 'vector-ref #(a b c) 2)

    (un-exact 'a 'unbox (box 'a))
    (un-exact 3 'vector-length (vector 'a 'b 'c))

    (bin-exact 1.1 'flvector-ref (flvector 1.1 2.2 3.3) 0)
    (bin-exact 3.3 'flvector-ref (flvector 1.1 2.2 3.3) 2)
    (un-exact 3 'flvector-length (flvector 1.1 2.2 3.3))

    (bin-exact #\a 'string-ref "abc\u2001" 0)
    (bin-exact #\b 'string-ref "abc\u2001" 1)
    (bin-exact #\c 'string-ref "abc\u2001" 2)
    (bin-exact #\u2001 'string-ref "abc\u2001" 3)

    (bin-exact 65 'bytes-ref #"Abc\xF7" 0)
    (bin-exact 99 'bytes-ref #"Abc\xF7" 2)
    (bin-exact #xF7 'bytes-ref #"Abc\xF7" 3)

    (un0 #(1) 'vector 1)
    (un0 #(1) 'vector-immutable 1)
    (bin0 #(1 2) 'vector 1 2)
    (bin0 #(1 2) 'vector-immutable 1 2)
    (tri0 #(1 2 3) 'vector (lambda () 1) 2 3 void)
    (tri0 #(1 2 3) 'vector-immutable (lambda () 1) 2 3 void)
    (un0 '(1) 'list 1)
    (bin0 '(1 2) 'list 1 2)
    (tri0 '(1 2 3) 'list (lambda () 1) 2 3 void)
    (un0 '1 'list* 1)
    (bin0 '(1 . 2) 'list* 1 2)
    (tri0 '(1 2 . 3) 'list* (lambda () 1) 2 3 void)
    (un0 '#&1 'box 1)

    (let ([test-setter
	   (lambda (make-X def-val set-val set-name set ref 3rd-all-ok?)
	     (let ([v (make-X 3 def-val)])
	       (check-error-message set-name (eval `(lambda (x) (,set-name ,v -1 ,set-val))))
	       (check-error-message set-name (eval `(lambda (x) (,set-name ,v 3 ,set-val))))
               (unless (integer? set-val)
                 (check-error-message set-name (eval `(lambda (x) (,set-name ,v 0 12)))))
	       (for-each (lambda (i)
			   (tri-exact (void) set-name (lambda () v) i set-val
				      (lambda ()
					(test set-val ref v i)
					(test def-val ref v (modulo (+ i 1) 3))
					(test def-val ref v (modulo (+ i 2) 3))
					(set v i def-val))
				      3rd-all-ok?))
			 '(0 1 2))))])
      (test-setter make-vector #f 7 'vector-set! vector-set! vector-ref #t)
      (test-setter make-bytes 0 7 'bytes-set! bytes-set! bytes-ref #f)
      (test-setter make-string #\a #\7 'string-set! string-set! string-ref #f)
      (test-setter make-flvector 1.0 7.0 'flvector-set! flvector-set! flvector-ref #f))

    ))

(define (comp=? c1 c2)
  (let ([s1 (open-output-bytes)]
	[s2 (open-output-bytes)])
    (write c1 s1)
    (write c2 s2)
    (let ([t1 (get-output-bytes s1)]
	  [t2 (get-output-bytes s2)])
      (or (bytes=? t1 t2)
	  (begin
	    (printf "~s\n~s\n" 
                    (zo-parse (open-input-bytes t1))
                    (zo-parse (open-input-bytes t2)))
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

; (require #%kernel) ; 

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
	   (syntax-property (datum->syntax #f '(lambda (x) x)) 'inferred-name 'f))

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

(test-comp '(module m mzscheme
              (define (f x) (+ x 1))
              (f 8))
	   '(module m mzscheme
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

(test-comp '(lambda (x) (if x x #f))
           '(lambda (x) x))
(test-comp '(lambda (x) (if (cons 1 x) 78 78))
           '(lambda (x) 78))

(test-comp '(lambda (x) (if (let ([r (something)])
                              (if r r (something-else)))
                            (a1)
                            (a2)))
           '(lambda (x)  (if (if (something) #t (something-else))
                             (a1)
                             (a2))))

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

(test-comp '(let ([x (+ (cons 1 2) 0)])
              (- x 8))
           '(- (+ (cons 1 2) 0) 8))

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

(test-comp '(procedure? add1)
           #t)
(test-comp '(procedure? (lambda (x) x))
           #t)
(test-comp '(let ([f (lambda (x) x)])
              (if (procedure? f)
                  (list f)
                  88))
           '(let ([f (lambda (x) x)])
              (list f)))

(test-comp '(procedure-arity-includes? integer? 1)
           #t)

(test-comp '(module m mzscheme
              (define foo integer?)
              (display (procedure-arity-includes? foo 1)))
           '(module m mzscheme
              (define foo integer?)
              (display #t)))

(test-comp '(module m mzscheme
              (void 10))
           '(module m mzscheme))

(test-comp '(module m mzscheme
              (void (quote-syntax unused!)))
           '(module m mzscheme))

(test-comp '(module m mzscheme
              (values 1 2))
           '(module m mzscheme))

(test-comp '(module m mzscheme
              (printf "pre\n")
              (void 10))
           '(module m mzscheme
              (printf "pre\n")))

(test-comp '(module m mzscheme
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
           '(module m mzscheme
              (define (q x)
                (+ 1 (+ 1 (+ 1 (+ 1 (+ 1 (+ 1 (+ 1 (+ 1 (+ 1 (+ 1 (+ x 10))))))))))))))

(let ([check (lambda (proc arities non-arities)
               (test-comp `(module m scheme/base
                             (define f ,proc)
                             (print (procedure? f)))
                          `(module m scheme/base
                             (define f ,proc)
                             (print #t)))
               (for-each
                (lambda (a)
                  (test-comp `(module m scheme/base
                                (define f ,proc)
                                (print (procedure-arity-includes? f ,a)))
                             `(module m scheme/base
                                (define f ,proc)
                                (print #t))))
                arities)
               (for-each
                (lambda (a)
                  (test-comp `(module m scheme/base
                                (define f ,proc)
                                (print (procedure-arity-includes? f ,a)))
                             `(module m scheme/base
                                (define f ,proc)
                                (print #f))))
                non-arities))])
  (check '(lambda (x) x) '(1) '(0 2))
  (check '(lambda (x . y) x) '(1 2 3) '(0))
  (check '(case-lambda [() 1] [(x y) x]) '(0 2) '(1 3))
  (check '(lambda (x [y #f]) y) '(1 2) '(0 3)))

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
           (test-dropped cons-name))])
    (test-multi 'list)
    (test-multi 'list*)
    (test-multi 'vector)
    (test-multi 'vector-immutable)))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Check bytecode verification of lifted functions

(let ([check
       (lambda (expr)
         (let-values ([(r w) (make-pipe)])
           (write (compile expr) w)
           (parameterize ([read-accept-compiled #t])
             (read r))))])
  (check '(module m mzscheme
            (provide f)
            (define (f x)
              (let loop ([n 0])
                (set! x (+ n 1)) ; close over mutated variable
                (loop n #f)
                (loop n)))))
  (check '(module m mzscheme
            (provide f)
            (define s (make-string 10))
            (define (f x)
              (let loop ([n 0])
                (set! x (+ n 1)) ; close over mutated variable
                (loop n s) ; and refer to global
                (loop n))))))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Make sure "mutated?" flag isn't confused with "ready" flag:
(module bad-order mzscheme  
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
(module cwv-1 mzscheme
  (define (cwv-1-f x)
    (call-with-values (lambda () (+ x 3))
      (lambda (y) (+ y 2))))
  (provide cwv-1-f))
(require 'cwv-1)
(test 15 cwv-1-f 10)

;; known function doesn't expect 1 argument
(module cwv-2 mzscheme
  (define (cwv-2-f x)
    (call-with-values (lambda () (+ x 3))
      (lambda (y z) (+ y 2))))
  (provide cwv-2-f))
(require 'cwv-2)
(err/rt-test (cwv-2-f 10) exn:fail:contract:arity?)

;; known function, unknown number of results:
(module cwv-3 mzscheme
  (define (cwv-3-f g)
    (call-with-values (lambda () (g))
      (lambda (y) (+ y 2))))
  (provide cwv-3-f))
(require 'cwv-3)
(test 12 cwv-3-f (lambda () 10))
(err/rt-test (cwv-3-f (lambda () (values 1 2))) exn:fail:contract:arity?)

;; ditto, need 2 results:
(module cwv-4 mzscheme
  (define (cwv-4-f g)
    (call-with-values (lambda () (g))
      (lambda (y z) (+ y z 2))))
  (provide cwv-4-f))
(require 'cwv-4)
(test 12 cwv-4-f (lambda () (values 4 6)))
(err/rt-test (cwv-4-f (lambda () 10)) exn:fail:contract:arity?)
(err/rt-test (cwv-4-f (lambda () (values 1 2 10))) exn:fail:contract:arity?)

;; unknown first function:
(module cwv-5 mzscheme
  (define (cwv-5-f g)
    (call-with-values g
      (lambda (y) (+ y 2))))
  (provide cwv-5-f))
(require 'cwv-5)
(test 12 cwv-5-f (lambda () 10))
(err/rt-test (cwv-5-f (lambda () (values 1 2))) exn:fail:contract:arity?)

;; ditto, need 2 results:
(module cwv-6 mzscheme
  (define (cwv-6-f g)
    (call-with-values g
      (lambda (y z) (+ y z 2))))
  (provide cwv-6-f))
(require 'cwv-6)
(test 12 cwv-6-f (lambda () (values 4 6)))
(err/rt-test (cwv-6-f (lambda () 10)) exn:fail:contract:arity?)
(err/rt-test (cwv-6-f (lambda () (values 1 2 10))) exn:fail:contract:arity?)

;; unknown second function:
(module cwv-2-1 mzscheme
  (define (cwv-2-1-f x h)
    (call-with-values (lambda () (+ x 3))
      h))
  (provide cwv-2-1-f))
(require 'cwv-2-1)
(test 15 cwv-2-1-f 10 (lambda (y) (+ y 2)))

;; unknown function doesn't expect 1 argument
(module cwv-2-2 mzscheme
  (define (cwv-2-2-f x h)
    (call-with-values (lambda () (+ x 3))
      h))
  (provide cwv-2-2-f))
(require 'cwv-2-2)
(err/rt-test (cwv-2-2-f 10 (lambda (y z) (+ y 2))) exn:fail:contract:arity?)

;; known function, unknown number of results:
(module cwv-2-3 mzscheme
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
(module cwv-2-5 mzscheme
  (define (cwv-2-5-f g h)
    (call-with-values g h))
  (provide cwv-2-5-f))
(require 'cwv-2-5)
(test 12 cwv-2-5-f (lambda () 10) (lambda (y) (+ y 2)))
(err/rt-test (cwv-2-5-f (lambda () (values 1 2)) (lambda (y) (+ y 2))) exn:fail:contract:arity?)
(err/rt-test (cwv-2-5-f (lambda () 1) (lambda (y z) (+ y 2))) exn:fail:contract:arity?)
(err/rt-test (cwv-2-5-f (lambda () (values 1 2 3)) (lambda (y z) (+ y 2))) exn:fail:contract:arity?)

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

(report-errs)
