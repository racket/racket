
(load-relative "loadtest.rktl")

(Section 'optimization)

(require racket/flonum
         racket/extflonum
         racket/fixnum
         racket/unsafe/ops
         compiler/zo-parse
         compiler/zo-marshal)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Check JIT inlining of primitives:
(parameterize ([current-namespace (make-base-namespace)]
	       [eval-jit-enabled #t])
  (namespace-require 'racket/flonum)
  (namespace-require 'racket/extflonum)
  (namespace-require 'racket/fixnum)
  (eval '(define-values (prop:thing thing? thing-ref) 
           (make-struct-type-property 'thing)))
  (eval '(struct rock (x) #:property prop:thing 'yes))
  (let* ([struct:rock (eval 'struct:rock)]
         [a-rock (eval '(rock 0))]
         [chap-rock (eval '(chaperone-struct (rock 0) rock-x (lambda (r v) (add1 v))))]
         [check-error-message (lambda (name proc [fixnum? #f]
                                            #:bad-value [bad-value (if fixnum? 10 'bad)]
                                            #:first-arg [first-arg #f]
                                            #:second-arg [second-arg #f])
				(unless (memq name '(eq? eqv? equal? 
                                                         not null? pair? list?
							 real? number? boolean?
							 procedure? symbol?
							 string? bytes?
							 vector? box?
							 eof-object?
                                                         exact-integer?
                                                         exact-nonnegative-integer?
                                                         exact-positive-integer?
                                                         thing?
                                                         continuation-mark-set-first))
				  (let ([s (with-handlers ([exn? exn-message])
                                             (let ([bad bad-value])
                                               (cond
                                                [first-arg (proc first-arg bad)]
                                                [second-arg (proc bad second-arg)]
                                                [else (proc bad)])))]
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
	 [un-exact (lambda (v op arg [check-fixnum-as-bad? #f])
		     (check-error-message op (eval `(lambda (x) (,op x))))
                     (when check-fixnum-as-bad?
                       (check-error-message op (eval `(lambda (x) (,op x))) #t))
		     (un0 v op arg))]
         
	 [un (lambda (v op arg [check-fixnum-as-bad? #f])
	       (un-exact v op arg check-fixnum-as-bad?)
	       (when (number? arg)
		 (let ([iv (if (number? v)
			       (exact->inexact v)
			       v)])
		   (un0 iv op (exact->inexact arg)))))]
	 [bin0 (lambda (v op arg1 arg2)
		 ;; (printf "Trying ~a ~a ~a\n" op arg1 arg2);
		 (let ([name `(,op ,arg1 ,arg2)])
                   (test v name ((eval `(lambda (x) (,op x ',arg2))) arg1))
		   (test v name ((eval `(lambda (x) (,op ',arg1 x))) arg2))
		   (test v name ((eval `(lambda (x y) (,op x y))) arg1 arg2))
		   (test v name ((eval `(lambda (x y) 
                                          (let ([z 'not-a-legitimate-argument])
                                            (,op (begin (set! z y) x) z))))
                                 arg1 arg2))
		   (when (boolean? v)
		     ;; (printf " for branch...\n")
		     (test (if v 'yes 'no) name ((eval `(lambda (x) (if (,op x ',arg2) 'yes 'no))) arg1))
		     (test (if v 'yes 'no) name ((eval `(lambda (x) (if (,op ',arg1 x) 'yes 'no))) arg2)))
                   (when (fixnum? arg2)
                     (test v name ((eval `(lambda (x) (let ([x2 (fx+ (random 1) ',arg2)])
                                                        (,op x x2))))
                                   arg1)))
                   (when (fixnum? arg1)
                     (test v name ((eval `(lambda (y) (let ([x1 (fx+ (random 1) ',arg1)])
                                                        (,op x1 y))))
                                   arg2)))))]
	 [bin-exact (lambda (v op arg1 arg2 [check-fixnum-as-bad? #f])
		      (check-error-message op (eval `(lambda (x) (,op x ',arg2))))
		      (check-error-message op (eval `(lambda (x) (,op ',arg1 x))))
		      (check-error-message op (eval `(lambda (x y) (,op x y))) #:first-arg arg1)
		      (check-error-message op (eval `(lambda (x y) (,op x y))) #:second-arg arg2)
                      (when check-fixnum-as-bad?
                        (check-error-message op (eval `(lambda (x) (,op x ',arg2))) #t)
                        (check-error-message op (eval `(lambda (x) (,op x 10))) #t)
                        (unless (fixnum? arg2)
                          (check-error-message op (eval `(lambda (x) (,op ',arg1 x))) #t)))
		      (bin0 v op arg1 arg2))]
	 [bin-int (lambda (v op arg1 arg2 [check-fixnum-as-bad? #f])
                    (bin-exact v op arg1 arg2 check-fixnum-as-bad?)
                    (let* ([iv (if (number? v)
                                   (exact->inexact v)
                                   v)]
                           [iv0 (if (and (memq op '(* /)) (zero? iv))
                                    0
                                    iv)])
                      (bin0 iv op (exact->inexact arg1) arg2)
                      (bin0 iv0 op arg1 (exact->inexact arg2))
                      (bin0 iv op (exact->inexact arg1) (exact->inexact arg2))))]
	 [bin (lambda (v op arg1 arg2 [check-fixnum-as-bad? #f])
		(bin-int v op arg1 arg2 check-fixnum-as-bad?)
                (let ([iv (if (number? v)
                              (if (eq? op '*)
                                  (/ v (* 33333 33333))
                                  (if (eq? op '/)
                                      v
                                      (/ v 33333)))
			      v)])
		  (bin0 iv op (/ arg1 33333) (/ arg2 33333)))
                (unless (eq? op 'make-rectangular)
                  (let ([iv (if (number? v) +nan.0 #f)])
                    (bin0 iv op (exact->inexact arg1) +nan.0)
                    (bin0 iv op +nan.0 (exact->inexact arg2))
                    (unless (eq? op 'eq?)
                      (bin0 iv op +nan.0 +nan.0)))))]
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
		   (check-effect)
		   (when (boolean? v)
		     ;; (printf " for branch...\n")
		     (test (if v 'yes 'no) name ((eval `(lambda (x y z) (if ,(wrap `(,op x y z)) 'yes 'no))) (get-arg1) arg2 arg3))
                     (check-effect))))]
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
    (un-exact #t 'null? '())
    (un #f 'pair? 0)
    (un-exact #t 'pair? '(1 2))
    (un #f 'list? 0)
    (un #f 'list? '(1 2 . 3))
    (un-exact #t 'list? '(1 2 3))
    (un-exact 3 'length '(1 2 3))
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
    (un #f 'thing? 10)
    (un #t 'thing? a-rock)
    (un #t 'thing? chap-rock)
    (un #t 'thing? struct:rock)

    (bin #f 'eq? 0 10)
    (bin-exact #t 'eq? 10 10)

    (bin-exact #f 'eqv? 0 10)
    (bin-exact #f 'eqv? "apple" "banana")
    (bin-exact #t 'eqv? 10 10)
    (bin-exact #t 'eqv? #\a #\a)
    (bin-exact #f 'eqv? #\a #\b)
    (bin-exact #t 'eqv? #\u3bb #\u3bb)
    (bin-exact #f 'eqv? #\u3bb #\u3bc)
    (bin-exact #t 'eqv? 1.0 1.0)
    (bin-exact #f 'eqv? 1.0 2.0)
    (bin-exact #t 'eqv? +nan.0 +nan.0)
    (bin-exact #t 'eqv? 1/2 1/2)
    (bin-exact #f 'eqv? 1/2 1/3)
    (bin-exact #t 'eqv? 1+2i 1+2i)
    (bin-exact #f 'eqv? 1+2i 1+3i)

    (bin-exact #f 'equal? 0 10)
    (bin-exact #t 'equal? "apple" "apple")

    (un #t 'zero? 0)
    (un #f 'zero? 1)
    (un #f 'zero? -1)

    (un #f 'positive? 0)
    (un #t 'positive? 1)
    (un #f 'positive? -1)

    (un #f 'negative? 0)
    (un #f 'negative? 1)
    (un #t 'negative? -1)

    (un #t 'even? 10)
    (un #f 'even? 11)
    (un #t 'even? -10)

    (un #f 'odd? 10)
    (un #t 'odd? 11)
    (un #f 'odd? -10)

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
    (bin-exact #t 'fl< 100.0 200.0 #t)
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
    (bin-exact #t 'fl<= 100.0 200.0 #t)
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
    (bin-exact #f 'fl> 100.0 200.0 #t)
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
    (bin-exact #f 'fl>= 100.0 200.0 #t)
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
    (bin-exact #f 'fl= 100.0 200.0 #t)
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
    (un-exact 3.0 'flabs -3.0 #t)

    (un-exact 3.0 'flsqrt 9.0 #t)
    (un-exact +nan.0 'flsqrt -9.0)

    (let ([test-trig
           (lambda (trig fltrig)
             (un (trig 1.0) fltrig 1.0 #t)
             (un +nan.0 fltrig +nan.0))])
      (test-trig sin 'flsin)
      (test-trig cos 'flcos)
      (test-trig tan 'fltan)
      (test-trig asin 'flasin)
      (test-trig acos 'flacos)
      (test-trig atan 'flatan)
      (test-trig log 'fllog)
      (test-trig exp 'flexp))

    (for-each
     (lambda (v)
       (define (once v)
         (un-exact (round v) 'flround v #t)
         (un-exact (ceiling v) 'flceiling v #t)
         (un-exact (floor v) 'flfloor v #t)
         (un-exact (truncate v) 'fltruncate v #t))
       (once v)
       (once (- v)))
     '(3.0 3.1 3.5 3.8 4.0 4.1 4.5 4.8 0.0))

    (bin-exact 9.0 'flexpt 3.0 2.0 #t)
    (bin-exact (expt 3.1 2.5) 'flexpt 3.1 2.5 #t)
    (bin-exact -1.0 'flexpt -1.0 3.0 #t)
    (bin-exact -0.125 'flexpt -2.0 -3.0 #t)
    (bin-exact +nan.0 'flexpt -1.0 3.1 #t)
    (bin-exact 0.0 'flexpt 0.0 10.0 #t)
    (bin-exact +inf.0 'flexpt 0.0 -1.0 #t)
    (bin-exact +1.0 'flexpt 0.0 0.0 #t)
    (bin-exact +nan.0 'flexpt +nan.0 2.7 #t)
    (bin-exact +nan.0 'flexpt 2.7 +nan.0 #t)
    (bin-exact +nan.0 'flexpt +nan.0 +nan.0 #t)
    
    (un 1.0 'exact->inexact 1)
    (un 1073741823.0 'exact->inexact (sub1 (expt 2 30)))
    (un -1073741824.0 'exact->inexact (- (expt 2 30)))
    (un 4611686018427387903.0 'exact->inexact (sub1 (expt 2 62)))
    (un -4611686018427387904.0 'exact->inexact (- (expt 2 62)))

    (un-exact 10.0 '->fl 10)
    (un-exact 10.0 'fx->fl 10)

    (un-exact 11 'fl->exact-integer 11.0 #t)
    (un-exact -1 'fl->exact-integer -1.0)
    (un-exact (inexact->exact 5e200) 'fl->exact-integer 5e200)
    (un-exact 11 'fl->fx 11.0 #t)
    (un-exact -11 'fl->fx -11.0)

    (bin 11 '+ 4 7)
    (bin -3 '+ 4 -7)
    (bin (expt 2 30) '+ (expt 2 29) (expt 2 29))
    (bin (- (expt 2 31) 2) '+ (sub1 (expt 2 30)) (sub1 (expt 2 30)))
    (tri 6 '+ (lambda () 1) 2 3 void)
    (tri 13/2 '+ (lambda () 1) 5/2 3 void)
    (bin-exact 25 'fx+ 10 15)
    (bin-exact 3.4 'fl+ 1.1 2.3 #t)

    (bin 3 '- 7 4)
    (bin 11 '- 7 -4)
    (bin 0 '- (expt 2 29) (expt 2 29))
    (bin (expt 2 30) '- (expt 2 29) (- (expt 2 29)))
    (bin (- (expt 2 30)) '- (- (expt 2 29)) (expt 2 29))
    (bin (- 2 (expt 2 31)) '- (- 1 (expt 2 30)) (sub1 (expt 2 30)))
    (tri 6 '- (lambda () 10) 3 1 void)
    (tri 13/2 '- (lambda () 10) 3 1/2 void)
    (bin-exact 13 'fx- 5 -8)
    (bin-exact -0.75 'fl- 1.5 2.25 #t)

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
    (bin-exact 2.53 'fl* 1.1 2.3 #t)

    (bin 0 '/ 0 4)
    (bin 1/4 '/ 1 4)
    (bin 4 '/ 4 1)
    (bin 4 '/ 16 4)
    (bin -4 '/ -16 4)
    (bin -4 '/ 16 -4)
    (bin 4 '/ -16 -4)
    (tri 3 '/ (lambda () 30) 5 2 void)
    (tri 12 '/ (lambda () 30) 5 1/2 void)
    (bin-exact (/ 1.1 2.3) 'fl/ 1.1 2.3 #t)
    (bin 4/3 '/ 4 3)
    (bin -4/3 '/ 4 -3)
    (bin -4/3 '/ -4 3)
    (bin 4/3 '/ -4 -3)
    (bin (expt 2 30) '/ (- (expt 2 30)) -1)
    (bin (expt 2 62) '/ (- (expt 2 62)) -1)

    (bin-int 3 'quotient 10 3)
    (bin-int -3 'quotient 10 -3)
    (bin-int 3 'quotient -10 -3)
    (bin-int -3 'quotient -10 3)
    (bin-exact 7 'quotient (* 7 (expt 2 100)) (expt 2 100))
    (bin-exact 3 'fxquotient 10 3)
    (bin-exact -3 'fxquotient 10 -3)
    (bin-exact (expt 2 30) 'quotient (- (expt 2 30)) -1)
    (bin-exact (expt 2 62) 'quotient (- (expt 2 62)) -1)

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
    (bin-exact 3.0 'flmin 3.0 4.5 #t)
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
    (bin-exact 4.5 'flmax 3.0 4.5 #t)
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

    (un 1 'real-part 1+2i)
    (un 105 'real-part 105)
    (un-exact 10.0 'flreal-part 10.0+7.0i #t)
    (check-error-message 'flreal-part (eval `(lambda (x) (flreal-part x))) #:bad-value 1+2i)
    (un 2 'imag-part 1+2i)
    (un-exact 0 'imag-part 106)
    (un-exact 0 'imag-part 106.0)
    (un-exact 7.0 'flimag-part 10.0+7.0i #t)
    (check-error-message 'flimag-part (eval `(lambda (x) (flimag-part x))) #:bad-value 1+2i)

    (bin 1+2i 'make-rectangular 1 2)
    (bin-exact 1.0+2.0i 'make-rectangular 1 2.0)
    (bin-exact 1.0+2.0i 'make-rectangular 1.0 2)
    (bin-exact 1.0+0.5i 'make-rectangular 1.0 1/2)
    (bin-exact 0.75+2.0i 'make-rectangular 3/4 2.0)
    (bin-exact 1 'make-rectangular 1 0)
    (bin-exact 1.0 'make-rectangular 1.0 0)

    (bin-exact #t 'char=? #\a #\a)
    (bin-exact #t 'char=? #\u1034 #\u1034)
    (bin-exact #f 'char=? #\a #\b)
    (bin-exact #f 'char=? #\u1034 #\a)

    (un-exact #\space 'integer->char 32)
    (un-exact #\nul 'integer->char 0)
    (un-exact #\uFF 'integer->char 255)
    (un-exact #\u100 'integer->char 256)
    (un-exact #\U10000 'integer->char #x10000)

    (un-exact 32 'char->integer #\space)
    (un-exact 0 'char->integer #\nul)
    (un-exact 255 'char->integer #\uFF)
    (un-exact #x10000 'char->integer #\U10000)

    (bin-exact 'a 'vector-ref #(a b c) 0 #t)
    (bin-exact 'b 'vector-ref #(a b c) 1)
    (bin-exact 'c 'vector-ref #(a b c) 2)

    (un-exact 'a 'unbox (box 'a) #t)
    (un-exact 3 'vector-length (vector 'a 'b 'c) #t)

    (bin-exact 1.1 'flvector-ref (flvector 1.1 2.2 3.3) 0 #t)
    (bin-exact 3.3 'flvector-ref (flvector 1.1 2.2 3.3) 2)
    (un-exact 3 'flvector-length (flvector 1.1 2.2 3.3) #t)

    (bin-exact 11 'fxvector-ref (fxvector 11 21 31) 0 #t)
    (bin-exact 31 'fxvector-ref (fxvector 11 21 31) 2)
    (un-exact 3 'fxvector-length (fxvector 11 21 31) #t)

    (bin-exact #\a 'string-ref "abc\u2001" 0 #t)
    (bin-exact #\b 'string-ref "abc\u2001" 1)
    (bin-exact #\c 'string-ref "abc\u2001" 2)
    (bin-exact #\u2001 'string-ref "abc\u2001" 3)

    (bin-exact 65 'bytes-ref #"Abc\xF7" 0 #t)
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

    (when (extflonum-available?)
      (define (extflonum-close? fl1 fl2)
        (extfl<= (extflabs (fl- fl1 fl2))
                 (real->extfl 1e-8)))

      (bin-exact 3.4t0 'extfl+ 1.1t0 2.3t0 #t)
      (bin-exact -0.75t0 'extfl- 1.5t0 2.25t0 #t)
      (bin-exact 2.53t0 'extfl* 1.1t0 2.3t0 #t)
      (bin-exact (extfl/ 1.1t0 2.3t0) 'extfl/ 1.1t0 2.3t0 #t)
      (bin-exact 3.0t0 'extflmin 3.0t0 4.5t0 #t)
      (bin-exact 2.5t0 'extflmin 3.0t0 2.5t0)
      (bin-exact 4.5t0 'extflmax 3.0t0 4.5t0 #t)
      (bin-exact 3.0t0 'extflmax 3.0t0 2.5t0)

      (bin-exact #t 'extfl< 100.0t0 200.0t0 #t)
      (bin-exact #f 'extfl< 200.0t0 100.0t0)
      (bin-exact #f 'extfl< 200.0t0 200.0t0)
      
      (bin-exact #t 'extfl<= 100.0t0 200.0t0 #t)
      (bin-exact #f 'extfl<= 200.0t0 100.0t0)
      (bin-exact #t 'extfl<= 200.0t0 200.0t0)
      
      (bin-exact #f 'extfl> 100.0t0 200.0t0 #t)
      (bin-exact #t 'extfl> 200.0t0 100.0t0)
      (bin-exact #f 'extfl> 200.0t0 200.0t0)
      
      (bin-exact #f 'extfl>= 100.0t0 200.0t0 #t)
      (bin-exact #t 'extfl>= 200.0t0 100.0t0)
      (bin-exact #t 'extfl>= 200.0t0 200.0t0)

      (bin-exact #f 'extfl= 100.0t0 200.0t0 #t)
      (bin-exact #t 'extfl= 200.0t0 200.0t0)

      (un-exact 3.0t0 'extflabs -3.0t0 #t)
      (un-exact 3.0t0 'extflsqrt 9.0t0 #t)
      (un-exact +nan.t 'extflsqrt -9.0t0)
      
      (let ([test-trig
             (lambda (trig extfltrig)
               ;;(un (real->extfl (trig 1.0)) extfltrig 1.0t0 #t)
               (un +nan.t extfltrig +nan.t))])
        (test-trig sin 'extflsin)
        (test-trig cos 'extflcos)
        (test-trig tan 'extfltan)
        (test-trig asin 'extflasin)
        (test-trig acos 'extflacos)
        (test-trig atan 'extflatan)
        (test-trig log 'extfllog)
        (test-trig exp 'extflexp))

      (when (extflonum-available?)
        (for-each
         (lambda (v)
           (define (once v)
             (define (->fl v) (extfl->inexact v))
             (define (->extfl v) (real->extfl v))
             (un-exact (->extfl (round (->fl v))) 'extflround v #t)
             (un-exact (->extfl (ceiling (->fl v))) 'extflceiling v #t)
             (un-exact (->extfl (floor (->fl v))) 'extflfloor v #t)
             (un-exact (->extfl (truncate (->fl v))) 'extfltruncate v #t))
          (once v)
          (once (extfl- 0.0t0 v)))
         '(3.0t0 3.1t0 3.5t0 3.8t0 4.0t0 4.1t0 4.5t0 4.8t0 0.0t0)))

      (bin-exact 9.0t0 'extflexpt 3.0t0 2.0t0 #t)
      (bin-exact (extflexpt 3.1t0 2.5t0) 'extflexpt 3.1t0 2.5t0 #t)
      (bin-exact -1.0t0 'extflexpt -1.0t0 3.0t0 #t)
      (bin-exact -0.125t0 'extflexpt -2.0t0 -3.0t0 #t)
      (bin-exact +nan.t 'extflexpt -1.0t0 3.1t0 #t)
      (bin-exact 0.0t0 'extflexpt 0.0t0 10.0t0 #t)
      (bin-exact +inf.t 'extflexpt 0.0t0 -1.0t0 #t)
      (bin-exact +1.0t0 'extflexpt 0.0t0 0.0t0 #t)
      (bin-exact +nan.t 'extflexpt +nan.t 2.7t0 #t)
      (bin-exact +nan.t 'extflexpt 2.7t0 +nan.t #t)
      (bin-exact +nan.t 'extflexpt +nan.t +nan.t #t)

      (un-exact 10.0t0 '->extfl 10)
      (un-exact 10.0t0 'fx->extfl 10)

      (un-exact 11 'extfl->exact-integer 11.0t0 #t)
      (un-exact -1 'extfl->exact-integer -1.0t0)
      (un-exact (inexact->exact 5e200) 'extfl->exact-integer (real->extfl 5e200))
      (un-exact 11 'extfl->fx 11.0t0 #t)
      (un-exact -11 'extfl->fx -11.0t0)

      (bin-exact -0.75t0 'extfl- 1.5t0 2.25t0 #t)

      (bin-exact 3.0t0 'extflmin 3.0t0 4.5t0 #t)
      (bin-exact 2.5t0 'extflmin 3.0t0 2.5t0)
      (bin0 3.5t0 '(lambda (x y) (extfl+ 1.0t0 (extflmin x y))) 3.0t0 2.5t0)
      (bin0 4.0t0 '(lambda (x y) (extfl+ 1.0t0 (extflmin x y))) 3.0t0 4.5t0)

      (bin-exact 4.5t0 'extflmax 3.0t0 4.5t0 #t)
      (bin-exact 3.0t0 'extflmax 3.0t0 2.5t0)
      (bin0 5.5t0 '(lambda (x y) (extfl+ 1.0t0 (extflmax x y))) 3.0t0 4.5t0)
      (bin0 4.0t0 '(lambda (x y) (extfl+ 1.0t0 (extflmax x y))) 3.0t0 2.5t0)

      (bin-exact 1.1t0 'extflvector-ref (extflvector 1.1t0 2.2t0 3.3t0) 0 #t)
      (bin-exact 3.3t0 'extflvector-ref (extflvector 1.1t0 2.2t0 3.3t0) 2)
      (un-exact 3 'extflvector-length (extflvector 1.1t0 2.2t0 3.3t0) #t)
      )
    
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
      (test-setter make-flvector 1.0 7.0 'flvector-set! flvector-set! flvector-ref #f)
      (test-setter make-fxvector 1 7 'fxvector-set! fxvector-set! fxvector-ref #f)
      
      (let ([chap-vec (lambda (vec)
                        (chaperone-vector vec (lambda (vec i val) val) (lambda (vec i val) val)))])
        (test-setter (lambda (n v) (chap-vec (make-vector n v)))
                     #f 7 'vector-set! vector-set! vector-ref #t)
        (test-setter (lambda (n v) (chap-vec (chap-vec (make-vector n v))))
                     #f 7 'vector-set! vector-set! vector-ref #t)))

    (err/rt-test (apply (list-ref (list (lambda (v) (vector-set! v 0 #t))) (random 1)) 
                        (list (vector-immutable 1 2 3))))
    (err/rt-test (apply (list-ref (list (lambda (s) (string-set! s 0 #\a))) (random 1)) 
                        (list "123")))
    (err/rt-test (apply (list-ref (list (lambda (s) (bytes-set! s 0 0))) (random 1)) 
                        (list #"123")))
    (err/rt-test (apply (list-ref (list (lambda (b) (set-box! b #t))) (random 1)) 
                        (list (box-immutable 1))))
    
    (let ([v (box 1)])
      (check-error-message 'set-box! (eval `(lambda (x) (set-box! x 10))))
      (tri0 (void) '(lambda (b i v) (set-box! b v))
            (lambda () v) 0 "other"
            (lambda () (test "other" unbox v))))

    (let ([v (box 10)])
      (check-error-message 'box-cas! (eval `(lambda (x) (box-cas! x 10 11))))
      (tri0 #t '(lambda (b i v) (box-cas! b (unbox b) v))
            (lambda () v) 0 "other"
            (lambda () (test "other" unbox v)))
      (set-box! v 77)
      (tri0 #f '(lambda (b i v) (box-cas! b (gensym) v))
            (lambda () v) 0 "other"
            (lambda () (test 77 unbox v))))

    (bin-exact #t 'procedure-arity-includes? cons 2 #t)
    (bin-exact #f 'procedure-arity-includes? cons 1)
    (bin-exact #f 'procedure-arity-includes? cons 3)
    (bin-exact #t 'procedure-arity-includes? car 1)
    (bin-exact #t 'procedure-arity-includes? car 1)
    (bin-exact #t 'procedure-arity-includes? (lambda (x) x) 1)
    (bin-exact #f 'procedure-arity-includes? (lambda (x) x) 2)
    (bin-exact #t 'procedure-arity-includes? (lambda x x) 2)

    (bin-exact #f 'continuation-mark-set-first #f 'key)
    (with-continuation-mark
        'key 'the-value
      (bin-exact 'the-value 'continuation-mark-set-first #f 'key))

    (un0 'yes 'thing-ref a-rock)
    (bin0 'yes 'thing-ref a-rock 99)
    (bin0 99 'thing-ref 10 99)

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
            #;
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
(test-comp 89
	   '(if (list? null) 89 88))
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

(test-comp '(lambda (w z)
              (let ([x (list* w z)]
                    [y (list* z w)])
                (error "bad")
                (equal? x y)))
           '(lambda (w z)
              (error "bad")
              (equal? (list* w z) (list* z w))))

;; Ok to move `box' past a side effect:
(test-comp '(let ([h (box 0.0)])
              (list (printf "hi\n") h))
           '(list (printf "hi\n") (box 0.0)))

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

(test-comp '(letrec ([f (case-lambda 
                         [(x) x]
                         [(x y) (f (+ x y))])])
	      (f 10))
	   '10)

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
               (test-comp `(module m racket/base
                             (define f ,proc)
                             (print (procedure? f)))
                          `(module m racket/base
                             (define f ,proc)
                             (print #t)))
               (for-each
                (lambda (a)
                  (test-comp `(module m racket/base
                                (define f ,proc)
                                (print (procedure-arity-includes? f ,a)))
                             `(module m racket/base
                                (define f ,proc)
                                (print #t))))
                arities)
               (for-each
                (lambda (a)
                  (test-comp `(module m racket/base
                                (define f ,proc)
                                (print (procedure-arity-includes? f ,a)))
                             `(module m racket/base
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
                    `(lambda (z) ',pred-name)))])
  (test-pred 'pair?)
  (test-pred 'mpair?)
  (test-pred 'list?)
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
  (test-pred 'string?)
  (test-pred 'bytes?)
  (test-pred 'path?)
  (test-pred 'char?)
  (test-pred 'boolean?)
  (test-pred 'chaperone?)
  (test-pred 'impersonator?)
  (test-pred 'procedure?)
  (test-pred 'eof-object?)
  (test-pred 'not))

(let ([test-bin
       (lambda (bin-name)
         (test-comp `(lambda (z)
                       (let ([x ',bin-name])
                         (let ([y (,bin-name z z)])
                           x)))
                    `(lambda (z) ',bin-name)))])
  (test-bin 'eq?)
  (test-bin 'eqv?))

(let ([test-use-unsafe
       (lambda (pred op unsafe-op)
         (test-comp `(module m racket/base
                       (require racket/unsafe/ops)
                       (define (f x)
                         (if (,pred x)
                             (,op x)
                             (cdr x))))
                    `(module m racket/base
                       (require racket/unsafe/ops)
                       (define (f x)
                         (if (,pred x)
                             (,unsafe-op x)
                             (cdr x)))))
         (test-comp `(module m racket/base
                       (require racket/unsafe/ops)
                       (define (f x)
                         (list (,op x) (,op x))))
                    `(module m racket/base
                       (require racket/unsafe/ops)
                       (define (f x)
                         (list (,op x) (,unsafe-op x)))))
         (test-comp `(module m racket/base
                       (require racket/unsafe/ops)
                       (define (f x)
                         (if (and (,pred x)
                                  (zero? (random 2)))
                             (,op x)
                             (cdr x))))
                    `(module m racket/base
                       (require racket/unsafe/ops)
                       (define (f x)
                         (if (and (,pred x)
                                  (zero? (random 2)))
                             (,unsafe-op x)
                             (cdr x))))))])
  (test-use-unsafe 'pair? 'car 'unsafe-car)
  (test-use-unsafe 'pair? 'cdr 'unsafe-cdr)
  (test-use-unsafe 'mpair? 'mcar 'unsafe-mcar)
  (test-use-unsafe 'mpair? 'mcdr 'unsafe-mcdr)
  (test-use-unsafe 'box? 'unbox 'unsafe-unbox))

(test-comp `(module m racket/base
              (require racket/unsafe/ops)
              (define (f x)
                (thread (lambda () (set! x 5)))
                (if (pair? x)
                    (car x)
                    (cdr x))))
           `(module m racket/base
              (require racket/unsafe/ops)
              (define (f x)
                (thread (lambda () (set! x 5)))
                (if (pair? x)
                    (unsafe-car x)
                    (cdr x))))
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

;; it's ok to delay `list', because there's no space-safety issue
(test-comp '(lambda (f x)
              (let ([y (list x)])
                (f)
                y))
           '(lambda (f x)
              (f)
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
    ;; can omit:
    (test-comp `(module m racket/base
                  (require racket/unsafe/ops)
                  (define (f x)
                    (f x)))
               `(module m racket/base
                  (require racket/unsafe/ops)
                  (define (f x)
                    ,expr
                    (f x)))
               yes?)
    ;; cannot reorder:
    (test-comp `(module m racket/base
                  (require racket/unsafe/ops)
                  (define (f x)
                    (let ([y ,expr])
                      (vector-ref x x)
                      (f x y))))
               `(module m racket/base
                  (require racket/unsafe/ops)
                  (define (f x)
                    (vector-ref x x)
                    (f x ,expr)))
               #f))
  (map check-omit-ok
       '((unsafe-vector-ref x x)
         (unsafe-vector*-ref x x)
         (unsafe-struct-ref x x)
         (unsafe-struct*-ref x x)
         (unsafe-mcar x)
         (unsafe-mcdr x)
         (unsafe-unbox x)
         (unsafe-unbox* x)
         (unsafe-bytes-ref x x)
         (unsafe-string-ref x x)
         (unsafe-flvector-ref x x)
         (unsafe-fxvector-ref x x)
         (unsafe-f64vector-ref x x)
         (unsafe-s16vector-ref x x)
         (unsafe-u16vector-ref x x)))
  (map (lambda (x) (check-omit-ok x #f))
       '((unsafe-vector-set! x x x)
         (unsafe-vector*-set! x x x)
         (unsafe-struct-set! x x x)
         (unsafe-struct*-set! x x x)
         (unsafe-set-mcar! x x)
         (unsafe-set-mcdr! x x)
         (unsafe-set-box! x x)
         (unsafe-set-box*! x x)
         (unsafe-bytes-set! x x x)
         (unsafe-string-set! x x x)
         (unsafe-flvector-set! x x x)
         (unsafe-fxvector-set! x x x)
         (unsafe-f64vector-set! x x x)
         (unsafe-s16vector-set! x x x)
         (unsafe-u16vector-set! x x x)))

  (when (extflonum-available?)
    (map check-omit-ok
         '((unsafe-extflvector-ref x x)
           (unsafe-f80vector-ref x x)))

    (map (lambda (x) (check-omit-ok x #f))
         '((unsafe-extflvector-set! x x x)
           (unsafe-f80vector-set! x x x)
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
;; Check for corect fixpoint calculation when lifting

;; This test is especilly fragile. It's a minimized(?) variant
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
    (define size (f 500000)) ; make sure that this still leads to a tail loop
    (size . < . 80000)))

(test #t (dynamic-require ''check-tail-call-by-jit-for-struct-predicate 'go))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Test bytecode validator's checking of constantness

(let ()
  (define c1
    '(module c1 racket/base
       (void ((if (zero? (random 1))
                  (lambda (f) (displayln (f)))
                  #f)
              (lambda ()
                ;; This access of i should raise an exception:
                i)))
       (define i (random 1))))

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
       [(compilation-top max-let-depth prefix code)
        (compilation-top max-let-depth prefix 
                         (let ([body (mod-body code)])
                           (struct-copy mod code [body
                                                  (match body 
                                                    [(list a b)
                                                     (list (match a
                                                             [(application rator (list rand))
                                                              (application 
                                                               rator
                                                               (list
                                                                (match rand
                                                                  [(application rator (list rand))
                                                                   (application
                                                                    rator
                                                                    (list
                                                                     (struct-copy 
                                                                      lam rand
                                                                      [body
                                                                       (match (lam-body rand)
                                                                         [(toplevel depth pos const? ready?)
                                                                          (toplevel depth pos #t #t)])])))])))])
                                                           b)])])))]))
    o2))

  ;; validator should reject this at read or eval time (depending on how lazy validation is):
  (err/rt-test (parameterize ([current-namespace (make-base-namespace)]
                              [read-accept-compiled #t])
                 (eval (read (open-input-bytes (get-output-bytes o2)))))
               exn:fail:read?))

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


(report-errs)
