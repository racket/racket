
(load-relative "loadtest.rktl")

(Section 'jitinline)

(require racket/flonum
         racket/extflonum
         racket/fixnum
         racket/unsafe/undefined
         racket/unsafe/ops)

;; Check JIT inlining of primitives:
(parameterize ([current-namespace (make-base-namespace)]
	       [eval-jit-enabled #t])
  (namespace-require 'racket/flonum)
  (namespace-require 'racket/extflonum)
  (namespace-require 'racket/fixnum)
  (namespace-require 'racket/unsafe/ops)
  (namespace-require 'racket/unsafe/undefined)
  (namespace-require '(prefix k: '#%kernel))
  (eval '(module rock racket/base
           (provide (all-defined-out))
           (define-values (prop:thing thing? thing-ref)
             (make-struct-type-property 'thing))
           (struct rock (x) #:property prop:thing 'yes)
           (struct stone (x) #:authentic)))
  (eval '(require 'rock))
  (let* ([struct:rock (eval 'struct:rock)]
         [a-rock (eval '(rock 0))]
         [a-stone (eval '(stone 0))]
         [chap-rock (eval '(chaperone-struct (rock 0) rock-x (lambda (r v) (add1 v))))]
         [check-error-message (lambda (name proc [fixnum? #f]
                                            #:bad-value [bad-value (if fixnum? 10 'bad)]
                                            #:first-arg [first-arg #f]
                                            #:second-arg [second-arg #f])
				(unless (memq name '(eq? eqv? equal? 
                                                         not k:true-object? null? pair? list? k:list-pair?
							 real? number? boolean?
							 procedure? symbol? symbol-interned? keyword?
							 string? bytes?
							 vector? box?
                                                         immutable?
							 eof-object?
                                                         exact-integer?
                                                         exact-nonnegative-integer?
                                                         exact-positive-integer?
                                                         thing? rock? stone?
                                                         continuation-mark-set-first))
				  (let ([s (with-handlers ([exn? exn-message])
                                             (let ([bad (if (eq? bad-value 'unsafe-undefined)
                                                            unsafe-undefined
                                                            bad-value)])
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
	 [un-exact (lambda (v op arg [check-fixnum-as-bad? #f] #:name [name op])
		     (check-error-message name (eval `(lambda (x) (,op x))))
                     (when check-fixnum-as-bad?
                       (check-error-message name (eval `(lambda (x) (,op x))) #t))
		     (un0 v op arg))]
         
	 [un (lambda (v op arg [check-fixnum-as-bad? #f] #:name [name op])
	       (un-exact v op arg check-fixnum-as-bad? #:name name)
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
	 [bin-exact (lambda (v op arg1 arg2 [check-fixnum-as-bad? #f]
                               #:bad-value [bad-value 'bad]
                               #:bad-as-second-only? [bad-as-second-only? #f])
                      (unless bad-as-second-only?
                        (check-error-message op (eval `(lambda (x) (,op x ',arg2))) #:bad-value bad-value))
		      (check-error-message op (eval `(lambda (x) (,op ',arg1 x))) #:bad-value bad-value)
		      (check-error-message op (eval `(lambda (x y) (,op x y))) #:first-arg arg1 #:bad-value bad-value)
                      (unless bad-as-second-only?
                        (check-error-message op (eval `(lambda (x y) (,op x y))) #:second-arg arg2 #:bad-value bad-value))
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
         [tri (lambda (v op get-arg1 arg2 arg3 check-effect #:wrap [wrap values] #:exact? [exact? #f])
                (define (e->i n) (if (number? n) (exact->inexact n) n))
                (tri0 v op get-arg1 arg2 arg3 check-effect #:wrap wrap)
                (unless exact?
                  (tri0 (e->i v) op (lambda () (exact->inexact (get-arg1))) (exact->inexact arg2) (exact->inexact arg3) check-effect
                        #:wrap wrap)
                  (tri0 (e->i v) op get-arg1 (exact->inexact arg2) arg3 check-effect
                        #:wrap wrap)))]
         [tri-if (lambda (v op get-arg1 arg2 arg3 check-effect #:exact? [exact? #f])
                   (tri v op get-arg1 arg2 arg3 check-effect #:exact? exact?)
                   (tri (if v 'true 'false) op get-arg1 arg2 arg3 check-effect
                        #:wrap (lambda (e) `(if ,e 'true 'false))
                        #:exact? exact?))]
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
    (un #f 'k:list-pair? 0)
    (un #f 'k:list-pair? '())
    (un #f 'k:list-pair? '(1 . 2))
    (un-exact #t 'k:list-pair? '(1))
    (un-exact #t 'k:list-pair? '(1 2))
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
    (un #t 'symbol-interned? 'ok)
    (un #f 'symbol-interned? (gensym))
    (un #t 'keyword? '#:ok)
    (un #f 'keyword? #f)
    (un #t 'vector? (vector 1 2 3))
    (un #t 'vector? (vector-immutable 1 2 3))
    (un #f 'vector? #f)
    (un #t 'box? (box 10))
    (un #f 'box? #f)
    (un #t 'string? "apple")
    (un #f 'string? #"apple")
    (un #f 'bytes? "apple")
    (un #t 'bytes? #"apple")
    (un #f 'thing? 10)
    (un #t 'thing? a-rock)
    (un #f 'thing? a-stone)
    (un #t 'thing? chap-rock)
    (un #t 'thing? struct:rock)
    (un #f 'rock? 10)
    (un #t 'rock? a-rock)
    (un #f 'rock? a-stone)
    (un #t 'rock? chap-rock)
    (un #f 'rock? struct:rock)
    (un #f 'stone? 10)
    (un #f 'stone? a-rock)
    (un #t 'stone? a-stone)
    (un #f 'stone? chap-rock)
    (un #f 'immutable? (vector 1 2 3))
    (un #t 'immutable? (vector-immutable 1 2 3))
    (un #f 'immutable? (box 1))
    (un #t 'immutable? (box-immutable 1))
    (un #f 'immutable? (bytes 1 2 3))
    (un #t 'immutable? (bytes->immutable-bytes (bytes 1 2 3)))
    (un #f 'immutable? (string #\1 #\2 #\3))
    (un #f 'immutable? (make-hash))
    (un #f 'immutable? (make-hasheq))
    (un #f 'immutable? (make-weak-hasheq))
    (un #f 'immutable? (make-ephemeron-hasheq))
    (un #t 'immutable? #hash())
    (un #t 'immutable? #hasheq())
    (un #t 'immutable? #hasheqv())
    (un #t 'immutable? #hashalw())
    (un #t 'immutable? (chaperone-vector '#(1 2 3) (lambda (vec i val) val) (lambda (vec i val) val)))
    (un #f 'immutable? (chaperone-vector (vector 1 2 3) (lambda (vec i val) val) (lambda (vec i val) val)))

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

    (un #f 'k:true-object? #f)
    (un #t 'k:true-object? #t)
    (un #f 'k:true-object? 10)

    (un #t '< 100)
    (bin #t '< 100 200)
    (bin #f '< 200 100)
    (bin #f '< 100 100)
    (bin #t '< -200 100)
    (bin #f '< 100 -200)
    (bin #t '< 1 (expt 2 30))
    (tri-if #t '< (lambda () 1) 2 3 void)
    (tri-if #f '< (lambda () 1) 3 3 void)
    (tri-if #f '< (lambda () 1) -1 3 void)
    (un-exact #t 'fx< 100)
    (bin-exact #t 'fx< 100 200)
    (bin-exact #f 'fx< 200 100)
    (bin-exact #f 'fx< 200 200)
    (tri-if #t 'fx< (lambda () 10) 20 30 void #:exact? #t)
    (tri-if #f 'fx< (lambda () 10) 30 30 void #:exact? #t)
    (tri-if #f 'fx< (lambda () 10) -10 30 void #:exact? #t)
    (un-exact #t 'fl< 100.0)
    (un-exact #t 'fl< 100.0 #t)
    (bin-exact #t 'fl< 100.0 200.0 #t)
    (bin-exact #f 'fl< 200.0 100.0)
    (bin-exact #f 'fl< 200.0 200.0)
    (tri-if #t 'fl< (lambda () 10.5) 20.5 30.5 void #:exact? #t)
    (tri-if #f 'fl< (lambda () 10.5) 30.5 30.5 void #:exact? #t)
    (tri-if #f 'fl< (lambda () 10.5) -10.5 30.5 void #:exact? #t)

    (un #t '<= 100)
    (bin #t '<= 100 200)
    (bin #f '<= 200 100)
    (bin #t '<= 100 100)
    (bin #t '<= -200 100)
    (bin #f '<= 100 -200)
    (tri-if #t '<= (lambda () 1) 2 3 void)
    (tri-if #t '<= (lambda () 1) 3 3 void)
    (tri-if #f '<= (lambda () 1) -1 3 void)
    (un-exact #t 'fx<= 100)
    (bin-exact #t 'fx<= 100 200)
    (bin-exact #f 'fx<= 200 100)
    (bin-exact #t 'fx<= 200 200)
    (tri-if #t 'fx<= (lambda () 10) 20 30 void #:exact? #t)
    (tri-if #t 'fx<= (lambda () 10) 30 30 void #:exact? #t)
    (tri-if #f 'fx<= (lambda () 10) -10 30 void #:exact? #t)
    (un-exact #t 'fl<= 100.0)
    (un-exact #t 'fl<= 100.0 #t)
    (bin-exact #t 'fl<= 100.0 200.0 #t)
    (bin-exact #f 'fl<= 200.0 100.0)
    (bin-exact #t 'fl<= 200.0 200.0)
    (tri-if #t 'fl<= (lambda () 10.5) 20.5 30.5 void #:exact? #t)
    (tri-if #t 'fl<= (lambda () 10.5) 30.5 30.5 void #:exact? #t)
    (tri-if #f 'fl<= (lambda () 10.5) -10.5 30.5 void #:exact? #t)

    (un #t '> 100)
    (bin #f '> 100 200)
    (bin #t '> 200 100)
    (bin #f '> 100 100)
    (bin #f '> -200 100)
    (bin #t '> 100 -200)
    (bin #f '> 1 (expt 2 30))
    (tri-if #t '> (lambda () 3) 2 1 void)
    (tri-if #f '> (lambda () 3) 3 1 void)
    (tri-if #f '> (lambda () 3) -1 1 void)
    (un-exact #t 'fx> 100)
    (bin-exact #f 'fx> 100 200)
    (bin-exact #t 'fx> 200 100)
    (bin-exact #f 'fx> 200 200)
    (tri-if #t 'fx> (lambda () 30) 20 10 void #:exact? #t)
    (tri-if #f 'fx> (lambda () 30) 30 10 void #:exact? #t)
    (tri-if #f 'fx> (lambda () 30) -10 10 void #:exact? #t)
    (un-exact #t 'fl> 100.0)
    (un-exact #t 'fl> 100.0 #t)
    (bin-exact #f 'fl> 100.0 200.0 #t)
    (bin-exact #t 'fl> 200.0 100.0)
    (bin-exact #f 'fl> 200.0 200.0)
    (tri-if #t 'fl> (lambda () 30.5) 20.5 10.5 void #:exact? #t)
    (tri-if #f 'fl> (lambda () 30.5) 30.5 10.5 void #:exact? #t)
    (tri-if #f 'fl> (lambda () 30.5) -10.5 10.5 void #:exact? #t)

    (un #t '>= 100)
    (bin #f '>= 100 200)
    (bin #t '>= 200 100)
    (bin #t '>= 100 100)
    (bin #f '>= -200 100)
    (bin #t '>= 100 -200)
    (tri-if #t '>= (lambda () 3) 2 1 void)
    (tri-if #t '>= (lambda () 3) 3 1 void)
    (tri-if #f '>= (lambda () 3) -1 1 void)
    (un-exact #t 'fx>= 100)
    (bin-exact #f 'fx>= 100 200)
    (bin-exact #t 'fx>= 200 100)
    (bin-exact #t 'fx>= 200 200)
    (tri-if #t 'fx>= (lambda () 30) 20 10 void #:exact? #t)
    (tri-if #t 'fx>= (lambda () 30) 30 10 void #:exact? #t)
    (tri-if #f 'fx>= (lambda () 30) -10 10 void #:exact? #t)
    (un-exact #t 'fl>= 100.0)
    (un-exact #t 'fl>= 100.0 #t)
    (bin-exact #f 'fl>= 100.0 200.0 #t)
    (bin-exact #t 'fl>= 200.0 100.0)
    (bin-exact #t 'fl>= 200.0 200.0)
    (tri-if #t 'fl>= (lambda () 30.5) 20.5 10.5 void #:exact? #t)
    (tri-if #t 'fl>= (lambda () 30.5) 30.5 10.5 void #:exact? #t)
    (tri-if #f 'fl>= (lambda () 30.5) -10.5 10.5 void #:exact? #t)

    (un #t '= 100)
    (bin #f '= 100 200)
    (bin #f '= 200 100)
    (bin #t '= 100 100)
    (bin #f '= -200 100)
    (bin #f '= +nan.0 +nan.0)
    (tri-if #t '= (lambda () 3) 3 3 void)
    (tri-if #f '= (lambda () 3) 3 1 void)
    (tri-if #f '= (lambda () 3) 1 3 void)
    (tri-if #f '= (lambda () 1) 3 3 void)
    (un-exact #t 'fx= 100)
    (bin-exact #f 'fx= 100 200)
    (bin-exact #t 'fx= 200 200)
    (tri-if #t 'fx= (lambda () 30) 30 30 void #:exact? #t)
    (tri-if #f 'fx= (lambda () 30) 30 10 void #:exact? #t)
    (tri-if #f 'fx= (lambda () 30) 10 30 void #:exact? #t)
    (tri-if #f 'fx= (lambda () 10) 30 30 void #:exact? #t)
    (un-exact #t 'fl= 100.0)
    (un-exact #t 'fl= 100.0 #t)
    (bin-exact #f 'fl= 100.0 200.0 #t)
    (bin-exact #t 'fl= 200.0 200.0)
    (tri-if #t 'fl= (lambda () 30.5) 30.5 30.5 void #:exact? #t)
    (tri-if #f 'fl= (lambda () 30.5) 30.5 10.5 void #:exact? #t)
    (tri-if #f 'fl= (lambda () 30.5) 10.5 30.5 void #:exact? #t)
    (tri-if #f 'fl= (lambda () 10.5) 30.5 30.5 void #:exact? #t)

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
    (un-exact 3 'fxabs -3)
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
    (un-exact 11 'fl->fx 11.5 #t)
    (un-exact -11 'fl->fx -11.0)
    (un-exact -11 'fl->fx -11.5)
    (check-error-message 'fl->fx (eval `(lambda (x) (fl->fx x))) #:bad-value (exact->inexact (expt 2 100)))
    (check-error-message 'fl->fx (eval `(lambda (x) (fl->fx x))) #:bad-value (exact->inexact (- (expt 2 100))))
    (check-error-message 'fl->fx (eval `(lambda (x) (fl->fx x))) #:bad-value +inf.0)
    (check-error-message 'fl->fx (eval `(lambda (x) (fl->fx x))) #:bad-value -inf.0)
    (check-error-message 'fl->fx (eval `(lambda (x) (fl->fx x))) #:bad-value +nan.0)

    (un 4 '+ 4)
    (bin 11 '+ 4 7)
    (bin -3 '+ 4 -7)
    (bin (expt 2 30) '+ (expt 2 29) (expt 2 29))
    (bin (- (expt 2 31) 2) '+ (sub1 (expt 2 30)) (sub1 (expt 2 30)))
    (tri 6 '+ (lambda () 1) 2 3 void)
    (tri 13/2 '+ (lambda () 1) 5/2 3 void)
    (bin-exact 25 'fx+ 10 15)
    (tri-exact 33 'fx+ (lambda () 10) 15 8 void #f)
    (bin-exact 25 'fx+/wraparound 10 15)
    (bin-exact 3.4 'fl+ 1.1 2.3 #t)
    (tri-exact 7.4 'fl+ (lambda () 1.1) 2.3 4.0 void #f)
    ;; 4.1995579896506e-322 has only its low byte as non-zero
    (bin-exact 4.1995579896506e-322 'fl+ 4.1995579896506e-322 0.0 #t)

    (un -3 '- 3)
    (bin 3 '- 7 4)
    (bin 11 '- 7 -4)
    (bin 0 '- (expt 2 29) (expt 2 29))
    (bin (expt 2 30) '- (expt 2 29) (- (expt 2 29)))
    (bin (- (expt 2 30)) '- (- (expt 2 29)) (expt 2 29))
    (bin (- 2 (expt 2 31)) '- (- 1 (expt 2 30)) (sub1 (expt 2 30)))
    (tri 6 '- (lambda () 10) 3 1 void)
    (tri 13/2 '- (lambda () 10) 3 1/2 void)
    (un-exact -3 'fx- 3)
    (bin-exact 13 'fx- 5 -8)
    (bin-exact 13 'fx-/wraparound 5 -8)
    (tri-exact 14 'fx- (lambda () 5) -8 -1 void #f)
    (un-exact -3.6 'fl- 3.6)
    (bin-exact -0.75 'fl- 1.5 2.25 #t)
    (tri-exact -1.5 'fl- (lambda () 1.5) 2.25 0.75 void #f)
    (un-exact -4.1995579896506e-322 'fl- 4.1995579896506e-322 #t)

    (un 4 '* 4)
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
    (un-exact 11 'fx* 11)
    (bin-exact 253 'fx* 11 23)
    (bin-exact 253 'fx*/wraparound 11 23)
    (bin-exact 2.53 'fl* 1.1 2.3 #t)
    (bin-exact 4.1995579896506e-322 'fl* 4.1995579896506e-322 1.0 #t)
    (tri-exact 506 'fx* (lambda () 11) 23 2 void #f)
    (tri-exact 7.59 'fl* (lambda () 1.1) 2.3 3.0 void #f)

    (un 1/4 '/ 4)
    (bin 0 '/ 0 4)
    (bin 1/4 '/ 1 4)
    (bin 4 '/ 4 1)
    (bin 4 '/ 16 4)
    (bin -4 '/ -16 4)
    (bin -4 '/ 16 -4)
    (bin 4 '/ -16 -4)
    (tri 3 '/ (lambda () 30) 5 2 void)
    (tri 12 '/ (lambda () 30) 5 1/2 void)
    (un-exact 0.25 'fl/ 4.0)
    (bin-exact (/ 1.1 2.3) 'fl/ 1.1 2.3 #t)
    (tri-exact (/ 1.1 2.3 0.5) 'fl/ (lambda () 1.1) 2.3 0.5 void #f)
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

    (un 3 'min 3)
    (bin 3 'min 3 300)
    (bin -300 'min 3 -300)
    (bin -400 'min -400 -300)
    (tri 5 'min (lambda () 10) 5 20 void)
    (tri 5 'min (lambda () 5) 10 20 void)
    (tri 5 'min (lambda () 20) 10 5 void)
    (un-exact 3.0 'flmin 3.0 #t)
    (bin-exact 3.0 'flmin 3.0 4.5 #t)
    (bin-exact 2.5 'flmin 3.0 2.5)
    (bin0 3.5 '(lambda (x y) (fl+ 1.0 (flmin x y))) 3.0 2.5)
    (bin0 4.0 '(lambda (x y) (fl+ 1.0 (flmin x y))) 3.0 4.5)
    (un-exact 30 'fxmin 30)
    (bin-exact 30 'fxmin 30 45)
    (bin-exact 25 'fxmin 30 25)

    (bin 300 'max 3 300)
    (bin 3 'max 3 -300)
    (bin -3 'max -3 -300)
    (tri 50 'max (lambda () 10) 50 20 void)
    (tri 50 'max (lambda () 50) 10 20 void)
    (tri 50 'max (lambda () 20) 10 50 void)
    (un-exact 4.5 'flmax 4.5 #t)
    (bin-exact 4.5 'flmax 3.0 4.5 #t)
    (bin-exact 3.0 'flmax 3.0 2.5)
    (bin0 5.5 '(lambda (x y) (fl+ 1.0 (flmax x y))) 3.0 4.5)
    (bin0 4.0 '(lambda (x y) (fl+ 1.0 (flmax x y))) 3.0 2.5)
    (un-exact 30 'fxmax 30)
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
    (un-exact 11 'fxand 11)
    (bin-exact 11 'fxand 11 43)
    (tri-exact 11 'fxand (lambda () 11) 43 75 void #f)

    (un-exact 8 'bitwise-ior 8)
    (bin-exact 11 'bitwise-ior 8 3)
    (bin-exact 11 'bitwise-ior 11 3)
    (bin-exact -1 'bitwise-ior 11 -1)
    (bin-exact (sub1 (expt 2 51)) 'bitwise-ior (sub1 (expt 2 50)) (expt 2 50))
    (bin-exact (add1 (expt 2 50)) 'bitwise-ior 1 (expt 2 50))
    (tri-exact #x10101 'bitwise-ior (lambda () #x1) #x100 #x10000 void #f)
    (un-exact 8 'fxior 8)
    (bin-exact 11 'fxior 8 3)
    (tri-exact 11 'fxior (lambda () 8) 3 1 void #f)

    (un-exact 8 'bitwise-xor 8)
    (bin-exact 11 'bitwise-xor 8 3)
    (bin-exact 8 'bitwise-xor 11 3)
    (bin-exact -2 'bitwise-xor 1 -1)
    (bin-exact (sub1 (expt 2 51)) 'bitwise-xor (sub1 (expt 2 50)) (expt 2 50))
    (bin-exact (add1 (expt 2 50)) 'bitwise-xor 1 (expt 2 50))
    (tri-exact #x10101 'bitwise-xor (lambda () #x1) #x110 #x10010 void #f)
    (un-exact 8 'fxxor 8)
    (bin-exact 11 'fxxor 8 3)
    (tri-exact 10 'fxxor (lambda () 8) 3 1 void #f)

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
    (bin-exact 4 'fxlshift 2 1 #:bad-value -2 #:bad-as-second-only? #t)
    (bin-exact 4 'fxlshift 2 1 #:bad-value 100 #:bad-as-second-only? #t)
    (bin-exact 1 'fxrshift 2 1)
    (bin-exact 1 'fxrshift 2 1 #:bad-value -2 #:bad-as-second-only? #t)
    (bin-exact 1 'fxrshift 2 1 #:bad-value 100 #:bad-as-second-only? #t)
    (bin-exact 1 'fxrshift/logical 2 1)
    (bin-exact -1 'fxrshift/logical -1 0)
    (bin-exact (most-positive-fixnum) 'fxrshift/logical -1 1)
    (bin-exact 1 'fxrshift/logical 2 1 #:bad-value -2 #:bad-as-second-only? #t)
    (bin-exact 1 'fxrshift/logical 2 1 #:bad-value 100 #:bad-as-second-only? #t)

    (bin-exact 4 'fxlshift/wraparound 2 1)
    (bin-exact 4 'fxlshift/wraparound 2 1 #:bad-value -2 #:bad-as-second-only? #t)
    (bin-exact 4 'fxlshift/wraparound 2 1 #:bad-value 100 #:bad-as-second-only? #t)

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

    (tri-exact #xB43544F2 'flbit-field (lambda () 3.141579e132) 16 48 void #f)

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
    (bin-exact 0.75+2.0i 'make-rectangular 0.75f0 2.0)
    (bin-exact 0.75+2.0i 'make-rectangular 0.75 2.0f0)
    (bin-exact 0.75f0+2.0f0i 'make-rectangular 0.75f0 2.0f0)
    (bin-exact 1 'make-rectangular 1 0)
    (bin-exact 1.0 'make-rectangular 1.0 0)

    (un-exact #t 'char=? #\a)
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

    (bin-exact 'a 'vector*-ref #(a b c) 0 #t)
    (bin-exact 'b 'vector*-ref #(a b c) 1)
    (bin-exact 'c 'vector*-ref #(a b c) 2)

    (un-exact 'a 'unbox (box 'a) #t)
    (un-exact 'a 'unbox* (box 'a) #t)
    (un-exact 3 'vector-length (vector 'a 'b 'c) #t)
    (un-exact 3 'vector*-length (vector 'a 'b 'c) #t)

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
      (un-exact 11 'extfl->fx 11.5t0 #t)
      (un-exact -11 'extfl->fx -11.0t0)
      (un-exact -11 'extfl->fx -11.5t0)
      (check-error-message 'extfl->fx (eval `(lambda (x) (extfl->fx x))) #:bad-value (->extfl (expt 2 100)))
      (check-error-message 'extfl->fx (eval `(lambda (x) (extfl->fx x))) #:bad-value (->extfl (- (expt 2 100))))
      (check-error-message 'extfl->fx (eval `(lambda (x) (extfl->fx x))) #:bad-value +inf.t)
      (check-error-message 'extfl->fx (eval `(lambda (x) (extfl->fx x))) #:bad-value -inf.t)
      (check-error-message 'extfl->fx (eval `(lambda (x) (extfl->fx x))) #:bad-value +nan.t)

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

      (bin-exact 5 'check-not-unsafe-undefined 5 'check-not-unsafe-undefined #:bad-value 'unsafe-undefined)
      )

    ;; Even if extflonums are not available, check that uses don't
    ;; confuse the JIT
    (let ()
      (define-syntax-rule (check-jit-ok (proc arg ...))
        (test 'no (eval '(lambda (? arg ...)
                           (if ?
                               (proc arg ...)
                               'no)))
              #f 'arg ...))
      (check-jit-ok (extfl+ x y))
      (check-jit-ok (extfl* x y))
      (check-jit-ok (extfl- x y))
      (check-jit-ok (extfl/ x y))
      (check-jit-ok (extflmin x y))
      (check-jit-ok (extflmax x y))
      (check-jit-ok (extfl< x y))
      (check-jit-ok (extfl> x y))
      (check-jit-ok (extfl<= x y))
      (check-jit-ok (extfl>= x y))
      (check-jit-ok (extfl= x y))
      (check-jit-ok (extflabs x))
      (check-jit-ok (extflsqrt x))
      (check-jit-ok (extflsin x))
      (check-jit-ok (extflcos x))
      (check-jit-ok (extfltan x))
      (check-jit-ok (extflasin x))
      (check-jit-ok (extflacos x))
      (check-jit-ok (extflatan x))
      (check-jit-ok (extfllog x))
      (check-jit-ok (extflexp x))
      (check-jit-ok (extflexpt x y))
      (check-jit-ok (->extfl x y))
      (check-jit-ok (fx->extfl x y))
      (check-jit-ok (extflvector-length x))
      (check-jit-ok (extflvector-ref x y))
      (check-jit-ok (extflvector-set! x y z)))
    
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
      (test-setter make-vector #f 7 'vector*-set! vector*-set! vector*-ref #t)
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

    (err/rt-test (apply (list-ref (list (lambda (v) (vector*-length v))) (random 1)) 
                        (list (chaperone-vector (vector 1 2 3) (lambda (vec i val) val) (lambda (vec i val) val)))))
    (err/rt-test (apply (list-ref (list (lambda (v) (vector*-ref v 0))) (random 1)) 
                        (list (chaperone-vector (vector 1 2 3) (lambda (vec i val) val) (lambda (vec i val) val)))))
    (err/rt-test (apply (list-ref (list (lambda (v) (unbox* v))) (random 1)) 
                        (list (chaperone-box (box 1) (lambda (b v) v) (lambda (b v) v)))))

    (err/rt-test (apply (list-ref (list (lambda (v) (vector-set! v 0 #t))) (random 1)) 
                        (list (vector-immutable 1 2 3))))
    (err/rt-test (apply (list-ref (list (lambda (v) (vector*-set! v 0 #t))) (random 1)) 
                        (list (vector-immutable 1 2 3))))
    (err/rt-test (apply (list-ref (list (lambda (v) (vector*-set! v 0 #t))) (random 1)) 
                        (list (chaperone-vector (vector 1 2 3) (lambda (vec i val) val) (lambda (vec i val) val)))))
    (err/rt-test (apply (list-ref (list (lambda (s) (string-set! s 0 #\a))) (random 1)) 
                        (list "123")))
    (err/rt-test (apply (list-ref (list (lambda (s) (bytes-set! s 0 0))) (random 1)) 
                        (list #"123")))
    (err/rt-test (apply (list-ref (list (lambda (b) (set-box! b #t))) (random 1)) 
                        (list (box-immutable 1))))
    (err/rt-test (apply (list-ref (list (lambda (b) (set-box*! b #t))) (random 1)) 
                        (list (box-immutable 1))))
    (err/rt-test (apply (list-ref (list (lambda (v) (set-box*! v 'no))) (random 1)) 
                        (list (chaperone-box (box 1) (lambda (b v) v) (lambda (b v) v)))))
    
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

    (let ([v (vector 1 0)])
      (check-error-message 'vector-cas! (eval `(lambda (x) (vector-cas! x 10 11 12))))
      (tri0 #t
            '(lambda (v i nv) (vector-cas! v i (vector-ref v i) nv))
            (lambda () v) 1 "other"
            (lambda ()
              (test 1 vector-ref v 0)
              (test "other" vector-ref v 1)))
      (tri0 #f
            '(lambda (v i nv) (vector-cas! v i (gensym) nv))
            (lambda () v) 1 "next"
            (lambda ()
              (test 1 vector-ref v 0)
              (test "other" vector-ref v 1))))

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

    (un 'b '(lambda (ht) (hash-ref ht 'a #f)) '#hash((a . b)) #t
        #:name 'hash-ref)

    (un-exact 7 'arity-at-least-value (make-arity-at-least 7) #t)
    (err/rt-test (let ([f (eval '(lambda () (arity-at-least-value)))]) (f)))

    (un-exact 20 'date-day (make-date 0 0 0 20 1 0 0 0 #f 0) #t)
    (err/rt-test (let ([f (eval '(lambda () (date-day)))]) (f)))

    (un-exact 12345 'date*-nanosecond (make-date* 0 0 0 20 1 0 0 0 #f 0 12345 "UTC") #t)
    (err/rt-test (let ([f (eval '(lambda () (date*-nanosecond)))]) (f)))

    (un-exact 'here 'srcloc-source (make-srcloc 'here #f #f #f #f) #t)
    (err/rt-test (let ([f (eval '(lambda () (srcloc-source)))]) (f)))

    ))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Make sure that the JIT doesn't try to inline
;; a vector allocation that is too large

(parameterize ([current-namespace (make-base-namespace)])
  (for ([tail? '(#t #f)])
    (let loop ([i 10])
      ((eval `(lambda (f x)
                ,(let ([e `(vector x ,@(for/list ([j (in-range i)])
                                         j))])
                   (if tail?
                       e
                       `(f ,e)))))
       values i)
      (when (i . < . 10000)
        (loop (floor (* i #e1.25)))))))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Check JIT handling of structure-reference sequences
(for ([options '(() (#:authentic))])
  (parameterize ([current-namespace (make-base-namespace)]
                 [eval-jit-enabled #t])
    (eval `(module paper racket/base
             (provide (all-defined-out))
             (struct paper (width height folds) #:transparent ,@options)
             (define (fold-letter l)
               (for/fold ([l l]) ([i (in-range 100)])
                 (and (paper? l)
                      (struct-copy paper l [folds i]))))
             (define (refine-letter l)
               (for/fold ([l l]) ([i (in-range 100)]) 
                 (and (paper? l)
                      (struct-copy paper l [width i]))))))
    (eval '(require 'paper))
    (eval '(define letter (paper 8.5 11 0)))
    (unless (equal? options '(#:authentic))
      (eval '(define formal-letter (chaperone-struct letter paper-height
                                                     (lambda (s v)
                                                       (unless (equal? v 11)
                                                         (error "wrong"))
                                                       v)))))
    (test #t eval '(equal? (fold-letter letter) (paper 8.5 11 99)))
    (test #t eval '(equal? (refine-letter letter) (paper 99 11 0)))
    (unless (equal? options '(#:authentic))
      (test #t eval '(equal? (fold-letter formal-letter) (paper 8.5 11 99)))
      (test #t eval '(equal? (refine-letter formal-letter) (paper 99 11 0))))))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Make sure the JIT handles struct-type property predicates and
;; accessors correctly, including distinguishing them from structure
;; predicates

(define (apply-a-predicate x pred)
  (if (pred x)
      '(1)
      '(2)))

(define (apply-an-accessor x acc)
  (cons (acc x) '(=)))

(let ()
  (define-values (prop:x x? x-ref) (make-struct-type-property 'x))
  (struct chi ()
          #:property prop:x 3)
  
  (test '(1) apply-a-predicate (chi) x?)
  (test '(3 =) apply-an-accessor (chi) x-ref)
  
  (struct alpha (a))
  (struct beta (b)
          #:property prop:procedure (lambda (v) #t))
  
  (test '(1) apply-a-predicate (alpha 'a) alpha?)
  (test '(1) apply-a-predicate (beta 'b) beta?)
  
  (test '(a =) apply-an-accessor (alpha 'a) alpha-a)
  (test '(b =) apply-an-accessor (beta 'b) beta-b)
  (test '(#t =) apply-an-accessor (alpha 'a) alpha?)
  (test '(#t =) apply-an-accessor (beta 'b) beta?)
  (test '(#f =) apply-an-accessor (alpha 'a) beta?)
  (test '(#f =) apply-an-accessor (beta 'b) alpha?)

  (test '(2) apply-a-predicate (alpha 'a) x?)
  (test '(2) apply-a-predicate (beta 'b) x?)
  (test '(#f =) apply-an-accessor (alpha 'a) x?)
  (test '(#f =) apply-an-accessor (beta 'b) x?))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Check case of an index that's a module-level variable that is definitely
;; defined but not a known constant

(module assign-to-bytes-array-with-non-constant-offset racket/base
  (provide out)
  (define buf (make-bytes 4 7))
  (define offset (if (even? (random 1)) 2 2))
  (define f (lambda () (bytes-set! buf offset 9) (bytes-ref buf offset)))
  (set! f f)
  (define out (f)))

(test 9 dynamic-require ''assign-to-bytes-array-with-non-constant-offset 'out)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Make sure `char=?` and similar don't get confused when trying to
;; reverse the argument order internally
(let ([f (lambda () (char=? (peek-char (open-input-string "")) #\x))])
  (set! f f)
  (err/rt-test (f)))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Regression test to that used to fail because the inlined form of
;; `unsafe-make-flrectangular` was missing a runstack sync

(let ()
  (define *iteration-limit* 50)

  (define (sqr x) (* x x))

  (define (mandel c)
    (define op
      (let ()
        (define (mandel-iter unboxed-real-6 unboxed-imag-7 i)
          (let-values (((z) 'check-syntax-binding))
            (if (let ((or-part (>= i *iteration-limit*)))
                  (if or-part
                      or-part
                      (fl>
                       (let ([r (flabs unboxed-real-6)])
                         (let ([i (flabs unboxed-imag-7)])
                           (if (zero? i)
                               r
                               (if (fl< i r)
                                   (let ([q (fl/ i r)])
                                     (fl* r (flsqrt (fl+ '1.0 (fl* q q)))))
                                   (let ([q (fl/ r i)])
                                     (fl* i (flsqrt (fl+ '1.0 (fl* q q)))))))))
                       2.0)))
                i
                (let ([g8 c])
                  (let-values (((unboxed-real-9) (flreal-part g8)))
                    (let-values (((unboxed-imag-10) (flimag-part g8)))
                      (let-values (((g11) (sqr (unsafe-make-flrectangular
                                                unboxed-real-6
                                                unboxed-imag-7))))
                        (let-values (((unboxed-real-12) (flreal-part g11)))
                          (let-values (((unboxed-imag-13) (flimag-part g11)))
                            (let-values (((unboxed-real-14) (fl+ (real->double-flonum
                                                                  unboxed-real-9)
                                                                 unboxed-real-12)))
                              (let-values (((unboxed-imag-15) (fl+
                                                               (real->double-flonum
                                                                unboxed-imag-10)
                                                               unboxed-imag-13)))
                                (let-values (((boxed-binding16) (+ i '1)))
                                  (mandel-iter
                                   unboxed-real-14
                                   unboxed-imag-15
                                   boxed-binding16)))))))))))))
        mandel-iter))
    (op 0.0 0.0 0))

  (define (brot xs ys)
    (for*/list ([y (in-list ys)]
                [x (in-list xs)])
      (mandel (unsafe-make-flrectangular x y))))

  (define (make-ticks min max resolution)
    (for/list ([v (in-range min max (/ (fl- max min) resolution))])
      v))

  (void (brot (make-ticks '-1.5 '0.5 '300)
              (make-ticks '-1.0 '1.0 '300))))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Try to trigger copying of arguments to a JITted
;; `case-lambda` into the runstack before the cases
;; of the `case-lambda` have been compiled. This
;; is intended as a regression test for a bug that
;; led to a runstack overflow in that case.

(let ()
  (define procs
    (parameterize ([current-namespace (make-base-namespace)])
      (for/list ([i 4096])
        (chaperone-procedure
         (case-lambda
           [(a b c d e f g h) 8]
           [(a b c) 3])
         (eval '(case-lambda
                  [(a b c d e f g h) (values a b c d e f g h)]
                  [(a b c) (values a b c)]))))))

  (define args '(1 2 3 4 5 6 7 8))
  (set! args args)

  (let loop ([procs procs])
    (if (null? procs)
        0
        (+ (loop (cdr procs))
           (apply (car procs) args)))))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(report-errs)
