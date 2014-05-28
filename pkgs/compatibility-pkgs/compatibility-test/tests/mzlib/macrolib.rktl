
(load-relative "loadtest.rktl")

(Section 'macrolib)

(require mzlib/etc)

(err/rt-test (let+ ([rec a a]) 5) exn:fail:contract:variable?)
(err/rt-test (let+ ([recs [b c] [c b]]) 5) exn:fail:contract:variable?)

(let ([l1
       (let+ ([rec d 1]
              [val e 1]
              [val x 1]
              [val y 2]
              [vals (x y) (y x)]
              [rec (values f) (values 1)]
              [vals [(values g h) (values 2 3)]]
              [val i 3]
              [_ (set! i 4)
                 (set! i 5)])
             'x
             (list d e x y f g h i))]
      [l2 (list 1 1 2 1 1 2 3 5)])
  (test l1 'let-plus l2))

(test 'hi 'local (local () 'hi))
(define x 7)
(test 6 'local (local ((define x 6)) x))
(test 7 'local x)
(test '... vector-ref (struct->vector (local ((define x 6) (define-struct a (b))) (make-a x))) 1)
(test #t 'local (local [(define o (lambda (x) (if (zero? x) #f (e (sub1 x)))))
			(define e (lambda (x) (if (zero? x) #t (o (sub1 x)))))]
		       (e 10)))
(test 'second 'local (local ((define x 10) (define u 'second)) (cons x 1) u))
(test-values '(4 6) (lambda () (local ((define y 6) (define x 4)) (values x y))))
(test 10 'local (let ([x 10]) (local ((define y (lambda () x))) (define x 5) (y))))
(test 5 'local (let ([x 10]) (local ((define y (lambda () x))) (define x 5) x)))
(test 8 'local (let ([lambda 9]) (local [(define lambda 8)] lambda)))
(test 9 'local (let ([lambda 10]) (local [(define lambda 9) (define lambda2 lambda)] lambda2)))
(test 19 'local (local [(define lambda 19) (define lambda2 lambda)] lambda2))
(test 1 'local (local ((define-values (a b c) (values 1 2 3))) a))
(test 1 (lambda () (local ((define-values (a b c) (values 1 2 3))) a)))
(test 8 'local (local [(define lambda 8)] lambda))
(test 12 'local (local [(define (f y) (add1 y))] (f 11)))
(test 120 'local (local [(define (f y) 'ignore-me (add1 y))] (f 119)))
(test 17 'local (local [(define-values (apple b) (values 12 17))] b))
(test 4 'local (local [(define-struct cons (car cdr))] (cons-car (make-cons 4 5))))
(test 40 'local (local [(define-struct (cons exn) (car cdr))] (cons-car (make-cons "" (current-continuation-marks) 40 50))))
(syntax-test #'(local))
(syntax-test #'(local . 1))
(syntax-test #'(local ()))
(syntax-test #'(local () . 1))
(syntax-test #'(local 1 1))
(syntax-test #'(local (1) 1))
(syntax-test #'(local (x) 1))
(syntax-test #'(local ((+ 1 2)) 1))
(syntax-test #'(local ((define x)) 1))
(syntax-test #'(local ((define x 4) (+ 1 2)) 1))
(syntax-test #'(local ((define x 4) (+ 1 2) (define y 10)) 1))
(syntax-test #'(local ((define (x 8) 4)) 1))
(syntax-test #'(local ((define (x . 8) 4)) 1))
(syntax-test #'(local ((define x 8 4)) 1))
(syntax-test #'(local ((define 1 8 4)) 1))
(syntax-test #'(let ([define 10]) (local ((define x 4)) 10)))
(syntax-test #'(let ([define-values 10]) (local ((define-values (x) 4)) 10)))
(syntax-test #'(let ([define-struct 10]) (local ((define-struct x ())) 10)))

(define else #t) ;; `evcase' needs unbound `else' !!!! <------------------ WARNING

(for-each syntax-test 
	  (list #'(evcase)
		#'(evcase 1 (a))
		#'(evcase 1 (a b) a)
		#'(evcase 1 (a . b) a)
		#'(evcase 1 [else 5] [1 10])))
(define => 17)
(test (void) 'void-evcase (with-handlers ([(lambda (x) #t) (lambda (x) 17)]) (evcase 1)))
(test #t andmap (lambda (x) (= x 17))
      (list
       (evcase 3 [3 17])
       (evcase 3 [(+ 1 2) 17] [3 1])
       (evcase 3 [3 4 5 17])
       (evcase 3 [4 1] [3 4 5 17])
       (evcase 3 [4 1 2 3 4] [3 4 5 17])
       (evcase 3 [4 4] [2 10] [else 17])
       (let ([else 10]) (evcase 3 [4 4] [2 10] [else 15] [3 17]))
       (let ([else 3]) (evcase 3 [else 17] [2 14]))
       (if (eq? (void) (evcase 1)) 17 'bad)
       (evcase 3 [3 =>])
       (evcase 3 [3 => 17])
       (let ([=> 12]) (evcase 3 [3 => 17]))
       (let ([=> 17]) (evcase 3 [3 =>]))))

(require (only-in scheme/base else)) ; fix `else'

(define (opt-lam-test exp expected)
   (let ([got (eval exp)])
     (unless (equal? got expected)
       (printf  "FAILED test: ~a\n   expected: ~s\n        got: ~s\n"
                exp expected got))))

(define (opt-lam-test/bad exp expected)
   (let ([got (with-handlers ([exn:syntax?
                               (lambda (exn) (exn-message exn))])
                (cons 'got-result (eval exp)))])
     (unless (regexp-match expected got)
       (printf  "FAILED test: ~a\n   expected: ~s\n        got: ~s\n"
                exp expected got))))

(test 1 (opt-lambda (start) start) 1)
(test 1 (opt-lambda ([start 1]) start))
(test 1 (opt-lambda ([start 2]) start) 1)
(test '(1) (opt-lambda args args) 1)
(test '(1) (opt-lambda (x . args) args) 2 1)
(test '(2 1) (opt-lambda ([x 1] . args) (cons x args)) 2 1)
(test '(1) (opt-lambda ([x 1] . args) (cons x args)))
(test '(1 2 3) (opt-lambda ([x 1] . args) (cons x args)) 1 2 3)

(syntax-test #'(opt-lambda))
(syntax-test #'(opt-lambda 1 x))
(syntax-test #'(opt-lambda (x [x 1]) x))
(syntax-test #'(opt-lambda ([x 1] y) x))
(syntax-test #'(opt-lambda (1) x))
(syntax-test #'(opt-lambda ([2 1]) x))

(report-errs)

