
(load-relative "loadtest.rktl")

(Section 'etc)

(require mzlib/etc)

(let () 
  (begin-with-definitions 
   (define-syntax (goo stx) 
     (syntax-case stx () 
       [(_ foo) #'(define-syntax (foo stx) (syntax-case stx () [(_ x) #'(define x 12)]))]))
   (goo foo)
   (foo x)
   (test 12 'bwd x)))

(let-syntax ([goo (lambda (stx) #'(begin (define z 13) (test 13 'bwd z)))])
  (let-syntax ([y (lambda (stx) #'goo)])
    (let () 
      (begin-with-definitions 
       (define-syntax (goo stx) 
	 (syntax-case stx () 
	   [(_ foo) #'(define-syntax (foo stx) (syntax-case stx () [(_ x) #'(define x 12)]))]))
       (goo foo)
       (foo x)
       y
       (test 12 'bwd x)))))

(let ()
  (begin-with-definitions 
   (define-struct a (b c))
   (test 2 'bwd (a-c (make-a 1 2)))))

(let ()
  (begin-with-definitions 
   (define-struct a (b c))
   (let ()
     (define-struct (d a) (e))
     (test 3 'bwd (d-e (make-d 1 2 3))))))

(let ()
  (begin-with-definitions 
   (define-struct a (b c))
   (define-struct (d a) (e))
   (test 3 'bwd (d-e (make-d 1 2 3)))))

(syntax-test #'(begin-with-definitions
		 (define-syntax goo 10)
		 (define goo 10)
		 12))

(let ()
  (test 3 (rec f (λ (x) 3)) 3)
  (test 3 (rec f (λ (x) x)) 3)
  (test 2 (rec f (λ (x) (if (= x 3) (f 2) x))) 3)
  (test 3 (rec (f x) 3) 3)
  (test 3 (rec (f x) x) 3)
  (test 2 (rec (f x) (if (= x 3) (f 2) x)) 3)
  (test 2 (rec (f x . y) (car y)) 1 2 3)
  (test 2 'no-duplications
        (let ([x 1]) (rec ignored (begin (set! x (+ x 1)) void)) x))
  (test 'f object-name (rec (f x) x))
  (test 'f object-name (rec (f x . y) x))
  (test 'f object-name (rec  f (lambda (x) x)))
  (test (list 2) (rec (f . x) (if (= (car x) 3) (f 2) x)) 3))


(report-errs)

