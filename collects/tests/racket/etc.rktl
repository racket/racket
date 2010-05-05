
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

(report-errs)

