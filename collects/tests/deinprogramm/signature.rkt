#lang scheme/base

(provide all-signature-tests)

(require rackunit
	 deinprogramm/define-record-procedures
	 deinprogramm/signature/signature
	 deinprogramm/signature/signature-syntax)

(require scheme/promise)

(define integer (make-predicate-signature 'integer integer? 'integer-marker))
(define boolean (make-predicate-signature 'boolean boolean? 'boolean-marker))
(define %a (make-type-variable-signature 'a 'a-marker))
(define %b (make-type-variable-signature 'b 'b-marker))

(define-syntax say-no
  (syntax-rules ()
    ((say-no ?body ...)
     (let/ec exit
       (call-with-signature-violation-proc
	(lambda (obj signature message blame)
	  (exit 'no))
	(lambda ()
	  ?body ...))))))

(define-syntax failed-signature
  (syntax-rules ()
    ((say-no ?body ...)
     (let/ec exit
       (call-with-signature-violation-proc
	(lambda (obj signature message blame)
	  (exit signature))
	(lambda ()
	  ?body ...))))))

(define signature-tests
  (test-suite
   "Tests for signature combinators"

   (test-case
    "flat"
    (check-equal? (say-no (apply-signature integer 5)) 5)
    (check-equal? (say-no (apply-signature integer "foo")) 'no))

   (test-case
    "list"
    (define integer-list (make-list-signature 'integer-list integer #f))
    (check-equal? (say-no (apply-signature integer-list '(1 2 3)))
		  '(1 2 3))
    (check-equal? (say-no (apply-signature integer-list '#f))
		  'no)
    (check-eq? (failed-signature (apply-signature integer-list '(1 #f 3)))
	       integer))

   (test-case
    "list-cached"
    (define integer-list (make-list-signature 'integer-list integer #f))
    (define boolean-list (make-list-signature 'integer-list boolean #f))
    (define l '(1 2 3))
    (define foo "foo")
    (define no '(1 #f 3))
    (define no2 '(1 #f 3))
    (define integer-list->bool (make-procedure-signature 'integer-list->bool (list integer-list) boolean 'int->bool-marker))

    (check-equal? (say-no (apply-signature integer-list l))
		  '(1 2 3))
    (check-equal? (say-no (apply-signature integer-list l))
		  '(1 2 3))
    (check-equal? (say-no (apply-signature boolean-list l))
		  'no)
    (check-equal? (say-no (apply-signature integer-list foo))
		  'no)
    (check-equal? (say-no (apply-signature integer-list foo))
		  'no)
    (check-eq? (failed-signature (apply-signature integer-list no))
	       integer)
    (check-eq? (failed-signature (apply-signature integer-list no))
	       integer)

    (let ((proc (say-no (apply-signature integer-list->bool (lambda (l) (even? (car l)))))))
      (check-equal? (say-no (proc no)) 'no)
      (check-equal? (say-no (proc no)) 'no)
      (check-equal? (say-no (proc no2)) 'no)
      (check-equal? (say-no (proc no2)) 'no))
    )
 
   (test-case
    "mixed"
    (define int-or-bool (make-mixed-signature 'int-or-bool
					     (list integer
						   boolean)
					     'int-or-bool-marker))
    (check-equal? (say-no (apply-signature int-or-bool #f))
		  #f)
    (check-equal? (say-no (apply-signature int-or-bool 17))
		  17)
    (check-equal? (say-no (apply-signature int-or-bool "foo"))
		  'no))

   (test-case
    "combined"
    (define octet (make-combined-signature
		   'octet
		   (list
		    integer
		    (make-predicate-signature '<256
					     (delay (lambda (x)
						      (< x 256)))
					     '<256-marker)
		    (make-predicate-signature 'non-negative
					     (delay (lambda (x)
						      (>= x 0)))
					     'non-negative-marker))
		   'octet-marker))
    (check-equal? (say-no (apply-signature octet #f))
		  'no)
    (check-equal? (say-no (apply-signature octet 17))
		  17)
    (check-equal? (say-no (apply-signature octet 0))
		  0)
    (check-equal? (say-no (apply-signature octet -1))
		  'no)
    (check-equal? (say-no (apply-signature octet 255))
		  255)
    (check-equal? (say-no (apply-signature octet 256))
		  'no)
    (check-equal? (say-no (apply-signature octet "foo"))
		  'no))

   (test-case
    "case"
    (define foo-or-bar (make-case-signature 'foo-or-bar '("foo" "bar") equal? 'foo-or-bar-marker))
    (check-equal? (say-no (apply-signature foo-or-bar #f))
		  'no)
    (check-equal? (say-no (apply-signature foo-or-bar "foo"))
		    "foo")
    (check-equal? (say-no (apply-signature foo-or-bar "bar"))
		  "bar"))

   (test-case
    "procedure"
    (define int->bool (make-procedure-signature 'int->bool (list integer) boolean 'int->bool-marker))
    (check-equal? (say-no (apply-signature int->bool #f))
		  'no)
    (check-equal? (say-no (apply-signature int->bool (lambda () "foo")))
		  'no)
    (check-equal? (say-no (apply-signature int->bool (lambda (x y) "foo")))
		  'no)
    (let ((proc (say-no (apply-signature int->bool (lambda (x) (odd? x))))))
      (check-pred procedure? proc)
      (check-equal? (proc 15) #t)
      (check-equal? (proc 16) #f)
      (check-equal? (say-no (proc "foo")) 'no))
    (let ((proc (say-no (apply-signature int->bool (lambda (x) (+ x 1))))))
      (check-equal? (say-no (proc 12)) 'no)))

   (test-case
    "type variable - simple"
    (check-equal? (say-no (apply-signature %a #f)) #f)
    (check-equal? (say-no (apply-signature %a 15)) 15))
   
   (test-case
    "type variable - list"
    (define a-list (make-list-signature 'a-list %a #f))
    (check-equal? (say-no (apply-signature a-list '(1 2 3)))
		  '(1 2 3))
    (check-equal? (say-no (apply-signature a-list '#f))
		  'no)
    (check-equal? (say-no (apply-signature a-list '(#f "foo" 5)))
		  '(#f "foo" 5)))

   (test-case
    "apply-signature/blame"
    (define int->bool (make-procedure-signature 'int->bool (list integer) boolean 'int->bool-marker))
    (let ((proc (say-no (apply-signature/blame int->bool (lambda (x) (odd? x))))))
      (check-pred procedure? proc)
      (check-equal? (proc 15) #t)
      (check-equal? (proc 16) #f)
      (check-equal? (say-no (proc "foo")) 'no))
    (let ((proc (say-no (apply-signature/blame int->bool (lambda (x) x)))))
      (call-with-signature-violation-proc
       (lambda (obj signature message blame)
	 (check-true (syntax? blame)))
       (lambda ()
	 (proc 5)))))
   ))

(define signature-syntax-tests
  (test-suite
   "Tests for signature syntax"

   (test-case
    "predicate"
    (define integer (signature (predicate integer?)))
    (check-equal? (say-no (apply-signature integer 5)) 5)
    (check-equal? (say-no (apply-signature integer "foo")) 'no))

   (test-case
    "list"
    (check-equal? (say-no (apply-signature (signature x (list %a)) 5)) 'no)
    (check-equal? (say-no (apply-signature (signature x (list %a)) '(1 2 3))) '(1 2 3))
    (check-equal? (say-no (apply-signature (signature x (list (predicate integer?))) '(1 2 3))) '(1 2 3))
    (check-equal? (say-no (apply-signature (signature x (list (predicate integer?))) '(1 #f 3))) 'no))

   (test-case
    "mixed"
    (define int-or-bool (signature (mixed integer boolean)))
    (check-equal? (say-no (apply-signature int-or-bool #f))
		  #f)
    (check-equal? (say-no (apply-signature int-or-bool 17))
		  17)
    (check-equal? (say-no (apply-signature int-or-bool "foo"))
		  'no))

   (test-case
    "combined"
    (define octet (signature (combined integer
				      (predicate (lambda (x)
						   (< x 256)))
				      (predicate (lambda (x)
						   (>= x 0))))))
    (check-equal? (say-no (apply-signature octet #f))
		  'no)
    (check-equal? (say-no (apply-signature octet 17))
		  17)
    (check-equal? (say-no (apply-signature octet 0))
		  0)
    (check-equal? (say-no (apply-signature octet -1))
		  'no)
    (check-equal? (say-no (apply-signature octet 255))
		  255)
    (check-equal? (say-no (apply-signature octet 256))
		  'no)
    (check-equal? (say-no (apply-signature octet "foo"))
		  'no))

   (test-case
    "procedure"
    (define int->bool (signature int->bool ((predicate integer?) -> (predicate boolean?))))
    (check-equal? (say-no (apply-signature int->bool #f))
		  'no)
    (check-equal? (say-no (apply-signature int->bool (lambda () "foo")))
		  'no)
    (check-equal? (say-no (apply-signature int->bool (lambda (x y) "foo")))
		  'no)
    (let ((proc (say-no (apply-signature int->bool (lambda (x) (odd? x))))))
      (check-pred procedure? proc)
      (check-equal? (proc 15) #t)
      (check-equal? (proc 16) #f)
      (check-equal? (say-no (proc "foo")) 'no))
    (let ((proc (say-no (apply-signature int->bool (lambda (x) (+ x 1))))))
      (check-equal? (say-no (proc 12)) 'no))) 


   (test-case
    "record-wrap"
    (define-record-procedures-parametric pare pare-of kons pare? (kar kdr))
    (define ctr (pare-of integer boolean))
    (let ((obj (apply-signature ctr (kons 1 #t))))
      (check-equal? (kar obj) 1)
      (check-equal? (kdr obj) #t))
    (let ((obj (apply-signature ctr (kons 1 2))))
      (check-equal? (say-no (kar obj)) 'no))
    )

   (test-case
    "record-wrap-2"
    (let ((count 0))
      (define counting-integer
	(make-predicate-signature 'counting-integer 
				 (lambda (obj)
				   (set! count (+ 1 count))
				   (integer? obj))
				 'integer-marker))
      (define-record-procedures-parametric pare pare-of kons pare? (kar kdr))
      (define ctr (signature (pare-of counting-integer boolean)))
      (let ((obj (apply-signature ctr (apply-signature ctr (kons 1 #t)))))
	(check-equal? count 0)
	(check-equal? (kar obj) 1)
	(check-equal? count 1)
	(check-equal? (kdr obj) #t)
	(check-equal? count 1))))


   (test-case
    "record-wrap-2"
    (let ((count 0))
      (define counting-integer
	(make-predicate-signature 'counting-integer 
				 (lambda (obj)
				   (set! count (+ 1 count))
				   (integer? obj))
				 'integer-marker))
      (define-record-procedures-parametric pare pare-of kons pare? (kar kdr))
      (define ctr (signature (pare-of counting-integer boolean)))
      (let ((obj (apply-signature ctr (apply-signature ctr (kons 1 #t)))))
	(check-equal? count 0)
	(check-equal? (kar obj) 1)
	(check-equal? count 1)
	(check-equal? (kdr obj) #t)
	(check-equal? count 1)
	;; after checking, the system should remember that it did so
	(let ((obj-2 (apply-signature ctr obj)))
	  (check-equal? count 1)
	  (check-equal? (kar obj) 1)
	  (check-equal? count 1)
	  (check-equal? (kdr obj) #t)
	  (check-equal? count 1)))))

   (test-case
    "double-wrap"
    (let ((count 0))
      (define counting-integer
	(make-predicate-signature 'counting-integer 
				 (lambda (obj)
				   (set! count (+ 1 count))
				   (integer? obj))
				 'integer-marker))
      (define-record-procedures-parametric pare pare-of raw-kons pare? (kar kdr))

      (define empty-list (signature (predicate null?)))

      (define list-of
	(lambda (x)
	  (signature (mixed empty-list
			   (pare-of x (list-of x))))))
      
      (define/signature kons (signature (%a (list-of %a) -> (pare-of %a (list-of %a))))
	raw-kons)
      
      (define/signature build-list (signature (integer -> (list-of counting-integer)))
	(lambda (n)
	  (if (= n 0)
	      '()
	      (kons n (build-list (- n 1))))))

      (define/signature list-length (signature ((list-of counting-integer) -> integer))
	(lambda (lis)
	  (cond
	   ((null? lis) 0)
	   ((pare? lis)
	    (+ 1 (list-length (kdr lis)))))))
      
      ;; one wrap each for (list-of %a), one for (list-of counting-integer)
      (let  ((l1 (build-list 10)))
	(check-equal? count 0)
	(let ((len1 (list-length l1)))
	  (check-equal? count 10)))))

   (test-case
    "wrap equality"
    (define-record-procedures-parametric pare pare-of raw-kons pare? (kar kdr))
    
    (define empty-list (signature (predicate null?)))
    
    (define list-of
      (lambda (x)
	(signature (mixed empty-list
			 (pare-of x (list-of x))))))
    
    (define/signature kons (signature (%a (list-of %a) -> (pare-of %a (list-of %a))))
      raw-kons)

    (check-equal? (raw-kons 1 '()) (raw-kons 1 '()))
    (check-equal? (kons 1 '()) (kons 1 '()))
    (check-equal? (kons 1 '()) (raw-kons 1 '()))
    (check-equal? (raw-kons 1 '()) (kons 1 '())))
      


))


(define all-signature-tests
  (test-suite
   "all-signature-tests"
   signature-tests
   signature-syntax-tests))
