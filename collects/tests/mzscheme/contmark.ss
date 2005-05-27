

(load-relative "loadtest.ss")

(require (lib "unit.ss"))

(SECTION 'continuation-marks)

(define (extract-current-continuation-marks key)
  (continuation-mark-set->list (current-continuation-marks) key))

(test null extract-current-continuation-marks 'key)

(syntax-test #'with-continuation-mark)
(syntax-test #'(with-continuation-mark))
(syntax-test #'(with-continuation-mark 1))
(syntax-test #'(with-continuation-mark 1))
(syntax-test #'(with-continuation-mark 1 2))
(syntax-test #'(with-continuation-mark 1 2 3 4))
(syntax-test #'(with-continuation-mark 1 2 3 . 4))

(test '(10) 'wcm (with-continuation-mark 'key 10 
		   (extract-current-continuation-marks 'key)))
(test '(#(10 #f)) 'wcm (with-continuation-mark 'key 10 
			 (continuation-mark-set->list* (current-continuation-marks) '(key no-key))))
(test '(#(#f 10)) 'wcm (with-continuation-mark 'key 10 
			 (continuation-mark-set->list* (current-continuation-marks) '(no-key key))))
(test '(#(nope 10)) 'wcm (with-continuation-mark 'key 10 
			   (continuation-mark-set->list* (current-continuation-marks) '(no-key key) 'nope)))

(test '(#(10 12)) 'wcm (with-continuation-mark 'key1 10 
			 (with-continuation-mark 'key2 12 
			   (continuation-mark-set->list* (current-continuation-marks) '(key1 key2)))))
(test '(#(#f 12) #(10 #f)) 'wcm 
      (with-continuation-mark 'key1 10 
	(let ([x (with-continuation-mark 'key2 12 
		   (continuation-mark-set->list* (current-continuation-marks) '(key1 key2)))])
	  (if (void? x)
	      x
	      x))))

(test '(11) 'wcm (with-continuation-mark 'key 10 
		   (with-continuation-mark 'key 11
		     (extract-current-continuation-marks 'key))))
(test '(9) 'wcm (with-continuation-mark 'key 10 
	     (with-continuation-mark 'key2 9
	       (with-continuation-mark 'key 11
		 (extract-current-continuation-marks 'key2)))))
(test '() 'wcm (with-continuation-mark 'key 10 
	    (with-continuation-mark 'key2 9
	      (with-continuation-mark 'key 11
		(extract-current-continuation-marks 'key3)))))

(test '() 'wcm (let ([x (with-continuation-mark 'key 10 (list 100))])
		 (extract-current-continuation-marks 'key)))

(test '(11) 'wcm (with-continuation-mark 'key 11
		   (let ([x (with-continuation-mark 'key 10 (extract-current-continuation-marks 'key))])
		     (extract-current-continuation-marks 'key))))

(test '((11) (10 11) (11)) 'wcm (with-continuation-mark 'key 11
				  (list (extract-current-continuation-marks 'key)
					(with-continuation-mark 'key 10 (extract-current-continuation-marks 'key))
					(extract-current-continuation-marks 'key))))

(test '(11) 'wcm-invoke/tail (with-continuation-mark 'x 10
			       (invoke-unit
				(unit 
				  (import)
				  (export)
				  
				  (with-continuation-mark 'x 11
				    (continuation-mark-set->list
				     (current-continuation-marks)
				     'x))))))

(test '(11 10) 'wcm-invoke/nontail (with-continuation-mark 'x 10
				     (invoke-unit
				      (unit 
					(import)
					(export)
					
					(define l (with-continuation-mark 'x 11
						    (continuation-mark-set->list
						     (current-continuation-marks)
						     'x)))
					l))))

(test '(11 10) 'wcm-begin0 (with-continuation-mark 'x 10
			     (begin0
			      (with-continuation-mark 'x 11
				(extract-current-continuation-marks 'x))
			      (+ 2 3))))
(test '(11 10) 'wcm-begin0/const (with-continuation-mark 'x 10
				   (begin0
				    (with-continuation-mark 'x 11
				      (extract-current-continuation-marks 'x))
				    'constant)))

;; full continuation, same thread
(test '(11 10) 'wcm-begin0 
      (let ([k (with-continuation-mark 'x 10
		 (begin0
		  (with-continuation-mark 'x 11
		    (let/cc k k))
		  (+ 2 3)))])
	(continuation-mark-set->list 
	 (continuation-marks k)
	 'x)))

;; full continuation, another thread
(test '(11 10) 'wcm-begin0 
      (let ([k (with-continuation-mark 'x 10
		 (begin0
		  (with-continuation-mark 'x 11
		    (let/cc k k))
		  (+ 2 3)))])
	(continuation-mark-set->list 
	 (let ([v #f])
	   (thread-wait (thread (lambda () 
				  (set! v (continuation-marks k)))))
	   v)
	 'x)))

;; escape continuation, same thread
(test '(11 10) 'wcm-begin0 
      (let ([m (with-continuation-mark 'x 10
		 (begin0
		  (with-continuation-mark 'x 11
		    (let/ec k 
		      (begin0
		       (with-continuation-mark 'x 12
			 (continuation-marks k))
		       (+ 17 7))))
		  (+ 2 3)))])
	(continuation-mark-set->list m 'x)))

;; escape continuation, another thread => not allowed
(test #f 'wcm-begin0 
      (with-continuation-mark 'x 10
	(let/ec k 
	  (with-continuation-mark 'x 12
	    (let ([v #f])
	      (thread-wait 
	       (thread (lambda () 
			 (set! v (continuation-marks k)))))
	      v)))))

;; escape continuation, dead
(err/rt-test (continuation-marks (let/ec k k)) exn:application:mismatch?)

(define (get-marks)
  (extract-current-continuation-marks 'key))

(define (tail-apply f)
  (with-continuation-mark 'key 'tail
    (f)))

(define (non-tail-apply f)
  (with-continuation-mark 'key 'non-tail
    (car (cons (f) null))))

(test '(tail) tail-apply get-marks)
(test '(non-tail) non-tail-apply get-marks)
(test '(tail non-tail) non-tail-apply (lambda () (tail-apply get-marks)))
(test '(non-tail) tail-apply (lambda () (non-tail-apply get-marks)))

(define (mark-x f)
  (lambda ()
    (with-continuation-mark 'key 'x (f))))

(test '(x) tail-apply (mark-x get-marks))
(test '(x non-tail) non-tail-apply (mark-x get-marks))

(test '(x) tail-apply (lambda () (tail-apply (mark-x get-marks))))
(test '(x non-tail non-tail) non-tail-apply (lambda () (non-tail-apply (mark-x get-marks))))
(test '(x non-tail) tail-apply (lambda () (non-tail-apply (mark-x get-marks))))
(test '(x non-tail) non-tail-apply (lambda () (tail-apply (mark-x get-marks))))

;; Make sure restoring continuations restores the marks:
(let ([l null])
 (let ([did-once? #f]
       [did-twice? #f]
       [try-again #f]
       [get-marks #f])
   
   (with-continuation-mark
    'key (let/cc k (set! try-again k) 1)
    (begin
      (unless did-once?
	(set! get-marks (let/cc k k)))
      (set! l (cons (extract-current-continuation-marks 'key) l))))

   (if did-once?
       (unless did-twice?
	 (set! did-twice? #t)
	 (get-marks #f))
       (begin
	 (set! did-once? #t)
	 (try-again 2))))

 (test '((1) (2) (1)) 'call/cc-restore-marks l))

;; Make sure restoring continuations restores the right marks
;;  for dynamic-wind thunks


(let* ([x (make-parameter 0)]
       [l null]
       [add (lambda (a b)
	      (set! l (append l (list (cons a b)))))]
       [cp #f])
  (let ([k (parameterize ([x 5])
	     (dynamic-wind
		 (lambda () (add 1 (x)))
		 (lambda () (parameterize ([x 6])
			      (let ([k+e (let/cc k (cons k void))])
				(set! cp (current-parameterization))
				(add 2 (x))
				((cdr k+e))
				(car k+e))))
		 (lambda () (add 3 (x)))))])
    (parameterize ([x 7])
      (let/cc esc
	(k (cons void esc)))))
  (test l values '((1 . 5) (2 . 6) (3 . 5) (1 . 5) (2 . 6) (3 . 5)))
  (test 6 call-with-parameterization cp (lambda () (x)))
  (test 0 call-with-parameterization (current-parameterization) (lambda () (x))))

(let* ([l null]
       [add (lambda (a b)
	      (set! l (append l (list (cons a b)))))]
       [x (lambda ()
	    (car (continuation-mark-set->list (current-continuation-marks) 
					      'x)))])
  (let ([k (with-continuation-mark 'x 5
	     (dynamic-wind
		 (lambda () (add 1 (x)))
		 (lambda () (with-continuation-mark 'x 6
			      (let ([k+e (let/cc k (cons k void))])
				(add 2 (x))
				((cdr k+e))
				(car k+e))))
		 (lambda () (add 3 (x)))))])
    (with-continuation-mark 'x 7
      (let/cc esc
	(k (cons void esc)))))
  (test l values '((1 . 5) (2 . 6) (3 . 5) (1 . 5) (2 . 6) (3 . 5))))

(let ([k0 #f]
      [k1 #f]
      [k2 #f]
      [k3 #f]
      [k4 #f]
      [k5 #f]
      [esc void])
  (define (go)
    (with-continuation-mark 'a 7
      (dynamic-wind
	  (lambda ()
	    ((let/cc k (set! k0 k) void))
	    (test '(7) extract-current-continuation-marks 'a))
	  (lambda ()
	    (with-continuation-mark 'a 8
	      (begin
		(test '(8 7) extract-current-continuation-marks 'a)
		((let/cc k (set! k1 k) void))
		(test '(8 7) extract-current-continuation-marks 'a)
		(dynamic-wind
		    (lambda ()
		      (test '(8 7) extract-current-continuation-marks 'a)
		      (with-continuation-mark 'a 9
			(begin
			  ((let/cc k (set! k2 k) void))
			  (test '(9 8 7) extract-current-continuation-marks 'a))))
		    (lambda ()
		      ((let/cc k (set! k3 k) void))
		      (test '(8 7) extract-current-continuation-marks 'a))
		    (lambda ()
		      (with-continuation-mark 'a 10
			(begin
			  ((let/cc k (set! k4 k) void))
			  (test '(10 8 7) extract-current-continuation-marks 'a)))
		      (test '(8 7) extract-current-continuation-marks 'a)))
		(test '(8 7) extract-current-continuation-marks 'a))))
	  (lambda ()
	    ((let/cc k (set! k5 k) void))
	    (test '(7) extract-current-continuation-marks 'a))))
    (esc))
  (go)
  (let ([k0 k0]
	[k1 k1]
	[k2 k2]
	[k3 k3]
	[k4 k4]
	[k5 k5])
    (let/cc k (set! esc k) (k1 void))
    (let/cc k (set! esc k) (k1 k))
    (let/cc k (set! esc k) (k2 void))
    (let/cc k (set! esc k) (k2 k))
    (let/cc k (set! esc k) (k3 void))
    (let/cc k (set! esc k) (k3 k))
    (let/cc k (set! esc k) (k4 void))
    (let/cc k (set! esc k) (k4 k))
    (let/cc k (set! esc k) (k5 void))
    (let/cc k (set! esc k) (k5 k))))

(test #t parameterization? (current-parameterization))
(test #f parameterization? (make-parameter 5))
(arity-test current-parameterization 0 0)
(arity-test call-with-parameterization 2 2)
(err/rt-test (call-with-parameterization 10 (lambda () 12)))
(err/rt-test (call-with-parameterization (current-parameterization) (lambda (x) 12)))

;; Create a deep stack with a deep mark stack

(define (p-equal? a b)
  (let loop ([a a][b b])
    (cond
     [(eq? a b) #t]
     [(equal? (car a) (car b))
      (loop (cdr a) (cdr b))]
     [else
      (printf "a: ~s~n" a)
      (printf "b: ~s~n" b)
      #f])))

(test #t
      'deep-stacks
      (p-equal?
       (let loop ([n 1000][l null])
	 (if (zero? n)
	     l
	     (loop (sub1 n) (cons n l))))
       (let loop ([n 1000])
	 (if (zero? n)
	     (extract-current-continuation-marks 'x)
	     (let ([x (with-continuation-mark 'x n (loop (sub1 n)))])
	       x)))))

;; Create a deep mark stack 10 times
(let loop ([n 10])
  (unless (zero? n)
    (let* ([max 1000]
	   [r (add1 (random max))])
      (test (list 0 r)
	    `(loop ,n)
	    (with-continuation-mark 'base 0
	      (let loop ([n max])
		(if (zero? n)
		    (append
		     (extract-current-continuation-marks 'base)
		     (extract-current-continuation-marks r))
		    (with-continuation-mark n n
		      (loop (sub1 n))))))))
    (loop (sub1 n))))

;; Make sure marks are separate in separate threads
(let ([s1 (make-semaphore 0)]
      [s2 (make-semaphore 0)]
      [result null])
  (thread (lambda ()
	    (with-continuation-mark 'key 'b.1
	      (begin
		(semaphore-wait s1)
		(with-continuation-mark 'key 'b.2
		  (begin
		    (semaphore-post s2)
		    (semaphore-wait s1)
		    (with-continuation-mark 'key 'b.4
		      (begin
			(set! result (extract-current-continuation-marks 'key))
			(semaphore-post s2)))
		    'ok))
		'ok))))
  (thread-wait
   (thread (lambda ()
	     (with-continuation-mark 'key 'a.1
	       (begin
		 (semaphore-post s1)
		 (with-continuation-mark 'key 'a.2
		   (begin
		     (semaphore-wait s2)
		     (with-continuation-mark 'key 'a.3
		       (begin
			 (semaphore-post s1)
			 (with-continuation-mark 'key 'a.4
			   (begin
			     (semaphore-wait s2)
			     (set! result (append (extract-current-continuation-marks 'key) result))))
			 'ok))
		     'ok))
		 'ok)))))
  (test '(a.4 a.3 a.2 a.1 b.4 b.2 b.1) 'thread-marks result))

(arity-test current-continuation-marks 0 0)
(arity-test continuation-mark-set->list 2 2)
(arity-test continuation-mark-set? 1 1)

(err/rt-test (continuation-mark-set->list 5 1))

(test #f continuation-mark-set? 5)
(test #t continuation-mark-set? (current-continuation-marks))

(let ([c #f]
      [l null])
  (thread-wait
   (thread (lambda ()
	     (dynamic-wind
		 (lambda () (collect-garbage))
		 (lambda ()
		   (let-values ([(a b) (let/cc k 
					 (set! c k)
					 (values 1 2))])
		     (set! l (append l (list a b)))))
		 void))))
  (thread-wait
   (thread (lambda ()
	     (c 4 5))))
  (test '(1 2 4 5) values l))

  
(report-errs)
