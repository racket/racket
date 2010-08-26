

(load-relative "loadtest.rktl")

(Section 'continuation-marks)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

(define (wcm f) (f))
(define (wcm-in-barrier f)
  (let loop ([n 100])
    (if (zero? n)
	(call-with-continuation-barrier
	 (lambda ()
	   (f)))
	(with-continuation-mark
	    'something-else
	    #t
	  (let ([v (loop (sub1 n))])
	    (if (and (number? v)
		     (zero? v))
		0
		v))))))

;; Test with an without wrapping `lambda', mainly to
;;  test JIT interactions.
;; Also test inside a continuation barrier.
(define-syntax wcm-test
  (syntax-rules (lambda)
    [(_ expect orig)
     (begin 
       (test expect wcm orig)
       (test expect 'wcm (orig))
       (test expect wcm-in-barrier orig))]))
	    

(wcm-test '(10) (lambda ()
		  (with-continuation-mark 'key 10 
		    (extract-current-continuation-marks 'key))))
(wcm-test '(#(10 #f)) (lambda ()
			(with-continuation-mark 'key 10 
			  (continuation-mark-set->list* (current-continuation-marks) '(key no-key)))))
(wcm-test '(#(#f 10)) (lambda ()
			(with-continuation-mark 'key 10 
			  (continuation-mark-set->list* (current-continuation-marks) '(no-key key)))))
(wcm-test '(#(nope 10)) (lambda ()
			  (with-continuation-mark 'key 10 
			    (continuation-mark-set->list* (current-continuation-marks) '(no-key key) 'nope))))

(wcm-test '(#(10 12)) (lambda ()
			(with-continuation-mark 'key1 10 
			  (with-continuation-mark 'key2 12 
			    (continuation-mark-set->list* (current-continuation-marks) '(key1 key2))))))
(wcm-test '(#(#f 12) #(10 #f)) 
	  (lambda ()
	    (with-continuation-mark 'key1 10 
	      (let ([x (with-continuation-mark 'key2 12 
			 (continuation-mark-set->list* (current-continuation-marks) '(key1 key2)))])
		(if (void? x)
		    x
		    x)))))

(wcm-test '(11) (lambda ()
		  (with-continuation-mark 'key 10 
		    (with-continuation-mark 'key 11
		      (extract-current-continuation-marks 'key)))))
(wcm-test '(9) (lambda () (with-continuation-mark 'key 10 
			    (with-continuation-mark 'key2 9
			      (with-continuation-mark 'key 11
				(extract-current-continuation-marks 'key2))))))
(wcm-test '() (lambda () (with-continuation-mark 'key 10 
			   (with-continuation-mark 'key2 9
			     (with-continuation-mark 'key 11
			       (extract-current-continuation-marks 'key3))))))

(wcm-test '() (lambda ()
		(let ([x (with-continuation-mark 'key 10 (list 100))])
		  (extract-current-continuation-marks 'key))))

(wcm-test '(11) (lambda ()
		  (with-continuation-mark 'key 11
		    (let ([x (with-continuation-mark 'key 10 (extract-current-continuation-marks 'key))])
		      (extract-current-continuation-marks 'key)))))

(wcm-test '((11) (10 11) (11)) (lambda ()
				 (with-continuation-mark 'key 11
				   (list (extract-current-continuation-marks 'key)
					 (with-continuation-mark 'key 10 (extract-current-continuation-marks 'key))
					 (extract-current-continuation-marks 'key)))))

(require (lib "mzlib/unit200.rkt"))

;; Hide keywords from scheme/unit.rkt:
(define import #f)
(define export #f)
(define link #f)

(wcm-test '(11)
	  (lambda ()
	    (with-continuation-mark 'x 10
	      (invoke-unit
	       (unit 
		 (import)
		 (export)
		 
		 (with-continuation-mark 'x 11
		   (continuation-mark-set->list
		    (current-continuation-marks)
		    'x)))))))

(wcm-test '(11 10) 
	  (lambda ()
	    (with-continuation-mark 'x 10
	      (invoke-unit
	       (unit 
		 (import)
		 (export)
		 
		 (define l (with-continuation-mark 'x 11
			     (continuation-mark-set->list
			      (current-continuation-marks)
			      'x)))
		 l)))))

(wcm-test '(11 10) 
	  (lambda ()
	    (with-continuation-mark 'x 10
	      (begin0
	       (with-continuation-mark 'x 11
		 (extract-current-continuation-marks 'x))
	       (+ 2 3)))))
(wcm-test '(11 10) 
	  (lambda ()
	    (with-continuation-mark 'x 10
	      (begin0
	       (with-continuation-mark 'x 11
		 (extract-current-continuation-marks 'x))
	       'constant))))

;; full continuation, same thread
(wcm-test '(11 10)
	  (lambda ()
	    (let ([k (with-continuation-mark 'x 10
		       (begin0
			(with-continuation-mark 'x 11
			  (let/cc k k))
			(+ 2 3)))])
	      (continuation-mark-set->list 
	       (continuation-marks k)
	       'x))))

;; full continuation, another thread
(wcm-test '(11 10)
	  (lambda ()
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
	       'x))))

;; continuation, mark replaced
(let* ([extract
	(lambda (k)
	  (continuation-mark-set->list 
	   (continuation-marks k)
	   'x))]
       [go
	(lambda (call/xc in?)
	  (wcm-test '(11 10)
		    (lambda ()
		      (let ([k (with-continuation-mark 'x 10
				 (begin0
				  (with-continuation-mark 'x 11
				    (call/xc 
				     (lambda (k )
				       (with-continuation-mark 'x 12
					 (if in?
					     (extract k)
					     k)))))
				  (+ 2 3)))])
			(if in?
			    k
			    (extract k))))))])
  (go call/cc #t)
  (go call/cc #f)
  (go call/ec #t))

;; nested continuation, mark replaced
(let* ([extract
	(lambda (k)
	  (continuation-mark-set->list 
	   (continuation-marks k)
	   'x))]
       [go
	(lambda (call/xc in? is-tail?)
	  (wcm-test (if is-tail?
                        '(12 10)
                        '(12 11 10))
		    (lambda ()
		      (let ([k (with-continuation-mark 'x 10
				 (begin0
				  (with-continuation-mark 'x 11
				    (call/xc
				     (lambda (k0)
				       (with-continuation-mark 'x 12
					 (call/xc 
					  (lambda (k) 
					    (if in?
						(extract k)
						k)))))))
				  (+ 2 3)))])
			(if in?
			    k
			    (extract k))))))])
  (go call/cc #t #t)
  (go call/cc #f #t)
  (go call/ec #t #f))

;; nested continuation, mark shared
(let* ([extract
	(lambda (k)
	  (continuation-mark-set->list 
	   (continuation-marks k)
	   'x))]
       [go
	(lambda (call/xc in?)
	  (wcm-test '(12 11 10)
		    (lambda ()
		      (let ([k (with-continuation-mark 'x 10
				 (begin0
				  (with-continuation-mark 'x 11
				    (call/xc 
				     (lambda (k0)
				       (begin0
					(with-continuation-mark 'x 12
					  (call/xc 
					   (lambda (k)
					     (if in?
						 (extract k)
						 k))))
					(cons 4 5)))))
				  (cons 2 3)))])
			(if in?
			    k
			    (extract k))))))])
  (go call/cc #t)
  (go call/cc #f)
  (go call/ec #t))

;; escape continuation, same thread
(wcm-test '(11 10)
	  (lambda ()
	    (let ([m (with-continuation-mark 'x 10
		       (begin0
			(with-continuation-mark 'x 11
			  (let/ec k 
			    (begin0
			     (with-continuation-mark 'x 12
			       (continuation-marks k))
			     (+ 17 7))))
			(+ 2 3)))])
	      (continuation-mark-set->list m 'x))))

;; escape continuation, another thread => not allowed
(wcm-test #f
	  (lambda ()
	    (with-continuation-mark 'x 10
	      (let/ec k 
		(with-continuation-mark 'x 12
		  (let ([v #f])
		    (thread-wait 
		     (thread (lambda () 
			       (set! v (continuation-marks k)))))
		    v))))))

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
      (printf "a: ~s\n" a)
      (printf "b: ~s\n" b)
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

(arity-test current-continuation-marks 0 1)
(arity-test continuation-mark-set->list 2 3)
(arity-test continuation-mark-set->list* 2 4)
(arity-test continuation-mark-set-first 2 4)
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

  
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Try to test internal caching strategies

(let ([ta (make-continuation-prompt-tag 'a)]
      [tb (make-continuation-prompt-tag 'b)]
      [tesc (make-continuation-prompt-tag 'esc)]
      [default-prompt-tag (default-continuation-prompt-tag)])
  (let ([mk-first
         (lambda (key tag val)
           (lambda ()
             (test val continuation-mark-set-first #f key tag)))]
        [mk-all
         (lambda (key tag vals)
           (lambda ()
             (test vals continuation-mark-set->list
                   (current-continuation-marks tag)
                   key
                   tag)))]
        [deeper (lambda (thunk)
                  (let loop ([n 32])
                    (if (zero? n)
                        (thunk)
                        (values (loop (sub1 n))))))])
    (let ([checks
           (list (mk-first 'a ta 'a3)
                 (mk-all 'a ta '(a3 a2 a1))
                 (mk-first 'a tb 'a3)
                 (mk-all 'a tb '(a3 a2))
                 (mk-first 'b ta 'b2)
                 (mk-all 'b ta '(b2 b1))
                 (mk-first 'b tb 'b2)
                 (mk-all 'b tb '(b2))
                 (mk-first 'a default-prompt-tag 'a3)
                 (mk-all 'a default-prompt-tag '(a3 a2 a1 a0))
                 (mk-first 'b default-prompt-tag 'b2)
                 (mk-all 'b default-prompt-tag '(b2 b1 b0))
                 (lambda ()
                   ;; trigger a first lookup using the NULL tag
                   (call-with-continuation-prompt
                    (lambda ()
                      (call-with-exception-handler
                       (lambda (exn) (abort-current-continuation tesc void))
                       (lambda () (/ 0))))
                    tesc))
                 (lambda ()
                   ;; trigger a mark-list lookup using the NULL tag by chaining
                   ;; continuations
                   (call-with-continuation-prompt
                    (lambda ()
                      (call-with-exception-handler
                       (lambda (exn) (abort-current-continuation tesc void))
                       (lambda ()
                         (values
                          (call-with-exception-handler
                           (lambda (exn) 'again)
                           (lambda () (/ 0)))))))
                    tesc)))])
      (for-each
       (lambda (one)
         (for-each
          (lambda (two)
            (with-continuation-mark 'a 'a0
              (with-continuation-mark 'b 'b0
                (call-with-continuation-prompt
                 (lambda ()
                   (with-continuation-mark 'a 'a1
                     (with-continuation-mark 'b 'b1
                       (deeper
                        (lambda ()
                          (call-with-continuation-prompt
                           (lambda ()
                             (with-continuation-mark 'a 'a2
                               (deeper
                                (lambda ()
                                  (with-continuation-mark 'a 'a3
                                    (with-continuation-mark 'b 'b2
                                      (begin
                                        (one)
                                        (two))))))))
                           tb))))))
                 ta))))
          checks))
       checks))))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(module m-wcm_ mzscheme
  (provide m-wcm-go)
  (define (m-wcm-go test)
    (let ([v #f])

      (test
       (with-continuation-mark
           'x 'y
         (with-continuation-mark
             'x2 'y
             (let/cc k
           (with-continuation-mark
               'x3 'y
           (with-continuation-mark
               'x4 'y
           (with-continuation-mark
               'x5 'y
               (with-continuation-mark
                   'x 'y3
                 (list
                  ((let/cc k2
                     (set! v k2)
                     (lambda () '(y3)))))))))))))
       
      (v (lambda () 
           (set! v void)
           (continuation-mark-set->list
            (current-continuation-marks)
            'x))))))
(require 'm-wcm_)
(m-wcm-go (lambda (a) (test '((y3)) values a)))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(test #f call-with-immediate-continuation-mark 'x (lambda (v) v))
(test 10 call-with-immediate-continuation-mark 'x (lambda (v) v) 10)
(test 12 'cwicm (with-continuation-mark 'x 12 (call-with-immediate-continuation-mark 'x (lambda (v) v))))
(test '(#f) 'cwiwcm (with-continuation-mark 'x 12 (list (call-with-immediate-continuation-mark 'x (lambda (v) v)))))
(test 12 'cwicm (with-continuation-mark 'x 12 
                  (with-continuation-mark 'y 13
                    (call-with-immediate-continuation-mark 'x (lambda (v) v)))))
(test 13 'cwicm (with-continuation-mark 'x 12 
                  (with-continuation-mark 'y 13
                    (call-with-immediate-continuation-mark 'y (lambda (v) v)))))
(test 14 'cwicm (with-continuation-mark 'x 12 
                  (with-continuation-mark 'y 13
                    (with-continuation-mark 'x 14
                      (call-with-immediate-continuation-mark 'x (lambda (v) v))))))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Check a wcm in tail position of a wcm that is not in tail position,
;; because the JIT avoids checking for wcm mark replacement when it sees
;; a wcm that can't be in tail position with respect to any other wcm.

(test '(((2 10)))
      (lambda (x) (list (with-continuation-mark
                            'x (list 1 (x))
                          (with-continuation-mark
                              'x (list 2 (x))
                            (continuation-mark-set->list (current-continuation-marks) 'x)))))
      (lambda () 10))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(report-errs)
