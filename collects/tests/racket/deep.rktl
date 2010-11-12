
(load-relative "loadtest.rktl")

(Section 'deep)

; Test deep stacks

(define (nontail-loop n esc)
  (let loop ([n n])
    (if (zero? n)
	(esc 0)
	(sub1 (loop (sub1 n))))))

(define proc-depth (find-depth (lambda (n) (nontail-loop n (lambda (x) x)))))
(printf "non-tail loop overflows at ~a\n" proc-depth)
  
(test (- proc-depth) 'deep-recursion (nontail-loop proc-depth (lambda (x) x)))

(test 0 'deep-recursion-escape/ec
      (let/ec k
	(nontail-loop proc-depth k)))

(test 0 'deep-recursion-escape/cc
      (let/cc k
	(nontail-loop proc-depth k)))

(test 0 'deep-recursion-resume/escape
      ((let/ec k
	 (nontail-loop proc-depth
		       (lambda (v)
			 (let/cc inside
			   (k inside))
			 (k (lambda () 0)))))))

(test 0 'deep-recursion-resume/abort
      (call-with-continuation-prompt
       (lambda ()
	 (nontail-loop proc-depth
		       (lambda (v)
                         (abort-current-continuation
                          (default-continuation-prompt-tag)
                          (lambda () 0)))))))

(test 0 'deep-recursion-resume/escape/thread
      (let ([v #f])
	(thread-wait
	 (thread 
	  (lambda ()
	    (set! v 
		  ((let/ec k
		     (nontail-loop proc-depth
				   (lambda (v)
				     (let/cc inside
				       (k (lambda () 
					    (thread-wait (thread inside))
					    0)))
				     (k (lambda () 0))))))))))
	v))

(test (- proc-depth) 'deep-recursion-resume
      ((lambda (x) (if (procedure? x) (x) x))
       (let/ec k
	 (nontail-loop proc-depth
		       (lambda (v)
			 (let/cc inside
			   (k inside))
			 0)))))

(define (read-deep depth)
  (define paren-port
    (let* ([depth depth]
	   [closing? #f]
	   [count depth])
      (make-input-port
       'name
       (lambda (s)
	 (bytes-set!
	  s
	  0
	  (cond
	   [closing?
	    (if (= count depth)
		eof
		(begin
		  (set! count (add1 count))
		  (char->integer #\)) ))]
	   [else
	    (set! count (sub1 count))
	    (when (zero? count)
	      (set! closing? #t))
	    (char->integer #\()]))
	 1)
       #f
       void)))
  (read paren-port))

(define read-depth (find-depth read-deep))
(printf "nested paren read overflows at ~a\n" read-depth)

(define deep-list (read-deep read-depth))

(test #t 'read-deep (pair? deep-list))

(define s (open-output-string))
(display deep-list s)
(test 'ok 'display 'ok)

(test #t 'equal? (equal? deep-list (read (open-input-string (get-output-string s)))))

(define (try-pairs mk-a mk-d cons)
  (let ()
    (define l1 (mk-a 0))
    (define l2 (mk-a 0))
    (define l3 (mk-a 1))
    (test #t equal? l1 l2)
    (test #f equal? l1 l3)
    (test #t equal? (cons l1 l3) (cons l2 l3))
    (test #f equal? (cons l1 l2) (cons l2 l3))
    (test #t = (equal-hash-code l1) (equal-hash-code l2)))

  (let ()
    (define l1 (mk-d #f))
    (define l2 (mk-d #f))
    (define l3 (mk-d #t))
    (test #t equal? l1 l2)
    (test #f equal? l1 l3)
    (test #t equal? (cons l1 l3) (cons l2 l3))
    (test #f equal? (cons l1 l2) (cons l2 l3))
    (test #t = (equal-hash-code l1) (equal-hash-code l2))))

(try-pairs (lambda (v)
             (read (open-input-string (format "#0=(cons ~a #0#)" v))))
           (lambda (v)
             (read (open-input-string (format "#0=(cons #0# ~a)" v))))
           cons)

(try-pairs (lambda (v)
             (let ([p (mcons v v)])
               (set-mcdr! p p)
               p))
           (lambda (v)
             (let ([p (mcons v v)])
               (set-mcar! p p)
               p))
           mcons)

(define (vec-test i)
  (define l1 (vector 0 0))
  (define l2 (vector 0 0))
  (define l3 (vector 1 1))
  (vector-set! l1 i l1)
  (vector-set! l2 i l2)
  (vector-set! l3 i l3)
  (test #t equal? l1 l2)
  (test #f equal? l1 l3)
  (test #t equal? (vector l1 l3) (vector l2 l3))
  (test #f equal? (vector l1 l2) (vector l2 l3))
  (test #t = (equal-hash-code l1) (equal-hash-code l2)))
(vec-test 0)
(vec-test 1)

(define-struct a (b c) #:inspector (make-inspector) #:mutable)
(define l1 (make-a 0 #f))
(set-a-b! l1 l1)
(define l2 (make-a 0 #f))
(set-a-b! l2 l2)
(define l3 (make-a 0 #t))
(set-a-b! l3 l2)
(test #t equal? l1 l2)
(test #f equal? l1 l3)
(test #t equal? (make-a l1 l3) (make-a l2 l3))
(test #f equal? (make-a l1 l2) (make-a l2 l3))
(test #t = (equal-hash-code l1) (equal-hash-code l2))

(define l1 (box 0))
(set-box! l1 l1)
(define l2 (box 0))
(set-box! l2 l2)
(test #t equal? l1 l2)
(test #t = (equal-hash-code l1) (equal-hash-code l2))

;; ----------------------------------------
;; Overflow in hashing:

(define (hash-deep n)
  (let loop ([n n][a null])
    (if (zero? n)
	a
	(loop (sub1 n) (list a "apple")))))

(define (init-hash-table ht)
  (let loop ([n 25])
    (unless (zero? n)
      (hash-set! ht (gensym) (gensym))
      (loop (sub1 n)))))

(define hash-depth 
  (let ([ht (make-hash)])
    (init-hash-table ht)
    (find-depth 
     (lambda (n) 
       (nontail-loop (quotient proc-depth 3) 
		     (lambda (x)
		       (hash-set! ht
                                  (hash-deep n)
                                  #t)
		       x))))))
(printf "hashing overflows at ~a\n" hash-depth)

(define (try-deep-hash hash-depth put-depth get-depth)
  (let* ([ht (make-hash)]
	 [val (gensym)]
	 [key (hash-deep hash-depth)]
	 [code (equal-hash-code key)])
    
    (init-hash-table ht)
    (nontail-loop put-depth
		  (lambda (x) 
		    (test code 'code (equal-hash-code key))
		    (hash-set! ht key val) 
		    x))
    (nontail-loop get-depth
		  (lambda (x)
		    (test code 'code (equal-hash-code key))
		    (test val 'deep-hash (hash-ref ht key))
		    x))))

(for-each (lambda (hash-depth)
	    (for-each (lambda (proc-depth)
			(try-deep-hash hash-depth 0 proc-depth))
		      (list 0
			    (quotient proc-depth 2)
			    (quotient proc-depth 3)
			    (quotient proc-depth 4))))
	  (list hash-depth
		(* 2 hash-depth)
		(quotient hash-depth 2)))
		      

;; ----------------------------------------

(report-errs)
