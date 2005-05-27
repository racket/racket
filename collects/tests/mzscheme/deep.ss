
(load-relative "loadtest.ss")

(SECTION 'deep)

; Test deep stacks

(define (nontail-loop n esc)
  (let loop ([n n])
    (if (zero? n)
	(esc 0)
	(sub1 (loop (sub1 n))))))

(define proc-depth (find-depth (lambda (n) (nontail-loop n (lambda (x) x)))))
(printf "non-tail loop overflows at ~a~n" proc-depth)

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
(printf "nested paren read overflows at ~a~n" read-depth)

(define deep-list (read-deep read-depth))

(test #t 'read-deep (pair? deep-list))

(define s (open-output-string))
(display deep-list s)
(test 'ok 'display 'ok)

(test #t 'equal? (equal? deep-list (read (open-input-string (get-output-string s)))))

(define going? #t)
(define (equal?-forever l1 l2 deep?)
  (let ([t (thread (lambda () 
		     (equal? l1 l2) ; runs forever; could run out of memory
		     (set! going? #f)))]
	[v1 (make-vector 6)]
	[v2 (make-vector 6)])
    (if deep?
	(begin
	  (sleep)
	  (vector-set-performance-stats! v1)
	  (let loop ()
	    (sleep)
	    (vector-set-performance-stats! v2)
	    (unless (> (vector-ref v2 5)
		       (+ (vector-ref v1 5) 2))
	      (loop))))
	(sleep 0.3))
    (kill-thread t)
    going?))


(define l1 (cons 0 #f))
(set-cdr! l1 l1)
(define l2 (cons 0 #f))
(set-cdr! l2 l2)
(test #t 'equal?-forever (equal?-forever l1 l2 #f))

(define l1 (cons 0 #f))
(set-car! l1 l1)
(define l2 (cons 0 #f))
(set-car! l2 l2)
(test #t 'equal?-forever/memory (equal?-forever l1 l2 #t))

(define l1 (vector 0))
(vector-set! l1 0 l1)
(define l2 (vector 0))
(vector-set! l2 0 l2)
(test #t 'equal?-forever/vector (equal?-forever l1 l2 #t))

(define-struct a (b c) (make-inspector))
(define l1 (make-a 0 #f))
(set-a-b! l1 l1)
(define l2 (make-a 0 #f))
(set-a-b! l2 l2)
(test #t 'equal?-forever/struct (equal?-forever l1 l2 #t))

(define l1 (box 0))
(set-box! l1 l1)
(define l2 (box 0))
(set-box! l2 l2)
(test #t 'equal?-forever/box (equal?-forever l1 l2 #f))

(test #t 'equal?-forever/box (call-in-nested-thread (lambda () (equal?-forever l1 l2 #f))))

(report-errs)
