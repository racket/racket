
(load-relative "loadtest.ss")

(SECTION 'wills)

(test #f will-executor? 5)
(test #t will-executor? (make-will-executor))

(define we (make-will-executor))

;; Never GC this one:
(test (void) will-register we test (lambda (x) (error 'bad-will-call)))

; There's no excuse for not GCing half or more:
(define counter null)
(let loop ([n 10])
  (unless (zero? n)
    (will-register we (cons n null)
		   (lambda (s)
		     (set! counter (cons (car s) counter))
		     12))
    (loop (sub1 n))))
(collect-garbage)
(collect-garbage)
(let* ([v #f]
       [t (thread (lambda () (set! v (will-execute we))))])
  (sleep 0.1)
  (test #f thread-running? t)
  (test v values 12))
(let loop ([m 1])
  (if (let ([v (will-try-execute we)])
	(test #t 'good-result (or (not v) (= v 12)))
	v)
      (loop (add1 m))
      (begin
	(test #t >= m 5)
	;; Make sure counter grew ok
	(test m length counter)
	;; Make sure they're all different
	(let loop ([l counter])
	  (unless (or (null? l) (null? (cdr l)))
	    (test #f member (car l) (cdr l))
	    (loop (cdr l)))))))

(err/rt-test (will-register we we we))
(err/rt-test (will-register we we (lambda () 10)))
(err/rt-test (will-register 5 we (lambda (s) 10)))

(err/rt-test (will-execute "bad"))
(err/rt-test (will-try-execute "bad"))

(arity-test make-will-executor 0 0)
(arity-test will-executor? 1 1)
(arity-test will-register 3 3)
(arity-test will-execute 1 1)
(arity-test will-try-execute 1 1)

(report-errs)
