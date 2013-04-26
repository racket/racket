
(load-relative "loadtest.rktl")

(Section 'parameters)

(define temp-compiled-file
  (path->string (build-path (find-system-path 'temp-dir) "param-temp-file")))

(let ([p (open-output-file temp-compiled-file #:exists 'replace)])
  (display (compile '(cons 1 2)) p)
  (close-output-port p))

(define-struct tester (x) #:inspector (make-inspector))
(define a-tester (make-tester 5))

(define (check-write-string display v s)
  (let ([p (open-output-string)])
    (display v p)
    (let ([s2 (get-output-string p)])
      (or (string=? s s2)
	  (error 'check-string "strings didn't match: ~s vs. ~s"
		 s s2)))))

(define exn:check-string? exn:fail?)

(define called-break? #f)

(define erroring-set? #f)

(define erroring-port
  (make-output-port 'errors
		    always-evt
		    (let ([orig (current-output-port)])
		      (lambda (s start end flush? breakable?) 
			(if erroring-set?
			    (begin
			      (set! erroring-set? #f)
			      (error 'output))
			    (display (subbytes s start end) orig))
			(- end start)))
		    void))

(define erroring-eval
  (let ([orig (current-eval)])
    (lambda (x)
      (if erroring-set?
	  (begin
	    (set! erroring-set? #f)
	    (error 'eval))
	  (orig x)))))

(define blocking-thread
  (lambda (thunk)
    (let ([x #f])
      (thread-wait (thread (lambda () (set! x (thunk)))))
      x)))

(define main-cust (current-custodian))

(define zero-arg-proc (lambda () #t))
(define one-arg-proc (lambda (x) #t))
(define two-arg-proc (lambda (x y) #t))
(define three-arg-proc (lambda (x y z) #t))

(define test-param1 (make-parameter 'one))
(define test-param2 (make-parameter 
		     'two
		     ; generates type error:
		     (lambda (x) (if (symbol? x)
				     x
				     (add1 'x)))))
(define test-param3 (make-parameter 'three list))
(define test-param4 (make-derived-parameter test-param3 box list))

(test 'one test-param1)
(test 'two test-param2) 
(test 'three test-param3) 

(test-param2 'other-two)
(test 'other-two test-param2) 
(test-param2 'two)
(test 'two test-param2) 
(parameterize ([test-param2 'yet-another-two])
  (test 'yet-another-two test-param2) 
  (parameterize ([test-param2 'yet-another-two!!])
    (test 'yet-another-two!! test-param2))
  (test-param2 'more-two?)
  (test 'more-two? test-param2) 
  (parameterize ([test-param2 'yet-another-two!!!])
    (test 'yet-another-two!!! test-param2)
    (test-param2 'more-two??)
    (test 'more-two?? test-param2))
  (test 'more-two? test-param2))
(test 'two test-param2) 

(test-param3 'other-three)
(test '(other-three) test-param3) 
(test '((other-three)) test-param4)
(test-param3 'three)
(test '(three) test-param3) 
(test '((three)) test-param4)
(parameterize ([test-param3 'yet-another-three])
  (test '(yet-another-three) test-param3) 
  (test '((yet-another-three)) test-param4)
  (parameterize ([test-param3 'yet-another-three!!])
    (test '(yet-another-three!!) test-param3)
    (test '((yet-another-three!!)) test-param4))
  (test-param3 'more-three?)
  (test '(more-three?) test-param3) 
  (test '((more-three?)) test-param4)
  (parameterize ([test-param3 'yet-another-three!!!])
    (test '(yet-another-three!!!) test-param3)
    (test '((yet-another-three!!!)) test-param4)
    (test-param3 'more-three??)
    (test '(more-three??) test-param3)
    (test '((more-three??)) test-param4))
  (test '(more-three?) test-param3)
  (test '((more-three?)) test-param4))
(test '(three) test-param3) 
(test '((three)) test-param4)
(test-param4 'other-three)
(test '(#&other-three) test-param3) 
(test '((#&other-three)) test-param4)
(parameterize ([test-param4 'yet-another-three])
  (test '(#&yet-another-three) test-param3) 
  (test '((#&yet-another-three)) test-param4))

(let ([cd (make-derived-parameter current-directory values values)])
  (test (current-directory) cd)
  (let* ([v (current-directory)]
         [sub (path->directory-path (build-path v "sub"))])
    (cd "sub")
    (test sub cd)
    (test sub current-directory)
    (cd v)
    (test v cd)
    (test v current-directory)
    (parameterize ([cd "sub"])
      (test sub cd)
      (test sub current-directory))
    (test v cd)
    (test v current-directory)
    (parameterize ([current-directory "sub"])
      (test sub cd)
      (test sub current-directory))))
(let ([l null])
  (let ([cd (make-derived-parameter current-directory
                                    (lambda (x)
                                      (set! l (cons x l))
                                      "sub")
                                    values)]
        [v (current-directory)])
    (let ([sub (path->directory-path (build-path v "sub"))])
      (parameterize ([cd "foo"])
        (test '("foo") values l)
        (test sub cd)
        (test sub current-directory))
      (test v cd)
      (test v current-directory)
      (cd "goo")
      (test '("goo" "foo") values l)
      (test sub cd)
      (test sub current-directory)
      (current-directory v)
      (test '("goo" "foo") values l)
      (test v cd)
      (test v current-directory))))

(arity-test make-parameter 1 2)
(err/rt-test (make-parameter 0 zero-arg-proc))
(err/rt-test (make-parameter 0 two-arg-proc))

(define-struct bad-test (value exn?))

(define params (list
		(list read-case-sensitive 
		      (list #f #t) 
		      '(if (eq? (read (open-input-string "HELLO")) (quote hello))
			   (void) 
			   (error (quote hello)))
		      exn:fail?
		      #f)
		(list read-square-bracket-as-paren
		      (list #t #f)
		      '(when (symbol? (read (open-input-string "[4]")))
			     (error 'read))
		      exn:fail?
		      #f)
		(list read-curly-brace-as-paren
		      (list #t #f)
		      '(when (symbol? (read (open-input-string "{4}")))
			     (error 'read))
		      exn:fail?
		      #f)
		(list read-accept-box
		      (list #t #f)
		      '(read (open-input-string "#&5"))
		      exn:fail:read?
		      #f)
		(list read-accept-graph
		      (list #t #f)
		      '(read (open-input-string "#0=(1 . #0#)"))
		      exn:fail:read?
		      #f)
		(list read-accept-compiled
		      (list #t #f)
		      `(let ([p (open-input-file ,temp-compiled-file)])
			 (dynamic-wind
			  void
			  (lambda () (void (read p)))
			  (lambda () (close-input-port p))))
		      exn:fail:read?
		      #f)
		(list read-accept-bar-quote
		      (list #t #f)
		      '(let ([p (open-input-string "|hello #$ there| x")])
			 (read p)
			 (read p))
		      exn:fail:read?
		      #f)
		(list read-accept-dot
		      (list #t #f)
		      '(let ([p (open-input-string "(1 . 2)")])
			 (read p))
		      exn:fail?
		      #f)
		(list read-accept-quasiquote
		      (list #t #f)
		      '(let ([p (open-input-string "`1")])
			 (read p)
			 (read p))
		      exn:fail:read?
		      #f)
		(list read-decimal-as-inexact
		      (list #f #t)
		      '(let ([p (open-input-string "1.0")])
			 (list-ref '(1 2) (read p)))
		      exn:application:type?
		      #f)
		(list print-graph
		      (list #t #f)
		      '(check-write-string display (let ([v '(1 2)]) (cons v v)) "(#0=(1 2) . #0#)")
		      exn:check-string?
		      #f)
		(list print-struct
		      (list #t #f)
		      '(check-write-string display a-tester "#(struct:tester 5)")
		      exn:check-string?
		      #f)
		(list print-box
		      (list #t #f)
		      '(check-write-string display (box 5) "#&5")
		      exn:check-string?
		      #f)
		(list print-vector-length
		      (list #t #f)
		      '(check-write-string write (vector 1 2 2) "#3(1 2)")
		      exn:check-string?
		      #f)

		(list current-input-port
		      (list (make-input-port 'in (lambda (s) (bytes-set! s 0 (char->integer #\x)) 1) #f void)
			    (make-input-port 'in (lambda (s) (error 'bad)) #f void))
		      '(read-char)
		      exn:fail?
		      '("bad string"))
		(list current-output-port
		      (list (current-output-port)
			    erroring-port)
		      '(begin 
			 (set! erroring-set? #t) 
			 (display 5) 
			 (set! erroring-set? #f))
		      exn:fail?
		      '("bad string"))

#|
		; Doesn't work since error-test sets the port!
		(list current-error-port
		      (list (current-error-port)
			    erroring-port)
		      '(begin 
			 (set! erroring-set? #t) 
			 ((error-display-handler) "hello")
			 (set! erroring-set? #f))
		      exn:fail?
		      "bad setting")
|#
		
		(list compile-allow-set!-undefined
		      (list #t #f)
		      '(eval `(set! ,(gensym) 9))
		      exn:fail:contract:variable?
		      #f)

		(list current-namespace
		      (list (make-base-namespace)
			    (make-empty-namespace))
		      '(begin 0)
		      exn:fail:syntax?
		      '("bad setting"))

		(list error-print-width
		      (list 10 50)
		      '(when (< 10 (error-print-width)) (error 'print-width))
		      exn:fail?
		      '("bad setting"))
		(list error-value->string-handler
		      (list (error-value->string-handler) (lambda (x w) (error 'converter)))
		      '(format "~e" 10)
		      exn:fail?
		      (list "bad setting" zero-arg-proc one-arg-proc three-arg-proc))

		(list current-print
		      (list (current-print)
			    (lambda (x) (display "frog")))
		      `(let ([i (open-input-string "5")]
			     [o (open-output-string)])
			 (parameterize ([current-input-port i]
					[current-output-port o])
                           (read-eval-print-loop))
			 (let ([s (get-output-string o)])
			   (printf "**~a**\n" s)
			   (unless (char=? #\5 (string-ref s 2))
				   (error "print:" s))))
		      exn:fail?
		      (list "bad setting" zero-arg-proc two-arg-proc))

		(list current-prompt-read
		      (list (current-prompt-read)
			    (let ([x #f]) 
			      (lambda () 
				(set! x (not x))
				(if x
				    '(quote hi)
				    eof))))
		      `(let ([i (open-input-string "5")]
			     [o (open-output-string)])
			 (parameterize ([current-input-port i]
					[current-output-port o])
			     (read-eval-print-loop))
			 (let ([s (get-output-string o)])
			   (unless (and (char=? #\> (string-ref s 0))
					(not (char=? #\h (string-ref s 0))))
				   (error 'prompt))))
		      exn:fail?
		      (list "bad setting" one-arg-proc two-arg-proc))

		(list current-load
		      (list (current-load) (lambda (f e) (error "This won't do it")))
		      `(load ,temp-compiled-file)
		      exn:fail?
		      (list "bad setting" zero-arg-proc one-arg-proc))
		(list current-eval
		      (list (current-eval) erroring-eval)
		      '(begin 
			 (set! erroring-set? #t) 
			 (eval 5)
			 (set! erroring-set? #f))
		      exn:fail?
		      (list "bad setting" zero-arg-proc two-arg-proc))

		(list current-load-relative-directory
		      (list (current-load-relative-directory) 
			    (build-path (current-load-relative-directory) 'up))
		      '(load-relative "loadable.rktl")
		      exn:fail:filesystem?
		      (append (list 0)
			      (map
			       (lambda (t)
				 (make-bad-test t exn:fail:contract?))
			       (list
				"definitely a bad path"
				(string #\a #\nul #\b)
				"relative"
				(build-path 'up))))
		      equal?)

		(list global-port-print-handler
		      (list write display)
		      '(let ([s (open-output-string)])
			 (print "hi" s)
			 (unless (char=? #\" (string-ref (get-output-string s) 0))
				 (error 'global-port-print-handler)))
		      exn:fail?
		      (list "bad setting" zero-arg-proc one-arg-proc three-arg-proc))

		(list current-custodian
		      (list main-cust (make-custodian))
		      '(let ([th (parameterize ([current-custodian main-cust])
				    (thread (lambda () (sleep 1))))])
			 (kill-thread th))
		      exn:application:mismatch?
		      (list "bad setting"))

		(list exit-handler
		      (list void (lambda (x) (error 'exit-handler)))
		      '(exit)
		      exn:fail?
		      (list "bad setting" zero-arg-proc two-arg-proc))

		(list test-param1
		      (list 'one 'bad-one)
		      '(when (eq? (test-param1) 'bad-one)
			     (error 'bad-one))
		      exn:fail?
		      #f)
		(list test-param2
		      (list 'two 'bad-two)
		      '(when (eq? (test-param2) 'bad-two)
			     (error 'bad-two))
		      exn:fail?
		      '("bad string"))))

(for-each
 (lambda (d)
   (let ([param (car d)]
	 [alt1 (caadr d)]
	 [alt2 (cadadr d)]
	 [expr (caddr d)]
	 [exn? (cadddr d)])
     (parameterize ([param alt1])
	  (test (void) void (teval expr)))
     (parameterize ([param alt2])
	  (error-test (datum->syntax #f expr #f) exn?))))
 params)

(define test-param3 (make-parameter 'hi))
(test 'hi test-param3)
(test 'hi 'thread-param
      (let ([v #f])
	(thread-wait (thread
		      (lambda ()
			(set! v (test-param3)))))
	v))
(test (void) test-param3 'bye)
(test 'bye test-param3)
(test 'bye 'thread-param
      (let* ([v #f]
	     [r (make-semaphore)]
	     [s (make-semaphore)]
	     [t (thread
		 (lambda ()
		   (semaphore-post r)
		   (semaphore-wait s)
		   (set! v (test-param3))))])
	(semaphore-wait r)
	(test-param3 'bye-again)
	(semaphore-post s)
	(thread-wait t)
	v))
(test 'bye-again test-param3)

(test #f parameter? add1)

(for-each
 (lambda (d)
   (let* ([param (car d)]
	  [alt1 (caadr d)]
	  [bads (cadddr (cdr d))])
     (test #t parameter? param)
     (arity-test param 0 1)
     (when bads
	   (for-each
	    (lambda (bad)
	      (let-values ([(bad exn?)
			    (if (bad-test? bad)
				(values (bad-test-value bad)
					(bad-test-exn? bad))
				(values bad
					exn:application:type?))])
		(err/rt-test (param bad) exn?)))
	    bads))))
 params)

(test #t parameter-procedure=? read-accept-compiled read-accept-compiled)
(test #f parameter-procedure=? read-accept-compiled read-case-sensitive)
(err/rt-test (parameter-procedure=? read-accept-compiled 5))
(err/rt-test (parameter-procedure=? 5 read-accept-compiled))
(arity-test parameter-procedure=? 2 2)
(arity-test parameter? 1 1)

;; ----------------------------------------

(let ([ch (make-channel)]
      [k-ch (make-channel)]
      [p1 (make-parameter 1)]
      [p2 (make-parameter 2)])
  (parameterize ([p1 0])
    (thread (lambda ()
	      (channel-put ch (cons (p1) (p2))))))
  (test '(0 . 2) channel-get ch)

  (let ([send-k
	 (lambda ()
	   (parameterize ([p1 0])
	     (thread (lambda ()
		       (let/ec esc
			 (channel-put ch
				      ((let/cc k
					 (channel-put k-ch k)
					 (esc)))))))))])
    (send-k)
    (thread (lambda () ((channel-get k-ch) (let ([v (p1)]) (lambda () v)))))
    (test 1 channel-get ch)
    (send-k)
    (thread (lambda () ((channel-get k-ch) p1)))
    (test 0 channel-get ch))

  (let ([send-k-param-in-thread
	 (lambda ()
	   (thread (lambda ()
		     (parameterize ([p1 3])
		       (let/ec esc
			 (channel-put ch
				      ((let/cc k
					 (channel-put k-ch k)
					 (esc)))))))))])
    (send-k-param-in-thread)
    (thread (lambda () ((channel-get k-ch) (let ([v (p1)]) (lambda () v)))))
    (test 1 channel-get ch)
    (send-k-param-in-thread)
    (thread (lambda () ((channel-get k-ch) p1)))
    (test 3 channel-get ch)))

; Test current-library-collection-paths?
; Test require-library-use-compiled?

(when (file-exists? temp-compiled-file) (delete-file temp-compiled-file))

(err/rt-test (read-on-demand-source 5))
(err/rt-test (read-on-demand-source "x"))
(test (find-system-path 'temp-dir) 'rods (parameterize ([read-on-demand-source (find-system-path 'temp-dir)])
                                           (read-on-demand-source)))
(test #f 'rods (parameterize ([read-on-demand-source #f])
                 (read-on-demand-source)))

;; ----------------------------------------

(report-errs)
