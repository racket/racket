
(module test-suite-utils mzscheme
  (require (lib "launcher.ss" "launcher")
	   (lib "pretty.ss")
	   (lib "list.ss")
           (lib "process.ss")
	   "debug.ss")

  (provide
   test-name
   failed-tests

   ;(struct eof-result ())
   eof-result?

   load-framework-automatically
   shutdown-listener shutdown-mred mred-running?
   use-3m
   send-sexp-to-mred queue-sexp-to-mred
   test
   wait-for-frame

   ;; sexp -> void
   ;; grabs the frontmost window, executes the sexp and waits for a new frontmost window
   wait-for-new-frame

   wait-for

   reset-section-jump!
   set-section-jump!
   reset-section-name!
   set-section-name!
   set-only-these-tests!
   get-only-these-tests)
  
  (define use-3m (make-parameter #f))

  (define section-jump void)
  (define (set-section-jump! _s) (set! section-jump _s))
  (define (reset-section-jump!) (set! section-jump #f))

  (define section-name "<<setup>>")
  (define (set-section-name! _s) (set! section-name _s))
  (define (reset-section-name!) (set! section-name "<<setup>>"))

  (define only-these-tests #f)
  (define (get-only-these-tests) only-these-tests)
  (define (set-only-these-tests! _t) (set! only-these-tests _t))

  (define test-name "<<setup>>")
  (define failed-tests null)

  (define-struct eof-result ())

  (define load-framework-automatically? #t)

  (define initial-port 6012)
  (define port-filename (build-path
			 (collection-path "tests" "framework")
			 "receive-sexps-port.ss"))

  (unless (file-exists? port-filename)
    (call-with-output-file port-filename
      (lambda (port)
	(write initial-port port))))

  (define listener
    (let loop ()
      (let ([port (load port-filename)])
	(with-handlers ([exn:fail?
			 (lambda (x)
			   (let ([next (+ port 1)])
			     (call-with-output-file port-filename
			       (lambda (p)
				 (write next p))
			       'truncate)
			     (debug-printf mz-tcp "  tcp-listen failed for port ~a, attempting ~a~n"
					   port
					   next)
			     (loop)))])
	  (debug-printf mz-tcp "listening to ~a~n" port)
	  (tcp-listen port)))))

  (define in-port #f)
  (define out-port #f)

  (define (restart-mred)
    (shutdown-mred)
    (case (system-type)
      [(macosx)
       (thread
        (lambda ()
          (system*
           (path->string
            (build-path (collection-path "mzlib")
                        'up
                        'up
                        "bin" 
                        (if (use-3m)
                            "mred3m"
                            "mred")))
           "-mvqt"
           (path->string 
            (build-path (collection-path "tests" "framework")
                        "framework-test-engine.ss")))))]
      [else (error 'test-suite-utils.ss "don't know how to start mred")])
    (debug-printf mz-tcp "accepting listener~n")
    (let-values ([(in out) (tcp-accept listener)])
      (set! in-port in)
      (set! out-port out))
    (when load-framework-automatically?
      (queue-sexp-to-mred
       '(begin (eval '(require (lib "framework.ss" "framework")))
	       (eval '(require (lib "gui.ss" "tests" "utils")))))))

  (define load-framework-automatically
    (case-lambda
     [(new-load-framework-automatically?)
      (unless (eq? (not (not new-load-framework-automatically?))
		   load-framework-automatically?)
	(set! load-framework-automatically? (not (not new-load-framework-automatically?)))
	(shutdown-mred))]
     [() load-framework-automatically?]))

  (define shutdown-listener
    (lambda ()
      (shutdown-mred)
      (debug-printf mz-tcp "closing listener~n")
      (tcp-close listener)))

  (define shutdown-mred
    (lambda ()
      (when (and in-port
		 out-port)
	(with-handlers ([exn:fail? (lambda (x) (void))])
	  (close-output-port out-port))
	(with-handlers ([exn:fail? (lambda (x) (void))])
	  (close-input-port in-port))
	(set! in-port #f)
	(set! in-port #f))))

  (define mred-running?
    (lambda ()
      (if (char-ready? in-port)
	  (not (eof-object? (peek-char in-port)))
	  #t)))

  (define queue-sexp-to-mred
    (lambda (sexp)
      (send-sexp-to-mred
       `(let ([thunk (lambda () ,sexp)]
	      [sema (make-semaphore 0)])
	  (queue-callback (lambda ()
			    (thunk)
			    (semaphore-post sema)))
	  (semaphore-wait sema)))))

  (define re:tcp-read-error (regexp "tcp-read:"))
  (define re:tcp-write-error (regexp "tcp-write:"))
  (define (tcp-error? exn)
    (or (regexp-match re:tcp-read-error (exn-message exn))
	(regexp-match re:tcp-write-error (exn-message exn))))

  (define (send-sexp-to-mred sexp)
    (let/ec k
      (let ([show-text 
	     (lambda (sexp)
	       
	       (debug-when messages
			   (parameterize ([pretty-print-print-line
					   (let ([prompt "  "]
						 [old-liner (pretty-print-print-line)])
					     (lambda (ln port ol cols)
					       (let ([ov (old-liner ln port ol cols)])
						 (if ln 
						     (begin (display prompt port)
							    (+ (string-length prompt) ov))
						     ov))))])
			     (pretty-print sexp)
			     (newline))))])
	(unless (and in-port
		     out-port
		     (with-handlers ([tcp-error?
				      (lambda (x) #f)])
		       (or (not (char-ready? in-port))
			   (not (eof-object? (peek-char in-port))))))
	  (restart-mred))
	(debug-printf messages "  ~a // ~a: sending to mred:~n" section-name test-name)
	(show-text sexp)
	(with-handlers ([exn:fail?
			 (lambda (x)
			   (cond
			     ;; this means that mred was closed
			     ;; so we can restart it and try again.
			     [(tcp-error? x) 
			      (restart-mred)
			      (write sexp out-port)
			      (newline out-port)]
			     [else (raise x)]))])
	  (write sexp out-port)
	  (newline out-port))
	(let ([answer
	       (with-handlers ([exn:fail?
				(lambda (x)
				  (if (tcp-error? x);; assume tcp-error means app closed
				      eof
				      (list 'cant-read
					    (string-append
					     (exn-message x)
					     "; rest of string: "
					     (format
					      "~s"
					      (apply
					       string
					       (let loop ()
						 (if (char-ready? in-port)
						     (let ([char (read-char in-port)])
						       (if (eof-object? char)
							   null
							   (cons char (loop))))
						     null))))))))])
		 (read in-port))])
	  (debug-printf messages "  ~a // ~a: received from mred:~n" section-name test-name)
	  (show-text answer)
	  (unless (or (eof-object? answer)
		      (and (list? answer)
			   (= 2 (length answer))
			   (memq (car answer)
				 '(error last-error cant-read normal))))
	    (error 'send-sexp-to-mred "unpected result from mred: ~s~n" answer))
	  (if (eof-object? answer)
	      (raise (make-eof-result))
	      (case (car answer)
		[(error)
		 (error 'send-sexp-to-mred "mred raised \"~a\"" (second answer))]
		[(last-error)
		 (error 'send-sexp-to-mred "mred (last time) raised \"~a\"" (second answer))]
		[(cant-read) (error 'mred/cant-parse (second answer))]
		[(normal) 
		 (eval (second answer))]))))))

  (define test
    (case-lambda
     [(in-test-name passed? sexp/proc) (test in-test-name passed? sexp/proc 'section)]
     [(in-test-name passed? sexp/proc jump)
      (fluid-let ([test-name in-test-name])
	(when (or (not only-these-tests)
		  (memq test-name only-these-tests))
	  (let* ([result
		  (with-handlers ([exn:fail?
				   (lambda (x)
				     (if (exn? x)
					 (exn-message x)
					 x))])
		    (if (procedure? sexp/proc)
			(sexp/proc)
			(begin0 (send-sexp-to-mred sexp/proc)
				(send-sexp-to-mred ''check-for-errors))))]
		 [failed (with-handlers ([exn:fail?
					  (lambda (x)
					    (string-append
					     "passed? test raised exn: "
					     (if (exn? x)
						 (exn-message x)
						 (format "~s" x))))])
			   (not (passed? result)))])
	    (when failed
	      (debug-printf schedule "FAILED ~a:~n  ~s~n" test-name result)
	      (set! failed-tests (cons (cons section-name test-name) failed-tests))
	      (case jump
		[(section) (section-jump)]
		[(continue) (void)]
		[else (jump)])))))]))

  (define (wait-for/wrapper wrapper sexp)
    (let ([timeout 10]
	  [pause-time 1/2])
      (send-sexp-to-mred
       (wrapper
	`(let ([test (lambda () ,sexp)])
	   (let loop ([n ,(/ timeout pause-time)])
	     (if (zero? n)
		 (error 'wait-for
			,(format "after ~a seconds, ~s didn't come true" timeout sexp))
		 (unless (test)
		   (sleep ,pause-time)
		   (loop (- n 1))))))))))

  (define (wait-for sexp) (wait-for/wrapper (lambda (x) x) sexp))

  (define (wait-for-new-frame sexp)
    (wait-for/wrapper
     (lambda (w)
       `(let ([frame (get-top-level-focus-window)])
	  ,sexp
	  ,w))
     `(not (eq? frame (get-top-level-focus-window)))))

  (define (wait-for-frame name)
    (wait-for `(let ([win (get-top-level-focus-window)])
		 (and win
		      (string=? (send win get-label) ,name))))))
