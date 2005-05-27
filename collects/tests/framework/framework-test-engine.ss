
(module framework-test-engine mzscheme
  (require (lib "pconvert.ss")
	   (lib "mred.ss" "mred")
	   "debug.ss")

  (define errs null)
  (define sema (make-semaphore 1))
  (define (protect f)
    (semaphore-wait sema)
    (begin0 (f)
	    (semaphore-post sema)))

  (define (exception->string x)
    (if (exn? x)
	(let ([p (open-output-string)])
	  (parameterize ([current-error-port p])
	    ((error-display-handler) (exn-message x) x))
	  (get-output-string p))
	(format "uncaught exn: ~s" x)))

  (thread
   (lambda ()
     (with-handlers ([(lambda (x) #t)
		      (lambda (x)
			(printf "test suite thread died: ~a~n"
				(if (exn? x)
				    (exn-message x)
				    (format "~s" x))))])
       (let ([port (load
		    (build-path
		     (collection-path "tests" "framework")
		     "receive-sexps-port.ss"))])
	 (debug-printf mr-tcp "about to connect to ~a~n" port)
	 (let*-values ([(in out) (tcp-connect "127.0.0.1" port)])
	   (let loop ()
	     (debug-printf mr-tcp "about to read~n")
	     (let ([sexp (read in)])
	       (if (eof-object? sexp)
		   (begin
		     (debug-printf mr-tcp "got eof~n")
		     (close-input-port in)
		     (close-output-port out)
		     (exit))
		   (begin
		     (debug-printf mr-tcp "got expression to evaluate~n")
		     (write
		      (let ([these-errs (protect (lambda () (begin0 errs (set! errs null))))])
			(if (null? these-errs)
			    (with-handlers ([(lambda (x) #t)
					     (lambda (x) (list 'error (exception->string x)))])
			      (list 'normal (print-convert (eval sexp))))
			    (list 'last-error
				  (apply string-append
					 (map (lambda (x) (string-append (exception->string x) (string #\newline)))
					      these-errs)))))
		      out)
		     (loop))))))))))

  (let ([od (event-dispatch-handler)]
	[port (current-output-port)])
    (event-dispatch-handler
     (lambda (evt)
       (parameterize ([current-exception-handler
		       (let ([oe (current-exception-handler)])
			 (lambda (exn)
			   (protect
			    (lambda ()
			      (set! errs (cons exn errs))))
			   (oe exn)))])
	 (od evt)))))

  (yield (make-semaphore 0)))
