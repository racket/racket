
(load-relative "loadtest.ss")

(SECTION 'multi-threaded-ports)

; Read from file with 3 threads, all writing to the same pipe
; read from pipe with 3 threads, all writing to the same output string
; compare resulting character content to the original file
(test 0 'threaded-ports
      (let*-values ([(f-in) (open-input-file 
			     (path->complete-path "testing.ss" 
						  (current-load-relative-directory)))]
		    [(p-in p-out) (make-pipe)]
		    [(s-out) (open-output-string)]
		    [(in->out) (lambda (in out)
				 (lambda ()
				   (let loop ()
				     (let ([c (read-char in)]
					   [dummy (lambda () 'hi)])
				       (unless (eof-object? c)
					       (write-char c out)
					       (loop))))))]
		    [(f->p) (in->out f-in p-out)]
		    [(p->s) (in->out p-in s-out)]
		    [(sthread) (lambda (thunk)
				 (let ([t (thread (lambda () (sleep) (thunk)))])
				   (thread-weight t 101)
				   t))])
		   (thread
		    (lambda ()
		      (for-each thread-wait
				(list (sthread f->p)
				      (sthread f->p)
				      (sthread f->p)))
		      (close-output-port p-out)))
		   (for-each thread-wait
			     (list (sthread p->s)
				   (sthread p->s)
				   (sthread p->s)))
		   (let ([s (get-output-string s-out)]
			 [hits (make-vector 256 0)])
		     (for-each (lambda (c)
				 (let ([n (char->integer c)])
				   (vector-set! hits n (add1 (vector-ref hits n)))))
			       (string->list s))
		     (file-position f-in 0)
		     (let loop ()
		       (let ([c (read-char f-in)])
			 (unless (eof-object? c)
				 (let ([n (char->integer c)])
				   (vector-set! hits n (sub1 (vector-ref hits n))))
				 (loop))))
		     (let loop ([i 0][total 0])
		       (if (= i 256)
			   total
			   (loop (add1 i) (+ total (abs (vector-ref hits i)))))))))

(report-errs)
