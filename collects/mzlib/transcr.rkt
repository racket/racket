
(module transcr mzscheme
  (require mzlib/port)
  (provide (rename -transcript-on transcript-on)
	   (rename -transcript-off transcript-off))

  ;; Bug: the implementation strategy used for the tees does not
  ;; guarantee that flushes to stdout and stderr are kept in order.
  ;; It depends instead on the whims of scheduling the tee threads.
  ;; This problem could be fixed using `make-output-port' instead
  ;; of pipes and `copy-port'.

  (define-values (-transcript-on -transcript-off)
    (let ([in #f]
	  [out #f]
	  [err #f]
	  [tee-out (lambda (p p2)
		     (let-values ([(r w) (make-pipe)])
		       (thread
			(lambda () 
			  (copy-port r p p2)))
		       w))]
	  [tee-in (lambda (in out)
		     (let-values ([(r w) (make-pipe)])
		       (thread
			(lambda ()
			  (copy-port in w out)))
		       r))])
      (values
       (lambda (file)
	 (when in
	   (error 'transcript-on "transcript is already on"))
	 (let ([p (open-output-file file)])
	   (set! in (current-input-port))
	   (set! out (current-output-port))
	   (set! err (current-error-port))
	   (current-output-port (tee-out out p))
	   (current-error-port (tee-out err p))
	   (current-input-port (tee-in in p))))
       (lambda ()
	 (unless in
	   (error 'transcript-on "transcript is not on"))
	 (current-input-port in)
	 (current-output-port out)
	 (current-error-port err)
	 (set! in #f)
	 (set! out #f)
	 (set! err #f))))))

