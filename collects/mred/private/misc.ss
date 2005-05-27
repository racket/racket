(module misc mzscheme
  (require (lib "class.ss")
	   (lib "file.ss")
	   (lib "process.ss")
	   (prefix wx: "kernel.ss"))

  (provide file-creator-and-type
	   hide-cursor-until-moved
	   sleep/yield
	   play-sound
	   timer%)

  ;; Currently only used for PS print and preview
  (wx:set-executer
   (let ([orig-err (current-error-port)])
     (lambda (prog . args)
       (let ([cmd (string-append
		   prog 
		   (let loop ([args args])
		     (if (null? args)
			 ""
			 (format " ~s~a" (car args) (loop (cdr args))))))])
	 (let-values ([(in out pid err x) (apply values (process cmd))])
	   (close-output-port out)
	   (let ([echo (lambda (p)
			 (thread (lambda ()
				   (dynamic-wind
				       void
				       (lambda ()
					 (let loop ()
					   (let ([l (read-line p)])
					     (unless (eof-object? l)
					       (fprintf orig-err "~a~n" l)
					       (loop)))))
				       (lambda () (close-input-port p))))))])
	     (echo in)
	     (echo err)
	     (void)))))))

  (define (sleep/yield secs)
    (unless (and (real? secs) (not (negative? secs)))
      (raise-type-error 'sleep/yield "non-negative real number" secs))
    (wx:yield (alarm-evt (+ (current-inexact-milliseconds)
			    (* secs 1000))))
    (void))

  (define file-creator-and-type
    (case-lambda
     [(fn) (wx:file-creator-and-type fn)]
     [(fn c t) (wx:file-creator-and-type fn c t)]))
  
  (define (hide-cursor-until-moved) 
    (wx:hide-cursor))

  (define (play-sound f async?)
    (if (not (eq? (system-type) 'unix))
	(wx:play-sound f async?)
	(begin
	  (unless (string? f)
	    (raise-type-error 'play-sound "string" f))
	  (let* ([subpath (system-library-subpath)]
		 [make-pattern (lambda (s) (string-append ".*" s ".*"))]
		 [b (box 
		     (cond 
		      [(regexp-match (make-pattern "linux") subpath)
		       ;; use play interface to sox
		       "play ~s"]
		      [(regexp-match (make-pattern "solaris") subpath)
		       "audioplay ~s"]
		      [else
		       (raise-mismatch-error
			'play-sound
			"not supported by default on this platform"
			subpath)]))])
					; see if user has overridden defaults 		  
	    (let ([r (get-preference '|MrEd:playcmd| (lambda () #f))])
	      (when (and r (string? r))
		(set-box! b r)))
	    ((if async? (lambda (x) (process x) #t) system)
	     (format (unbox b) (expand-path f)))))))

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Timers:

  (define timer%
    (class wx:timer%
      (init [notify-callback void]
	    [interval #f]
	    [just-once? #f])
      
      (inherit start)
      
      (define -notify-callback notify-callback)
      
      (define/override (notify) (-notify-callback))
      
      (super-make-object)

      (when interval
	(start interval just-once?)))))
