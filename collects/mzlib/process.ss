
(module process mzscheme
  (provide process
	   process*
	   process/ports
	   process*/ports
	   system
	   system*
	   system/exit-code
	   system*/exit-code)

  (require (lib "port.ss"))

  ;; Helpers: ----------------------------------------

  (define (shell-path/args who argstr)
    (case (system-type)
      ((unix macosx) (append '("/bin/sh" "-c") (list argstr)))
      ((windows) (let ([cmd
			(let ([d (find-system-path 'sys-dir)])
			  (let ([cmd (build-path d "cmd.exe")])
			    (if (file-exists? cmd)
				cmd
				(let ([cmd (build-path d "command.com")])
				  (if (file-exists? cmd)
				      cmd
				      ;; One last try: up a dir
				      (build-path d 'up "command.com"))))))])
		   (list cmd
			 'exact
			 (format "~a /c \"~a\"" (path->string cmd) argstr))))
      (else (raise-mismatch-error 
	     who
	     (format "~a: don't know what shell to use for platform: " who)
	     (system-type)))))

  (define (if-stream-out p)
    (if (or (not p) (file-stream-port? p))
	p
	(if (output-port? p)
	    #f
	    (raise-type-error
	     'subprocess
	     "output port"
	     p))))

  (define (if-stream-in p)
    (if (or (not p) (file-stream-port? p))
	p
	(if (input-port? p)
	    #f
	    (raise-type-error
	     'subprocess
	     "input port"
	     p))))

  (define (streamify-in cin in get-thread? ready-for-break)
    (if (and cin (not (file-stream-port? cin)))
	(let ([t (thread (lambda () 
			   (dynamic-wind
			       void
			       (lambda ()
				 (with-handlers ([exn:break? void])
				   (ready-for-break #t)
				   (copy-port cin in)
				   (ready-for-break #f)))
			       (lambda () (close-output-port in)))
			   (ready-for-break #t)))])
	  (and get-thread? t))
	in))

  (define (streamify-out cout out get-thread?)
    (if (and cout (not (file-stream-port? cout)))
	(let ([t (thread (lambda () (copy-port out cout)))])
	  (and get-thread? t))
	out))

  ;; Old-style functions: ----------------------------------------

  (define (process*/ports cout cin cerr exe . args)
    (let-values ([(subp out in err) (apply subprocess 
					   (if-stream-out cout)
					   (if-stream-in cin)
					   (if-stream-out cerr)
					   exe args)]
		 [(it-ready) (make-semaphore)])
      (let ([so (streamify-out cout out #t)]
	    [si (streamify-in cin in #t (lambda (ok?)
					  (if ok?
					      (semaphore-post it-ready)
					      (semaphore-wait it-ready))))]
	    [se (streamify-out cerr err #t)]
	    [aport (lambda (x)
		     (and (port? x) x))])
	(when (thread? si)
	  ;; Wait for process to end, then stop copying input:
	  (thread (lambda ()
		    (sync subp si)
		    (semaphore-wait it-ready)
		    (break-thread si))))
	(let ([threads-still-going?
	       (lambda ()
		 (ormap (lambda (s)
			  (and (thread? s)
			       (thread-running? s)))
			(list so si se)))])
	  (list (aport so)
		(aport si)
		(subprocess-pid subp)
		(aport se)
		(letrec ((control
			  (lambda (m)
			    (case m
			      ((status) (let ((s (subprocess-status subp)))
					  (cond ((or (not (integer? s))
						     (threads-still-going?))
						 'running)
						((zero? s) 'done-ok)
						(else 'done-error))))
			      ((exit-code) (if (threads-still-going?)
					       #f
					       (let ((s (subprocess-status subp)))
						 (and (integer? s) s))))
			      ((wait) 
			       (subprocess-wait subp)
			       (let ([twait (lambda (t)
					      (when (thread? t)
						(thread-wait t)))])
				 (twait so)
				 (twait si)
				 (twait se)))
			      ((interrupt) (subprocess-kill subp #f))
			      ((kill) (subprocess-kill subp #t))
			      (else
			       (raise-type-error 'control-process
						 "'status, 'exit-code, 'wait, 'interrupt, or 'kill" m))))))
		  control))))))

  (define (process/ports out in err str)
    (apply process*/ports out in err (shell-path/args "process/ports" str)))

  (define (process* exe . args)
    (apply process*/ports #f #f #f exe args))

  (define (process str)
    (apply process* (shell-path/args "process" str)))

  ;; Note: these always use current ports
  (define (system*/exit-code exe . args)
    (if (eq? (system-type) 'macos)
	(if (null? args)
          (raise-mismatch-error
           'system*/exit-code "command-line arguments not supported for MacOS" args)
	  (subprocess #f #f #f exe))
	(let ([cout (current-output-port)]
	      [cin (current-input-port)]
	      [cerr (current-error-port)]
	      [it-ready (make-semaphore)])
	  (let-values ([(subp out in err)
			(apply
			 subprocess
			 (if-stream-out cout)
			 (if-stream-in cin)
			 (if-stream-out cerr)
			 exe args)])
	    (let ([ot (streamify-out cout out #t)]
		  [it (streamify-in cin in #t (lambda (ok?)
						(if ok?
						    (semaphore-post it-ready)
						    (semaphore-wait it-ready))))]
		  [et (streamify-out cerr err #t)])
	      (subprocess-wait subp)
	      (when it
		;; stop piping output to subprocess
		(semaphore-wait it-ready)
		(break-thread it))
	      ;; wait for other pipes to run dry:
	      (when (thread? ot)
		(thread-wait ot))
	      (when (thread? et)
		(thread-wait et))
	      (when err
		(close-input-port err))
	      (when out
		(close-input-port out))
	      (when in
		(close-output-port in)))
	    (subprocess-status subp)))))

  (define (system* exe . args)
    (if (eq? (system-type) 'macos)
	(if (null? args)
          (raise-mismatch-error
           'system* "command-line arguments not supported for MacOS" args)
	  (subprocess #f #f #f exe))
	(zero? (apply system*/exit-code exe args))))

  (define (system str)
    (if (eq? (system-type) 'macos)
	(subprocess #f #f #f "by-id" str)
	(apply system* (shell-path/args "system" str))))

  (define (system/exit-code str)
    (if (eq? (system-type) 'macos)
	(subprocess #f #f #f "by-id" str)
	(apply system*/exit-code (shell-path/args "system" str)))))
