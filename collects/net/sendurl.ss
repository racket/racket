(module sendurl mzscheme
  (require (lib "process.ss")
           (lib "file.ss")
           (lib "etc.ss")
           (lib "thread.ss")
           (lib "sendevent.ss"))
  
  (provide send-url unix-browser-list browser-preference? external-browser)
  
  (define separate-by-default?
    (get-preference 'new-browser-for-urls (lambda () #t)))

  ; : any -> bool
  (define (browser-preference? x)
    (or (not x) (eq? 'plt x) (memq x unix-browser-list) (custom-browser? x)
        (procedure? x)))

  (define external-browser
    (make-parameter
     #f ; #f means "consult the preferences file"
     (lambda (x)
       (if (browser-preference? x)
           x
           (error 'external-browser "~a is not a valid browser preference" x)))))
  
  ; send-url : str [bool] -> void
  (define send-url
    (opt-lambda (url-str [separate-window? separate-by-default?])
      (cond
       [(procedure? (external-browser))
	((external-browser) url-str)]
       [(eq? (system-type) 'macos)
	(if (regexp-match "Blue Box" (system-type #t))
	    ;; Classic inside OS X:
	    (let loop ([l '("MSIE" "NAVG")])
	      (if (null? l)
		  (error 'send-url "couldn't start Internet Explorer or Netscape")
		  (with-handlers ([not-break-exn? (lambda (x) (loop (cdr l)))])
		    (subprocess #f #f #f "by-id" (car l))
		    (let loop ([retries 2]) ;; <<< Yuck <<<
		      (if (zero? retries)
			  (error "enough already") ; caught above
			  (with-handlers ([not-break-exn? (lambda (x)
							    (loop (sub1 retries)))])
			    (let ([t (thread (lambda ()
					       (send-event (car l) "GURL" "GURL" url-str)))])
			      (object-wait-multiple 1 t) ;; <<< Yuck (timeout) <<<
			      (when (thread-running? t)
				(kill-thread t)
				(error "timeout")))))))))
	    ;; Normal OS Classic:
	    (send-event "MACS" "GURL" "GURL" url-str))]
       [(or (eq? (system-type) 'macosx)
	    (equal? "ppc-darwin" (system-library-subpath)))
	;; not sure what changed, but this is wrong now.... -robby
	;;(browser-process (format "osascript -e 'open location \"~a\"'" (regexp-replace* "%" url-str "%25")))	
	(browser-process (format "osascript -e 'open location \"~a\"'" url-str))]
       [(eq? (system-type) 'windows)
	(shell-execute #f url-str "" (current-directory) 'SW_SHOWNORMAL)]
       [(eq? (system-type) 'unix)
	(let ([preferred (or (external-browser) (get-preference 'external-browser))])
	  (cond
	   [(use-browser 'opera preferred)
	    =>
	    (lambda (browser-path) 
	      ;; opera may not return -- always open asyncronously
	      ;; opera starts a new browser automatically, if it can't find one
	      (browser-process* browser-path "-remote"
				(format "openURL(~a)"
					(if separate-window?
					    (format "~a,new-window" url-str)
					    url-str))))]
	   [(use-browser 'galeon preferred)
	    =>
	    (lambda (browser-path)
	      (browser-process* browser-path
				(if separate-window? "-w" "-x")
				url-str))]
	   [(or (use-browser 'netscape preferred)
		(use-browser 'mozilla preferred))
	    =>
	    (lambda (browser-path)
	      ;; netscape's -remote returns with an error code, if no
	      ;; netscape is around. start a new netscape in that case.
	      (or (system* browser-path "-remote"
			   (format "openURL(~a)"
				   (if separate-window?
				       (format "~a,new-window" url-str)
				       url-str)))
		  (browser-process* browser-path url-str)))]
	   [(use-browser 'dillo preferred)
	    =>
	    (lambda (browser-path)
	      (browser-process* browser-path url-str))]
	   [(custom-browser? preferred)
	    (let ([cmd (string-append (car preferred)
				      url-str
				      (cdr preferred))])
	      (browser-process cmd))]
	   [else
	    (error 'send-url "Couldn't find ~a to open URL: ~e" (orify unix-browser-list) url-str)]))]
       [else (error 'send-url "don't know how to open URL on platform: ~s" (system-type))])))
  
  ; : tst -> bool
  (define (custom-browser? x)
    (and (pair? x) (string? (car x)) (string? (cdr x))))
  
  (define unix-browser-list '(opera galeon netscape mozilla dillo))
  
  ; : (cons tst (listof tst)) -> str
  (define (orify l)
    (cond
      [(null? (cdr l)) (format "~a" (car l))]
      [(null? (cddr l)) (format "~a or ~a" (car l) (cadr l))]
      [else 
    (let loop ([l l])
      (cond
        [(null? (cdr l)) (format "or ~a" (car l))]
        [else (string-append (format "~a, " (car l)) (loop (cdr l)))]))]))
  
  ; : sym sym -> (U #f str)
  ; to find the path for the named browser, unless another browser is preferred
  (define (use-browser browser-name preferred)
    (and (or (not preferred)
	     (eq? preferred browser-name))
	 (find-executable-path (symbol->string browser-name) #f)))
    
  ; null-output : oport
  (define null-output
    (make-custom-output-port #f (lambda (s start end flush?) (- end start)) void void))

  ;; run-browser : process-proc list-of-strings -> void
  (define (run-browser process*/ports args)
    (let-values ([(stdout stdin pid stderr control)
		  (apply values (apply process*/ports
				       null-output
				       #f
				       (current-error-port)
				       args))])
      (close-output-port stdin)
      (thread (lambda ()
		(control 'wait)
		(when (eq? 'done-error (control 'status))
		  (error 'run-browser "process execute failed: ~e" args))))
      (void)))

  (define (browser-process* . args)
    (run-browser process*/ports args))

  (define (browser-process cmd)
    (run-browser process/ports (list cmd))))
