
(module sendurl mzscheme
  (require (lib "process.ss")
	   (lib "file.ss")
	   (lib "etc.ss"))
  
  (provide send-url)
  
  (define separate-by-default?
    (get-preference 'new-browser-for-urls (lambda () #t)))

  ; send-url : str -> void
  (define send-url
    (opt-lambda (str [separate-window? separate-by-default?])
      (parameterize ([current-input-port null-input]
		     [current-error-port null-output] ; comment out this line to see error messages
		     [current-output-port null-output])
	(case (system-type)
	  [(macos macosx)
	   ;; actually, I think GURL means something slightly different...
	   (send-event "MACS" "GURL" "GURL" str)]
	  [(windows)
	   ;; Try to get a MrEd function...
	   (let ([get-res (with-handlers ([not-break-exn? (lambda (x) #f)])
			    (dynamic-require '(lib "mred.ss" "mred") 'get-resource))])
	     (if get-res
		 (let ([b (box "")])
		   (unless (get-res "HKEY_CLASSES_ROOT" "htmlfile\\shell\\open\\command" b)
		     (error 'send-url "couldn't find URL opener in the registry"))
		   (let-values ([(out in id err status) (apply 
							 values
							 (process (format "~a ~a" (unbox b) str)))])
		     (close-output-port in)
		     (close-input-port out)
		     (close-input-port err)))
		 (error 'send-url "don't know how to open URL in Windows without MrEd")))]
	  [(unix)
	   (let ([preferred (get-preference 'external-browser (lambda () #f))])
	     (cond
	       [(and (or (not preferred)
			 (eq? preferred 'opera))
		     (find-executable-path "opera" #f))
		=>
		(lambda (browser-path) 
		  ;; opera may not return -- always open asyncronously
		  ;; opera starts a new browser automatically, if it can't find one
		  (process*/close-ports browser-path "-remote"
					(format "openURL(~a)"
						(if separate-window?
						    (format "~a,new-window" str)
						    str))))]
	       [(and (and (or (not preferred)
			      (eq? preferred 'netscape)))
		     (find-executable-path "netscape" #f))
		=>
		(lambda (browser-path)
		  ;; netscape's -remote returns with an error code, if no
		  ;; netscape is around. start a new netscape in that case.
		  (or (system* browser-path "-remote"
			       (format "openURL(~a)"
				       (if separate-window?
					   (format "~a,new-window" str)
					   str)))
		      (process*/close-ports browser-path str)))]
	       [else
		(error 'open-url "Couldn't find Netscape or Opera to open URL: ~e" str)]))]
	  [else (error 'send-url "don't know how to open URL on platform: ~s" (system-type))]))))
  
  ; null-input : iport
  (define null-input
    (make-input-port (lambda () eof)
                     (lambda () #t)
                     void))
  
  ; null-output : oport
  (define null-output
    (make-output-port void void))
  
  (define dev-null "/dev/null") ; unix specific, so slashes are okay
  
  ; process*/close-ports : [arg]* -> void
  ; This is a funny hack.  Closing unused scheme pipe ports right away closes
  ; the ports the browser is using, resulting in errors if they are read from or written
  ; to.  However, closing file ports (i.e. to and from /dev/null) after the subprocess is
  ; spawned does _not_ close the browser's ports since they are copied when the subprocess
  ; loads.  Blech.
  ; All this is necessary because system administrators replace netscape with a funny
  ; perl/zsh/whatever script that prints out extra crud or does weird, system dependent
  ; setup stuff before launching the original browser executable.
  (define (process*/close-ports . args)
    (if (and (memq (system-type) '(unix macosx))
             ; we can't check for _the_ dev-null, so check what we can
             (file-exists? dev-null)
             (let ([perms (file-or-directory-permissions dev-null)])
               (and (memq 'read perms)
                    (memq 'write perms)))
             (zero? (file-size dev-null)))
        (let ([out (open-output-file dev-null 'append)]
              [in (open-input-file dev-null)]
              [err (open-output-file dev-null 'append)])
          (let-values ([(false-out false-in id false-err status) 
                        (apply values (apply process*/ports out in err args))])
            (close-output-port out)
            (close-input-port in)
            (close-output-port err)))
        (let-values ([(out in id err status) 
                      (apply values (apply process*/ports #f #f #f args))])
          (close-input-port out)
          (close-output-port in)
          (close-input-port err)))))
