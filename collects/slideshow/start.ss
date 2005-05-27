
(module start "slideshow.ss"
  (require "start-param.ss"
	   (lib "mred.ss" "mred")
	   (lib "class.ss"))

  (define (load-content content)
    (unless (trust-me?)
      (current-security-guard
       (make-security-guard (current-security-guard)
			    (lambda (who what mode)
			      (when (memq 'write mode)
				(error 'slideshow
				       "slide program attempted to write to filesystem: ~e"
				       what))
			      (when (memq 'execute mode)
				(error 'slideshow
				       "slide program attempted to execute external code: ~e"
				       what)))
			    (lambda (who where-name where-port-num mode)
			      (error 'slideshow
				     "slide program attempted to make a network connection")))))
    (dynamic-require (path->complete-path content) #f)
    (done-making-slides))

  (when (file-to-load)
    (load-content (string->path (file-to-load))))

  (unless (most-recent-slide)
    (let ([link (lambda (label thunk)
		  (clickback (colorize 
			      (let ([p (t label)])
				(refocus (vc-append p
						    (linewidth 2 (hline (pict-width p) 2)))
					 p))
			      "blue")
			     thunk))])
      (slide/center
       (hc-append
	(* 4 gap-size)
	(titlet "Welcome to Slideshow")
	(vl-append
	 gap-size
	 (page-para* (link "Run Tutorial"
			   (lambda ()
			     (retract-most-recent-slide)
			     (load-content
			      (build-path (collection-path "slideshow")
					  "tutorial-show.ss")))))
	 (page-para* (link "Open File..."
			   (lambda ()
			     (let ([file (get-file)])
			       (when file
				 (retract-most-recent-slide)
				 (let-values ([(base name dir?) (split-path file)])
				   (current-directory base))
				 (load-content file))))))
	 (blank)
	 (page-para* (link "Quit"
			   (lambda ()
			     (exit))))))
       (blank)
       (blank)
       (parameterize ([current-main-font `(italic . ,main-font)]
		      [current-font-size (floor (* #e0.8 (current-font-size)))])
	 (if (eq? (system-type) 'unix)
	     (page-para* "To start a talk directly,"
			 "provide the talk file as a command-line" 
			 "argument")
	     (page-para* "To start a talk directly,"
			 "drag the talk file onto the" (bt "Slideshow")
			 "application icon")))))))
