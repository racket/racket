(module start slideshow/slideshow
  (require slideshow/start-param
           planet/config
           mred
           mzlib/class)

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Path utilities

  (define (resolve-path/complete p base)
    (let ([p2 (resolve-path p)])
      (if (complete-path? p2)
	  p2
	  (path->complete-path p2 base))))

  (define (normal-path p)
    (let loop ([p (simplify-path (expand-path (path->complete-path p)))])
      (let-values ([(base name dir?) (split-path p)])
	(if (path? base)
	    (let ([base (loop base)])
	      (let ([p (build-path base name)])
		(resolve-path/complete p base)))
	    (resolve-path/complete p base)))))

  (define (same-path? a b)
    (let-values ([(abase aname adir?) (split-path a)]
		 [(bbase bname bdir?) (split-path b)])
      (and (equal? aname bname)
	   (or (equal? abase bbase)
	       (and (path? abase)
		    (path? bbase)
		    (same-path? abase bbase))))))

  (define (sub-path? a b)
    (let-values ([(abase aname adir?) (split-path a)]
		 [(bbase bname bdir?) (split-path b)])
      (or (and (equal? aname bname)
	       (or (and (not abase) (not bbase))
		   (and (path? abase)
			(path? bbase)
			(same-path? abase bbase))))
	  (and (path? abase)
	       (sub-path? abase b)))))
      
  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Loader with security guard

  (define (load-content content)
    (unless (trust-me?)
      (current-security-guard
       (make-security-guard (current-security-guard)
			    (lambda (who what mode)
                              (when (memq 'write mode)
                                (unless (let ([np-what (normal-path what)])
                                          (or (sub-path? np-what (normal-path (find-system-path 'temp-dir)))
                                              (equal? np-what (normal-path (LINKAGE-FILE)))))
                                  (error 'slideshow
                                         "slide program attempted to write to filesystem: ~e"
                                         what)))
			      (when (memq 'execute mode)
                                (error 'slideshow
				       "slide program attempted to execute external code: ~e"
				       what)))
			    (lambda (who where-name where-port-num mode)
			      (error 'slideshow
				     "slide program attempted to make a network connection")))))
    (define content-path (path->complete-path content))
    (dynamic-require content-path #f)
    (ormap (lambda (sm)
             (define submod-path `(submod ,content-path ,sm))
             (and (module-declared? submod-path #t)
                  (begin
                    (dynamic-require submod-path #f)
                    #t)))
           '(slideshow main)))

  (when (file-to-load)
    (load-content (string->path (file-to-load))))

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Run tutorial

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
			      (collection-file-path "tutorial-show.rkt" "slideshow")))))
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
