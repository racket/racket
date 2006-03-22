(module filedialog mzscheme
  (require (lib "class.ss")
	   (lib "class100.ss")
	   (lib "etc.ss")
	   (lib "list.ss")
	   "wxkernel.ss"
	   "lock.ss"
	   "wx.ss"
	   "cycle.ss"
	   "check.ss"
	   "helper.ss"
	   "editor.ss"
	   "mrtop.ss"
	   "mrcanvas.ss"
	   "mrpopup.ss"
	   "mrmenu.ss"
	   "mritem.ss"
	   "mrpanel.ss"
	   "mrtextfield.ss"
	   "messagebox.ss")

  (provide get-file
	   get-file-list
	   put-file
	   get-directory)

  (define last-visted-directory #f)

  (define (files->list s)
    (let ([s (open-input-bytes s)])
      (let loop ()
	(let ([n (read s)])
	  (if (eof-object? n)
	      null
	      (begin
		(read-byte s) ; drop space
		(cons (read-bytes n s)
		      (loop))))))))

  (define (mk-file-selector who put? multi? dir? force-unix?)
    (lambda (message parent directory filename extension style filters)
      ;; Calls from C++ have wrong kind of window:
      (when (is-a? parent wx:window%)
	(set! parent (as-entry (lambda () (wx->mred parent)))))

      (check-label-string/false who message)
      (check-top-level-parent/false who parent)
      (check-path/false who directory) (check-path/false who filename) (check-string/false who extension)
      (check-style who #f (cond
			   [dir? '(enter-packages)]
			   [else '(packages enter-packages)]) style)
      (unless (and (list? filters)
		   (andmap (lambda (p)
			     (and (list? p)
				  (= (length p) 2)
				  (string? (car p))
				  (string? (cadr p))))
			   filters))
	(raise-type-error who "list of 2-string lists" filters))
      (if (not (or (eq? (system-type) 'unix)
		   force-unix?))
	  (let ([s (wx:file-selector message directory filename extension 
				     ;; file types:
				     (apply string-append
					    (map (lambda (s) (format "~a|~a|" (car s) (cadr s)))
						 filters))
				     ;; style:
				     (cons
				      (cond
				       [dir? 'dir]
				       [put? 'put]
				       [multi? 'multi]
				       [else 'get])
				      style)
				     ;; parent:
				     (and parent (mred->wx parent)))])
	    (if (and multi? s)
		(map bytes->path (files->list (path->bytes s)))
		s))
	  (letrec ([ok? #f]
		   [typed-name #f]
		   [dir (or (and directory (if (string? directory)
					       (string->path directory)
					       directory))
			    last-visted-directory 
			    (current-directory))]
		   [f (make-object dialog% (if dir? "Select Directory" (if put? "Save" "Open")) parent 500 300)]
		   [__ (when message
			 (let ([p (make-object vertical-pane% f)])
			   (send p stretchable-height #f)
			   (make-object message% (protect& message) p)))]
		   [dir-pane (instantiate horizontal-pane% (f) (stretchable-height #f))]
		   [m (make-object message% (protect& (path->string dir)) dir-pane)]
		   [lp (make-object horizontal-pane% f)]
		   [change-dir (lambda (d) (let ([sd (send d get-string-selection)])
					     (when sd
					       (set! dir (simplify-path (build-path dir sd)))
					       (reset-directory))))]
		   [dirs (make-object (class list-box%
					(define/override (on-subwindow-char w e)
					  (cond
					   [(and (send e get-meta-down)
						 (eq? (send e get-key-code) 'down))
					    (change-dir w)]
					   [(and (send e get-meta-down)
						 (eq? (send e get-key-code) 'up))
					    (send dirs set-selection 0)
					    (change-dir dirs)]
					   [else
					    (super on-subwindow-char w e)]))
					(super-instantiate ()))
				      #f null lp (lambda (d e)
						   (update-ok)
						   (when (eq? (send e get-event-type) 'list-box-dclick)
						     (change-dir d))))]
		   [dir-paths null]
		   [files (make-object list-box% #f null lp (lambda (d e)
							      (update-ok)
							      (when (eq? (send e get-event-type) 'list-box-dclick)
								(done)))
				       (if multi? '(multiple) '(single)))]
		   [file-paths null]
		   [do-text-name (lambda ()
				   (let ([v (send dir-text get-value)])
				     (if (or dir? (directory-exists? v))
					 (begin
					   (set! dir (string->path v))
					   (reset-directory))
					 ;; Maybe specifies a file:
					 (let-values ([(super file) 
						       (with-handlers ([void #f])
							 (let-values ([(base name dir?) (split-path v)])
							   (let ([super (and (not dir?) 
									     (or (and (path? base) 
										      (directory-exists? base)
										      base)
										 (and (eq? base 'relative) 
										      (directory-exists? dir) dir)))])
							     (if super
								 (values super name)
								 (values #f #f)))))])
					   (if super
					       (begin
						 (set! dir super)
						 (set! typed-name file)
						 (done))
					       (begin
						 (set! dir (string->path v))
						 (reset-directory)))))))]
		   [dir-text (make-object text-field% #f f (lambda (t e)
							     (if (eq? (send e get-event-type) 'text-field-enter)
								 (do-text-name)
								 (begin
					; typing in the box; disable the lists and enable ok
								   (send dirs enable #f)
								   (send files enable #f)
								   (when create-button
								     (send create-button enable #t))
								   (send ok-button enable #t)))))]
		   [bp (make-object horizontal-pane% f)]
		   [dot-check (make-object check-box% "Show files/directories that start with \".\"" bp (lambda (b e) (reset-directory)))]
		   [spacer (make-object vertical-pane% bp)]
		   [create-button (and dir? (make-object button% "Create" bp
							 (lambda (b e)
							   (with-handlers ([void
									    (lambda (exn)
									      (message-box "Error"
											   (exn-message exn)
											   f
											   '(ok stop)))])
							     (make-directory (send dir-text get-value))
							     (do-text-name)))))]
		   [cancel-button (make-object button% "Cancel" bp (lambda (b e) (set! ok? #f) (send f show #f)))]
		   [ok-button (make-object button% 
					   (if dir? "Goto" "OK")
					   bp (lambda (b e) 
						(if (send (if dir? dirs files) is-enabled?)
						    ;; normal mode
						    (if dir?
							(change-dir dirs)
							(done))
						    ;; handle typed text
						    (do-text-name))) 
					   '(border))]
		   [update-ok (lambda () (send ok-button enable (not (null? (send (if dir? dirs files) get-selections)))))]
		   [select-this-dir (and dir?
					 (make-object button% "<- &Select" dir-pane
						      (lambda (b e)
							(send f show #f)
							(done))))]
		   [path-string-locale<? (lambda (p1 p2)
					   (string-locale<? (path->string p1) (path->string p2)))]
		   [reset-directory (lambda ()
				      (wx:begin-busy-cursor)
				      (let ([dir-exists? (directory-exists? dir)])
					(send m set-label (protect&
							   (if dir-exists?
							       (begin
								 (unless directory
								   (set! last-visted-directory dir))
								 (path->string dir))
							       (string-append "BAD DIRECTORY: " (path->string dir)))))
					(when select-this-dir
					  (send select-this-dir enable dir-exists?))
					(when create-button
					  (send create-button enable (not dir-exists?))))
				      (send dir-text set-value (path->string dir))
				      (let ([l (with-handlers ([void (lambda (x) null)])
						 (directory-list dir))]
					    [dot? (send dot-check get-value)])
					(let-values ([(ds fs)
						      (let loop ([l l][ds null][fs null])
							(cond
							 [(null? l) (values (cons (string->path "..")
										  (quicksort ds path-string-locale<?)) 
									    (quicksort fs path-string-locale<?))]
							 [(and (not dot?) 
							       (char=? (string-ref (path->string (car l)) 0) #\.)) 
							  (loop (cdr l) ds fs)]
							 [(file-exists? (build-path dir (car l))) (loop (cdr l) ds (cons (car l) fs))]
							 [else (loop (cdr l) (cons (car l) ds) fs)]))])
					  (set! dir-paths ds)
					  (send dirs set (map path->string ds))
					  (set! file-paths fs)
					  (send files set (map path->string fs))
					  (send dirs enable #t)
					  (unless dir?
					    (send files enable #t))
					  (update-ok)
					  (wx:end-busy-cursor))))]
		   [get-filename (lambda () 
				   (if dir?
				       dir
				       (let ([mk (lambda (f) (simplify-path (build-path dir f)))])
					 (let ([l (map mk (if typed-name
							      (list typed-name)
							      (map (lambda (p) (list-ref (if dir? dir-paths file-paths) p))
								   (send (if dir? dirs files) get-selections))))])
					   (if multi? l (car l))))))]
		   [done (lambda ()
			   (let ([name (get-filename)])
			     (unless (and put? (file-exists? name)
					  (eq? (message-box "Warning" 
							    (format "Replace ~s?" (path->string name) )
							    f '(yes-no)) 
					       'no)
					  (set! typed-name #f))
			       (set! ok? #t)
			       (send f show #f))))])
	    (send bp stretchable-height #f)
	    (send m stretchable-width #t)
	    (reset-directory)
	    (when filename
	      (when (string? filename)
		(set! filename (string->path filename)))
	      (let ([d (send dir-text get-value)])
		(send dir-text set-value (path->string (build-path d filename)))
		(set! typed-name filename)
		(send ok-button enable #t)))
	    (when put?
	      (send dir-text focus))
	    (when dir?
	      (send files enable #f))
	    (send f center)
	    (send f show #t)
	    (and ok? (get-filename))))))

					; We duplicate the case-lambda for `get-file', `get-file-list', and `put-file' so that they have the
					;   right arities and names

  (define default-filters '(("Any" "*.*")))

  (define get-file
    (case-lambda
     [() (get-file #f #f #f #f #f null)]
     [(message) (get-file message #f #f #f #f null)]
     [(message parent) (get-file message parent #f #f #f null)]
     [(message parent directory) (get-file message parent directory #f #f null)]
     [(message parent directory filename) (get-file message parent directory filename #f null)]
     [(message parent directory filename extension) (get-file message parent directory filename extension null)]
     [(message parent directory filename extension style)
      (get-file message parent directory filename extension style default-filters)]
     [(message parent directory filename extension style filters)
      ((mk-file-selector 'get-file #f #f #f #f) message parent directory filename extension style filters)]))

  (define get-file-list
    (case-lambda
     [() (get-file-list #f #f #f #f #f null)]
     [(message) (get-file-list message #f #f #f #f null)]
     [(message parent) (get-file-list message parent #f #f #f null)]
     [(message parent directory) (get-file-list message parent directory #f #f null)]
     [(message parent directory filename) (get-file-list message parent directory filename #f null)]
     [(message parent directory filename extension) (get-file-list message parent directory filename extension null)]
     [(message parent directory filename extension style)
      (get-file-list message parent directory filename extension style default-filters)]
     [(message parent directory filename extension style filters)
      ((mk-file-selector 'get-file-list #f #t #f #f) message parent directory filename extension style filters)]))

  (define put-file
    (case-lambda
     [() (put-file #f #f #f #f #f null)]
     [(message) (put-file message #f #f #f #f null)]
     [(message parent) (put-file message parent #f #f #f null)]
     [(message parent directory) (put-file message parent directory #f #f null)]
     [(message parent directory filename) (put-file message parent directory filename #f null)]
     [(message parent directory filename extension) (put-file message parent directory filename extension null)]
     [(message parent directory filename extension style)
      (put-file message parent directory filename extension style default-filters)]
     [(message parent directory filename extension style filters)
      ((mk-file-selector 'put-file #t #f #f #f) message parent directory filename extension style filters)]))

  (define get-directory
    (case-lambda
     [() (get-directory #f #f #f null)]
     [(message) (get-directory message #f #f null)]
     [(message parent) (get-directory message parent #f null)]
     [(message parent directory) (get-directory message parent directory null)]
     [(message parent directory style)
      ((mk-file-selector 'get-directory #f #f #t #f) message parent directory #f #f style null)]))

  (set-get-file! get-file))
