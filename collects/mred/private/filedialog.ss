(module filedialog mzscheme
  (require (lib "class.ss")
	   (lib "etc.ss")
	   (prefix wx: "kernel.ss")
	   "lock.ss"
	   "wx.ss"
	   "cycle.ss"
	   "check.ss"
	   "mrtop.ss"
	   "path-dialog.ss")

  (provide get-file
	   get-file-list
	   put-file
	   get-directory)

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
      (if (or (eq? (system-type) 'unix) force-unix?)
        (send (new path-dialog%
                [put?      put?]
                [dir?      dir?]
                [multi?    multi?]
                [message   message]
                [parent    parent]
                [directory directory]
                [filename  filename]
                [filters
                 (cond [(eq? filters default-filters) #t] ; has its own defaults
                       [dir? #f]
                       [else filters])])
              run)
        (let ([s (wx:file-selector
                  message directory filename extension
                  ;; file types:
                  (apply string-append
                         (map (lambda (s) (format "~a|~a|" (car s) (cadr s)))
                              filters))
                  ;; style:
                  (cons (cond [dir? 'dir]
                              [put? 'put]
                              [multi? 'multi]
                              [else 'get])
                        style)
                  ;; parent:
                  (and parent (mred->wx parent)))])
          (if (and multi? s)
            (map bytes->path (files->list (path->bytes s)))
            s)))))

  (define default-filters '(("Any" "*.*")))

  ;; We duplicate the case-lambda for `get-file', `get-file-list', and
  ;; `put-file' so that they have the right arities and names

  (define-syntax define-file-selector
    (syntax-rules ()
      [(_ name put? multi?)
       (define name
         (opt-lambda ([message #f] [parent #f] [directory #f] [filename #f]
                      [extension #f] [style null] [filters default-filters])
           ((mk-file-selector 'name put? multi? #f #f)
            message parent directory filename extension style filters)))]))

  (define-file-selector get-file      #f #f)
  (define-file-selector get-file-list #f #t)
  (define-file-selector put-file      #t #f)

  (define get-directory
    (opt-lambda ([message #f] [parent #f] [directory #f] [style null])
      ((mk-file-selector 'get-directory #f #f #t #f)
       message parent directory #f #f style null)))

  (set-get-file! get-file))
