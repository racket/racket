
(load-relative "loadtest.ss")

(Section 'file)

(require mzlib/file
	 mzlib/process
	 mzlib/list)

(parameterize ([current-directory (current-load-relative-directory)])
  (let ([rel (find-files values)]
	[abs (find-files values (current-directory))])
    (test #t = (length rel) (sub1 (length abs)))
    (test #f member "filelib.ss" abs)
    (test #f null? (member "filelib.ss" rel))
    (test #f null? (member (build-path (current-directory) "filelib.ss") abs))

    (test (list (string->path "filelib.ss")) find-files (lambda (f) (regexp-match "^filelib[.]ss$" (path->string f))))
    (test (list (build-path (current-directory) "filelib.ss"))
	  find-files (lambda (f) (regexp-match "filelib[.]ss$" (path->string f)))
	  (current-directory))

    (let ([rel2 (fold-files (lambda (name kind accum)
			      (test kind name (if (file-exists? name)
                                                  'file
                                                  'dir))
			      (cons name accum))
			    null)]
	  [sort (lambda (l)
		  (sort l (lambda (a b)
			    (bytes<? (path->bytes a) (path->bytes b)))))])
      (test #t equal? (sort rel) (sort rel2))

      (when (eq? (system-type) 'unix)
	(system "ln -s filelib.ss filelib-link.ss")
	(system "ln -s . loop-link")

	(test (+ 2 (length rel2))
	      fold-files 
	      (lambda (name kind accum)
		(test kind values (cond
				   [(link-exists? name) 'link]
				   [(file-exists? name) 'file]
				   [(directory-exists? name) 'dir]
				   [else '???]))
		(when (member name '("filelib-link.ss" "loop-link"))
		  (test kind name 'link))
		(add1 accum))
	      0
	      #f
	      #f)

	(system "rm loop-link")

	(test (+ 1 (length rel2))
	      fold-files 
	      (lambda (name kind accum)
		(test kind values (cond
				   [(file-exists? name) 'file]
				   [else 'dir]))
		(when (member name '("filelib-link.ss"))
		  (test kind name 'file))
		(add1 accum))
	      0
	      #f
	      #t)

	(system "rm filelib-link.ss")

	'done))))

(report-errs)
