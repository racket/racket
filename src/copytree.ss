
(module copytree mzscheme
  
  (define-values (srcdir bindir collectsdir docdir libdir includepltdir libpltdir mandir origtree)
    (apply
     values
     (vector->list (current-command-line-arguments))))

  (define pltdir (build-path srcdir 'up))

  (define (skip-name? n)
    (let ([s (path->bytes n)])
      (or (regexp-match #rx#"^[.]svn$" s)
	  (regexp-match #rx#"^compiled$" s))))

  (define (copytree src dest)
    (for-each (lambda (n)
		(unless (skip-name? n)
		  (let ([p (build-path src n)])
		    (cond
		     [(file-exists? p) 
		      (let ([q (build-path dest n)])
			(when (file-exists? q)
			  (delete-file q))
			(copy-file p q)
			(let ([t (file-or-directory-modify-seconds p)])
			  (file-or-directory-modify-seconds q t)))]
		     [(directory-exists? p)
		      (let ([q (build-path dest n)])
			(unless (directory-exists? q)
			  (make-directory q))
			(copytree p q))]))))
	      (directory-list src)))

  (define (copytree* src dest)
    (printf "Copying ~a\n   to ~a\n" src dest)
    (copytree src dest))
		
  (copytree* (build-path pltdir "collects") collectsdir)
  (copytree* (build-path pltdir "doc") docdir)
  (copytree* (build-path pltdir "man") mandir)

  (unless (equal? origtree "yes")
    ;; Replace "config.ss"
    (with-output-to-file (build-path collectsdir "config" "config.ss")
      (lambda ()
	(printf "(module config (lib \"configtab.ss\" \"setup\")\n")
	(printf "  (define doc-dir ~s)\n" docdir)
	(printf "  (define lib-dir ~s)\n" libpltdir)
	(printf "  (define include-dir ~s)\n" includepltdir)
	(printf "  (define bin-dir ~s))\n" bindir))
      'truncate/replace))

  )
