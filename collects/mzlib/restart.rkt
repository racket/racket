
(module restart scheme/base
  (require racket/cmdline)

  (provide restart-mzscheme)

  (define (restart-mzscheme init-argv adjust-flag-table argv init-namespace)
    (let* ([result #t]
	   [args #f]
	   [version? #f]
	   [rep? #f]
	   [no-coll-paths? #f]
	   [no-init-file? #f]
	   [no-compiled? #f]
	   [scheme? #f]
	   [scheme-set? #f]
	   [print-error
	    (lambda (e)
	      (if (exn? e)
		  (eprintf "~a\n" (exn-message e))
		  (eprintf "Exception in init file: ~e\n" e)))]
           [beginize (lambda (l)
                       (string-append
                        "(begin "
                        (apply string-append l)
                        ")"))]
	   [load-scheme (lambda (s?)
			  (unless scheme-set?
				  (set! scheme? s?)
				  (set! scheme-set? #t)))]
           [script (lambda (flags proc like s?)
                     `[,flags
                       ,(lambda (f file) (load-scheme s?) (lambda () (proc file)))
                       (,(format "Same as -~a-" like) "file")])]
	   [table
	    `([multi
	       [("-e" "--eval")
		,(lambda (f expr) (load-scheme #t) expr)
		("Evaluates <expr>" "expr")]
	       [("-f" "--load")
		,(lambda (f file) (load-scheme #t) (lambda () (load file)))
		("Loads <file>" "file")]
	       [("-t" "--require")
		,(lambda (f file) (load-scheme #f) (lambda () (namespace-require `(file ,file))))
		("Requires <file>" "file")]
	       [("-l" "--lib")
		,(lambda (f file) (load-scheme #f) (lambda () (namespace-require `(lib ,file))))
		("Imports library <file>" "file")]]
              [final
               ,(script '("-r" "--script") load "f" #t)
               ,(script '("-u" "--require-script") (lambda (f) (namespace-require `(file ,f))) "t" #f)]
              [multi
	       [("-k")
		,(lambda (f n m) (error 'mzscheme "The -k flag is not supported in this mode"))
		("Load executable-embedded code from file offset <n> to <m>" "n" "m")]
               [("-m" "--main")
                ,(lambda (f) (lambda ()
                               (when result
				 (eval `(main ,@(cdr args))))))
                ("Calls `main' with a list of argument strings, if no prior errors")]
	       [("-x" "--no-init-path")
		,(lambda (f) (set! no-coll-paths? #t))
		("Don't set current-library-collection-paths")]
	       [("-q" "--no-init-file")
		,(lambda (f) (set! no-init-file? #t))
		("Don't load \"~/.mzschemerc\" or \"mzscheme.rc\"")]
	       [("-c" "--no-compiled")
		,(lambda (f) (set! no-compiled? #t))
		("Don't use compiled bytecode")]
	       [("-v" "--version")
		,(lambda (f) (set! version? #t))
		("Show the version")]
	       [("-i" "--repl")
		,(lambda (f) (load-scheme #t) (set! rep? #t))
		("Run the read-eval-print loop")]
	       [("-b" "--binary")
		,(lambda (f) (error 'mzscheme "The -b flag is not supported in this mode"))
		("Read stdin and write stdout/stderr in binary mode")]])])
      (parse-command-line
       "mzscheme"
       init-argv
       table
       void
       '("ignored"))
      (set! args #f)
      (when (equal? argv #())
	(set! scheme? #t)
	(set! rep? #t))
      (parse-command-line
       "mzscheme"
       argv
       (adjust-flag-table table)
       (lambda (exprs . rest)
	 (unless (null? rest)
	   (set! args rest))
	 ;(when args (set! rest args))
	 (let ([n (make-base-empty-namespace)]
	       [argv (if args (list->vector args) (vector))])
	   (parameterize ([current-command-line-arguments argv]
                          [print-as-expression #f])
	     (thread-wait
	      (thread
	       (lambda ()
		 (current-namespace n)
		 (when no-compiled?
		   (use-compiled-file-paths null))
		 (let ([program (find-system-path 'exec-file)])
		   (when version? (display (banner)))
		   
		   (find-library-collection-paths))
		 
		 (init-namespace)

		 (when scheme?
		   (namespace-require 'scheme/init))
		 
		 (let/ec k
		   (exit-handler
		    (lambda (status)
		      (set! result status)
		      (k #f)))
		   (let/ec escape
		     (for-each
		      (lambda (e)
			(with-handlers ([void (lambda (e) 
						(print-error e) 
						(set! result #f)
						(escape #f))])
			  (if (string? e)
			      (eval (read (open-input-string e)))
			      (e))))
		      exprs))
		   (when rep? 
		     (unless no-init-file?
		       (let ([f (find-system-path 'init-file)])
			 (when (file-exists? f)
			       (with-handlers ([void print-error])
					      (load f)))))
		     (read-eval-print-loop)))))))))
       `("arg"))
      result)))
