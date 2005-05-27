
(module restart mzscheme
  (require "cmdline.ss")

  (provide restart-mzscheme)

  (define (restart-mzscheme init-argv adjust-flag-table argv init-namespace)
    (let* ([result #t]
	   [args #f]
	   [mute-banner? #f]
	   [no-rep? #f]
	   [no-coll-paths? #f]
	   [no-init-file? #f]
           [no-define-argv? #f]
	   [case-sensitive? #f]
	   [allow-set!-undefined? #t]
	   [print-error
	    (lambda (e)
	      (if (exn? e)
		  (fprintf (current-error-port) "~a~n" (exn-message e))
		  (fprintf (current-error-port) "Exception in init file: ~e~n" e)))]
           [beginize (lambda (l)
                       (string-append
                        "(begin "
                        (apply string-append l)
                        ")"))]
           [script (lambda (flags proc like)
                     `[,flags
                       ,(lambda (f file) 
                          (begin0
                            (format proc file)
                            (set! mute-banner? #t)
                            (set! no-rep? #t)))
                       (,(format "Same as -~amv-" like) "file")])]
	   [table
	    `([multi
	       [("-e" "--eval")
		,(lambda (f expr) expr)
		("Evaluates <expr>" "expr")]
	       [("-f" "--load")
		,(lambda (f file) (format "(load ~s)" file))
		("Loads <file>" "file")]
	       [("-d" "--load-cd")
		,(lambda (f file) (format "(load/cd ~s)" file))
		("Load/cds <file>" "file")]
	       [("-t" "--require")
		,(lambda (f file) (format "(require (file ~s))" file))
		("Requires <file>" "file")]
	       [("-F" "--Load")
		,(lambda (f . files) (beginize
                                      (map (lambda (file)
                                             (format "(load ~s)" file))
                                           files)))
		("Loads all <file>s" "file")]
	       [("-D" "--Load-cd")
		,(lambda (f . files) (beginize
                                      (map (lambda (file)
                                             (format "(load/cd ~s)" file))
                                           files)))
		("Load/cds all <file>s" "file")]
	       [("-T" "--Require")
		,(lambda (f . files) (beginize
                                      (map (lambda (file)
                                             (format "(require (file ~s))" file))
                                           files)))
		("Requires all <file>s" "file")]
	       [("-l" "--mzlib")
		,(lambda (f file) (format "(require (lib ~s))" file))
		("Imports library <file>" "file")]
	       [("-L")
		,(lambda (f file collection) (format "(require (lib ~s ~s))" file collection))
		("Imports library <file> in <collection>" "file" "collection")]
	       [("-M")
		,(lambda (f collection) (format "(require (lib ~s ~s))"
                                                (format "~a.ss" collection)
                                                collection))
		("Imports library <collection>.ss in <collection>" "collection")]]
              [final
               ,(script '("-r" "--script") "(load ~s)" "f")
               ,(script '("-u" "--require-script") "(require (file ~s))"  "t")
	       ,(script '("-i" "--script-cd") "(load/cd ~s)" "d")]
              [multi
	       [("-w" "--awk")
		,(lambda (f) "(require-library \"awk.ss\")")
		("Same as -l awk.ss")]
	       [("-k")
		,(lambda (f n m) (error 'mzscheme "The -k flag is not supported in this mode"))
		("Load executable-embedded code from file offset <n> to <m>" "n" "m")]
               [("-C" "--main")
                ,(lambda (f) (lambda ()
                               (when result
                                 ((eval 'main) args))))
                ("Calls `main' with a list of argument strings, if no prior errors")]
	       [("-x" "--no-init-path")
		,(lambda (f) (set! no-coll-paths? #t))
		("Don't set current-library-collection-paths")]
	       [("-q" "--no-init-file")
		,(lambda (f) (set! no-init-file? #t))
		("Don't load \"~/.mzschemerc\" or \"mzscheme.rc\"")]
               [("-A")
                ,(lambda (f) (set! no-define-argv? #t))
		("Don't define `argv' or `program'")]
	       [("-g" "--case-sens")
		,(lambda (f) (set! case-sensitive? #t))
		("Identifiers and symbols are initially case-sensitive")]
	       [("-s" "--set-undef")
		,(lambda (f) (set! allow-set!-undefined? #t))
		("Set! works on undefined identifiers")]
	       [("-m" "--mute-banner")
		,(lambda (f) (set! mute-banner? #t))
		("Suppresses the startup banner text")]
	       [("-v" "--version")
		,(lambda (f) (set! no-rep? #t))
		("Suppresses the read-eval-print loop")]
	       [("-b" "--binary")
		,(lambda (f) (error 'mzscheme "The -b flag is not supported in this mode"))
		("Read stdin and write stdout/stderr in binary mode")]
	       [("--restore")
		,(lambda (f) (error 'mzscheme "The --restore flag is not supported in this mode"))
		("Not supported")]])])
      (parse-command-line
       "mzscheme"
       init-argv
       table
       void
       '("ignored"))
      (set! args #f)
      (parse-command-line
       "mzscheme"
       argv
       (adjust-flag-table table)
       (lambda (exprs . rest)
	 (unless (null? rest)
	   (set! args rest))
	 ;(when args (set! rest args))
	 (let ([n (make-namespace)]
	       [argv (if args (list->vector args) (vector))])
	   (parameterize ([current-command-line-arguments argv])
	     (thread-wait
	      (thread
	       (lambda ()
		 (current-namespace n)
		 (namespace-transformer-require 'mzscheme)
		 (let ([program (find-system-path 'exec-file)])
		   (read-case-sensitive case-sensitive?)
		   (compile-allow-set!-undefined allow-set!-undefined?)
		   
		   (unless mute-banner? (display (banner)))
		   
		   (unless no-define-argv?
		     (eval `(define-values (argv) (quote ,argv)))
		     (eval `(define-values (program) (quote ,program))))
		   
		   (find-library-collection-paths))
		 
		 (init-namespace)
		 
		 (unless no-init-file?
		   (let ([f (find-system-path 'init-file)])
		     (when (file-exists? f)
		       (with-handlers ([void print-error])
			 (load f)))))
		 
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
		   (unless no-rep? 
		     (read-eval-print-loop)))))))))
       `("arg"))
      result)))
