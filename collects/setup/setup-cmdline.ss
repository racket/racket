
;; Command-line parsing is in its own module because it has to be used
;;  both in setup.ss (pre-zo, pre-cm) and setup-go.ss (use zos and cm).
;; This means that command lines will be parsed twice.

(module setup-cmdline mzscheme
  (require (lib "cmdline.ss"))
  
  (provide parse-cmdline)

  ;; The result of parse-cmdline is three lists:
  ;;  - An assoc list mapping flag symbols to booleans
  ;;     (nearly all symbols correspond to parameter names
  ;;      in setup-go.ss)
  ;;  - A list of specific collections
  ;;  - A list of archives

  (define (parse-cmdline argv)
    (define x-flags null)
    (define (add-flags l)
      (set! x-flags (append (reverse l) x-flags)))
    
    (define-values (x-specific-collections x-archives)
      (command-line
       "setup-plt"
       argv
       (once-each
	[("-c" "--clean") "Delete existing compiled files; implies -nxi"
	 (add-flags '((clean #t)
		      (make-zo #f)
		      (call-install #f)
		      (make-launchers #f)
		      (make-info-domain #f)))]
	[("-n" "--no-zo") "Do not produce .zo files"
	 (add-flags '((make-zo #f)))]
	[("-x" "--no-launcher") "Do not produce launcher programs"
	 (add-flags '((make-launchers #f)))]
	[("-i" "--no-install") "Do not call collection-specific pre-installers"
	 (add-flags '((call-install #f)))]
	[("-d" "--no-info-domain") "Do not produce info-domain caches"
	 (add-flags '((make-info-domain #f)))]
	[("-e" "--extension") "Produce native code extensions"
	 (add-flags '((make-so #t)))]
	[("-v" "--verbose") "See names of compiled files and info printfs"
	 (add-flags '((verbose #t)))]
	[("-m" "--make-verbose") "See make and compiler usual messages"
	 (add-flags '((make-verbose #t)))]
	[("-r" "--compile-verbose") "See make and compiler verbose messages"
	 (add-flags '((make-verbose #t)
		      (compiler-verbose #t)))]
	[("--trust-zos") "Trust existing .zos (use only with prepackaged .zos)"
	 (add-flags '((trust-existing-zos #t)))]
	[("-p" "--pause") "Pause at the end if there are any errors"
	 (add-flags '((pause-on-errors #t)))]
	[("--force") "Treat version mismatches for archives as mere warnings"
	 (add-flags '((force-unpacks #t)))]
	[("-a" "--all-users") "Install archives into PLTHOME, not user-specific directory"
	 (add-flags '((all-users #t)))]
	[("--mode") mode "Select a compilation mode"
	 (add-flags `((compile-mode ,mode)))]
	[("-l") =>
	 (lambda (flag . collections)
	   (map list collections))
	 '("Setup specific <collection>s only" "collection")])
       (=>
	(lambda (collections . archives)
	  (values (if (null? collections)
		      null
		      (car collections))
		  archives))
	'("archive")
	(lambda (s)
	  (display s)
	  (printf "If no <archive> or -l <collection> is specified, all collections are setup~n")
	  (exit 0)))))

    (values x-flags x-specific-collections x-archives)))
