;; Compiler driver routines 
;; (c) 1996-1997 Sebastian Good
;; (c) 1997-2001 PLT
;;
;; Scheme->C compilation Overview
;; ------------------------------
;;
;; Compilation is performed in a number of phases,
;;  each of which is implemented in its own unit:
;;
;;   1) Reading/parsing - Zodiac collection
;;   2) Prephase - prephase.ss
;;   3) A-normalization - anorm.ss
;;   4) Known-value analysis - known.ss
;;   5) Lexical analysis and inlining - analyze.ss
;;   6) Static procedure lifting - lift.ss
;;   7) Static procedure lifting after LWCC - lift.ss (optional)
;;   8) Closure conversion - closure.ss
;;   9) Closure vehicle assignment - vehicle.ss
;;  10) Representation choosing - rep.ss
;;  11) Scheme to virtual machine translation - vmphase.ss
;;  12) Optimizations on VM code - vmopt.ss
;;  13) VM to C translation - vm2c.ss
;;
;; For more information about a phase, see the file
;;  implementing that phase.
;;
;; All steps up to vmphase.ss work on a Scheme program, representated
;;  as a zodiac AST. The AST produced by zodiac is destructively
;;  modified by each phase (usually); mzc-specific information is
;;  stored in the AST as ``annotations''. At the implementation file
;;  for each phase, the annotations installed or changed by the phase
;;  are listed.
;;
;; All nodes in the AST must be unique, except for nodes representing
;;  constant values. Don't even reuse varref or binding nodes within
;;  an AST.
;;
;; C code is compiled and linked via procedures provided by the
;;  dynext collection.
;;
;; In this implementation, `var' is used for variable names in
;;  confusing and inconsistent ways. There are two different
;;  AST entities that could be called "var":
;;     1) binding instances of variables, e.g., the formal
;;        arguments of a lambda; these are always called
;;        `bindings' in Zodiac terminology
;;     2) bound instances of variables, e.g., a free variable 
;;        in a sub-expression; these are always called
;;        `varrefs' in Zodiac terminology.
;;  Almost all information about a variable is stored with a
;;   zodiac:binding AST node, and very little information is
;;   stored with a zodiac:varref AST node.
;;  To make matters worse, the name `binding' is overloaded.
;;    `zodiac:binding' is the name of a Zodiac structure
;;    type, and `binding' is also the name of the structure
;;    type for annotations attached to zodiac:binding objects.
;;  If you create a new lexical binding, note that the procedure
;;    zodiac:binding->lexical-varref will create varrefs to 
;;    the binding.

(module driver mzscheme
  (require (lib "unitsig.ss")
	   (lib "list.ss")
	   (lib "file.ss")
	   (lib "etc.ss")
	   (lib "pretty.ss")
	   (prefix src2src: "../src2src.ss"))
  
  (require (lib "zodiac-sig.ss" "syntax")
	   (lib "toplevel.ss" "syntax")
	   (lib "compile-sig.ss" "dynext")
	   (lib "link-sig.ss" "dynext")
	   (lib "file-sig.ss" "dynext"))

  (require "../sig.ss")
  (require "sig.ss")

  (provide driver@)

  (define driver@
    (unit/sig compiler:driver^
      (import (compiler:option : compiler:option^)
	      compiler:library^
	      compiler:cstructs^
	      (zodiac : zodiac^)
	      compiler:zlayer^
	      compiler:prephase^
	      compiler:anorm^
	      compiler:known^
	      compiler:analyze^
	      compiler:const^
	      compiler:lift^
	      compiler:closure^
	      compiler:vehicle^
	      compiler:rep^
	      compiler:vmstructs^
	      compiler:vmphase^
	      compiler:vmopt^
	      compiler:vm2c^
	      compiler:top-level^
	      dynext:compile^
	      dynext:link^
	      dynext:file^)
      (rename (compile-extension* compile-extension))
      
      (define debug:file "dump.txt")
      (define debug:port #f)
      (define (debug:get-port) debug:port)
      (define debug
	(lambda x
	  (when (and (compiler:option:debug) debug:port)
	    (apply fprintf (cons debug:port x))
	    (flush-output debug:port))))
  
      ;;----------------------------------------------------------------------------
      ;; FILE PROCESSING FUNCTIONS
      ;;
      
      ;; takes an input-name from the compile command and returns 4 values
      ;; 1) an input path
      ;; 2) a C output path
      ;; 3) a constant pool output path
      ;; 4) an obj output path
      ;; 5) a dll output path
      ;; 6) a scheme_setup suffix
      (define s:process-filenames
	(lambda (input-name dest-dir from-c? tmp-c? tmp-o?)
	  (let-values ([(basedir file dir?) (split-path input-name)])
	    (let* ([dest-dir (if (eq? dest-dir 'auto)
				 (let ([d (build-path (if (eq? basedir 'relative)
							  'same
							  basedir)
						      "compiled"
						      "native"
						      (system-library-subpath))])
				   (unless (directory-exists? d)
				     (make-directory* d))
				   d)
				 dest-dir)]
		   [path-prefix (lambda (a b)
				  (bytes->path (bytes-append a (path->bytes b))))]
		   [sbase (extract-base-filename/ss file (if from-c? #f 'mzc))]
		   [cbase (extract-base-filename/c file (if from-c? 'mzc #f))]
		   [base (or sbase cbase)]
		   [c-dir (if tmp-c?
			      (find-system-path 'temp-dir)
			      dest-dir)]
		   [c-prefix (if tmp-c?
				 (lambda (s) (path-prefix #"mzcTMP" s))
				 values)]
		   [o-dir (if tmp-o?
			      (find-system-path 'temp-dir)
			      dest-dir)]
		   [o-prefix (if tmp-o?
				 (lambda (s) (path-prefix #"mzcTMP" s))
				 values)])
	      (unless base
		(error 'mzc "not a Scheme or C file: ~a" input-name))
	      (values (if sbase 
			  input-name
			  #f)
		      (if cbase 
			  input-name 
			  (build-path c-dir (c-prefix (append-c-suffix base))))
		      (build-path o-dir (o-prefix (append-constant-pool-suffix base)))
		      (build-path o-dir (o-prefix (append-object-suffix base)))
		      (build-path dest-dir (append-extension-suffix base))
		      (string-append (compiler:clean-string (compiler:option:setup-prefix)) 
				     "_" 
				     (compiler:clean-string (path->string base))))))))
      
      (define elaboration-exn-handler
	(lambda (exn)
	  (compiler:fatal-error 
	   #f
	   (format "Error during elaboration: ~a" 
		   (if (exn? exn)
		       (exn-message exn)
		       exn)))
	  (raise exn)))
      
      (define prefix-exn-handler
	(lambda (exn)
	  (compiler:fatal-error 
	   #f
	   (format "Error during prefix loading: ~a" 
		   (if (exn? exn)
		       (exn-message exn)
		       exn)))
	  (raise exn)))
      
      (define top-level-exn-handler
	(lambda (exn)
	  (set! compiler:messages (reverse! compiler:messages))
	  (compiler:report-messages! #t)
	  (exit 1)))
  
      (define s:expand-top-level-expressions!
	(lambda (input-directory reader verbose?)
	  (when verbose? (printf "~n Reading... ") (flush-output))
	  ;; During reads, errors are truly fatal
	  (let ([exprs (let ([failed? #f])
			 (let loop ([n 1])
			   (let ([sexp (reader)])
			     (if (eof-object? sexp)
				 null
				 (begin 
				   (when (compiler:option:debug)
				     (debug "~a[~a.~a]_"
					    n
					    (zodiac:location-line
					     (zodiac:zodiac-start sexp))
					    (zodiac:location-column
					     (zodiac:zodiac-start sexp))))
				   (cons sexp (loop (+ n 1))))))))])
	    (unless (null? compiler:messages) (when (compiler:option:verbose) (newline)))
	    (compiler:report-messages! #t)
	    (when verbose? (printf " expanding...~n"))
	    (parameterize ([current-load-relative-directory input-directory])
	      (map (lambda (expr)
		     (let ([expanded ((if has-prefix?
					  expand-top-level-with-compile-time-evals
					  expand) 
				      expr)])
		       (zodiac:syntax->zodiac 
			(let ([p (src2src:optimize expanded #t)])
			  '(with-output-to-file "/tmp/l.ss"
			     (lambda () (pretty-print (syntax-object->datum p)))
			     'replace)
			  (let ([opt-expanded (expand p)])
			    ;; (pretty-print (syntax-object->datum opt-expanded))
			    opt-expanded)))))
		   exprs)))))

      (define elaborate-namespace (make-namespace))
	   
      (define has-prefix? #f)

      (define (eval-compile-prefix prefix)
	(set! has-prefix? (and prefix #t))
	(with-handlers ([void top-level-exn-handler])
	  (with-handlers ([void prefix-exn-handler])
	    (parameterize ([current-namespace elaborate-namespace])
	      (eval (or prefix
			;; Need MzScheme and cffi:
			'(begin
			   (require (lib "cffi.ss" "compiler"))
			   (require-for-syntax mzscheme))))))))

      ;;----------------------------------------------------------------------
      ;; Misc utils

      ;; see (single) use for info:
      (define (split-module m)
	(let ([info (get-annotation m)])
	  (let ([mk
		 (lambda (expr mode)
		   (let ([ast (zodiac:make-module-form
			       (zodiac:zodiac-stx expr)
			       (make-empty-box)
			       (zodiac:module-form-name m)
			       (zodiac:module-form-requires m)
			       (zodiac:module-form-for-syntax-requires m)
			       (zodiac:module-form-for-template-requires m)
			       expr #f
			       (zodiac:module-form-provides m)
			       (zodiac:module-form-syntax-provides m)
			       (zodiac:module-form-indirect-provides m)
			       (zodiac:module-form-kernel-reprovide-hint m)
			       (zodiac:module-form-self-path-index m))])
		     (set-annotation! 
		      ast 
		      (make-module-info ((if (eq? mode 'syntax-body)
					     module-info-syntax-invoke
					     module-info-invoke)
					 info)
					#f 
					mode))
		     ast))]
		[body->list
		 (lambda (expr)
		   (if (zodiac:begin-form? expr)
		       (zodiac:begin-form-bodies expr)
		       (list expr)))])
	    (append
	     (map (lambda (x) (mk x 'body))
		  (body->list
		   (zodiac:module-form-body m)))
	     (map (lambda (x) (mk x 'syntax-body))
		  (body->list
		   (zodiac:module-form-syntax-body m)))
	     (list (mk 
		    ;; Construct constant expression for module construction info:
		    (let ([q (zodiac:make-quote-form
			      (zodiac:zodiac-stx m)
			      (make-empty-box)
			      (zodiac:make-zread
			       (datum->syntax-object
				#f
				(list (zodiac:module-form-name m)
				      (zodiac:module-form-requires m)
				      (zodiac:module-form-for-syntax-requires m)
				      (zodiac:module-form-for-template-requires m)
				      (filter (if (zodiac:module-form-kernel-reprovide-hint m)
						  (lambda (x) (or (symbol? x)
								  (not (eq? '#%kernel (car x)))))
						  (lambda (x) #t))
					      (zodiac:module-form-provides m))
				      (zodiac:module-form-syntax-provides m)
				      (zodiac:module-form-indirect-provides m)
				      (zodiac:module-form-kernel-reprovide-hint m))
				(zodiac:zodiac-stx m))))])
		      (set-annotation! q 'immutable)
		      q)
		    'constructor))))))

      ;; takes a list of a-normalized expressions and analyzes them
      ;; returns the analyzed code, a list of local variable lists, 
      ;; used variable lists, and captured variable lists
      (define s:analyze-source-list
	(lambda (source)
	  (let loop ([sexps source] [source-acc null] 
				    [locals-acc null] [globals-acc null] [used-acc null] [captured-acc null]
				    [children-acc null]
				    [max-arity 0])
	    (if (null? sexps)
		(values (reverse! source-acc)
			(map (lambda (loc glob used cap children)
			       (let ([c (make-code empty-set
						   loc
						   glob
						   used
						   cap
						   #f
						   #f
						   children)])
				 (for-each (lambda (child) (set-code-parent! child c)) children)
				 c))
			     (reverse! locals-acc)
			     (reverse! globals-acc)
			     (reverse! used-acc)
			     (reverse! captured-acc)
			     (reverse! children-acc))
			max-arity)
		(begin
		  (varref:current-invoke-module 
		   (and (zodiac:module-form? (car sexps))
			(let ([info (get-annotation (car sexps))])
			  (and (not (eq? (module-info-part info) 'constructor))
			       (module-info-invoke info)))))

		  ;; (printf "~a~n" (syntax-line (zodiac:zodiac-stx (car sexps))))

		  (let-values ([(exp free-vars local-vars global-vars used-vars captured-vars
				     children new-max-arity multi)
				(analyze-expression! (car sexps) empty-set null (null? (cdr sexps)))])

		    
		    (let ([sc-max-arity
			   ;; Adds to const, per-load-const, per-invoke-const lists:
			   (if (or (null? (cdr sexps))
				   (not (zodiac:module-form? (car sexps)))
				   (not (zodiac:module-form? (cadr sexps)))
				   (let ([a1 (get-annotation (car sexps))]
					 [a2 (get-annotation (cadr sexps))])
				     (not (and (eq? (module-info-part a1)
						    (module-info-part a2))
					       (eq? (module-info-invoke a1)
						    (module-info-invoke a2))))))
			       (compiler:finish-syntax-constants!)
			       0)])

		      (varref:current-invoke-module #f)
		      
		      (loop (cdr sexps) 
			    (cons exp source-acc) 
			    (cons local-vars locals-acc)
			    (cons global-vars globals-acc)
			    (cons used-vars used-acc)
			    (cons captured-vars captured-acc)
			    (cons children children-acc)
			    (max max-arity new-max-arity sc-max-arity)))))))))

      ;; Lift static procedures
      (define s:lift
	(lambda ()
	  (compiler:init-lifted-lambda-list!)
	  (compiler:init-once-closure-lists!)

	  (let ([l (map lift-lambdas! (block-source s:file-block) (block-codes s:file-block))]
		[reset-globals (lambda (code globals)
				 (set-code-global-vars! code globals)
				 code)])
	    
	    ;; Splice lifted lambda definitions into the program in the right
	    ;; place: statics after true constants, and per-load statics after
	    ;; per-load constants.
	    (let loop ([n number-of-true-constants]
		       [l l][c (block-codes s:file-block)]
		       [l-acc null][c-acc null])
	      (if (zero? n)
		  (let loop ([n number-of-per-load-constants]
			     [l l][c c]
			     [pll-acc null][plc-acc null])
		    (if (zero? n)
			(begin
			  (set-block-source! 
			   s:file-block
			   (append (reverse l-acc)
				   (compiler:get-lifted-lambdas)
				   (reverse pll-acc)
				   (compiler:get-once-closures-list)
				   (map car l)))
			  (set-block-codes!
			   s:file-block 
			   (append (reverse c-acc) 
				   (map
				    (lambda (ll)
				      (make-code empty-set
						 empty-set
						 empty-set ; no globals
						 empty-set
						 empty-set
						 #f #f
						 (list 
						  (get-annotation 
						   (zodiac:define-values-form-val ll)))))
				    (compiler:get-lifted-lambdas))
				   (reverse plc-acc)
				   (map (lambda (ll globs) 
					  (make-code empty-set
						     empty-set
						     globs
						     empty-set
						     empty-set
						     #f #f
						     (list
						      (get-annotation 
						       (zodiac:define-values-form-val 
							(if (zodiac:module-form? ll)
							    (zodiac:module-form-body ll)
							    ll))))))
					(compiler:get-once-closures-list)
					(compiler:get-once-closures-globals-list))
				   (map reset-globals c (map cdr l)))))
			(loop (sub1 n) (cdr l) (cdr c) 
			      (cons (caar l) pll-acc) (cons (reset-globals (car c) (cdar l)) plc-acc))))
		  (loop (sub1 n) (cdr l) (cdr c) 
			(cons (caar l) l-acc) (cons (reset-globals (car c) (cdar l)) c-acc)))))
	  
	  ;; Lifted lambdas are true constants:
	  (set! number-of-true-constants (+ number-of-true-constants
					    (length (compiler:get-lifted-lambdas))))))

      (define s:append-block-sources!
	(lambda (file-block l)
	  (set-block-codes!
	   file-block
	   (append!
	    (map
	     (lambda (glob)
	       (make-code empty-set
			  empty-set
			  empty-set
			  empty-set
			  empty-set
			  #f #f null))
	     l)
	    (block-codes file-block)))
	  (set-block-source! 
	   file-block
	   (append! l (block-source file-block)))))

      (define (open-input-scheme-file path)
	(let ([p (let ([open (with-handlers ([exn:fail? (lambda (x) #f)])
			       (dynamic-require '(lib "mred.ss" "mred") 'open-input-graphical-file))])
		   (if open
		       ;; Handles WXME files:
		       (open path)
		       ;; Check for WXME and give a nice error message:
		       (let ([p (open-input-file path)])
			 (when (regexp-match-peek "^WXME01[0-9][0-9] ## " p)
			   (close-input-port p)
			   (error 'compile-file
				  "file appears to have graphical syntax (try graphical-mzc): ~a"
				  path))
			 p)))])
	  ;; Skip leading "#!:
	  (let loop ([s (regexp-match-peek #rx"^#![^\r\n]*" p)])
	    (when s
	      (read-line p)
	      (when (regexp-match #rx"\\\\$" (car s))
		(loop (regexp-match-peek #rx"^[^\r\n]*" p)))))
	  p))

      ;;-------------------------------------------------------------------------------
      ;; ERROR/WARNING REPORTING/HANDLING ROUTINES
      ;;
      (define compiler:messages null)
      (define compiler:make-message
	(lambda (constructor)
	  (lambda (ast message)
	    (set! compiler:messages (cons (constructor ast message)
					  compiler:messages)))))
      (define compiler:error (compiler:make-message make-compiler:fatal-error-msg))
      (define compiler:fatal-error compiler:error)
      (define compiler:internal-error
	(case-lambda
	 [(ast message)
	  (set! compiler:messages 
		(reverse! (cons (make-compiler:internal-error-msg ast message)
				compiler:messages)))
	  (compiler:report-messages! #t)]
	 [(ast fmt . args)
	  (compiler:internal-error ast (apply format fmt args))]))
      
      (define compiler:warning (compiler:make-message make-compiler:warning-msg))

      (define compiler:report-messages!
	(lambda (stop-on-errors?)
	  (let ([error-count 0]
		[fatal-error-count 0]
		[msgs (reverse! compiler:messages)])
	    (set! compiler:messages null)
	    (for-each (lambda (message)
			(when (compiler:error-msg? message) 
			  (set! error-count (add1 error-count)))
			(when (or (compiler:fatal-error-msg? message) 
				  (compiler:internal-error-msg? message))
			  (set! fatal-error-count (add1 fatal-error-count)))

			(let* ([ast (compiler:message-ast message)]
			       [string (compiler:message-message message)])
			  (zodiac:print-start! (current-output-port) ast)
			  (printf 
			   "~a: ~a~n"
			   (cond
			    [(compiler:error-msg? message) "Error"]
			    [(compiler:warning-msg? message) "Warning"]
			    [(compiler:fatal-error-msg? message) "Error"]
			    [(compiler:internal-error-msg? message) "INTERNAL ERROR"]
			    [else (error 'report-messages "internal error")])
			   string)
			  (when (compiler:internal-error-msg? message)
			    (printf 
			     (string-append
			      " please report the bug using Help Desk~n"
			      "  or http://bugs.plt-scheme.org/~n"
			      "  and include a transcript in verbose mode~n")))))
		      
		      msgs)
	    (when (and stop-on-errors?
		       (or (positive? error-count)
			   (positive? fatal-error-count)))
	      (error "Errors encountered.  Compilation aborted.")))))

      (define total-cpu-time 0)
      (define total-real-time 0)
      (define verbose-time
	(lambda (thunk)
	  (let-values ([(vals cpu real gc) (time-apply thunk null)])
	    (set! total-cpu-time (+ total-cpu-time cpu))
	    (set! total-real-time (+ total-real-time real))
	    (when (compiler:option:verbose)
	      (printf "      [cpu: ~ams, real: ~ams, gc: ~ams]~n" cpu real gc))
	    (apply values vals))))

      ;;-----------------------------------------------------------------------------
      ;; File-level Block information
      
      (define s:file-block (make-empty-block))
      (define s:max-arity 0) ; compilation-wide max
      (define s:register-max-arity!
	(lambda (n) (set! s:max-arity (max s:max-arity n))))

      (define number-of-true-constants 0)
      (define number-of-per-load-constants 0)
      
      (define s:unit-list null) ; list of units in the code
      
      (define compiler:setup-suffix "")

      (define (get-s:file-block) s:file-block)
      (define (compiler:get-setup-suffix) compiler:setup-suffix)

      (define c-declares null)
      (define (register-c-declaration str)
	(set! c-declares (cons str c-declares)))

      (define c-lambdas null)
      (define (register-c-lambda-function name body)
	(set! c-lambdas (cons (cons name body) c-lambdas)))

      ;;-----------------------------------------------------------------------------
      ;; THE MAIN DRIVING ROUTINE

      (define (compile-extension* input-name dest-directory)
	(s:compile #f #f #f input-name dest-directory))
      (define (compile-extension-to-c input-name dest-directory)
	(s:compile #t #f #f input-name dest-directory))
      (define (compile-c-extension input-name dest-directory)
	(s:compile #f #f #t input-name dest-directory))

      (define (compile-extension-part input-name dest-directory)
	(s:compile #f #t #f input-name dest-directory))
      (define (compile-extension-part-to-c input-name dest-directory)
	(s:compile #t #t #f input-name dest-directory))
      (define (compile-c-extension-part input-name dest-directory)
	(s:compile #f #t #t input-name dest-directory))

      (define compiler:multi-o-constant-pool (make-parameter #f))

      (define compiler:module-decl-name #f)

      (define s:compile
	(lambda (c-only? multi-o? from-c? input-name dest-directory)
	  (define input-directory 
	    (let-values ([(base file dir?)
			  (split-path (path->complete-path input-name))])
	      base))
	  (compiler:multi-o-constant-pool multi-o?)
	  (set! s:file-block (make-empty-block))
	  (set! s:max-arity 0)
	  (set! total-cpu-time 0)
	  (set! total-real-time 0)
	  (random-seed (compiler:option:seed))
	  (set! compiler:messages null)
	  (set! c-declares null)
	  (set! c-lambdas null)
	  (const:init-tables!)
	  (compiler:init-closure-lists!)
	  (varref:reset-module-id!)
	  ; process the input string - try to open the input file
	  (let-values ([(input-path c-output-path 
				    constant-pool-output-path obj-output-path dll-output-path 
				    setup-suffix)
			(s:process-filenames input-name dest-directory from-c? 
					     (and (compiler:option:clean-intermediate-files)
						  (not c-only?))
					     (and (compiler:option:clean-intermediate-files)
						  (not multi-o?)))])
	    (unless (or (not input-path) (file-exists? input-path))
	      (error 's:compile "could not open ~a for input" input-path))
	    (set! compiler:setup-suffix
		  (if multi-o?
		      setup-suffix
		      ""))
	    
	    (for-each (lambda (path)
			(when (file-exists? path) (delete-file path)))
		      (list (if input-path c-output-path obj-output-path) 
			    (if input-path constant-pool-output-path obj-output-path) 
			    obj-output-path dll-output-path))
	
	    (when (compiler:option:debug)
	      (when (file-exists? debug:file) (delete-file debug:file))
	      (set! debug:port (open-output-file debug:file 'text)))
	
	    (when input-path 
	      (parameterize ([main-source-file input-path])
		(let ([input-port (open-input-scheme-file input-path)])
		  (port-count-lines! input-port)

		  ;;-----------------------------------------------------------------------
		  ;; read all top-level s-expressions
		  ;;
	      
		  (printf "\"~a\": " input-path)
		  (unless (compiler:option:verbose) (newline))
		  (let ([read-thunk
			 (lambda ()
			   (with-handlers ([void top-level-exn-handler])
			     (with-handlers ([void elaboration-exn-handler])
			       (parameterize ([current-namespace elaborate-namespace]
					      [compiler:escape-on-error #t])
				 (set-block-source! 
				  s:file-block
				  (s:expand-top-level-expressions! 
				   input-directory
				   (lambda ()
				     (read-syntax (path->complete-path input-path) input-port))
				   (compiler:option:verbose)))))))])
		    (verbose-time read-thunk)
		    (close-input-port input-port)
		    (set! input-port #f)
		
		    (compiler:report-messages! #t))
	
		  ;; (print-struct #t) (map (lambda (ast) (pretty-print ast)) (block-source s:file-block))
		  
		  ;; (map (lambda(ast)(pretty-print (zodiac->sexp/annotate ast))) (block-source s:file-block))
		  
		  ;; (print-graph #t) (display (car (block-source s:file-block))) (newline)

		  ;;-----------------------------------------------------------------------
		  ;; record module name, if a single declaration

		  (set-single-module-mode! #f)
		  (when (and (= 1 (length (block-source s:file-block)))
			     (zodiac:module-form? (car (block-source s:file-block))))
		    (set-single-module-mode! #t)
		    (set! compiler:module-decl-name 
			  (syntax-e (zodiac:module-form-name (car (block-source s:file-block))))))

		  ;;-----------------------------------------------------------------------
		  ;; Run a preprocessing phase on the input
		  ;;
	      
		  (when (compiler:option:verbose) (printf " pre-processing and scanning for errors~n"))
		  (when (compiler:option:debug) (debug " = PREPHASE =~n"))
	      
		  (let ([prephase-thunk 
			 (lambda () 
			   (set-block-source! 
			    s:file-block
			    (let loop ([source (block-source s:file-block)]
				       [errors compiler:messages])
			      (if (null? source)
				  source
				  (let ([ast (prephase! (car source) #f (null? (cdr source)) #f)])
				    (if (eq? errors compiler:messages)
					
					;; no errors here
					(if (zodiac:module-form? ast)
					    ;; If it's a module, split it into three parts:
					    ;;   - body
					    ;;   - syntax definitions
					    ;;   - module registration
					    ;; That way, the global variable sets, etc., are
					    ;; kept separate.
					    (append (split-module ast) (loop (cdr source) errors))

					    ;; Normal expr
					    (cons ast (loop (cdr source) errors)))
					
					;; error, drop this one
					(loop (cdr source) compiler:messages)))))))])
		    (verbose-time prephase-thunk))
		  (compiler:report-messages! (not (compiler:option:test)))
		  (when (compiler:option:test)
		    (printf "skipping over top-level expressions with errors...~n"))
	      
	      ; (map (lambda (ast) (pretty-print (zodiac->sexp/annotate ast))) (block-source s:file-block))
	      
	      ;;-----------------------------------------------------------------------
	      ;; A-normalize input
	      ;;
	      
	      (when (compiler:option:verbose) (printf " transforming to a-normal form~n"))
	      (when (compiler:option:debug) (debug " = ANORM =~n"))
	      
	      (let ([anorm-thunk
		     (lambda ()
		       (set-block-source! 
			s:file-block 
			(map (lambda (s) (a-normalize s identity)) 
			     (block-source s:file-block))))])
		(verbose-time anorm-thunk))
	      
	      ; (map (lambda (ast) (pretty-print (zodiac->sexp/annotate ast))) (block-source s:file-block))
	      
	      ;;-----------------------------------------------------------------------
	      ;; known-value analysis
	      ;;

	      (when (compiler:option:verbose) 
		(printf " determining known bindings~n"))
	      (when (compiler:option:debug)
		(debug " = KNOWN =~n"))

	      ; analyze top level expressions
	      (let ([known-thunk
		     (lambda ()
		       (set-block-source! 
			s:file-block 
			(map (lambda (s) (analyze-knowns! s)) 
			     (block-source s:file-block))))])
		(verbose-time known-thunk))

	      (compiler:report-messages! #t)
	      
	      ; (map (lambda (ast) (pretty-print (zodiac->sexp/annotate ast))) (block-source s:file-block))

	      ;;-----------------------------------------------------------------------
	      ;; B-form transformation and analysis
	      ;;

	      (when (compiler:option:verbose) 
		(printf " transforming to b-normal form, analyzing, and inlining~n"))
	      (when (compiler:option:debug)
		(debug " = ANALYZE =~n"))

	      ; analyze top level expressions, cataloguing local variables
	      (compiler:init-define-lists!)
	      (const:reset-syntax-constants!)
	      (let ([bnorm-thunk
		     (lambda ()
		       (let-values ([(new-source new-codes max-arity)
				     (s:analyze-source-list 
				      (block-source s:file-block))])
			 (set-block-source! s:file-block new-source)
			 (set-block-codes! s:file-block new-codes)
			 (block:register-max-arity! s:file-block max-arity)
			 (s:register-max-arity! max-arity))
		       
		       ; take constant construction code and place it in front of the 
		       ; previously generated code. True constants first.
		       (set! number-of-true-constants (length (compiler:get-define-list)))
		       (set! number-of-per-load-constants (+ (length (compiler:get-per-load-define-list))
							     (length (compiler:get-per-invoke-define-list))))
		       (s:append-block-sources! s:file-block 
						(append
						 (compiler:get-define-list)
						 (compiler:get-per-load-define-list)
						 (compiler:get-per-invoke-define-list))))])
		(verbose-time bnorm-thunk))
	      (compiler:report-messages! #t)
	      
	      ; (map (lambda (ast) (pretty-print (zodiac->sexp/annotate ast))) (block-source s:file-block))

	      ;;-----------------------------------------------------------------------
	      ;; Lift static procedures
	      ;;

	      (when (compiler:option:verbose) 
		(printf " finding static procedures~n"))
	      (when (compiler:option:debug)
		(debug " = LIFT =~n"))

	      (let ([lift-thunk s:lift])
		(verbose-time lift-thunk))
	      (compiler:report-messages! #t)
	      
	      ; (map (lambda (ast) (pretty-print (zodiac->sexp/annotate ast))) (block-source s:file-block))

	      ;;-----------------------------------------------------------------------
	      ;; Closure conversion and explicit control transformation
	      ;;
	      
	      (when (compiler:option:verbose) 
		(printf " closure conversion and explicit control transformation~n"))
	      
	      (let ([closure-thunk
		     (lambda ()
		       (set-block-source! 
			s:file-block
			(map closure-expression! (block-source s:file-block))))])
		(verbose-time closure-thunk))

	      ;;-----------------------------------------------------------------------
	      ;; Vehicle assignment
	      ;;
	      ;; Set export list offset for units at the same time
	      ;;
	      
	      (when (compiler:option:verbose)
		(printf " closure->vehicle mapping~n"))
	      
	      (when (eq? (compiler:option:vehicles) 'vehicles:automatic)
		(for-each 
		 (lambda (L)
		   (when (zodiac:case-lambda-form? L)
		     (map (lambda (body)
			    (relate-lambdas! L body))
			  (zodiac:case-lambda-form-bodies L))))
		 (compiler:get-closure-list)))
	      
	      (when (eq? (compiler:option:vehicles) 'vehicles:units)
		(compiler:fatal-error 
		 #f 
		 "unit-wise vehicle mapping not currently supported~n"))
	      (let ([vehicle-thunk
		     (lambda ()
		       (compiler:init-vehicles!)
		       (compiler:reset-label-number!)
		       (choose-vehicles!))])
		(verbose-time vehicle-thunk))

	      ;;-----------------------------------------------------------------------
	      ;; Representation Choosing 
	      ;;    From this stage, we have to work with separate code bodies, as well
	      ;;    as the list of top-level expressions.
	      
	      (when (compiler:option:verbose) 
		(printf 
		 " choosing data representations~n"))
	      
	      (let ([rep-thunk
		     (lambda ()
		       (compiler:init-structs!)
		       ; top-level
		       (map
			(lambda (c)
			  (choose-binding-representations! 
			   (code-local-vars c)
			   (code-global-vars c)
			   (code-used-vars c)
			   (code-captured-vars c)))
			(block-codes s:file-block))
		       ; code-bodies
		       (for-each (lambda (L)
				   (let* ([code (get-annotation L)]
					  [locals (code-local-vars code)]
					  [globals (code-global-vars code)]
					  [used (code-used-vars code)]
					  [captured (code-captured-vars code)])
				     (choose-binding-representations! locals globals used captured)
				     (choose-closure-representation! code)))
				 (compiler:get-closure-list)))])
		(verbose-time rep-thunk))

	      ; (map (lambda (ast) (pretty-print (zodiac->sexp/annotate ast))) (block-source s:file-block))
	      
	      ;;-----------------------------------------------------------------------
	      ;; Virtual Machine Scheme translation
	      ;;   Here we turn our code into VM Scheme as we enter the arena of
	      ;;    low level transformations and optimizations.
	      ;;   This transformation may create new local variables, so
	      ;;    we have to update the local variable set for each top-level
	      ;;    expression or code body.
	      
	      (when (compiler:option:verbose) (printf " transforming to Virtual Machine form~n"))
	      (when (compiler:option:debug) (debug " = VMPHASE =~n"))
	      
	      (let ([vmphase-thunk
		     (lambda ()
		       ; top-level.  The last expression will be in tail position and should
		       ; return its value
		       (let loop ([s (block-source s:file-block)]
				  [l (block-codes s:file-block)])
			 (unless (null? s)
			   (let-values ([(vm new-locals)
					 (vm-phase (car s) 
						   #t
						   #f
						   (if (null? (cdr s))
						       (lambda (ast)
							 (make-vm:return 
							  (zodiac:zodiac-stx ast)
							  ast))
						       (lambda (ast)
							 (make-vm:void
							  (zodiac:zodiac-stx ast)
							  ast)))
						   (null? (cdr s)))])
			     (set-car! s vm)
			     (add-code-local+used-vars! (car l) new-locals))
			   (loop (cdr s) (cdr l))))
		       ; code-bodies
		       (for-each 
			(lambda (L)
			  (let* ([code (get-annotation L)]
				 [tail-pos (lambda (ast)
					     (make-vm:return 
					      (zodiac:zodiac-stx ast)
					      ast))]
				 [new-locals
				  (cond
				   [(zodiac:case-lambda-form? L)
				    (let-values ([(vms new-locals)
						  (let loop ([l (zodiac:case-lambda-form-bodies L)]
							     [case-codes (procedure-code-case-codes 
									  (get-annotation L))]
							     [vms null])
						    (if (null? l)
							(values (reverse! vms) 
								; empty: already added via case
								empty-set) 
							(let-values ([(vm new-locals)
								      (vm-phase (car l) #t #f tail-pos #t)])
							  (add-code-local+used-vars! 
							   (car case-codes)
							   new-locals )
							  (loop (cdr l)
								(cdr case-codes)
								(cons vm vms)))))])
				      (zodiac:set-case-lambda-form-bodies! L vms)
				      new-locals)]
				   [else (compiler:internal-error
					  L
					  "vmphase: unknown closure type")])])
			    (add-code-local+used-vars! code new-locals)))
			(compiler:get-closure-list)))])
		(verbose-time vmphase-thunk))

	      (compiler:report-messages! #t)

	      ; (print-struct #t)
	      ; (map (lambda (ast) (pretty-print ast)) (block-source s:file-block))

	      ;;-----------------------------------------------------------------------
	      ;; Virtual Machine Optimization Pass
	      ;;
	      ;;  As in the previous phase, new local variables may be created.
	      
	      (when (compiler:option:verbose) (printf " optimizing Virtual Machine code~n"))
	      
	      (let ([vmopt-thunk
		     (lambda ()
		       ; top-level
		       (let loop ([bl (block-source s:file-block)]
				  [cl (block-codes s:file-block)])
			 (unless (null? bl)
			   (let-values ([(b new-locs) ((vm-optimize! #f #f) (car bl))])
			     (set-car! bl b)
			     (add-code-local+used-vars! (car cl) new-locs))
			   (loop (cdr bl) (cdr cl))))
		       
		       ; code-bodies
		       (for-each (lambda (L)
				   (let ([code (get-annotation L)])
				     (cond
				      [(zodiac:case-lambda-form? L)
				       (let loop ([bodies (zodiac:case-lambda-form-bodies L)]
						  [case-codes (procedure-code-case-codes code)]
						  [i 0])
					 (unless (null? bodies)
					   (let-values ([(new-body new-locs) ((vm-optimize! L i) (car bodies))])
					     (set-car! bodies new-body)
					     (add-code-local+used-vars! (car case-codes)
									new-locs)
					     (loop (cdr bodies)
						   (cdr case-codes)
						   (add1 i)))))]
				      [else (compiler:internal-error
					     L
					     "vmopt: unknown closure type")])))
				 (compiler:get-closure-list)))])
		(verbose-time vmopt-thunk))
		 
	      (compiler:report-messages! #t)
			  
	      ;;-----------------------------------------------------------------------
	      ;; Virtual Machine -> ANSI C translation
	      ;;
	      (when (compiler:option:verbose)
		(printf " [emitting ~a C to \"~a\"]~n" 
			"ANSI"
			c-output-path))
	
	      (let ([vm2c-thunk
		     (lambda ()
		       (parameterize ([read-case-sensitive #t]) ;; so symbols containing uppercase print like we want
			 (let ([c-port #f])
			   (dynamic-wind 
					;pre
			       (lambda () (set! c-port (open-output-file c-output-path)))
			       
					;value
			       (lambda ()
				 (fprintf c-port "#define MZC_SRC_FILE ~s~n" input-name)
				 (when (compiler:option:unsafe) (fprintf c-port "#define MZC_UNSAFE 1~n"))
				 (when (compiler:option:disable-interrupts) (fprintf c-port "#define MZC_DISABLE_INTERRUPTS 1~n"))
				 (when (compiler:option:fixnum-arithmetic) (fprintf c-port "#define MZC_FIXNUM 1~n"))
				 
				 (fprintf c-port "~n#include \"~ascheme.h\"~n"
					  (if (compiler:option:compile-for-embedded)
					      ""
					      "e"))
				 
				 (unless (null? c-declares)
				   (fprintf c-port "~n/* c-declare literals */~n~n")
				   (for-each
				    (lambda (c-declare)
				      (fprintf c-port "~a~n" c-declare))
				    (reverse c-declares))
				   (fprintf c-port "~n/* done with c-declare literals */~n~n"))

				 (unless (null? c-lambdas)
				   (fprintf c-port "~n/* c-lambda implementations */~n~n")
				   (for-each
				    (lambda (c-lambda)
				      (let ([name (car c-lambda)]
					    [body (cdr c-lambda)])
					(fprintf c-port "Scheme_Object *~a(int argc, Scheme_Object **argv) {\n"
						 name)
					(fprintf c-port "~a~n" body)
					(fprintf c-port "}~n")))
				    (reverse c-lambdas))
				   (fprintf c-port "~n/* done with c-lambda implementations */~n~n"))

				 (fprintf c-port "#include \"mzc.h\"~n~n")
				 (vm->c:emit-struct-definitions! (compiler:get-structs) c-port)
				 (vm->c:emit-symbol-declarations! c-port)
				 (vm->c:emit-syntax-string-declarations! c-port)
				 (vm->c:emit-inexact-declarations! c-port)
				 (vm->c:emit-string-declarations! c-port)
				 (vm->c:emit-prim-ref-declarations! c-port)
				 (vm->c:emit-static-declarations! c-port)
				 
				 (let loop ([n 0])
				   (unless (= n (compiler:get-total-vehicles))
				     (vm->c:emit-vehicle-declaration c-port n)
				     (loop (+ n 1))))
				 (newline c-port)
				 
				 (unless (compiler:multi-o-constant-pool)
				   (fprintf c-port "~nstatic void make_symbols()~n{~n")
				   (vm->c:emit-symbol-definitions! c-port)
				   (fprintf c-port "}~n"))

				 (fprintf c-port "~nstatic void make_syntax_strings()~n{~n")
				 (vm->c:emit-syntax-string-definitions! c-port)
				 (fprintf c-port "}~n")
				 
				 (unless (zero? (const:get-inexact-counter))
				   (fprintf c-port "~nstatic void make_inexacts()~n{~n")
				   (vm->c:emit-inexact-definitions! c-port)
				   (fprintf c-port "}~n"))

				 (fprintf c-port "~nstatic void gc_registration()~n{~n")
				 (vm->c:emit-registration! c-port)
				 (fprintf c-port "}~n")

				 (fprintf c-port "~nstatic void init_prims(Scheme_Env * env)~n{~n")
				 (vm->c:emit-prim-ref-definitions! c-port)
				 (fprintf c-port "}~n")
				 
				 (unless (null? (compiler:get-case-lambdas))
				   (fprintf c-port "~nstatic void init_cases_arities()~n{~n")
				   (vm->c:emit-case-arities-definitions! c-port)
				   (fprintf c-port "}~n"))
				 (newline c-port)

				 (let* ([codes (block-codes s:file-block)]
					[locals (map code-local-vars codes)]
					[globals (map code-global-vars codes)]
					[init-constants-count
					 (if (zero? number-of-true-constants)
					     -1
					     (vm->c:emit-top-levels! "init_constants" #f #f #t number-of-true-constants
								     (block-source s:file-block)
								     locals globals
								     (block-max-arity s:file-block)
								     #f #f ; no module entries
								     c-port))]
					[invoke-counts
					 (let loop ([i 0])
					   (if (= i (get-num-module-invokes))
					       null
					       (cons
						(let loop ([syntax? #f])
						  (cons
						   (vm->c:emit-top-levels! (format "module~a_body_~a" 
										   (if syntax? "_syntax" "")
										   i)
									   #f #f #f -1
									   (block-source s:file-block)
									   locals
									   globals
									   (block-max-arity s:file-block)
									   i syntax?
									   c-port)
						   (if syntax? 
						       null
						       (loop #t))))
						(loop (add1 i)))))]
					[_ (let loop ([i 0][counts invoke-counts])
					     (unless (= i (get-num-module-invokes))
					       (vm->c:emit-module-glue! c-port i (caar counts) (cadar counts))
					       (loop (add1 i) (cdr counts))))]
					[top-level-count
					 (vm->c:emit-top-levels! "top_level" #t #t #f -1
								 (list-tail (block-source s:file-block) number-of-true-constants)
								 (list-tail locals number-of-true-constants)
								 (list-tail globals number-of-true-constants)
								 (block-max-arity s:file-block)
								 #f #f ; no module entries
								 c-port)])
				   (fprintf c-port
					    "Scheme_Object * scheme_reload~a(Scheme_Env * env)~n{~n"
					    compiler:setup-suffix)
				   (fprintf c-port"~aScheme_Per_Load_Statics *PLS;~n"
					    vm->c:indent-spaces)
				   (fprintf c-port 
					    "~aPLS = (Scheme_Per_Load_Statics *)scheme_malloc(sizeof(Scheme_Per_Load_Statics));~n"
					    vm->c:indent-spaces)
				   (let loop ([c 0])
				     (fprintf c-port "~a~atop_level_~a(env, PLS);~n" 
					      vm->c:indent-spaces 
					      (if (= c top-level-count) "return " "")
					      c)
				     (unless (= c top-level-count)
				       (loop (add1 c))))
				   (fprintf c-port 
					    "}~n~n")

				   (fprintf c-port
					    "~nvoid scheme_setup~a(Scheme_Env * env)~n{~n"
					    compiler:setup-suffix)
				   (fprintf c-port
					    "~ascheme_set_tail_buffer_size(~a);~n"
					    vm->c:indent-spaces
					    s:max-arity)
				   (fprintf c-port "~agc_registration();~n"
					    vm->c:indent-spaces)
				   (unless (compiler:multi-o-constant-pool)
				     (fprintf c-port "~amake_symbols();~n"
					      vm->c:indent-spaces))
				   (fprintf c-port "~amake_syntax_strings();~n"
					    vm->c:indent-spaces)
				   (unless (zero? (const:get-inexact-counter))
				     (fprintf c-port "~amake_inexacts();~n"
					      vm->c:indent-spaces))
				   (fprintf c-port "~ainit_prims(env);~n"
					    vm->c:indent-spaces)
				   (unless (null? (compiler:get-case-lambdas))
				     (fprintf c-port "~ainit_cases_arities();~n"
					      vm->c:indent-spaces))		       

				   (let loop ([c 0])
				     (unless (> c init-constants-count)
				       (fprintf c-port "~ainit_constants_~a(env);~n" 
						vm->c:indent-spaces
						c)
				       (loop (add1 c))))

				   (fprintf c-port 
					    "}~n~n")
				   
				   (when (string=? "" compiler:setup-suffix)
				     (fprintf c-port
					      "~nScheme_Object * scheme_initialize(Scheme_Env * env)~n{~n")
				     (fprintf c-port "~ascheme_setup~a(env);~n"
					      vm->c:indent-spaces
					      compiler:setup-suffix)
				     (fprintf c-port "~areturn scheme_reload~a(env);~n"
					      vm->c:indent-spaces
					      compiler:setup-suffix)
				     (fprintf c-port 
					      "}~n~n"))

				   (fprintf c-port
					    "~nScheme_Object * ~ascheme_module_name()~n{~n~areturn "
					    compiler:setup-suffix
					    vm->c:indent-spaces)
				   (if compiler:module-decl-name
				       (let ([s (symbol->string compiler:module-decl-name)])
					 (fprintf c-port "scheme_intern_exact_symbol(~s, ~a)" s (string-length s)))
				       (fprintf c-port "scheme_false"))
				   (fprintf c-port ";~n}~n"))

				 (let emit-vehicles ([vehicle-number 0])
				   (unless (= vehicle-number (compiler:get-total-vehicles))
				     (let* ([vehicle (get-vehicle vehicle-number)]
					    [lambda-list (vehicle-lambdas vehicle)])
				       
				       (vm->c:emit-vehicle-header c-port vehicle-number)
				       (vm->c:emit-vehicle-prologue c-port vehicle)
				       
				       ;; get the lambdas that appear in this vehicle
				       
				       ;; sort the functions by index to get an optimal case statement
				       ;; even for stupid compilers
				       (set! lambda-list
					     (quicksort lambda-list
							(lambda (l1 l2)
							  (< (closure-code-label (get-annotation l1))
							     (closure-code-label (get-annotation l2))))))
				       (for-each (lambda (L)
						   (let ([code (get-annotation L)]
							 [start (zodiac:zodiac-start L)])
						     (fprintf c-port "~a/* code body ~a ~a [~a,~a] */~n"
							      vm->c:indent-spaces (closure-code-label code)
							      (let ([n (closure-code-name code)])
								(if n
								    (protect-comment 
								     (vm->c:extract-inferred-name n))
								    ""))
							      (zodiac:location-line start)
							      (zodiac:location-column start))
						     (cond
						      [(zodiac:case-lambda-form? L)
						       (let-values ([(count suffix?) 
								     (vm->c:emit-function-prologue L c-port)])
							 (let loop ([i 0])
							   (unless (= i count)
							     (let* ([indent
								     (string-append
								      vm->c:indent-spaces vm->c:indent-spaces
								      (if suffix?  vm->c:indent-spaces ""))]
								    [undefines
								     (vm->c:emit-case-prologue L i
											       (lambda ()
												 (if suffix?
												     (fprintf c-port "~a~a/* begin case ~a */~n~a~a{~n" 
													      vm->c:indent-spaces vm->c:indent-spaces i
													      vm->c:indent-spaces vm->c:indent-spaces)
												     (when (zero? i)
												       (fprintf c-port "~a{~n" vm->c:indent-spaces))))
											       (if suffix? (format "c~a" i) "")
											       indent
											       c-port)])
							       (vm->c-expression (list-ref (zodiac:case-lambda-form-bodies L) i)
										 code
										 c-port
										 (* (if suffix? 3 2) vm->c:indent-by)
										 #f)
							       (vm->c:emit-case-epilogue L i undefines indent c-port)
							       (when suffix?
								 (fprintf c-port "~a~a} /* end case ~a */~n" 
									  vm->c:indent-spaces 
									  vm->c:indent-spaces i)))
							     
							     (loop (add1 i))))
							 (vm->c:emit-function-epilogue code 
										       (if suffix? "" "}")
										       c-port))]
						      [else
						       (compiler:internal-error
							L
							"vm2c: unknown closure type")])
						     (newline c-port)))
						 lambda-list))
				     
				     (vm->c:emit-vehicle-epilogue c-port vehicle-number)
				     (newline c-port)
				     (emit-vehicles (+ 1 vehicle-number)))))
			       
			       ;; post (dynamic wind cleanup)
			       (lambda ()  (close-output-port c-port))))))])
		(with-handlers ([void (lambda (exn)
					(delete-file c-output-path)
					(raise exn))])
		  (verbose-time vm2c-thunk)))
	      
	      (compiler:report-messages! #t)
	      
	      ;; Write out symbols for multi-o constant pool
	      (when (compiler:multi-o-constant-pool)
		(call-with-output-file constant-pool-output-path
		  (lambda (port)
		    (fprintf port "(~s~n (symbols~n" compiler:setup-suffix)
		    (vm->c:emit-symbol-list! port "" #f)
		    (fprintf port "  )~n )~n")))))))

	;;--------------------------------------------------------------------
	;; COMPILATION TO NATIVE CODE
	;;
	
	(if c-only?
	    (printf " [output to \"~a\"]~n" c-output-path)
	    
	    (begin
	      (unless input-path
		(printf "\"~a\": ~n" c-output-path))
	      
	      (when (compiler:option:verbose) (printf " [compiling native code to \"~a\"]~n"
						      obj-output-path))
	      
	      ;; Compile
	      (let ([compile-thunk
		     (lambda ()
		       (with-handlers
			   ([void (lambda (exn)
				    (compiler:fatal-error
				     #f
				     (string-append
				      " C compiler did not complete successfully"
				      (string #\newline)
				      (exn-message exn)))
				    (compiler:report-messages! #t))])
			 (compile-extension (not (compiler:option:verbose)) 
					    c-output-path obj-output-path
					    (list (collection-path "compiler")))))])
		(verbose-time compile-thunk))
	      
	      ;; clean-up
	      (when (and (compiler:option:clean-intermediate-files)
			 input-path)
		(delete-file c-output-path))
	      
	      (if multi-o?
		  (printf " [output to \"~a\"]~n" obj-output-path)
		  
		  (begin
		    ;; Link
		    (when (compiler:option:verbose) (printf " [linking to \"~a\"]~n"
							    dll-output-path))
		    (let ([link-thunk
			   (lambda ()
			     (with-handlers
				 ([void (lambda (exn)
					  (compiler:fatal-error 
					   #f 
					   (string-append 
					    " linker did not link successfully"
					    (string #\newline)
					    (exn-message exn)))
					  (compiler:report-messages! #t))])
			       (link-extension (not (compiler:option:verbose)) (list obj-output-path) dll-output-path)))])
		      (verbose-time link-thunk))
		    
		    ;; clean-up
		    (when (compiler:option:clean-intermediate-files)
		      (delete-file obj-output-path))
		    
		    (printf " [output to \"~a\"]~n" dll-output-path)))))
	
	(when debug:port
	  (close-output-port debug:port))
	
	;; clean up for the garbage collector
	(compiler:init-define-lists!)
	(const:init-tables!)
	(compiler:init-closure-lists!)
	(compiler:init-structs!)
	(set! s:file-block #f)
	(when (compiler:option:verbose)
	  (printf " finished [cpu ~a, real ~a].~n"
		  total-cpu-time
		  total-real-time))))))))
