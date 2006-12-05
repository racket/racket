
(module ld-unit mzscheme
  (require (lib "unit.ss")
	   (lib "list.ss"))

  (require "sig.ss")

  (require (lib "file-sig.ss" "dynext")
	   (lib "link-sig.ss" "dynext")
	   (lib "compile-sig.ss" "dynext"))

  (provide ld@)

  (define-unit ld@
      (import dynext:compile^
	      dynext:link^
	      dynext:file^
	      (prefix compiler:option: compiler:option^))
      (export (rename compiler:linker^
                      [link-extension* link-extension]))


      ;; Copied from library.ss; please fix me!
      (define compiler:bad-chars
	(string->list "#+-.*/<=>!?:$%_&~^@;^()[]{}|\\,~\"`' "))
      (define (compiler:clean-string s)
	(let* ((str (string->list s)))
	  (list->string
	   (map (lambda (c) (if (member c compiler:bad-chars)
				#\_
				c))
		str))))

      (define (link-extension*
	       files
	       dest-dir)
	(do-link-extension #t #t files dest-dir))

      (define (glue-extension
	       files
	       dest-dir)
	(do-link-extension #f #t files dest-dir))

      (define (glue-extension-source
	       files
	       dest-dir)
	(do-link-extension #f #f files dest-dir))

      (define (do-link-extension
	       link?
	       compile?
	       files
	       dest-dir)

	(define _loader.c (append-c-suffix "_loader"))
	(define _loader.o (append-object-suffix "_loader"))
	(define _loader.so (append-extension-suffix "_loader"))
	
	(define __ (printf "\"~a\":~n" (build-path dest-dir _loader.c)))

	(define all-names
	  (map
	   (lambda (file)
	     (let*-values ([(base name dir?) (split-path file)])
	       (let ([o (extract-base-filename/o name)]
		     [kp (extract-base-filename/kp name)])
		 (cond
		  [o (list 'o file o)]
		  [kp (cons 'kp file)]
		  [else (error 'mzld "file is not a compiled object for constant pool file: ~a"
			       file)]))))
	   files))

	(define-values (o-files ; just .o files
			names ; just .o names
			kps) ; just .kp files
	  (let loop ([l all-names][ofs null][os null][kps null])
	    (if (null? l)
		(values (reverse ofs) 
			(map path->string (reverse os))
			(reverse kps))
		(if (eq? (caar l) 'o)
		    (loop (cdr l) (cons (cadar l) ofs) (cons (caddar l) os) kps)
		    (loop (cdr l) ofs os (cons (cdar l) kps))))))

	(define linker-prefix (compiler:option:setup-prefix))

	(define suffixes
	  (let ([linker-prefix (compiler:clean-string linker-prefix)])
	    (map
	     (lambda (name)
	       (string-append linker-prefix "_" (compiler:clean-string name)))
	     names)))

	(define symbol-table (make-hash-table))
	(define (add-symbol s spos pos uninterned?)
	  (let ([v (hash-table-get symbol-table s (lambda () null))])
	    (hash-table-put! symbol-table s
			     (cons (list spos pos) v))))
	
	;; Read in symbol info
	(define kp-suffixes/counts
	  (let loop ([kps kps][kpos 0])
	    (if (null? kps)
		null
		(let-values ([(suffix count)
			      (call-with-input-file (car kps)
				(lambda (in)
				  (let ([info (read in)])
				    (let ([suffix (car info)]
					  [symbols (cdadr info)])
				      (let loop ([l symbols][p 0])
					(unless (null? l)
					  (let ([s (car l)])
					    ;; s might be a list containing a symbol to
					    ;; indicate that it's uninterned
					    (add-symbol (if (string? s) 
							    (string->symbol s)
							    (string->uninterned-symbol (car s)))
							kpos p
							(pair? s)))
					  (loop (cdr l) (add1 p))))
				      (values suffix (length symbols))))))])
		  (let ([rest (loop (cdr kps) 
				    (if (zero? count)
					kpos
					(add1 kpos)))])
		    (if (zero? count)
			rest
			(cons (cons suffix count) rest)))))))

	;; Compile content of symbol table into dispatching information
	(define symbols (hash-table-map symbol-table (lambda (key info) key)))
	(define symbol-dispatches
	  (apply
	   append
	   (hash-table-map 
	    symbol-table
	    (lambda (key info)
	      (cons (length info)
		    (apply append info))))))

	(with-output-to-file
	    (build-path dest-dir _loader.c)
	  (lambda ()
	    (printf "#include \"~ascheme.h\"~n"
		    (if (compiler:option:compile-for-embedded)
			""
			"e"))
	    (printf "#include \"mzclink.h\"~n~n")
	    
	    (for-each
	     (lambda (suffix)
	       (printf "extern Scheme_Object * scheme_setup~a(Scheme_Env *e);~n" suffix)
	       (printf "extern Scheme_Object * scheme_reload~a(Scheme_Env *e);~n" suffix))
	     suffixes)
	    (for-each
	     (lambda (kp-suffix/count)
	       (let ([suffix (car kp-suffix/count)]
		     [count (cdr kp-suffix/count)])
		 (printf "extern Scheme_Object * SYMBOLS~a[~a];~n" 
			 suffix count)))
	     kp-suffixes/counts)
	    
	    (printf "~nstatic struct {~n")
	    (for-each
	     (lambda (suffix)
	       (printf "  Scheme_Object * ~a_symbol;~n" suffix))
	     suffixes)
	    (printf "} syms;~n~n")

	    
	    (unless (null? symbols)
	      (printf "static const char *SYMBOL_STRS[~a] = {~n" (length symbols))
	      (for-each
	       (lambda (s)
		 (printf "  ~s,~n" (symbol->string s)))
	       symbols)
	      (printf "}; /* end of SYMBOL_STRS */~n~n")

	      (printf "static long SYMBOL_LENS[~a] = {~n" (length symbols))
	      (for-each
	       (lambda (s)
		 (printf "  ~s,~n" (string-length (symbol->string s))))
	       symbols)
	      (printf "}; /* end of SYMBOL_LENS */~n~n")
	      
	      (printf "static char SYMBOL_INTERNS[~a] = {~n" (length symbols))
	      (for-each
	       (lambda (s)
		 (printf "  ~s,~n" (if (eq? s (string->symbol (symbol->string s))) 1 0)))
	       symbols)
	      (printf "}; /* end of SYMBOL_INTERNS */~n~n")
	      
	      (printf "static const int SYMBOL_DISPATCHES[~a] = {~n  " (length symbol-dispatches))
	      (let loop ([l symbol-dispatches][line 0])
		(unless (null? l)
		  (if (= line 20)
		      (begin
			(printf "~n  ")
			(loop l 0))
		      (begin
			(printf "~a, " (car l))
			(loop (cdr l) (add1 line))))))
	      (printf "~n}; /* end of SYMBOL_DISPATCHES */~n~n")

	      (printf "static setup_pooled_symbols(void) {~n  Scheme_Object * * symbol_tables[~a];~n  int i, j;~n"
		      (length kp-suffixes/counts))
	      (let loop ([l kp-suffixes/counts][p 0])
		(unless (null? l)
		  (printf "  symbol_tables[~a] = SYMBOLS~a;~n  scheme_register_extension_global(&SYMBOLS~a, sizeof(SYMBOLS~a));~n"
			  p (caar l)
			  (caar l) (caar l))
		  (loop (cdr l) (add1 p))))
	      (printf "  for (i = j = 0; i < ~a; i++) {~n" (length symbols))
	      (printf "    Scheme_Object * s;~n")
	      (printf "    int c, k;~n")
	      (printf "    if (SYMBOL_INTERNS[i])~n")
	      (printf "       s = scheme_intern_exact_symbol(SYMBOL_STRS[i], SYMBOL_LENS[i]);~n")
	      (printf "    else~n")
	      (printf "       s = scheme_make_exact_symbol(SYMBOL_STRS[i], SYMBOL_LENS[i]);~n")
	      (printf "    c = SYMBOL_DISPATCHES[j++];~n")
	      (printf "    for (k = c; k--; j += 2)~n")
	      (printf "      symbol_tables[SYMBOL_DISPATCHES[j]][SYMBOL_DISPATCHES[j+1]] = s;~n")
	      (printf "  }~n")
	      (printf "}~n~n"))

	    (printf "static Scheme_Object * loader_dispatch(void *v, int argc, Scheme_Object * * argv) {~n")
	    (printf "  Scheme_Env * env = scheme_get_env(scheme_current_config());~n")
	    (printf "  return ((Scheme_Object *(*)(Scheme_Env *))v)(env);~n}~n~n")
	    
	    (printf "static Scheme_Object * loader_dispatch_all(int argc, Scheme_Object * * argv) {~n")
	    (printf "  Scheme_Env * env = scheme_get_env(scheme_current_config());~n")
	    (printf "  Scheme_Object * v = scheme_void;~n")
	    (for-each
	     (lambda (suffix)
	       (printf "  v = LOCAL_PROC(scheme_reload~a)(env);~n"
		       suffix))
	     suffixes)
	    (printf "  return v;~n}~n~n")
	    
	    (printf "static Scheme_Object * loader(int argc, Scheme_Object * * argv) {~n")
	    (printf "  Scheme_Object *a[2];~n")
	    (printf "  Scheme_Object * name = argv[0];~n")
	    (printf "  if (name == scheme_true) {~n")
	    (printf "    a[0] = scheme_make_prim_w_arity(loader_dispatch_all, \"_loader-dispatch-all\", 0, 0);~n")
	    (printf "    a[1] = scheme_false;~n")
	    (printf "  }~n")
	    (for-each
	     (lambda (suffix)
	       (printf "  else if (name == syms.~a_symbol) {~n" suffix)
	       (printf "    a[0] = scheme_make_closed_prim_w_arity(loader_dispatch, LOCAL_PROC(scheme_reload~a), \"_loader-dispatch\", 0, 0);~n" suffix)
	       (printf "    a[1] = ~ascheme_module_name();~n" suffix)
	       (printf "  }~n"))
	     suffixes)
	    (printf "  else {~n")
	    (printf "    a[0] = scheme_false;~n")
	    (printf "    a[1] = scheme_false;~n")
	    (printf "  }~n")
	    (printf "  return scheme_values(2, a);~n}~n~n")
	    
	    (printf "Scheme_Object * scheme_reload(Scheme_Env * env) {~n")
	    (printf "  return scheme_make_prim_w_arity(loader, \"_loader\", 1, 1);~n}~n~n")
	    
	    (printf "Scheme_Object * scheme_initialize(Scheme_Env * env) {~n")
	    (unless (null? symbols)
	      (printf "  setup_pooled_symbols();~n"))
	    (for-each
	     (lambda (suffix)
	       ;; (printf "  printf(\"~a is %lx\\n\", scheme_setup~a);~n" suffix suffix)
	       (printf "  LOCAL_PROC(scheme_setup~a)(env);~n" suffix))
	     suffixes)
	    (printf "  scheme_register_extension_global(&syms, sizeof(syms));~n")
	    (for-each
	     (lambda (suffix name)
	       (printf "  syms.~a_symbol = scheme_intern_exact_symbol(~s, ~a);~n" suffix name (string-length name)))
	     suffixes names)
	    (printf "  return scheme_reload(env);~n}~n")
	    (printf "Scheme_Object * scheme_module_name() { return NULL; }~n"))
	  'truncate)
	
	(when compile?
	  (let ([tmp-dir (let ([d (getenv "PLTLDTMPDIR")])
			   (and d (directory-exists? d) d))])
	    
	    (compile-extension (not (compiler:option:verbose))
			       (build-path dest-dir _loader.c)
			       (build-path dest-dir _loader.o)
			       (list (collection-path "compiler")))
	    
	    (when (compiler:option:clean-intermediate-files)
	      (delete-file (build-path dest-dir _loader.c)))

	    (if link?
		(begin
		  (link-extension (not (compiler:option:verbose))
				  (cons (build-path dest-dir _loader.o) o-files) 
				  (build-path (if tmp-dir
						  tmp-dir
						  dest-dir)
					      _loader.so))    
		  (when tmp-dir
		    (copy-file (build-path tmp-dir _loader.so)
			       (build-path dest-dir _loader.so))
		    (delete-file (build-path tmp-dir _loader.so)))
		  
		  (when (compiler:option:clean-intermediate-files)
		    (delete-file (build-path dest-dir _loader.o)))
		  
		  (printf " [output to \"~a\"]~n" (build-path dest-dir _loader.so)))
		(printf " [output to \"~a\"]~n" (build-path dest-dir _loader.o))))))))
