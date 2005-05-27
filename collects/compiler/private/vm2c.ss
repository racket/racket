;; VM Scheme -> C translation module
;; (c) 1996-1997 Sebastian Good
;; (c) 1997-2001 PLT

(module vm2c mzscheme
  (require (lib "unitsig.ss")
	   (lib "list.ss"))

  (require (lib "zodiac-sig.ss" "syntax")
	   (lib "primitives.ss" "syntax"))

  (require "sig.ss")
  (require "../sig.ss")

  (provide vm2c@)
  (define vm2c@
    (unit/sig
	compiler:vm2c^
      (import (compiler:option : compiler:option^)
	      compiler:library^
	      compiler:cstructs^
	      (zodiac : zodiac^)
	      compiler:zlayer^
	      compiler:analyze^
	      compiler:const^
	      compiler:rep^
	      compiler:closure^
	      compiler:vehicle^
	      compiler:vmstructs^
	      compiler:driver^)

      (define local-vars-at-top? #f)

      (define (interned? sym)
	(eq? sym (string->symbol (symbol->string sym))))

      (define vm->c:indent-by 4)
      (define vm->c:indent-spaces
	(make-string vm->c:indent-by #\space))

      (define (vm->c:generate-modglob-name m s)
	(when (symbol? m)
	  (compiler:get-symbol-const! #f m)) ;; generates symbol const
	(compiler:get-symbol-const! #f s) ;; generates symbol const
	(let ([mname (cond
		      [(symbol? m) m]
		      [(not m) '||]
		      [else 
		       ;; try to find a useful part of the module-path-index;
		       ;; this is just for debugging.
		       (let-values ([(path base) (module-path-index-split m)])
			 (cond
			  [(and (pair? path)
				(eq? 'lib (car path))
				(pair? (cdr path))
				(string? (cadr path)))
			   (string->symbol (cadr path))]
			  [else (string->symbol "MoD")]))])])
	  (let ([name (symbol-append 'GL (compiler:gensym) mname '_ s)])
	    (string->symbol (compiler:clean-string (symbol->string name))))))

      (define vm->c:bucket-name
	(lambda (mod var)
	  ;; Shouldn't generate any new names:
	  (mod-glob-cname (compiler:add-global-varref! mod var #f #f #f #f))))

      (define (vm->c:SYMBOLS-name)
	(if (compiler:multi-o-constant-pool)
	    (format "SYMBOLS~a" (compiler:get-setup-suffix))
	    "SYMBOLS"))

      (define (vm->c:INEXACTS-name)
	"INEXACTS")

      (define (vm->c:STRING-name)
	"STRINGS")

      (define (vm->c:make-symbol-const-string sc)
	(format "~a[~a]" (vm->c:SYMBOLS-name) (zodiac:varref-var sc)))

      (define (vm->c:emit-list! port comma c-comment? table counter -symbol->string)
	(let ([v (make-vector counter)])
	  (hash-table-for-each
	   table
	   (lambda (sym b)
	     (vector-set! v (string->number (symbol->string (zodiac:varref-var b))) sym)))
	  (let loop ([i 0])
	    (unless (= i (vector-length v))
	      (fprintf port "  ~s~a ~a~n" (-symbol->string (vector-ref v i)) comma 
		       (if c-comment? 
			   (format "/* ~a */" i)
			   (format "; ~a" i)))
	      (loop (add1 i))))))

      (define (vm->c:emit-symbol-list! port comma c-comment?)
	(vm->c:emit-list! port comma c-comment? (const:get-symbol-table) (const:get-symbol-counter) 
			  (if c-comment?
			      symbol->string
			      ;; Hack: wrap with parens to indicate uninterned
			      (lambda (s)
				((if (interned? s)
				     values
				     list)
				 (symbol->string s))))))

      (define (vm->c:emit-symbol-length-list! port comma c-comment?)
	(vm->c:emit-list! port comma c-comment? (const:get-symbol-table) (const:get-symbol-counter) 
			  (lambda (s) (string-length (symbol->string s)))))

      (define (vm->c:emit-symbol-declarations! port)
	(unless (zero? (const:get-symbol-counter))
	  (unless (compiler:multi-o-constant-pool)
	    (fprintf port "static const char *SYMBOL_STRS[~a] = {~n" (const:get-symbol-counter))
	    (vm->c:emit-symbol-list! port "," #t)
	    (fprintf port "}; /* end of SYMBOL_STRS */~n~n")
	    (fprintf port "static const long SYMBOL_LENS[~a] = {~n" (const:get-symbol-counter))
	    (vm->c:emit-symbol-length-list! port "," #t)
	    (fprintf port "}; /* end of SYMBOL_LENS */~n~n"))

	  (fprintf port "~aScheme_Object * ~a[~a];~n~n"
		   (if (compiler:multi-o-constant-pool) "" "static ")
		   (vm->c:SYMBOLS-name)
		   (const:get-symbol-counter))))

      (define (vm->c:emit-syntax-string-declarations! port)
	(let ([l (const:get-syntax-strings)])
	  (unless (null? l)
	    (fprintf port "static Scheme_Object *SS[~a];~n~n" (length l))
	    (for-each
	     (lambda (ss)
	       (emit-string port
			    "char"
			    (syntax-string-str ss)
			    (format "SYNTAX_STRING_~a" (syntax-string-id ss))))
	     l))))

      (define (vm->c:emit-inexact-list! port comma comment?)
	(vm->c:emit-list! port comma comment? (const:get-inexact-table) (const:get-inexact-counter)
			  (lambda (x) (string->number (symbol->string x)))))

      (define (vm->c:emit-inexact-declarations! port)
	(unless (zero? (const:get-inexact-counter))
	  (fprintf port "static const double INEXACT_NUMBERS[~a] = {~n" (const:get-inexact-counter))
	  (vm->c:emit-inexact-list! port "," #t)
	  (fprintf port "}; /* end of INEXACT_NUMBERS */~n~n")
	  (fprintf port "static Scheme_Object * ~a[~a];~n~n" 
		   (vm->c:INEXACTS-name)
		   (const:get-inexact-counter))))

      (define (emit-string-declarations! port table kind)
	(hash-table-for-each
	 table
	 (lambda (str index)
	   (emit-string port kind str (format "STRING_~a" index)))))

      (define (vm->c:emit-string-declarations! port)
	(emit-string-declarations! port (const:get-string-table) "mzchar")
	(emit-string-declarations! port (const:get-bytes-table) "char"))

      (define emit-string
	(lambda (port kind str name)
	  (let* ([len (if (string? str)
			  (string-length str)
			  (bytes-length str))])
	    (let ([friendly (if (string? str)
				(substring str 0 (min len 24))
				(bytes->string/latin-1 (subbytes str 0 (min len 24))))])
	      (fprintf port
		       "/* ~a */~n"
		       (list->string (map (lambda (i)
					    (cond
					     [(eq? i #\/) #\_]
					     [(<= 32 (char->integer i) 121)
					      i]
					     [else #\_]))
					  (string->list friendly)))))
	    (fprintf port "static const ~a ~a[~a] = {" kind name (add1 len))
	    (let loop ([i 0])
	      (unless (= i len)
		(when (zero? (modulo i 20))
		  (fprintf port "~n    "))
		(fprintf port "~a, " (if (string? str)
					 (char->integer (string-ref str i))
					 (bytes-ref str i)))
		(loop (add1 i)))))
	  (fprintf port "0 }; /* end of ~a */~n~n" name)))

      (define (vm->c:emit-symbol-definitions! port)
	(unless (zero? (const:get-symbol-counter))
	  (fprintf port "  int i;~n")
	  (fprintf port "  for (i = ~a; i--; )~n    SYMBOLS[i] = scheme_intern_exact_symbol(SYMBOL_STRS[i], SYMBOL_LENS[i]);~n"
		   (const:get-symbol-counter))
	  ;; Some symbols might be uninterned...
	  (hash-table-for-each
	   (const:get-symbol-table)
	   (lambda (sym b)
	     (unless (interned? sym)
	       (let ([pos (zodiac:varref-var b)])
		 (fprintf port "  SYMBOLS[~a] = scheme_make_exact_symbol(SYMBOL_STRS[~a], SYMBOL_LENS[~a]); /* uninterned */~n"
			  pos pos pos)))))))

      (define (vm->c:emit-syntax-string-definitions! port)
	(let ([l (const:get-syntax-strings)])
	  (unless (null? l)
	    (for-each
	     (lambda (ss)
	       (let ([id (syntax-string-id ss)]
		     [symbols (vm->c:SYMBOLS-name)])
		 (fprintf port "  SS[~a] = scheme_load_compiled_stx_string(SYNTAX_STRING_~a, ~a);~n"
			  id id (bytes-length (syntax-string-str ss)))
		 ;; Reset uninterned symbols:
		 (let loop ([uposes (syntax-string-uposes ss)][i (syntax-string-ustart ss)])
		   (unless (null? uposes)
		     (fprintf port "  ~a[~a] = scheme_compiled_stx_symbol(SCHEME_VEC_ELS(SS[~a])[~a]);~n"
			      symbols (car uposes) id i)
		     (loop (cdr uposes) (add1 i))))))
	     l))))

      (define (vm->c:emit-inexact-definitions! port)
	(unless (zero? (const:get-inexact-counter))
	  (fprintf port "  int i;~n")
	  (fprintf port "  for (i = ~a; i--; )~n    INEXACTS[i] = scheme_make_double(INEXACT_NUMBERS[i]);~n"
		   (const:get-inexact-counter))))

      (define vm->c:emit-prim-ref-declarations!
	(lambda (port)
	  (unless (set-empty? (compiler:get-primitive-refs))
	    (fprintf port "/* primitives referenced by the code */~n")
	    (fprintf port "static struct {~n")
	    (for-each (lambda (a)
			(fprintf port "  Scheme_Object * ~a;~n"
				 (vm->c:convert-symbol 
				  (vm->c:bucket-name
				   '#%kernel
				   a))))
		      (set->list (compiler:get-primitive-refs)))
	    (fprintf port "} P;~n")
	    (newline port))))

      (define vm->c:emit-prim-ref-definitions!
	(lambda (port)
	  (unless (set-empty? (compiler:get-primitive-refs))
	    (fprintf port "   /* primitives referenced by the code */~n")
	    (for-each (lambda (a)
			(fprintf port "~aP.~a = scheme_module_bucket(~a, ~a, -1, env)->val;~n"
				 vm->c:indent-spaces
				 (vm->c:convert-symbol (vm->c:bucket-name '#%kernel a))
				 (vm->c:make-symbol-const-string (compiler:get-symbol-const! #f '#%kernel))
				 (vm->c:make-symbol-const-string (compiler:get-symbol-const! #f a))))
		      (set->list (compiler:get-primitive-refs))))))

      (define vm->c:emit-struct-definitions!
	(lambda (structs port)
	  (fprintf port "/* compiler-written structures */~n")
	  (for-each (lambda (struct)
		      (fprintf port "struct ~a~n{~n"
			       (vm->c:convert-symbol
				(rep:struct-name struct)))
		      (for-each
		       (lambda (field)
			 (fprintf port "~a~a ~a;~n"
				  vm->c:indent-spaces
				  (vm->c:convert-type-definition
				   (rep:struct-field-rep field))
				  (vm->c:convert-symbol
				   (rep:struct-field-name field)))) 
		       (rep:struct-fields struct))
		      (fprintf port "};~n"))
		    (reverse structs))
	  (newline port)))

      (define (compiler:any-statics?)
	(not (and (null? (compiler:get-static-list))
		  (null? (compiler:get-case-lambdas))
		  (null? (compiler:get-lifted-lambda-vars)))))

      (define (emit-static-variable-fields! port l)
	(unless (null? l)
	  (fprintf port "  /* Write fields as an array to help C compilers */~n")
	  (fprintf port "  /* that don't like really big records. */~n")
	  (fprintf port "  Scheme_Object * _consts_[~a];~n" (length l))
	  (let svloop ([l l][n 0])
	    (unless (null? l)
	      (fprintf port "# define ~a _consts_[~a]~n"
		       (vm->c:convert-symbol (car l)) n)
	      (svloop (cdr l) (add1 n))))))

      ;; when statics have binding information, this will look more like 
      ;; emit-local-variable-declarations!
      (define vm->c:emit-static-declarations!
	(lambda (port)
	  (unless (not (compiler:any-statics?))
	    (fprintf port "/* compiler-written static variables */~n")
	    (fprintf port "static struct {~n")
	    (emit-static-variable-fields! port (compiler:get-static-list))
	    (unless (null? (compiler:get-case-lambdas))
	      (fprintf port "  mzshort *casesArities[~a];~n"
		       (length (compiler:get-case-lambdas))))
	    (for-each
	     (lambda (ll)
	       (fprintf port "  Scheme_Object * ~a;~n" 
			(vm->c:convert-symbol (zodiac:varref-var ll))))
	     (compiler:get-lifted-lambda-vars))
	    (fprintf port "} S;~n~n"))

	  (fprintf port "/* compiler-written per-load static variables */~n")
	  (fprintf port "typedef struct Scheme_Per_Load_Statics {~n")
	  (if (null? (compiler:get-per-load-static-list))
	      (fprintf port "  int dummy;~n")
	      (emit-static-variable-fields! port (compiler:get-per-load-static-list)))
	  (fprintf port "} Scheme_Per_Load_Statics;~n")
	  (newline port)

	  (let ([ht (make-hash-table)])
	    ;; Gather per-invoke statics with the same invoke id
	    (let ([l (compiler:get-per-invoke-static-list)])
	      (for-each (lambda (p)
			  (let ([mi (cdr p)]
				[var (car p)])
			    (hash-table-put! ht (varref:module-invoke-id mi)
					     (cons (cons var (varref:module-invoke-syntax? mi))
						   (hash-table-get
						    ht
						    (varref:module-invoke-id mi)
						    (lambda () null))))))
			l)
	      ;; Make sure that every module has a struct:
	      (let loop ([i 0])
		(unless (= i (get-num-module-invokes))
		  (hash-table-get ht i (lambda ()
					 (hash-table-put! ht i null)))
		  (loop (add1 i))))
	      (hash-table-for-each
	       ht
	       (lambda (id vars)
		 (fprintf port "/* compiler-written per-invoke variables for module ~a */~n" id)
		 (let ([vars (map car (filter (lambda (i) (not (cdr i))) vars))]
		       [syntax-vars (map car (filter (lambda (i) (cdr i)) vars))])
		   (fprintf port "typedef struct Scheme_Per_Invoke_Statics_~a {~n" id)
		   (if (null? vars)
		       (fprintf port "  int dummy;~n")
		       (emit-static-variable-fields! port vars))
		   (fprintf port "} Scheme_Per_Invoke_Statics_~a;~n" id)
		   (fprintf port "typedef struct Scheme_Per_Invoke_Syntax_Statics_~a {~n" id)
		   (if (null? syntax-vars)
		       (fprintf port "  int dummy;~n")
		       (emit-static-variable-fields! port syntax-vars))
		   (fprintf port "} Scheme_Per_Invoke_Syntax_Statics_~a;~n" id)
		   (newline port))))))))
      
      ;; when statics have binding information, this need only register
      ;; pointer declarations
      (define vm->c:emit-registration!
	(lambda (port)
	  (fprintf port "~a/* register compiler-written static variables with GC */~n"
		   vm->c:indent-spaces)
	  (let ([register
		 (lambda (v)
		   (fprintf port "~ascheme_register_extension_global(&~a, sizeof(~a));~n"
			    vm->c:indent-spaces v v))])
	    (unless (or (zero? (const:get-symbol-counter)) (compiler:multi-o-constant-pool))
	      (register "SYMBOLS"))
	    (unless (zero? (const:get-inexact-counter))
	      (register "INEXACTS"))
	    (unless (set-empty? (compiler:get-primitive-refs))
	      (register "P"))
	    (unless (not (compiler:any-statics?))
	      (register "S"))
	    (unless (null? (const:get-syntax-strings))
	      (register "SS")))
	  (newline port)))

      (define (vm->c:emit-case-arities-definitions! port)
	(fprintf port "   /* arity information for compiled case-lambdas */~n")
	(let caloop ([l (reverse (compiler:get-case-lambdas))][pos 0])
	  (unless (null? l)
	    (let* ([ast (car l)]
		   [args (zodiac:case-lambda-form-args ast)])
	      (if (null? args)
		  (fprintf port "~aS.casesArities[~a] = NULL;~n"
			   vm->c:indent-spaces pos)
		  (begin
		    (fprintf port "~a{~n~a  mzshort * arities;~n" 
			     vm->c:indent-spaces vm->c:indent-spaces)
		    (fprintf port "~a  arities = (mzshort *)scheme_malloc_atomic(~a * sizeof(mzshort));~n"
			     vm->c:indent-spaces
			     (* 2 (length args)))
		    (let cailoop ([l args][n 0])
		      (unless (null? l)
			(let-values ([(min-arity max-arity) (compiler:formals->arity (car l))])
			  (fprintf port "~a  arities[~a] = ~a;~n~a  arities[~a] = ~a;~n"
				   vm->c:indent-spaces (* 2 n) min-arity
				   vm->c:indent-spaces (add1 (* 2 n)) max-arity))
			(cailoop (cdr l) (add1 n))))
		    (fprintf port "~a  S.casesArities[~a] = arities;~n"
			     vm->c:indent-spaces pos)
		    (fprintf port "~a}~n" vm->c:indent-spaces))))
	    (caloop (cdr l) (add1 pos)))))

      (define (vm->c:emit-top-levels! kind return? per-load? null-self-modidx? count vm-list locals-list 
				      globals-list max-arity module mod-syntax? c-port)
	;; count == -1 => go to the end of the list
	(let tls-loop ([i 0]
		       [n 0]
		       [vml vm-list]
		       [ll locals-list]
		       [bl globals-list])
	  (fprintf c-port
		   "static ~a ~a_~a(Scheme_Env * env~a~a)~n{~n"
		   (if return? "Scheme_Object *" "void")
		   kind i
		   (if (or per-load? module) ", Scheme_Per_Load_Statics *PLS" "")
		   (if module
		       (format 
			", long phase_shift, Scheme_Object *self_modidx, Scheme_Per_Invoke_~aStatics_~a *PMIS" 
			(if mod-syntax? "Syntax_" "")
			module)
		       ""))
	  (when null-self-modidx? (fprintf c-port "#define self_modidx NULL~n"))
	  (when (> max-arity 0)
	    (fprintf c-port
		     "~aScheme_Object * arg[~a];~n"
		     vm->c:indent-spaces
		     max-arity)
	    (fprintf c-port "~aScheme_Thread * pr = scheme_current_thread;~n"
		     vm->c:indent-spaces)
	    (fprintf c-port "~aScheme_Object ** tail_buf;~n"
		     vm->c:indent-spaces))
	  (let loop ([c (compiler:option:max-exprs-per-top-level-set)][n n][vml vml][ll ll][bl bl])
	    (if (or (zero? c) (null? vml) (= n count))
		(begin
		  (unless (or (null? vml) (= n count) (not return?))
		    (fprintf c-port "~areturn NULL;~n" vm->c:indent-spaces))
		  (when null-self-modidx? (fprintf c-port "#undef self_modidx~n"))
		  (fprintf c-port 
			   "} /* end of ~a_~a */~n~n" kind i)
		  (if (or (null? vml) (= n count))
		      i
		      (tls-loop (add1 i) n vml ll bl)))
		(if (not (or (and (not module)
				  (not (vm:module-body? (car vml))))
			     (and module
				  (vm:module-body? (car vml))
				  (is-module-invoke? (vm:module-body-invoke (car vml))  module)
				  (eq? (vm:module-body-syntax? (car vml)) mod-syntax?))))
		    (loop c n (cdr vml) (cdr ll) (cdr bl))
		    (begin
		      (let ([start (zodiac:zodiac-start (car vml))])
			(fprintf c-port "~a{ /* [~a,~a] */~n" vm->c:indent-spaces
				 (zodiac:location-line start)
				 (zodiac:location-column start)))
		      (vm->c:emit-local-variable-declarations! 
		       (car ll)
		       (string-append vm->c:indent-spaces vm->c:indent-spaces)
		       c-port)
		      (vm->c:emit-local-bucket-declarations! 
		       (car bl)
		       (string-append vm->c:indent-spaces vm->c:indent-spaces)
		       #t
		       c-port)
		      (vm->c:emit-bucket-lookups! 
		       (car bl)
		       (string-append vm->c:indent-spaces vm->c:indent-spaces)
		       c-port)
		      
		      (vm->c-expression (car vml) #f c-port vm->c:indent-by #t)
		      
		      (fprintf c-port "~a}~n" vm->c:indent-spaces)
		      
		      (loop (sub1 c) (add1 n) (cdr vml) (cdr ll) (cdr bl))))))))

      (define (vm->c:emit-module-glue! port id num num-syntax)
	(define (out syntax? n)
	  (fprintf port "static void module_invoke~a_~a(" 
		   (if syntax? "_syntax" "") id)
	  (fprintf port "Scheme_Env *env, long phase_shift, Scheme_Object *self_modidx, void *pls)~n")
	  (fprintf port "{~n~aScheme_Per_Invoke_~aStatics_~a *PMIS;~n" 
		   vm->c:indent-spaces (if syntax? "Syntax_" "") id)
	  (let ([s (format "Scheme_Per_Invoke_~aStatics_~a"
			   (if syntax? "Syntax_" "") id)])
	    (fprintf port "~aPMIS = (~a *)scheme_malloc(sizeof(~a));~n" 
		     vm->c:indent-spaces s s))
	  (let loop ([j 0])
	    (unless (j . > . n)
	      (fprintf port "~amodule_~abody_~a_~a(env, (Scheme_Per_Load_Statics *)pls, phase_shift, self_modidx, PMIS);~n"
		       vm->c:indent-spaces (if syntax? "syntax_" "") id j)
	      (loop (add1 j))))
	  (fprintf port "}~n~n"))

	(out #f num)
	(out #t num-syntax))

      (define vm->c:emit-vehicle-prototype
	(lambda (port number)
	  (let ([v (get-vehicle number)])
	    (fprintf port 
		     "static ~a vehicle_~a(~a)"
		     "Scheme_Object *"
		     number
		     (cond
		      [(procedure-vehicle? v)
		       "void * void_param, int argc, Scheme_Object *argv[]"]
		      [else
		       (compiler:internal-error
			#f
			"vm->c:emit-vehicle-prototype: unknown closure kind ~a"
			v)])))))

      (define vm->c:emit-vehicle-declaration
	(lambda (port number)
	  (vm->c:emit-vehicle-prototype port number)
	  (fprintf port "; /* ~a */ ~n"
		   (vehicle-total-labels (get-vehicle number)))))

      (define vm->c:emit-vehicle-header
	(lambda (port number)
	  (vm->c:emit-vehicle-prototype port number)
	  (fprintf port "~n{~n")))

      (define vm->c:emit-vehicle-prologue
	(lambda (port vehicle)
	  (let ([max-arity (vehicle-max-arity vehicle)]
		[max-args (if (procedure-vehicle? vehicle)
			      (procedure-vehicle-max-args vehicle)
			      0)])
	    (when (> max-arity 0)
					; emit declaration of argument stack 
	      (fprintf port "~aScheme_Object * arg[~a];~n" 
		       vm->c:indent-spaces 
		       max-arity))
	    (when (> max-args 0)
					; emit declaration of global variables for argument passing
	      (let loop ([n 0])
		(unless (= n max-args)
		  (fprintf port "~aregister long reg~a;~n" vm->c:indent-spaces n)
		  (loop (+ n 1)))))
	    (when (> max-arity 0)
					; tail-buffer-setup
	      (fprintf port "~aScheme_Thread * pr = scheme_current_thread;~n"
		       vm->c:indent-spaces)
	      (fprintf port "~aScheme_Object ** tail_buf;~n"
		       vm->c:indent-spaces)))

	  (when local-vars-at-top?
	    (for-each
	     (lambda (L)
	       (let ([locals (code-local-vars (get-annotation L))])
		 (vm->c:emit-local-variable-declarations! locals vm->c:indent-spaces port)))
	     (vehicle-lambdas vehicle)))
	  
					; emit jump to function...
	  (when (> (vehicle-total-labels vehicle) 1)
					; emit switch dispatcher
	    (fprintf port "~aswitch(*(unsigned int*)void_param)~n~a{ " 
		     vm->c:indent-spaces
		     vm->c:indent-spaces )
	    (let loop ([n 0])
	      (when (and (zero? (modulo n 3))
			 (not (= n (compiler:get-label-number))))
		(fprintf port "~n~a~a" vm->c:indent-spaces vm->c:indent-spaces))
	      (if (= n (sub1 (vehicle-total-labels vehicle)))
		  (fprintf port "default: goto FGN~a;" n)
		  (begin
		    (fprintf port "case ~a: goto FGN~a;" n n)
		    (loop (add1 n)))))
	    (fprintf port "~n~a}~n" vm->c:indent-spaces))))

      (define vm->c:emit-vehicle-epilogue
	(lambda (port number)
	  (fprintf port "} /* end of vehicle # ~a */~n" number)))

      ;; Will be expanded to hold environments, perhaps, etc.
      (define vm->c:convert-type-definition
	(lambda (rep)
	  (cond
	   [(rep:atomic? rep) (case (rep:atomic-type rep)
				[(scheme-object) "Scheme_Object *"]
				[(scheme-bucket) "Scheme_Bucket *"]
				[(scheme-per-load-static) "struct Scheme_Per_Load_Statics *"]
				[(scheme-per-invoke-static) 
				 (let ([mi (rep:atomic/invoke-module-invoke rep)])
				   (format "struct Scheme_Per_Invoke_~aStatics_~a *"
					   (if (varref:module-invoke-syntax? mi)
					       "Syntax_"
					       "")
					   (varref:module-invoke-id mi)))]
				[(label) "int"]
				[(prim) "Scheme_Closed_Primitive_Proc"]
				[(prim-case) "Scheme_Closed_Case_Primitive_Proc"]
				[(begin0-saver) "_Scheme_Begin0_Rec"]
				[(wcm-saver) "_Scheme_WCM_Rec"]
				[else (compiler:internal-error 
				       #f
				       (format
					"vm->c:convert-type-definition: ~a not valid atomic type"
					(rep:atomic-type rep)))])]
	   [(rep:pointer? rep)
	    (string-append (vm->c:convert-type-definition (rep:pointer-to rep))
			   " *")]
	   [(rep:struct? rep)
	    (format "struct ~a" (vm->c:convert-symbol (rep:struct-name rep)))]
	   [else (compiler:internal-error 
		  #f
		  (format "vm->c:convert-type-definition: ~a not a valid representation" rep))])))

      ;; must handle structs as well as atomic types
      (define vm->c:type-definition->malloc
	(lambda (rep)
	  (let ([s (if (rep:struct? rep)
		       (string-append "struct " (vm->c:convert-symbol (rep:struct-name rep)))
		       (vm->c:convert-type-definition rep))])
	  (format "(~a *)scheme_malloc(sizeof(~a))" s s))))

      (define vm->c:emit-local-variable-declarations!
	(lambda (locals indent port)
	  (let loop ([locals (set->list locals)])
	    (if (null? locals)
		(void)
		(let* ([bound (car locals)]
		       [rep (binding-rep (get-annotation bound))])
		  (fprintf port "~a~a ~a;~n"
			   indent
			   (vm->c:convert-type-definition rep)
			   (vm->c:convert-symbol (zodiac:binding-var bound)))
		  (loop (cdr locals)))))))

      (define vm->c:emit-local-bucket-declarations!
	(lambda (globals indent top-level? port)
	  (for-each 
	   (lambda (var)
	     (cond
	      [(const:per-load-statics-table? var)
	       (unless top-level?
		 (fprintf port "~aScheme_Per_Load_Statics * PLS;~n"
			  indent))]
	      [(varref:module-invoke? var)
	       (unless top-level?
		 (fprintf port "~aScheme_Per_Invoke_Statics_~a * PMIS;~n"
			  indent (varref:module-invoke-id var)))]
	      [else
	       (fprintf port "~aScheme_Bucket * G~a;~n"
			indent
			(vm->c:convert-symbol (mod-glob-cname var)))]))
	   (set->list globals))))

      (define vm->c:emit-bucket-lookups!
	(lambda (globals indent port)
	  (for-each 
	   (lambda (var)
	     (unless (or (const:per-load-statics-table? var)
			 (varref:module-invoke? var))
	       (let* ([name (vm->c:convert-symbol (mod-glob-cname var))]
		      [et? (mod-glob-exp-time? var)]
		      [ed? (mod-glob-exp-def? var)]
		      [position (mod-glob-position var)]
		      [mod (mod-glob-modname var)]
		      [in-mod? (mod-glob-in-module? var)]
		      [var (mod-glob-varname var)]
		      [modidx (and (not (symbol? mod))
				   (compiler:get-module-path-constant mod))]
		      [mod-local (and mod (not (symbol? mod)) (not modidx))]
		      [mod-far (and mod (or (symbol? mod) modidx))])
		 (fprintf port "~aG~a = scheme_~a~a~a_bucket(~a~a~a, ~a~a~a);~n"
			  indent 
			  name 
			  (if et? "exptime_" "")
			  (if ed? "expdef_" "")
			  (if mod-far "module" "global")
			  (if mod-far
			      (if (symbol? mod)
				  (vm->c:make-symbol-const-string (compiler:get-symbol-const! #f mod))
				  (format
				   "~a~a"
				   (cond
				    [(varref:has-attribute? modidx varref:per-load-static) "PLS->"]
				    [(varref:has-attribute? modidx varref:per-invoke-static) "PMIS->"]
				    [else "S."])
				   (vm->c:convert-symbol (zodiac:varref-var modidx))))
			      "")
			  (if mod-far ", " "")
			  (vm->c:make-symbol-const-string (compiler:get-symbol-const! #f var))
			  (if mod-far (or position -1) "")
			  (if mod-far ", " "")
			  (if (or mod-local in-mod?) "env" "SCHEME_CURRENT_ENV(pr)")))))
	   (set->list globals))))

      (define binding-boxed? binding-mutable?)

      (define vm->c:extract-arguments-into-variables!
	(lambda (args normal? get-rep get-dest dest-boxed? get-src get-cast has-default? indent port)
					; Reverse order for the sake of noticing default arguments
	  (let loop ([args (reverse args)] [n (sub1 (length args))] [last? #t])
	    (unless (null? args)
	      (let* ([has-default? (has-default? n)]
		     [argv-n
		      (lambda ()
			(if has-default?
			    (format "((argc > ~a) ? ~a : (arg_set_level = ~a, scheme_undefined))" n (get-src n) n)
			    (get-src n)))])
		(cond
		 [(or normal? (not last?))
		  (fprintf port "~a~a = " indent (get-dest n))
		  (if (dest-boxed? n)
		      ;; if the binding is mutable, we need to make a box and fill it with
		      ;; the correct value
		      (let ([rep (get-rep n)])
			(fprintf port "~ascheme_malloc(sizeof(~a));~n" 
				 (get-cast n #f)
				 (vm->c:convert-type-definition
				  (rep:pointer-to rep)))
			(fprintf port "~a*(~a)~a = (~a)~a;~n"
				 indent
				 (vm->c:convert-type-definition rep)
				 (get-dest n) 
				 (vm->c:convert-type-definition (rep:pointer-to rep))
				 (argv-n)))
		      
		      (fprintf port "~a~a;~n" (get-cast n #t) (argv-n)))
		  (loop (cdr args) (sub1 n) #f)]
		 
		 [else ; the rest get pulled into a list
		  (when (dest-boxed? n)
		    (fprintf port
			     "~a~a = ~ascheme_malloc(sizeof(Scheme_Object *));~n"
			     indent
			     (get-dest n)
			     (get-cast n #f)))
		  (fprintf port 
			   "~a~a~a = ~ascheme_build_list(argc-~a, argv+~a);~n"
			   indent
			   (if (dest-boxed? n)
			       "*(Scheme_Object * *)" 
			       "")
			   (get-dest n)
			   (if (dest-boxed? n)
			       ""
			       (get-cast n #t))
			   n
			   n)

		  (loop (cdr args) (sub1 n) #f)]))))))

      (define vm->c:pack-global-registers!
	(lambda (L which indent port)
	  (let* ([arglist (list-ref (zodiac:case-lambda-form-args L) which)]
		 [args (zodiac:arglist-vars arglist)])
	    (vm->c:extract-arguments-into-variables!
	     args
	     (zodiac:list-arglist? arglist)
	     (lambda (n) "") ; rep not used since never boxed
	     (lambda (n) (format "reg~a" n))
	     (lambda (n) #f) ; never boxed
	     (lambda (n) (format "argv[~a]" n))
	     (lambda (n deref) "(long)")
	     (lambda (n) #f)
	     indent port))))

      (define vm->c:emit-private-box-initializations
					; Currently, each is filled with undefined, but specialized representations
					;  will require something different
	(lambda (bindings indent port)
	  (for-each
	   (lambda (binding)
	     (let* ([rep (binding-rep (get-annotation binding))]
		    [derep (rep:pointer-to rep)])
	       (fprintf port "~a~a = (~a)~a;~n~a*(~a) = scheme_undefined;~n"
			indent
			(vm->c:convert-symbol (zodiac:binding-var binding))
			(vm->c:convert-type-definition rep)
			(vm->c:type-definition->malloc derep)
			
			indent
			(vm->c:convert-symbol (zodiac:binding-var binding)))))
	   bindings)))

      (define vm->c:emit-undefines
	(lambda (undefines indent port)
	  (for-each
	   (lambda (name)
	     (fprintf port "#~aundef ~a~n"
		      indent name))
	   undefines)))

      (define vm->c:emit-function-prologue
	(lambda (L port)
	  (let* ([code (get-annotation L)]
		 [label (closure-code-label code)])
	    (if (= 1 (length (zodiac:case-lambda-form-bodies L)))
		(values 1 #f)
		(begin
		  ;; The foreign entry label      
		  (fprintf port "FGN~a:~n" label)
		  (let loop ([args (zodiac:case-lambda-form-args L)][i 0])
		    (if (null? args)
			(begin
			  (fprintf port "~a~ascheme_case_lambda_wrong_count(~s, argc, argv, ~a, ~a"
				   vm->c:indent-spaces vm->c:indent-spaces
				   (vm->c:extract-inferred-name (closure-code-name code))
				   (if (procedure-code-method? code) "1" "0")
				   (length (zodiac:case-lambda-form-args L)))
			  (let loop ([l (zodiac:case-lambda-form-args L)])
			    (unless (null? l)
			      (let-values ([(min-arity max-arity)
					    (compiler:formals->arity (car l))])
				(fprintf port ", ~a, ~a" min-arity max-arity)
				(loop (cdr l)))))
			  (fprintf port ");~n")
			  (fprintf port "~a~areturn NULL;~n"
				   vm->c:indent-spaces vm->c:indent-spaces)
			  (values i #t))
			(let ([a (car args)])
			  (cond
			   [(zodiac:sym-arglist? a)
			    (fprintf port "~a~agoto FGN~ac~a;~n" 
				     vm->c:indent-spaces vm->c:indent-spaces
				     label
				     i)
			    (values (add1 i) #t)]
			   [(zodiac:list-arglist? a)
			    (fprintf port "~a~aif (argc == ~a) goto FGN~ac~a;~n"
				     vm->c:indent-spaces vm->c:indent-spaces
				     (length (zodiac:arglist-vars a))
				     label
				     i)
			    (loop (cdr args) (add1 i))]
			   [else
			    (fprintf port "~a~aif (argc >= ~a) goto FGN~ac~a;~n"
				     vm->c:indent-spaces vm->c:indent-spaces
				     (sub1 (length (zodiac:arglist-vars a)))
				     label
				     i)
			    (loop (cdr args) (add1 i))])))))))))

      (define vm->c:emit-extract-env-variables
	(lambda (code vars indent port)
	  ;; now pull environment variables into registers
	  ;; this is easy because of the way we've set up environments
	  (let loop ([vars vars][undefines null])
	    (if (null? vars)
		undefines
		(let* ([var (if (pair? vars) (car vars) vars)]
		       [vname (zodiac:binding-var var)]
		       [name (vm->c:convert-symbol vname)]
		       [fname (rep:find-field (closure-code-rep code) vname)])
		  (fprintf port (if (compiler:option:unpack-environments)
				    "~a~a = env->~a;~n"
				    "#~adefine ~a env->~a~n")
			   indent
			   name
			   fname)
		  (let ([undefines (if (compiler:option:unpack-environments)
				       undefines
				       (cons name undefines))])
		    (if (pair? vars)
			(loop (cdr vars) undefines)
			undefines)))))))

      (define vm->c:emit-extract-bucket-variables
	(lambda (code vars indent port)
	  ;; pull bucket variables into registers
	  (let loop ([vars vars][undefines null])
	    (if (null? vars)
		undefines
		(let ([var (car vars)])
		  (cond
		   [(const:per-load-statics-table? var)
		    (begin
		      (fprintf port 
			       (if (compiler:option:unpack-environments)
				   "~aPLS = env->pls;~n"
				   "#~adefine PLS env->pls~n")
			       indent)
		      (loop (cdr vars)
			    (if (compiler:option:unpack-environments)
				undefines
				(cons "PLS" undefines))))]
		   [(varref:module-invoke? var)
		    (begin
		      (fprintf port 
			       (if (compiler:option:unpack-environments)
				   "~aPMIS = env->pmis;~n"
				   "#~adefine PMIS env->pmis~n")
			       indent)
		      (loop (cdr vars)
			    (if (compiler:option:unpack-environments)
				undefines
				(cons "PMIS" undefines))))]
		   [else
		    (let* ([vname (mod-glob-cname var)]
			   [name (vm->c:convert-symbol vname)]
			   [fname (rep:find-field (closure-code-rep code) vname)])
		      (fprintf port 
			       (if (compiler:option:unpack-environments)
				   "~aG~a = env->~a;~n"
				   "#~adefine G~a env->~a~n")
			       indent
			       name
			       fname)
		      (loop (cdr vars)
			    (if (compiler:option:unpack-environments)
				undefines
				(cons (string-append "G" name) undefines))))]))))))
      
      (define vm->c:emit-case-prologue
	(lambda (L which pre-decl lsuffix indent port)
	  (let* ([code (get-annotation L)]
		 [case-code (list-ref (procedure-code-case-codes code) which)]
		 [label (closure-code-label code)]
		 [undefines null]
		 [used-free-set
					; Only unpack anchors if they're captured
		  (let* ([free-set (code-free-vars case-code)]
			 [free-list (set->list free-set)]
			 [captured-list (set->list (code-captured-vars code))]
			 [uncaptured-anchor-set
			  (list->set
			   (let loop ([l free-list])
			     (if (null? l)
				 null
				 (let ([zb (car l)])
				   (let ([a (binding-anchor (get-annotation zb))])
				     (if (and a (not (member zb captured-list)))
					 (cons a (loop (cdr l)))
					 (loop (cdr l))))))))])
		    (set-minus free-set uncaptured-anchor-set))])
					; The foreign entry label      
	    (fprintf port "FGN~a~a:~n" label lsuffix)
					; Pull arguments to global registers      
	    (vm->c:pack-global-registers! L which indent port)

					; The local entry label
	    (fprintf port "LOC~a~a:~n" label lsuffix)
	    (pre-decl)
	    (unless local-vars-at-top?
	      (vm->c:emit-local-variable-declarations! (code-local-vars case-code) indent port))

	    (when (compiler:option:unpack-environments)
	      (vm->c:emit-local-variable-declarations! used-free-set indent port)
	      (vm->c:emit-local-bucket-declarations! (code-global-vars case-code) indent #f port))
	    
	    (let ([r (closure-code-rep code)])
	      (when r
					; (fprintf port "~aconst ~a * env;~n" indent (vm->c:convert-type-definition r))
		(fprintf port "#~adefine env ((const ~a *)void_param)~n" indent (vm->c:convert-type-definition r))))
	    
					; Registers into local vars
	    (let* ([args (zodiac:arglist-vars (list-ref (zodiac:case-lambda-form-args L) which))])
	      (vm->c:extract-arguments-into-variables!
	       args
	       #t ; since regN already builds lists as appropriate
	       (lambda (n) (binding-rep (get-annotation (list-ref args n))))
	       (lambda (n) (vm->c:convert-symbol (zodiac:binding-var (list-ref args n))))
	       (lambda (n) (binding-boxed? (get-annotation (list-ref args n))))
	       (lambda (n) (format "reg~a" n))
	       (lambda (n deref) (format "(~a)"
					 (vm->c:convert-type-definition
					  (let* ([binding (get-annotation (list-ref args n))]
						 [rep (binding-rep binding)])
					    (if (and deref (binding-boxed? binding))
						(rep:pointer-to rep)
						rep)))))
	       (lambda (n) #f)
	       indent port))

					; reduce register pressure by doing all the env calculations
					; after the args have been done
					; equate the local registers with the global argument registers
					; starting with the env
	    #|
	    (let ([r (closure-code-rep code)])
	      (when r
		(fprintf port "~aenv = (~a *)void_param;~n"
			 indent
			 (vm->c:convert-type-definition r))))
	    |#

					; now pull environment variables into registers
	    (set! undefines
		  (append (vm->c:emit-extract-env-variables
			   code
			   (set->list used-free-set)
			   indent port)
			  undefines))

					; pull bucket variables into registers
	    (set! undefines
		  (append (vm->c:emit-extract-bucket-variables
			   code
			   (set->list (code-global-vars case-code))
			   indent port)
			  undefines))

	    (when (case-code-has-continue? case-code)
	      (fprintf port "~awhile(1)~n" indent))

	    undefines)))

      (define vm->c:emit-case-epilogue
	(lambda (code which undefines indent port)
	  (fprintf port "#~aundef env~n" indent)
	  (vm->c:emit-undefines undefines indent port)))

      (define vm->c:emit-function-epilogue
	(lambda (code close port)
	  (fprintf port "~a~a /* end of function body ~a */~n" 
		   vm->c:indent-spaces close (closure-code-label code))))

      (define vm->c:convert-symbol
	(lambda (sym)
	  (compiler:clean-string (symbol->string sym))))

      (define vm->c:convert-char
	(lambda (char)
	  (if ((char->integer char) . > . 127)
	      (char->integer char)
	      (format
	       "'~a'"
	       (cond 
		[(char=? char #\tab) "\\t"]
		[(char=? char #\newline) "\\n"]
		[(char=? char #\return) "\\r"]
		[(char=? char #\space) " "]
		[(or (char-alphabetic? char) (char-numeric? char)) (string char)]
		[else (let ([text (number->string (char->integer char) 8)])
			(string-append "\\"
				       (make-string (- 3 (string-length text)) #\0)
				       text))])))))

      (define vm->c:convert-special-constant
	(lambda (ast)
	  (cond
	   [(void? (syntax-e (zodiac:zodiac-stx ast))) "scheme_void"]
	   [(undefined? (syntax-e (zodiac:zodiac-stx ast))) "scheme_undefined"]
	   [else (compiler:internal-error 
		  #f 
		  (format
		   "vm->c:convert-special-constant: ~a not correct" ast))])))
      

      (define vm->c:block-statement?
	(one-of vm:if? vm:sequence? vm:module-body?))

      (define vm->c:extract-inferred-name
	(let ([nullsym (string->symbol "NULL")])
	  (lambda (var)
	    (cond
	     [(list? var)
	      (if (= (length var) 1)
		  (vm->c:extract-inferred-name (car var))
		  nullsym)]
	     [(zodiac:binding? var)
	      (symbol->string (zodiac:binding-orig-name var))]
	     [(zodiac:bound-varref? var)
	      (vm->c:extract-inferred-name (zodiac:bound-varref-binding var))]
	     [(zodiac:varref? var)
	      (symbol->string (zodiac:varref-var var))]
	     [(not var) nullsym]
	     [else (compiler:internal-error
		    #f
		    (format "vm->c:extract-inferred-name: bad var type: ~a"
			    var))]))))

      (define single-arity?
	(one-of vm:global-varref?
		vm:local-varref?
		vm:static-varref?
		vm:primitive-varref?
		vm:symbol-varref?
		vm:struct-ref?
		vm:deref?
		vm:ref?
		vm:cast?
		vm:immediate?))

      (define is-primitive?
	(one-of primitive? primitive-closure?))

      (define vm->c-expression
	(lambda (ast code port indent-level no-seq-braces?)
	  (let process ([ast ast] [indent-level indent-level] [own-line? #t] [braces? (not no-seq-braces?)])
	    (letrec ([emit-indentation (lambda () (display
						   (make-string indent-level #\ )
						   port))]
		     [indent (lambda () (+ indent-level vm->c:indent-by))]
		     [emit (lambda s (apply fprintf (cons port s)))]
		     [emit-expr (lambda s
				  (when own-line? (emit-indentation))
				  (apply emit s))]
		     [emit-macro-application
		      (lambda (ast)
			(let ([args (vm:macro-apply-args ast)])
			  (emit "~a(" (vm:macro-apply-name ast))
			  (process (vm:macro-apply-primitive ast) indent-level #f #f)
			  (for-each (lambda (a) 
				      (emit ", ~a" (vm->c:convert-symbol (zodiac:binding-var a))))
				    args)
			  (emit ")")))])

	      (cond
	       
	       ;; (%sequence V ...) -> { M; ... }
	       [(or (vm:sequence? ast)
		    (vm:module-body? ast))
		(let* ([seq ((if (vm:sequence? ast) vm:sequence-vals vm:module-body-vals) ast)])
		  (when braces? (emit-indentation) (emit "{~n"))
		  (for-each (lambda (v)
			      (process v (indent) #t #t)
			      (unless (vm->c:block-statement? v) (emit ";~n")))
			    seq)
		  (when braces? (emit-indentation) (emit "}~n")))]
	       
	       ;; (if R (sequence V) (sequence V)) ->
	       ;;    if (!SCHEME_FALSEP(A)) { V ... } else { V ...}
	       [(vm:if? ast)
		(emit-indentation)
		(let iloop ([ast ast])
		  (let ([test (vm:if-test ast)]
			[then (vm:if-then ast)]
			[else (vm:if-else ast)])
		    
		    (emit "if (")
		    (let ([direct? (and (vm:macro-apply? test)
					(vm:macro-apply-bool? test))])
		      (if direct?
			  (emit-macro-application test)
			  (begin
			    (emit "!SCHEME_FALSEP(")
			    (process test indent-level #f #t)
			    (emit ")"))))
		    (emit ")~n")
		    (process (vm:if-then ast) indent-level #t #t)
		    (let ([else-vals (vm:sequence-vals else)])
		      (cond 
		       [(and (= 1 (length else-vals))
			     (vm:if? (car else-vals)))
			(emit-indentation) (emit "else ")
			(iloop (car else-vals))]
		       [(not (null? else-vals))
			(emit-indentation) (emit "else~n")
			(process (vm:if-else ast) indent-level #f #t)]
		       [else (void)]))))]
	       
	       ;; begin0 stuff
	       [(vm:begin0-mark!? ast)
		(let ([var (vm->c:convert-symbol
			    (vm:local-varref-var (vm:begin0-mark!-var ast)))])
		  (emit-indentation)
		  (emit "~a.val = " var)
		  (process (vm:begin0-mark!-val ast) indent-level #f #t))]
	       [(vm:begin0-setup!? ast)
		(let ([var (vm->c:convert-symbol
			    (vm:local-varref-var (vm:begin0-setup!-var ast)))])
		  (emit-indentation)
		  (emit "if (~a.val == SCHEME_MULTIPLE_VALUES) {~n" var)
		  (emit-indentation)
		  (emit "  ~a.array = pr->ku.multiple.array;~n" var)
		  (emit-indentation)
		  (emit "  ~a.count = pr->ku.multiple.count;~n" var)
		  (emit-indentation)
		  (emit "  SCHEME_DETATCH_MV_BUFFER(~a.array, pr);~n" var)
		  (emit-indentation)
		  (emit "} else ~a.array = NULL" var))]
	       [(vm:begin0-extract? ast)
		(let ([var (vm->c:convert-symbol
			    (vm:local-varref-var (vm:begin0-extract-var ast)))])
		  (emit "(pr->ku.multiple.array = ~a.array," var)
		  (emit " pr->ku.multiple.count = ~a.count, " var)
		  (emit " ~a.val)" var))]

	       ;; single value: (set! L R) -> L = R;
	       ;; multiple value:
	       [(vm:set!? ast)
		(let* ([process-target!
			(lambda (target)
			  (let ([type (car target)]
				[target (cdr target)])
			    (cond
			     [(eq? type target-type:lexical) 
			      (process target indent-level #f #t)]
			     [(eq? type target-type:global)
			      (let ([bucket-name (vm->c:convert-symbol (mod-glob-cname target))])
				(emit "G~a->val" bucket-name))]
			     [else (compiler:internal-error 
				    #f
				    (format "~a: bad set! target type" type))])))]
		       [process-set!
			(lambda (target val process-val?)
			  (let ([mode (vm:set!-mode ast)])
			    (if mode
				(begin
				  (emit "scheme_set_global_bucket(~s, " (car mode))
				  (emit "G~a, " (vm->c:convert-symbol (mod-glob-cname (cdr target))))
				  (if process-val? 
				      (process val indent-level #f #t)
				      (emit val))
				  (emit ", ~a)" (cadr mode)))
				(begin
				  (process-target! target)
				  (emit " = ")
				  (if process-val? 
				      (process val indent-level #f #t)
				      (emit val))))))]
		       [vars (vm:set!-vars ast)]
		       [val (vm:set!-val ast)]
		       [num-to-set (length vars)]
		       [return-arity
			(or (and (single-arity? val)
				 1)
			    (and (vm:apply? val) 
				 (vm:apply-prim val)
				 (let ([proc (global-defined-value* (vm:apply-prim val))])
				   (and
				    (is-primitive? proc)
				    (primitive-result-arity proc))))
			    
			    )])
		  (emit-indentation)
		  (let ([return-arity-ok?
			 (and return-arity
			      (number? return-arity)
			      (= return-arity num-to-set))])
		    (if (= num-to-set 1)		      
			
			(process-set! (car vars) val #t)
			
			(begin
			  (emit "{ Scheme_Object * res = ")
			  (process val indent-level #f #t)
			  (emit "; ")
			  (unless return-arity-ok?
			    (emit "CHECK_MULTIPLE_VALUES(res, ~a);" num-to-set))
			  (emit "}")
			  (if (not (null? vars))
			      (emit "~n"))
			  (let aloop ([vars vars] [n 0])
			    (unless (null? vars)
			      (emit-indentation)
			      (process-set! (car vars) (format "scheme_multiple_array[~a]" n) #f)
			      (emit ";~n")
			      (aloop (cdr vars) (+ n 1))))
			  ))))]
	       
	       
	       ;; (define-syntax! x R) or (define-for-syntax! x R)
	       [(vm:syntax!? ast)
		(let* ([process-set!
			(lambda (target val process-val? return-arity-ok?)
			  (let ([sym
				 (vm->c:make-symbol-const-string 
				  (compiler:get-symbol-const! #f (zodiac:varref-var target)))]
				[in-module? (varref:has-attribute? target varref:in-module)])
			    (when process-val? 
			      (emit "{ Scheme_Object *mcv = ")
			      (process val indent-level #f #t)
			      (emit "; "))
			    (unless return-arity-ok?
			      (emit " if (mcv != SCHEME_MULTIPLE_VALUES || scheme_multiple_count) {")
			      (emit " NO_MULTIPLE_VALUES(mcv); "))
			    (let ([for-stx? (zodiac:top-level-varref-expdef? target)])
			      (emit "scheme_~a(~ascheme_global_~abucket(~a, ~a), "
				    (if for-stx? "set_global_bucket" "install_macro")
				    (if for-stx? "NULL, " "")
				    (if for-stx? "" "keyword_")
				    sym
				    (if in-module? "env" "SCHEME_CURRENT_ENV(pr)"))
			      (if process-val? 
				  (emit "mcv")
				  (emit val))
			      (when for-stx?
				(emit ", 1"))
			      (emit ")"))
			    (when (or (not return-arity-ok?) process-val?)
			      (emit ";"))
			    (unless return-arity-ok?
			      (emit " }"))
			    (when process-val?
			      (emit " }"))))]
		       [vars (vm:syntax!-vars ast)]
		       [val (vm:syntax!-val ast)]
		       [in-mod? (vm:syntax!-in-mod? ast)]
		       [num-to-set (length vars)]
		       [return-arity (if (single-arity? val) 
					 1
					 #f)])
		  (emit-indentation)
		  (let ([return-arity-ok?
			 (and return-arity
			      (number? return-arity)
			      (= return-arity num-to-set))])
		    (if (= num-to-set 1)		      
			
			(process-set! (car vars) val #t return-arity-ok?)
			
			(begin
			  (emit "{ Scheme_Object * res = ")
			  (process val indent-level #f #t)
			  (emit "; ")
			  (unless return-arity-ok?
			    (unless in-mod?
			      (emit "if (res != SCHEME_MULTIPLE_VALUES || scheme_multiple_count) "))
			    (emit "CHECK_MULTIPLE_VALUES(res, ~a);" num-to-set))
			  (emit "}")
			  (if (not (null? vars))
			      (emit "~n"))
			  (unless in-mod?
			    (emit-indentation)
			    (emit "if (scheme_multiple_count) {~n"))
			  (let aloop ([vars vars] [n 0])
			    (unless (null? vars)
			      (emit-indentation)
			      (process-set! (car vars) (format "scheme_multiple_array[~a]" n) #f #t)
			      (emit ";~n")
			      (aloop (cdr vars) (+ n 1))))
			  (unless in-mod?
			    (emit-indentation)
			    (emit "}~n"))
			  ))))]

	       ;; (%args A ...) -> arg[0] = A; ...
	       [(vm:args? ast)
		;; skip tail_buf setup if no args
		(when (and (eq? arg-type:tail-arg (vm:args-type ast))
			   (not (null? (vm:args-vals ast))))
		  (emit-indentation)
		  (emit "tail_buf = scheme_tail_apply_buffer_wp(~a, pr);~n"
			(length (vm:args-vals ast))))
		(if (null? (vm:args-vals ast))
		    (emit-indentation)
		    (let arloop ([n 0] [args (vm:args-vals ast)])
		      (unless (null? args)
			(emit-indentation)
			(let ([argtype (vm:args-type ast)])
			  (cond
			   [(eq? arg-type:arg argtype) (emit "arg[~a] = " n)]
			   [(eq? arg-type:tail-arg argtype) (emit "tail_buf[~a] = " n)]
			   [(eq? arg-type:register argtype) (emit "reg~a = (long)" n)]
			   [else (compiler:internal-error 
				  #f (format "vm->c: ~a unknown arg type" (vm:args-type ast)))]))
			;; (emit "DEBUG_CHECK(") ;; DEBUGGING
			(process (car args) indent-level #f #t)
			;; (emit ")") ;; DEBUGGING
			(unless (null? (cdr args))
			  (emit ";~n"))
			(arloop (add1 n) (cdr args)))))]

	       [(vm:register-args? ast)
		(let ([vars (vm:register-args-vars ast)]
		      [vals (vm:register-args-vals ast)])
		  (let raloop ([vars vars][vals vals])
		    (let ([var (car vars)]
			  [val (car vals)])
		      (emit-indentation)
		      (emit "~a = " (vm->c:convert-symbol (zodiac:binding-var var)))
		      (process val indent-level #f #f)
		      (unless (null? (cdr vars))
			(emit ";~n")
			(raloop (cdr vars) (cdr vals))))))]

	       ;; (alloc ) -> malloc
	       ;; a bit complicated
	       [(vm:alloc? ast)
		(emit (vm->c:type-definition->malloc (vm:alloc-type ast)))]

	       ;; (make-closure) -> _scheme_make_c_closure
	       [(vm:make-procedure-closure? ast)
		(emit "_scheme_make_c_proc_closure~a(vehicle_~a, " 
		      (if (vm:make-procedure-closure-empty? ast)
			  "_empty"
			  "")
		      (vm:make-procedure-closure-vehicle ast))
		(process (vm:make-closure-closure ast) indent-level #f #t)
		(emit ", ~s, ~a, ~a, ~a)" 
		      (vm->c:extract-inferred-name (vm:make-procedure-closure-name ast))
		      (vm:make-procedure-closure-min-arity ast)
		      (vm:make-procedure-closure-max-arity ast)
		      (if (vm:make-procedure-closure-method? ast) "SCHEME_PRIM_IS_METHOD" "0"))]

	       [(vm:make-case-procedure-closure? ast)
		(emit "_scheme_make_c_case_proc_closure~a(vehicle_~a, " 
		      (if (vm:make-case-procedure-closure-empty? ast)
			  "_empty"
			  "")
		      (vm:make-case-procedure-closure-vehicle ast))
		(process (vm:make-closure-closure ast) indent-level #f #t)
		(emit ", ~s, ~a, S.casesArities[~a], ~a)" 
		      (vm->c:extract-inferred-name (vm:make-case-procedure-closure-name ast))
		      (vm:make-case-procedure-closure-num-cases ast)
		      (vm:make-case-procedure-closure-case-arities ast)
		      (if (vm:make-case-procedure-closure-method? ast) "SCHEME_PRIM_IS_METHOD" "0"))]

	       [(vm:deref? ast)
		(emit "(*")
		(process (vm:deref-var ast) indent-level #f #t)
		(emit ")")]

	       [(vm:ref? ast)
		(emit "(&")
		(process (vm:ref-var ast) indent-level #f #t)
		(emit ")")]
	       
					; optimize (*X).Y to X->Y
	       [(vm:struct-ref? ast)
		(let ([var (vm:struct-ref-var ast)])
		  (if (vm:deref? var)
		      (begin
			(process (vm:deref-var var) indent-level #f #t)
			(emit "->"))
		      (begin
			(process (vm:struct-ref-var ast) indent-level #f #t)
			(emit ".")))
		  (emit "~a" (vm->c:convert-symbol (vm:struct-ref-field ast))))]

	       [(vm:cast? ast)
		(emit "(")
		(emit (vm->c:convert-type-definition (vm:cast-rep ast)))
		(emit ")(")
		(process (vm:cast-val ast) indent-level #f #t)
		(emit ")")]

	       [(vm:check-global? ast)
		(emit-expr (format "CHECK_GLOBAL_BOUND(G~a)"
				   (vm->c:convert-symbol
				    (mod-glob-cname (vm:check-global-var ast)))))]
	       
	       [(vm:module-create? ast)
		(emit-expr "scheme_declare_module(")
		(process (vm:module-create-shape ast) indent-level #f #f)
		(emit ", module_invoke_~a, module_invoke_syntax_~a, PLS, SCHEME_CURRENT_ENV(pr)"
		      (vm:module-create-id ast) (vm:module-create-id ast))
		(emit ")")]

	       ;; with-continuation-mark
	       [(vm:wcm-mark!? ast)
		(emit-expr "scheme_set_cont_mark(")
		(process (vm:wcm-mark!-key ast) indent-level #f #f)
		(emit ", ")
		(process (vm:wcm-mark!-val ast) indent-level #f #f)
		(emit ")")]
	       [(vm:wcm-push!? ast)
		(let ([var (vm->c:convert-symbol
			    (vm:local-varref-var (vm:wcm-push!-var ast)))])
		  (emit-indentation)
		  (emit "scheme_push_continuation_frame(&~a.cf)" var))]
	       [(vm:wcm-pop!? ast)
		(let ([var (vm->c:convert-symbol
			    (vm:local-varref-var (vm:wcm-pop!-var ast)))])
		  (emit-indentation)
		  (emit "scheme_pop_continuation_frame(&~a.cf)" var))]
	       [(vm:wcm-remember!? ast)
		(let ([var (vm->c:convert-symbol
			    (vm:local-varref-var (vm:wcm-remember!-var ast)))])
		  (emit-indentation)
		  (emit "scheme_temp_dec_mark_depth();~n")
		  (emit-indentation)
		  (emit "~a.val = " var)
		  (process (vm:wcm-remember!-val ast) indent-level #f #t)
		  (emit ";~n")
		  (emit-indentation)
		  (emit "scheme_temp_inc_mark_depth()"))]
	       [(vm:wcm-extract? ast)
		(let ([var (vm->c:convert-symbol
			    (vm:local-varref-var (vm:wcm-extract-var ast)))])
		  (emit "~a.val" var))]
	       
	       ;; (continue) -> continue;
	       [(vm:continue? ast)
		(unless (compiler:option:disable-interrupts)
		  (emit-expr "SCHEME_USE_FUEL(1);~n"))
		(emit-expr "continue")]
	       
	       ;; use NULL instead of tail_buf if no args
	       ;; (tail-apply A <argc>) -> return _scheme_tail_apply(A, argc);
	       [(vm:tail-apply? ast)
		(emit-expr "return _scheme_tail_apply_no_copy_wp(")
		(process (vm:tail-apply-closure ast) indent-level #f #t)
		(let ([c (vm:tail-apply-argc ast)])
		  (emit ", ~a, ~a, pr)" c (if (zero? c) "NULL" 'tail_buf)))]
	       
	       ;; (tail-call <label> <closure>) -> void_param = SCHEME_CLSD_PRIM_DATA(<closure>);
	       ;;                                  goto LOC<label>;
	       [(vm:tail-call? ast)
		(when (vm:tail-call-set-env? ast)
		  (emit-indentation)
		  (emit "void_param = SCHEME_CLSD_PRIM_DATA(")
		  (process (vm:tail-call-closure ast) indent-level #f #t)
		  (emit ");~n"))
		;; be nice to threads & user breaks:
		(unless (compiler:option:disable-interrupts)
		  (emit-indentation)
		  (emit "SCHEME_USE_FUEL(1);~n"))
		(emit-indentation)
					; unless its to a variable arity function! ARGH
		(let* ([label (vm:tail-call-label ast)]
		       [l (if (number? label)
			      label
			      (format "~ac~a" (car label) (cdr label)))])
		  (emit "goto LOC~a" l))]

	       ;; (return R) -> return R
	       [(vm:return? ast)
		(emit-indentation)
		(emit "return ")
		(process (vm:return-val ast) indent-level #f #t)]

	       ;; fortunately, void contexts can accept any number of values,
	       ;; so there's no need to check for return arity
	       [(vm:void? ast)
		(emit-indentation)
		(process (vm:void-val ast) indent-level #f #t)]
	       
	       ;; (global-varref x) --> GLOBAL_VARREF(x)
	       [(vm:global-varref? ast)
		(emit-expr "GLOBAL_VARREF(G~a)"
			   (vm->c:convert-symbol
			    (mod-glob-cname (vm:global-varref-var ast))))]

	       ;; (global-varref x) --> Gx
	       [(vm:bucket? ast)
		(emit-expr "G~a"
			   (vm->c:convert-symbol
			    (mod-glob-cname (vm:bucket-var ast))))]

	       [(vm:per-load-statics-table? ast)
		(emit-expr "PLS")]

	       [(vm:per-invoke-statics-table? ast)
		(emit-expr "PMIS")]

	       ;; use apply-known? flag
	       ;; 0 args => pass NULL for arg vector
	       ;; (apply A <argc>) --> _scheme_apply(A, argc, arg)
	       [(vm:apply? ast)
		(emit-expr "")
		(when (vm:apply-simple-tail-prim? ast)
		  (emit "return "))
		(emit "_scheme_~a("
		      (let ([v (global-defined-value* (vm:apply-prim ast))])
			(cond
			 [(and (primitive-closure? v) 
			       (not (memq (object-name v) (internal-tail-chain-prims))))
			  (if (or (vm:apply-multi? ast)
				  (primitive-result-arity v))
			      "direct_apply_closed_primitive_multi"
			      "direct_apply_closed_primitive")]
			 [(and (primitive? v)
			       (not (memq (object-name v) (internal-tail-chain-prims))))
			  (if (or (vm:apply-multi? ast)
				  (primitive-result-arity v))
			      "direct_apply_primitive_multi"
			      "direct_apply_primitive")]
			 [(vm:apply-known? ast) 
			  (if (vm:apply-multi? ast)
			      (if (compiler:option:disable-interrupts)
				  "direct_apply_closed_primitive_multi_fv"
				  "apply_known_closed_prim_multi")
			      (if (compiler:option:disable-interrupts)
				  (if (compiler:option:unsafe)
				      "direct_apply_closed_primitive_multi_fv"
				      "direct_apply_closed_primitive_fv")
				  "apply_known_closed_prim"))]
			 [(vm:apply-multi? ast) "apply_multi"]
			 [else "apply"])))
		(process (vm:apply-closure ast) indent-level #f #t)
		(let ([c (vm:apply-argc ast)])
		  (emit ", ~a, ~a)" c (if (zero? c) "NULL" 'arg)))]

	       ;; Inlined macro-based applications
	       [(vm:macro-apply? ast) 
		(emit-expr "")
		(when (vm:macro-apply-tail? ast)
		  (emit "return "))
		(when (vm:macro-apply-bool? ast) (emit "(("))
		(emit-macro-application ast)
		(when (vm:macro-apply-bool? ast) (emit ") ? scheme_true : scheme_false)"))]
	       
	       [(vm:call? ast)
		(emit-expr "_scheme_force_value(compiled(SCHEME_CLSD_PRIM_DATA(")
		(process (vm:call-closure ast) indent-level #f #t)
		(emit "), 0, arg))")]

	       ;; (bound-varref x) -> x
	       [(vm:local-varref? ast) 
		(emit-expr (vm->c:convert-symbol
			    (vm:local-varref-var ast)))]
	       
	       ;; (primitive-varref x) -> x->val
	       [(vm:primitive-varref? ast)
		(emit-expr "P.~a"
			   (vm->c:convert-symbol (mod-glob-cname (vm:primitive-varref-var ast))))]
	       
	       ;; (symbol-varref x) -> symbols[x]
	       [(vm:symbol-varref? ast)
		(emit-expr "~a[~a]" 
			   (vm->c:SYMBOLS-name)
			   (vm:symbol-varref-var ast))]

	       ;; (inexact-varref x) -> inexacts[x]
	       [(vm:inexact-varref? ast)
		(emit-expr "~a[~a]" 
			   (vm->c:INEXACTS-name)
			   (vm:inexact-varref-var ast))]

	       [(vm:per-load-static-varref? ast)
		(emit-expr "PLS->~a" (vm->c:convert-symbol (vm:static-varref-var ast)))]
	       
	       [(vm:per-invoke-static-varref? ast)
		(emit-expr "PMIS->~a" (vm->c:convert-symbol (vm:static-varref-var ast)))]
	       
	       [(vm:static-varref? ast)
		(emit-expr "S.~a" (vm->c:convert-symbol (vm:static-varref-var ast)))]
	       
	       ;; (immediate x)
	       [(vm:immediate? ast)
		(let ([tast (vm:immediate-text ast)])
		  (cond 
		   
		   ;;--------------------------------------------------------------
		   ;; CONSTANTS
		   ;;
		   ;; labels
		   [(number? tast)
		    (emit-expr "~a" tast)]
		   
		   [(boolean? (zodiac:zread-object tast))
		    (if (zodiac:zread-object tast)
			(emit-expr "scheme_true")
			(emit-expr "scheme_false"))]
		   
		   [(number? (zodiac:zread-object tast))
		    (emit-expr "scheme_make_integer(~a)" (zodiac:zread-object tast))]
		   
		   [(char? (zodiac:zread-object tast))
		    (emit-expr "scheme_make_character(~a)"
			       (vm->c:convert-char
				(zodiac:zread-object tast)))]
		   
		   [(null? (zodiac:zread-object tast))
		    (emit-expr "scheme_null")]
		   
		   [(eq? (zodiac:zread-object tast) self_modidx)
		    (emit-expr "self_modidx")]
		   
		   [(or (void? (zodiac:zread-object tast))
			(undefined? (zodiac:zread-object tast)))
		    (emit-expr (vm->c:convert-special-constant tast))]
		   
		   [else (compiler:internal-error
			  ast
			  (format "vm->c-expression: ~a not an immediate: ~e" 
				  tast (zodiac:zread-object tast)))]))]
	       
	       [(vm:build-constant? ast)
		(let ([ast (vm:build-constant-text ast)])
		  (cond
		   [(string? (zodiac:zread-object ast))
		    (fprintf port "scheme_make_immutable_sized_char_string((mzchar *)STRING_~a, ~a, 0)" 
			     (const:intern-string (zodiac:zread-object ast))
			     (string-length (zodiac:zread-object ast)))]
		   [(bytes? (zodiac:zread-object ast))
		    (fprintf port "scheme_make_immutable_sized_byte_string((char *)STRING_~a, ~a, 0)" 
			     (const:intern-string (zodiac:zread-object ast))
			     (bytes-length (zodiac:zread-object ast)))]
		   [(symbol? (zodiac:zread-object ast))
		    (let ([s (symbol->string (zodiac:zread-object ast))])
		      (emit-expr "scheme_intern_exact_symbol(~s, ~a)" s (string-length s)))]
		   [(number? (zodiac:zread-object ast))
		    (let process ([num (zodiac:zread-object ast)])
		      (cond
		       ;; NaN, inf
		       [(member num  (list +NaN.0 +inf.0 -inf.0 -0.0)) 
			(emit-expr "scheme_eval_string(\"~a\", env)" num)]
		       ;; complex numbers
		       [(not (eqv? 0 (imag-part num)))
			(emit-expr "scheme_make_complex(")
			(process (real-part num))
			(emit ", ")
			(process (imag-part num))
			(emit ")")]
		       ;; floating point numbers
		       [(inexact? num)
			(emit-expr "scheme_make_double(~a)" num)]
		       ;; integers (fixnums & bignums)
		       [(integer? num)
			(if (vm:fixnum? num) 
			    (emit-expr "scheme_make_integer(~a)" num)
			    (emit-expr "scheme_read_bignum_bytes(\"~a\", 0, 10)" num))]
					; rational numbers
		       [else
			(emit-expr "scheme_make_rational(")
			(process (numerator num))
			(emit ", ")
			(process (denominator num))
			(emit ")")]))]

		   [(void? (zodiac:zread-object ast))
		    (emit "scheme_void")]

		   [(eq? (zodiac:zread-object ast) self_modidx)
		    (emit-expr "self_modidx")]
		   
		   ;; HACK! - abused constants to communicate
		   ;;  a direct call to scheme_make_prim_w_arity
		   [(c-lambda? (zodiac:zread-object ast))
		    (let ([cl (zodiac:zread-object ast)])
		      (emit-expr "scheme_make_prim_w_arity(~a, ~s, ~a, ~a)"
				 (c-lambda-function-name cl)
				 (symbol->string (c-lambda-scheme-name cl))
				 (c-lambda-arity cl)
				 (c-lambda-arity cl)))]
		   
		   ;; HACK! - abused constants to communicate
		   ;;  a direct call to scheme_read_compiled_stx_string():
		   [(syntax-string? (zodiac:zread-object ast))
		    (let ([id (syntax-string-id (zodiac:zread-object ast))]
			  [for-mod? (syntax-string-mi (zodiac:zread-object ast))])
		      (emit-expr "scheme_eval_compiled_stx_string(SS[~a], SCHEME_CURRENT_ENV(pr), ~a, ~a)"
				 id
				 (if for-mod? "phase_shift" "0")
				 (if for-mod? "self_modidx" "NULL")))]
		   
		   ;; HACK! - abused constants to communicate
		   ;;  a direct call to scheme_eval_compiled_string():
		   [(compiled-string? (zodiac:zread-object ast))
		    (let ([cs (zodiac:zread-object ast)])
		      (emit-expr "scheme_eval_compiled_sized_string(STRING_~a, ~a, NULL)"
				 (compiled-string-id cs)
				 (compiled-string-len cs)))]
		   
		   [else (compiler:internal-error
			  ast
			  (format "vm:build-constant: not supported ~a" ast))]))]
	       
	       [else (compiler:internal-error #f (format "vm2c: ~a not supported" ast))]))))))))
