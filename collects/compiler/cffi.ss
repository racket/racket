
;; Implements the c-lambda and c-declare forms for interfacing to C.
;; The macros communicate information to mzc using a 'mzc-cffi
;; property. When this property information is ignored, the result is
;; an expression that (eventually) produces an error. The mzc compiler
;; notices the information, however, and substitutes a different kind
;; of expression.

(module cffi mzscheme
  (require (lib "stx.ss" "syntax"))
  (require-for-syntax (lib "name.ss" "syntax")
                      (lib "path-spec.ss" "syntax"))

  (define-syntax c-lambda
    (let ([re:fname (regexp "^[a-zA-Z_0-9]+$")]
	  [parse-type
	   (lambda (t stx for-return?)
	     (let ([literals (syntax-e
			      (quote-syntax 
			       (bool
				char unsigned-char signed-char 
				int unsigned-int
				short unsigned-short
				long unsigned-long
				float double
				scheme-object
				char-string nonnull-char-string)))])
	       (let ([v (and (identifier? t)
			     (ormap (lambda (i) 
				      (and (module-identifier=? i t) i))
				    literals))])
		 (cond
		  [v (syntax-e v)]
		  [(and for-return? 
			;; FIXME: void is not lexically scoped
			(eq? (syntax-e t) 'void))
		   'void]
		  [(let ([l (syntax->list t)])
		     (and l (= 2 (length l))
			  (identifier? (car l))
			  (string? (syntax-e (cadr l)))
			  (module-identifier=? (car l) (quote-syntax pointer))
			  l))
		   => (lambda (l)
			`(pointer , (syntax-e (cadr l))))]
		  [else
		   (raise-syntax-error
		    'c-lambda
		    "bad type"
		    stx
		    t)]))))]
	  [make-declaration (lambda (type name)
			      (if (pair? type)
				  (format "  ~a* ~a;\n"
					  (cadr type)
					  name)
				  (format "  ~a ~a;\n"
					  (cadr (assq type
						      '((bool "int")
							(char "char")
							(unsigned-char "unsigned char")
							(signed-char "signed char") 
							(int "int")
							(unsigned-int "unsigned int")
							(short "short")
							(unsigned-short "unsigned short")
							(long "long")
							(unsigned-long "unsigned long")
							(float "float")
							(double "double")
							(scheme-object "Scheme_Object*")
							(char-string "char*")
							(nonnull-char-string "char*"))))
					  name)))]
	  [extract-c-value (lambda (type c-var scheme-var pos proc-name)
			     (cond
			      [(eq? type 'bool) 
			       (format "  ~a = SCHEME_TRUEP(~a);\n" c-var scheme-var)]
			      [(eq? type 'scheme-object) 
			       (format "  ~a = ~a;\n" c-var scheme-var)]
			      [else
			       (let-values ([(setup tester unwrapper type-name done)
					     (if (pair? type)
						 (values #f
							 (format "(SCHEME_FALSEP(~~a) ~
                                                                  || (SCHEME_CPTRP(~a) && SCHEME_BYTE_STRINGP(SCHEME_CPTR_TYPE(~a)) ~
                                                                      && !strcmp(SCHEME_BYTE_STR_VAL(SCHEME_CPTR_TYPE(~a)), ~s)))"
								 scheme-var scheme-var scheme-var (cadr type))
							 (format "(SCHEME_TRUEP(~~a) ? SCHEME_CPTR_VAL(~a) : NULL)"
								 scheme-var)
							 (format "cpointer of type ~s or #f"
								 (cadr type))
							 #f)
						 (case type
						   [(char unsigned-char signed-char) 
						    (values #f
							    "SCHEME_CHARP(~a)"
							    "SCHEME_CHAR_VAL(~a)"
							    "character"
							    #f)]
						   [(int long) 
						    (values " { long tmp;\n"
							    "scheme_get_int_val(~a, &tmp)"
							    "tmp  /* ~a */"
							    (format "exact integer between (expt 2 -31) ~
                                                                     and (sub1 (expr 2 31)) inclusive")
							    " }\n")]
						   [(unsigned-int unsigned-long)
						    (values " { unsigned long tmp;\n"
							    "scheme_get_unsigned_int_val(~a, &tmp)"
							    "tmp /* ~a */"
							    "exact integer between 0 and (sub1 (expr 2 32)) inclusive"
							    " }\n")]
						   [(short)
						    (values #f
							    "(SCHEME_INTP(~a) \
                                                             && (long)((short)SCHEME_INT_VAL(~a)) == SCHEME_iNT_VAL(~a))"
							    "SCHEME_INT_VAL(~a)"
							    (format "exact integer between (expt 2 -15) ~
                                                                     and (sub1 (expr 2 15)) inclusive")
							    #f)]
						   [(unsigned-short)
						    (values #f
							    "(SCHEME_INTP(~a) ~
                                                              && (long)((unsigned short)SCHEME_INT_VAL(~a)) ~
                                                                        == SCHEME_iNT_VAL(~a))"
							    "SCHEME_INT_VAL(~a)"
							    "exact integer between 0 and (sub1 (expr 2 16)) inclusive"
							    #f)]
						   [(float double)
						    (values #f
							    "SCHEME_REALP(~a)"
							    "scheme_real_to_double(~a)"
							    "real number"
							    #f)]
						   [(char-string)
						    (values #f
							    (format "SCHEME_FALSEP(~~a) || SCHEME_BYTE_STRINGP(~a)"
								    scheme-var)
							    (format "(SCHEME_FALSEP(~~a) ? NULL : SCHEME_BYTE_STR_VAL(~a))" 
								    scheme-var)
							    "byte string or #f"
							    #f)]
						   [(nonnull-char-string)
						    (values #f
							    "SCHEME_BYTE_STRINGP(~a)"
							    "SCHEME_BYTE_STR_VAL(~a)" 
							    "byte string"
							    #f)]
						   [else (error 'cffi "bad type for arg: ~a" type)]))])
				 (string-append
				  (or setup "")
				  (format "  if (~a) {\n" (format tester scheme-var))
				  (format "    ~a = ~a;\n" c-var (format unwrapper scheme-var))
				  (format "  } else {\n")
				  (format "    scheme_wrong_type(~s, ~s, ~a, argc, argv);\n    return NULL;\n"
					  (symbol->string proc-name) type-name pos)
				  (format "  }\n")
				  (or done "")))]))]

	  [build-scheme-value (lambda (type scheme-var c-var)
				(let ([builder
				       (if (pair? type)
					   (format "(~a ? scheme_make_cptr(~~a, scheme_make_byte_string(~s)) : scheme_false)" 
						   c-var
						   (cadr type))
					   (case type
					     [(bool) "(~a ? scheme_true : scheme_false)"]
					     [(char unsigned-char signed-char) 
					      "scheme_make_character((unsigned char)~a)"]
					     [(int long) 
					      "scheme_make_integer_value(~a)"]
					     [(unsigned-int unsigned-long)
					      "scheme_make_integer_value_from_unsigned(~a)"]
					     [(char-string)
					      (format "(~~a ? scheme_make_byte_string(~a) : scheme_false)"
						      c-var)]
					     [(float double)
					      "scheme_make_double(~a)"]
					     [(nonnull-char-string)
					      "scheme_make_byte_string(~a)"]
					     [(scheme-object) "~a"]
					     [else (error 'cffi "bad type for result: ~a" type)]))])
				  (format "  ~a = ~a;\n"
					  scheme-var
					  (format builder c-var))))]
	  [ffi-index 0])

      (lambda (stx)
	(syntax-case stx ()
	  [(_ (arg-type ...) result-type code)
	   (let ([arg-types (map
			     (lambda (t)
			       (parse-type t stx #f))
			     (syntax->list (syntax (arg-type ...))))]
		 [result-type (parse-type (syntax result-type) stx #t)]
		 [code (syntax code)]
		 [proc-name (or (let ([s (syntax-local-infer-name stx)])
				  (if (syntax? s)
				      (syntax-e s)
				      s))
				'c-lambda-procedure)])
	     (unless (string? (syntax-e code))
	       (raise-syntax-error
		'c-lambda
		"not a code or function-name string"
		stx
		code))
	     
	     ;; Generate the function body
	     (let ([fname (format "mzc_cffi_~a" ffi-index)]
		   [code (apply
			  string-append
			  (append
			   (let loop ([n 1][arg-types arg-types])
			     (if (null? arg-types)
				 null
				 (cons
				  (make-declaration (car arg-types) (format "___arg~a" n))
				  (loop (add1 n) (cdr arg-types)))))
			   (if (eq? 'void result-type)
			       null
			       (list (make-declaration result-type "___result")
				     "  Scheme_Object *converted_result;\n"))
			   (let loop ([n 1][arg-types arg-types])
			     (if (null? arg-types)
				 null
				 (cons
				  (extract-c-value (car arg-types)
						   (format "___arg~a" n) 
						   (format "argv[~a]" (sub1 n))
						   (sub1 n)
						   proc-name)
				  (loop (add1 n) (cdr arg-types)))))
			   (list " {\n")
			   (list
			    (if (regexp-match re:fname (syntax-e code))
				;; Generate function call
				(string-append
				 (if (eq? result-type 'void)
				     "  "
				     "  ___result = ")
				 (syntax-e code)
				 "("
				 (let loop ([n 1][arg-types arg-types])
				   (if (null? arg-types)
				       ""
				       (string-append
					(format "___arg~a~a" 
						n
						(if (pair? (cdr arg-types))
						    ", "
						    ""))
					(loop (add1 n) (cdr arg-types)))))
				 ");\n")
				;; Use literal code
				(string-append (syntax-e code) "\n")))
			   (if (eq? result-type 'void)
			       null
			       (list
				(build-scheme-value result-type "converted_result" "___result")))
			   (list
			    "#ifdef ___AT_END\n"
			    "  ___AT_END\n"
			    "#undef ___AT_END\n"
			    "#endif\n")
			   (list " }\n")
			   (list
			    (if (eq? result-type 'void)
				"  return scheme_void;\n"
				"  return converted_result;\n"))))])
	       (set! ffi-index (add1 ffi-index))
	       (with-syntax ([fname fname]
			     [code code]
			     [arity (length arg-types)]
			     [proc-name proc-name]
			     [args (generate-temporaries arg-types)])
		 (let ([stx-out (syntax
				 (lambda args
				   (error 'proc-name "c-lambda expression not compiled by mzc")
				   '(fname
				     proc-name
				     arity
				     code)))])
		   (syntax-property stx-out 'mzc-cffi 'c-lambda)))))]))))
  
  (define-syntax (c-declare stx)
    (unless (memq (syntax-local-context) '(top-level module))
      (raise-syntax-error #f "only allowed at the top-level or a module top-level" stx))
    (syntax-case stx ()
      [(_ str)
       (let ([decl (syntax str)])
	 (unless (string? (syntax-e decl))
	   (raise-syntax-error 'c-declare "declaration is not a string" stx decl))
	 (let ([stx-out (syntax
			 (error 'c-declare 
				"declaration not compiled by mzc: ~e"
				str))])
	   (syntax-property stx-out 'mzc-cffi 'c-declare)))]))


  (define-syntax (c-include stx)
    (unless (memq (syntax-local-context) '(top-level module))
      (raise-syntax-error #f "only allowed at the top-level or a module top-level" stx))
    (syntax-case stx () 
      [(_ path)
       (let ([pathname (resolve-path-spec #'path #'path stx #'build-path)])
         (let ([str
                (with-handlers ([exn:fail?
                                 (lambda (x)
                                   (raise-syntax-error
                                    #f
                                    (format "error reading file ~e: ~a"
                                            pathname
                                            (if (exn? x)
                                                (exn-message x)
                                                x))))])
                  (with-input-from-file pathname
                    (lambda () (read-string (file-size pathname)))))])
           (quasisyntax/loc stx (c-declare #,str))))]))
      
  (provide c-lambda
	   c-declare
           c-include))
