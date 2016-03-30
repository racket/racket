(module xform racket/base
  (require racket/list
  	   (for-syntax racket/base)
	   racket/system)
  
  (provide xform)
  
  (define (xform quiet?
		 cpp
		 file-in
		 file-out
                 keep-lines?
		 palm? pgc? pgc-really?
		 precompiling-header? precompiled-header
		 show-info? output-depends-info?
		 gc-variable-stack-through-funcs?)
    (parameterize ([current-output-port (current-output-port)] ; because we mutate these...
                   [error-escape-handler (error-escape-handler)]
                   [current-inspector (current-inspector)])
      (let ()
        (define power-inspector (current-inspector))
        (current-inspector (make-inspector))
        
        (define check-arith? #t)
        
        ;; Selects whether to reset GC_variable_stack on return,
        ;; or to just reset it on every call.
        (define callee-restore? #t)
        
        (define palm-out #f)
        
        (define (filter-false s)
          (if (equal? s "-")
              #f
              s))
        
        (define source-is-c++? (regexp-match #rx"([.]cc$)|([.]cxx$)" file-in))
        
        (define (change-suffix filename new)
          (path-replace-suffix filename new))
        
        ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        ;; "AST" structures
        ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        
        (define-struct tok (n line file) #:inspector (make-inspector))
        (define-struct (sysheader-tok tok) ())
        (define-struct (seq tok) (close in) #:inspector (make-inspector))
        (define-struct (parens seq) () #:inspector (make-inspector))
        (define-struct (brackets seq) ())
        (define-struct (braces seq) ())
        (define-struct (callstage-parens parens) ())
        (define-struct (creation-parens parens) ())
        (define-struct (nosrc-parens parens) ())
        (define-struct (call tok) (func args live tag nonempty?)) ;; a converted function call
        (define-struct (block-push tok) (vars tag super-tag top?))
        (define-struct (note tok) (s))
        
        (define-struct (nested-setup tok) ())
        (define nested-pushable (make-nested-setup 'nested #f #f))
        (define no-nested-pushable (make-nested-setup 'no-nested #f #f))
        (define undefine-nested-pushable (make-nested-setup 'undefine #f #f))
        
        (define-struct pragma (s file line))
        
        ;; For very long lists, it's worth the effort to use a vector instead
        ;;   of a list to save space:
        (define (seq->list s) (if (vector? s) (vector->list s) s))
        (define (list->seq s) (if (or (null? s) (null? (cdr s)) (null? (cddr s)))
                                  s
                                  (list->vector s)))
        (define seqce vector)
        
        ;; A cheap way of getting rid of unneeded prototypes:
        (define used-symbols (make-hasheq))
        (hash-set! used-symbols (string->symbol "GC_variable_stack") 1)
        (hash-set! used-symbols (string->symbol "GC_cpp_delete") 1)
	(hash-set! used-symbols (string->symbol "GC_get_variable_stack") 1)
	(hash-set! used-symbols (string->symbol "GC_set_variable_stack") 1)
        (hash-set! used-symbols (string->symbol "memset") 1)
	(hash-set! used-symbols (string->symbol "scheme_thread_local_key") 1)
	(hash-set! used-symbols (string->symbol "scheme_thread_locals") 1)
	(hash-set! used-symbols (string->symbol "pthread_getspecific") 1)
	(hash-set! used-symbols (string->symbol "scheme_get_mz_setjmp") 1)
        
        ;; For dependency tracking:
        (define depends-files (make-hash))
        
        (define (make-triple v src line sysheader?)
          (when (symbol? v)
            (hash-set! used-symbols v
                             (add1 (hash-ref
                                    used-symbols
                                    v
                                    (lambda () 0)))))
          (when (and src output-depends-info?)
            (hash-set! depends-files src #t))
          (if sysheader?
              (make-sysheader-tok v line src)
              (make-tok v line src)))
        
        (define (make-a-seq opener src line body)
          ((case opener
             [(#\() make-parens]
             [(#\[) make-brackets]
             [(#\{) make-braces])
           (case opener
             [(#\() "("]
             [(#\[) "["]
             [(#\{) "{"])
           line
           src
           (case opener
             [(#\() ")"]
             [(#\[) "]"]
             [(#\{) "}"])
           (list->seq body)))
        
        ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        ;;  Tokenizer
        ;;   Relies on make-triple, make-a-seq, and make-pragma
        ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        
        (define (trans pattern)
          (byte-regexp (string->bytes/utf-8 (format "^(~a)" pattern))))
        
        (define (translations . t)
          (let loop ([t t])
            (if (null? t)
                null
                (let ([pattern (car t)]
                      [result (cadr t)])
                  (cons (cons (trans pattern)
                              result)
                        (loop (cddr t)))))))
        
        (define (a-regexp-match-positions re s p)
          (regexp-match-positions re s p))
        
        (define seqs string-append)
        (define startseq seqs)
        (define (arbno s) (format "(?:~a)*" s))
        (define (arbno/ s) (format "~a*" s))
        (define (one+ s) (format "(?:~a)+" s))
        (define (one+/ s) (format "~a+" s))
        (define (maybe s) (format "(?:~a)?" s))
        (define (maybe/ s) (format "~a?" s))
        (define (alt a b) (format "~a|~a" a b))
        (define (alt* . l)
          (let loop ([l l])
            (if (null? (cdr l))
                (format "(?:~a)" (car l))
                (format "(?:~a)|~a" (car l) (loop (cdr l))))))
        
        ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        
        (define (line-comment s p)
          (let loop ([p (add1 p)])
            (let ([c (bytes-ref s p)])
              (if (or (equal? c 10)
                      (equal? c 13))
                  (add1 p)
                  (loop (add1 p))))))
        
        (define re:line #rx#"^#[^\n\r]* ([0-9]+) \"([^\"]*)\"([^\r\n]*)" )
        (define re:pragma #rx#"^#pragma ([^\r\n]*)")
        
        (define (do-cpp s p)
          (let ([m (regexp-match re:line s p)])
            (when m
              (set! source-line (string->number (bytes->string/utf-8 (cadr m))))
              (set! source-file (caddr m))
              (set! source-sysheader? (regexp-match? #px#"\\b3\\b" (cadddr m)))))
          (let ([pragma (regexp-match re:pragma s p)])
            (if (and pragma
                     (not (regexp-match-positions re:boring (car pragma))))
                (values (make-pragma (cadr pragma) source-file source-line) (line-comment s p))
                (values #f (line-comment s p)))))
        
        (define (result s)
          (make-triple
           s
           source-file
           source-line
           source-sysheader?))
        
        (define (symbol s)
          (result (string->symbol (bytes->string/utf-8 s))))
        
        (define re:octal #rx#"^0[0-9]+$")
        (define re:int #rx#"^[0-9]*$")
        (define (number s)
          (result
           (cond
             [(regexp-match-positions re:octal s) 
              (string->number (bytes->string/utf-8 s) 8)]
             [(regexp-match-positions re:int s)
              (string->number (bytes->string/utf-8 s))]
             [else (string->symbol (bytes->string/utf-8 s))])))
        
        (define (character s)
          (count-newlines s)
          (symbol s))

	(define (character? s)
	  (and (symbol? s)
	       (regexp-match #rx"'[\\]?.+'" (symbol->string s))))
        
        (define (mk-string s)
          (count-newlines s)
          (result (bytes->string/utf-8 (subbytes s 1 (sub1 (bytes-length s))))))
        
        (define (start s)
          'start)
        
        (define (stop s)
          #f)
        
        (define (count-newlines s)
          (let loop ([p (sub1 (bytes-length s))])
            (unless (= p -1)
              (when (= 10 (bytes-ref s p))
                (set! source-line (add1 source-line)))
              (loop (sub1 p)))))
        
        ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        
        (define D "[0-9]")
        (define L "[a-zA-Z_]")
        (define H "[a-fA-F0-9]")
        (define E (format "[Ee][+-]?~a+" D))
        (define FS "(?:f|F|l|L)")
        (define IS "(?:u|U|l|L)*")
        
        (define symbol-complex (trans (seqs L (arbno (alt L D)))))

        ;; Accomodate things like 10_1 in `availability` attributes:
        (define pseudo-symbol-complex (trans (seqs (arbno D) "_" (arbno D))))
        
        (define number-complex
          (trans (alt*
                  (seqs (arbno/ D) "[.]" (one+/ D) (maybe E) (maybe/ FS))
                  (seqs (one+/ D) "[.]" (arbno D) (maybe E) (maybe/ FS))
                  (seqs (one+/ D) E (maybe/ FS))
                  
                  "0x1[.]0p2047" ;; strange thing in huge_val.h
                  
                  (seqs "0" "[xX]" (one+/ H) IS) ;; hex
                  (seqs "0" (one+/ D) IS) ;; octal
                  (seqs (one+/ D) IS))))  ;; integer
        
        (define char-complex (trans "'([^\\']|\\\\.)+'"))
        (define string-complex (trans "\"([^\\\"]|\\\\.)*\""))
        
        (define simple-table (make-vector 256 #f))
        
        (define (simple-translations . l)
          (let loop ([l l])
            (unless (null? l)
              (loop (cddr l))
              (let* ([pattern (car l)]
                     [result (cadr l)]
                     [n (bytes-ref pattern 0)])
                (vector-set! simple-table
                             n
                             (cons
                              (list* pattern (bytes-length pattern) 
                                     result)
                              (or
                               (vector-ref simple-table n)
                               null)))))))
        
        (simple-translations
         #"#" symbol
         #"##" symbol
         #"..." symbol
         #">>=" symbol
         #"<<=" symbol
         #"+=" symbol
         #"-=" symbol
         #"*=" symbol
         #"/=" symbol
         #"%=" symbol
         #"&=" symbol
         #"^=" symbol
         #"|=" symbol
         #">>" symbol
         #"<<" symbol
         #"++" symbol
         #"--" symbol
         #"->" symbol
         #"&&" symbol
         #"||" symbol
         #"<=" symbol
         #">=" symbol
         #"==" symbol
         #"!=" symbol
         #";" symbol
         #"{" start
         #"}" stop
         #"," symbol
         #"::" symbol
         #":" symbol
         #"=" symbol
         #"(" start
         #")" stop
         #"[" start
         #"]" stop
         #"." #f ; => symbol/num
         #"&" symbol
         #"!" symbol
         #"~" symbol
         #"-" #f ; => symbol/num
         #"+" #f ; => symbol/num
         #"*" symbol
         #"/" symbol
         #"%" symbol
         #"<" symbol
         #">" symbol
         #"^" symbol
         #"|" symbol
         #"?" symbol)
        
        ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        
        (define source-file #f)
        (define source-line 0)
        (define source-sysheader? #f)
        
        ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        
        (define (read-all p)
          (let loop ([l null])
            (let ([s (read-bytes 4096 p)])
              (if (eof-object? s)
                  (apply bytes-append (reverse l))
                  (loop (cons s l))))))
        
        (define (tokenize)
          (let* ([s (read-all (current-input-port))]
                 [len (bytes-length s)])
            (let loop ([p 0][result null])
              (if (= p len)
                  (cons (reverse result) p)
                  (let ([char (bytes-ref s p)])
                    (when (eq? char 10)
                      (set! source-line (add1 source-line)))
                    (cond
                      [(char-whitespace? (integer->char char))
                       (loop (add1 p) result)]
                      [(eq? char (char->integer #\#)) ;; We assume only #-based preprocessor left
                       (let-values ([(pragma p) (do-cpp s p)])
                         (if pragma
                             (loop p (cons pragma result))
                             (loop p result)))]
                      [else
                       (let ([simple (let ([sl (vector-ref simple-table char)])
                                       (and sl
                                            (ormap 
                                             (lambda (t)
                                               (and (or (= 1 (cadr t))
                                                        (bytes=? (car t) (subbytes s p (+ p (cadr t)))))
                                                    (let ([f (cddr t)])
                                                      (if f
                                                          (cons (f (car t))
                                                                (+ p (cadr t)))
                                                          (let ([m (regexp-match-positions number-complex s p)])
                                                            (if m
                                                                (cons (number (subbytes s (caar m) (cdar m)))
                                                                      (cdar m))
                                                                (cons (symbol (car t))
                                                                      (+ p (cadr t)))))))))
                                             sl)))])
                         (cond
                           [(not simple)
                            (cond
                              [(regexp-match-positions symbol-complex s p)
                               => (lambda (m)
                                    (loop (cdar m)
                                          (cons (symbol (subbytes s (caar m) (cdar m)))
                                                result)))]
                              [(regexp-match-positions pseudo-symbol-complex s p)
                               => (lambda (m)
                                    (loop (cdar m)
                                          (cons (symbol (subbytes s (caar m) (cdar m)))
                                                result)))]
                              [(regexp-match-positions number-complex s p)
                               => (lambda (m)
                                    (loop (cdar m)
                                          (cons (number (subbytes s (caar m) (cdar m)))
                                                result)))]
                              [(regexp-match-positions char-complex s p)
                               => (lambda (m)
                                    (loop (cdar m)
                                          (cons (character (subbytes s (caar m) (cdar m)))
                                                result)))]
                              [(regexp-match-positions string-complex s p)
                               => (lambda (m)
                                    (loop (cdar m)
                                          (cons (mk-string (subbytes s (caar m) (cdar m)))
                                                result)))]
                              [else
                               (error 'c-tokenize "strange: ~e ~e" p (subbytes s p (min len (+ p 100))))])]
                           [(not (car simple))
                            (cons (reverse result) (cdr simple))]
                           [(eq? (car simple) 'start)
                            (let ([sf source-file]
                                  [sl source-line]
                                  [sub (loop (cdr simple) null)])
                              (loop (cdr sub) (cons (make-a-seq
                                                     (integer->char (bytes-ref s p))
                                                     sf
                                                     sl
                                                     (car sub))
                                                    result)))]
                           [simple
                            (loop (cdr simple) (cons (car simple) result))]))]))))))
        
        ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        ;; Pre-process and S-expr-ize
        ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        
	(define (verbose f)
	  (if quiet?
	      f
	      (lambda args
		(printf "xform-cpp: ~a\n" args)
		(apply f args))))

	(define (maybe-add-exe p)
	  (cond
	   [(and (eq? 'windows (system-type))
		 (not (regexp-match? #rx"[.]exe$" p)))
	    (format "~a.exe" p)]
	   [else p]))

        ;; To run cpp:
        (define process2
          (if (eq? (system-type) 'windows)
              (lambda (s)
                (let ([split (let loop ([s s])
                               (let ([m (regexp-match #rx"([^ ]*) +(.*)" s)])
                                 (if m
                                     (cons (cadr m) (loop (caddr m)))
                                     (list s))))])
                  (apply (verbose process*) (find-executable-path (maybe-add-exe (car split)) #f)
                         (cdr split))))
              (verbose process)))
        
        (define cpp-process
	  (if (string? cpp)
	      (process2 (format "~a~a~a ~a"
				cpp
				(if pgc?
				    (if pgc-really? 
					" -DMZ_XFORM -DMZ_PRECISE_GC"
					" -DMZ_XFORM")
				    "")
				(if callee-restore? " -DGC_STACK_CALLEE_RESTORE" "")
				file-in))
	      (apply (verbose process*)
		     (append
		      cpp
		      (if pgc-really? 
			  '("-DMZ_XFORM" "-DMZ_PRECISE_GC")
			  '("-DMZ_XFORM"))
		      (if callee-restore? 
			  '("-DGC_STACK_CALLEE_RESTORE")
			  null)
		      (list file-in)))))
        (close-output-port (cadr cpp-process))
        
        (define (mk-error-thread proc)
          (thread (lambda ()
                    (let loop ()
                      (let ([l (read-bytes-line (list-ref proc 3) 'any)])
                        (unless (eof-object? l)
                          (eprintf "~a\n" l)
                          (loop))))
                    (close-input-port (list-ref proc 3)))))
        
        (define cpp-error-thread (mk-error-thread cpp-process))
        
        ;; Pipe cpp results through here; we insert a filter
        ;; between the pipe ends.
        (define-values (local-ctok-read local-ctok-write)
          (make-pipe 100000))
        
        (define recorded-cpp-out
          (and precompiling-header?
               (open-output-file (change-suffix file-out #".e") #:exists 'truncate)))
        (define recorded-cpp-in
          (and precompiled-header
               (open-input-file (change-suffix precompiled-header #".e"))))
        (define re:boring #rx#"^(?:(?:[ \t]*)|(?:# .*)|(?:#line .*)|(?:#pragma implementation.*)|(?:#pragma interface.*)|(?:#pragma once)|(?:#pragma warning.*)|(?:#ident.*))$")
        (define re:uninteresting #rx#"^(?:(?:[ \t]*)|(?:# .*)|(?:#line .*)|(?:#pragma implementation.*)|(?:#pragma interface.*)|(?:#pragma once)|(?:#pragma GCC diagnostic.*)|(?:#pragma warning.*)|(?:#ident.*))$")
        (define (skip-to-interesting-line p)
          (let ([l (read-bytes-line p 'any)])
            (cond
              [(eof-object? l) l]
              [(regexp-match-positions re:uninteresting l) (skip-to-interesting-line p)]
              [else l])))
        
        (when recorded-cpp-in
          ;; Skip over common part:
          (let loop ([lpos 1])
            (let ([pl (read-bytes-line recorded-cpp-in 'any)])
              (unless (eof-object? pl)
                (let ([l (skip-to-interesting-line (car cpp-process))])
                  (unless (equal? pl l)
                    (error 'precompiled-header "line mismatch with precompiled: ~s (line ~a) versus ~s"
                           pl
                           lpos
                           l))
                  (loop (add1 lpos))))))
          (close-input-port recorded-cpp-in))
        
        ;; cpp output to ctok input, also writes filtered lines to
        ;; cpp-out when reading a recompiled header
        (thread (lambda ()
                  (if recorded-cpp-out
                      ;; line-by-line, so we can filter:
                      (begin
                        (let loop ()
                          (let ([l (read-bytes-line (car cpp-process) 'any)])
                            (unless (eof-object? l)
                              (unless (regexp-match-positions re:boring l)
                                (display l recorded-cpp-out)
                                (newline  recorded-cpp-out))
                              (display l local-ctok-write)
                              (newline local-ctok-write)
                              (loop))))
                        (close-output-port recorded-cpp-out)
                        (close-input-port (car cpp-process))
                        (close-output-port local-ctok-write))
                      ;; block copy:
                      (let ([s (make-bytes 4096)])
                        (let loop ()
                          (let ([l (read-bytes-avail! s (car cpp-process))])
                            (unless (eof-object? l)
                              (write-bytes s local-ctok-write 0 l)
                              (loop))))
                        (close-input-port (car cpp-process))
                        (close-output-port local-ctok-write)))))
        
        (define e-raw #f)
        
        (define read-thread
          (thread
           (lambda ()
             (parameterize ([current-input-port local-ctok-read])
               (set! e-raw (car (tokenize)))))))
        
        ((list-ref cpp-process 4) 'wait)
        (thread-wait cpp-error-thread)
        (when (eq? ((list-ref cpp-process 4) 'status) 'done-error)
          (error 'xform "cpp failed"))
        
        (thread-wait read-thread)
        (set! read-thread #f)
        (when (exn? e-raw)
          (raise e-raw))
        
        ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        ;; Output and error-handling
        ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        
        (current-output-port (if file-out
                                 (open-output-file file-out #:exists 'truncate)
                                 (make-output-port 'dev/null
                                                   always-evt
                                                   (lambda (s st ed f?)
                                                     (- ed st))
                                                   void)))
        
        (let ([eh (error-escape-handler)])
          (error-escape-handler
           (lambda ()
	     (close-output-port (current-output-port))
	     (current-output-port (current-error-port))
             (when file-out
               (delete-file file-out))
             (eh))))
        
        (define exit-with-error? #f)
        
        (define (log-error format . args)
          (eprintf "Error ")
          (apply eprintf format args)
          (newline (current-error-port))
          (set! exit-with-error? #t))
        
        (define log-warning log-error)
        
        (define map-port
          (if palm-out
              (open-output-file palm-out #:exists 'truncate)
              #f))
        
        ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        ;; Output common defns
        ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        
        (define per-block-push? #t)
        (define gc-var-stack-mode
          (let loop ([e-raw e-raw])
            (ormap (lambda (e)
                     (cond
                      [(and (pragma? e)
                            (regexp-match #rx"GC_VARIABLE_STACK_THOUGH_TABLE" (pragma-s e)))
                       'table]
                      [(and (tok? e)
                            (eq? (tok-n e) 'XFORM_GC_VARIABLE_STACK_THROUGH_THREAD_LOCAL))
                       'thread-local]
                      [(and (tok? e)
                            (eq? (tok-n e) 'XFORM_GC_VARIABLE_STACK_THROUGH_GETSPECIFIC))
                       'getspecific]
                      [(and (tok? e)
                            (eq? (tok-n e) 'XFORM_GC_VARIABLE_STACK_THROUGH_FUNCTION))
                       'function]
                      [(and (tok? e)
                            (eq? (tok-n e) 'XFORM_GC_VARIABLE_STACK_THROUGH_DIRECT_FUNCTION))
                       'direct-function]
                      [(braces? e) (loop (seq->list (seq-in e)))]
                      [else #f]))
                   e-raw)))
        
        ;; The code produced by xform uses a number of macros. These macros
        ;; make the transformation about a little easier to debug, and they
        ;; enable experimentation with different variable-registration
        ;; strategies without modifying the xform process.
        
        (when (and pgc? (not precompiled-header))
          ;; Setup GC_variable_stack macro
          (printf (case gc-var-stack-mode
                   [(table)
                    "#define GC_VARIABLE_STACK (scheme_extension_table->GC_variable_stack)\n"]
                   [(getspecific)
                    "#define GC_VARIABLE_STACK (((Thread_Local_Variables *)pthread_getspecific(scheme_thread_local_key))->GC_variable_stack_)\n"]
                   [(function)
                    "#define GC_VARIABLE_STACK ((scheme_get_thread_local_variables())->GC_variable_stack_)\n"]
                   [(thread-local)
                    "#define GC_VARIABLE_STACK ((&scheme_thread_locals)->GC_variable_stack_)\n"]
                   [else "#define GC_VARIABLE_STACK GC_variable_stack\n"]))

          (if (or gc-variable-stack-through-funcs?
		  (eq? gc-var-stack-mode 'direct-function))
	      (begin
		(printf "#define GET_GC_VARIABLE_STACK() GC_get_variable_stack()\n")
		(printf "#define SET_GC_VARIABLE_STACK(v) GC_set_variable_stack(v)\n"))
	      (begin
		(printf "#define GET_GC_VARIABLE_STACK() GC_VARIABLE_STACK\n")
		(printf "#define SET_GC_VARIABLE_STACK(v) (GC_VARIABLE_STACK = (v))\n")))
          
          ;; Declare stack-registration record of a particular size:
          (printf (string-append
                   "#define PREPARE_VAR_STACK(size) void *__gc_var_stack__[size+2]; __gc_var_stack__[0] = GET_GC_VARIABLE_STACK();"
                   (if callee-restore?
                       " SET_GC_VARIABLE_STACK(__gc_var_stack__);"
                       "")
                   "\n"))
          
          ;; Same, but in a function where the number of registered variables
          ;;  never changes within the procedure (i.e., in nested blocks):
          (printf "#define PREPARE_VAR_STACK_ONCE(size) PREPARE_VAR_STACK(size); __gc_var_stack__[1] = (void *)size;\n")
          
          ;; Full setup to use before a function call, normally used with FUNCCALL:
          (printf (string-append 
                   "#define SETUP(x) ("
                   (if callee-restore?
                       ""
                       "SET_GC_VARIABLE_STACK(__gc_var_stack__), ")
                   "__gc_var_stack__[1] = (void *)x)\n"))

          ;; Debugging support:
          (printf "#ifdef MZ_3M_CHECK_VAR_STACK\n")
          (printf "static int _bad_var_stack_() { *(long *)0x0 = 1; return 0; }\n")
          (printf "# define CHECK_GC_V_S ((GC_VARIABLE_STACK == __gc_var_stack__) ? 0 : _bad_var_stack_()),\n")
          (printf "#else\n")
          (printf "# define CHECK_GC_V_S /*empty*/\n")
          (printf "#endif\n")

          ;; Call a function where the number of registered variables can change in
          ;;  nested blocks:          
          (printf "#define FUNCCALL_each(setup, x) (CHECK_GC_V_S setup, x)\n")
          ;; The same, but a "tail" call:
          (printf "#define FUNCCALL_EMPTY_each(x) (SET_GC_VARIABLE_STACK((void **)__gc_var_stack__[0]), x)\n")
          ;; The same, but the number of registered variables for this call is definitely
          ;;  the same as for the previous call:
          (printf (if callee-restore?
                      "#define FUNCCALL_AGAIN_each(x) (CHECK_GC_V_S x)\n"
                      "#define FUNCCALL_AGAIN_each(x) FUNCCALL_each(SET_GC_VARIABLE_STACK(__gc_var_stack__), x)\n"))
          
          ;; As above, but when the number of registered variables never changes
          ;;  within a procedure:
          (printf "#define FUNCCALL_once(setup, x) FUNCCALL_AGAIN_each(x)\n")
          (printf "#define FUNCCALL_EMPTY_once(x) FUNCCALL_EMPTY_each(x)\n")
          (printf "#define FUNCCALL_AGAIN_once(x) FUNCCALL_AGAIN_each(x)\n")
          
          ;; Register a particular variable locally:
          (printf "#define PUSH(v, x) (__gc_var_stack__[x+2] = (void *)&(v))\n")
          ;; Register a particular array variable locally:
          (printf (string-append
                   "#define PUSHARRAY(v, l, x) (__gc_var_stack__[x+2] = (void *)0, __gc_var_stack__[x+3] = (void *)&(v), "
                   "__gc_var_stack__[x+4] = (void *)l)\n"))
          
          ;; Wraps code to setup a block's variables:
          (printf "#define BLOCK_SETUP_TOP(x) ~a\n" (if per-block-push? "x" "/* skipped */"))
          ;; Same, but specifically in a function where nested blocks register
          ;;  extra variables:
          (printf "#define BLOCK_SETUP_each(x) BLOCK_SETUP_TOP(x)\n")
          ;; Same, but specifically in a function where nested blocks DO NOT
          ;;  register extra variables:
          (printf "#define BLOCK_SETUP_once(x) /* no effect */\n")
          
          ;; Wrap a normal return:
          (printf (if callee-restore?
                      "#define RET_VALUE_START return (__ret__val__ = \n"
                      "#define RET_VALUE_START return\n"))
          (printf (if callee-restore?
                      "#define RET_VALUE_END , SET_GC_VARIABLE_STACK((void **)__gc_var_stack__[0]), __ret__val__)\n"
                      "#define RET_VALUE_END \n"))
          ;; Wrap a return where the value is produced by a FUNCCALL_EMPTY expression:
          (printf "#define RET_VALUE_EMPTY_START return\n")
          (printf "#define RET_VALUE_EMPTY_END \n")
          ;; Replacement for non-value return:
          (printf "#define RET_NOTHING { SET_GC_VARIABLE_STACK((void **)__gc_var_stack__[0]); return; }\n")
          ;; A non-value return inserted at the end of a void-returning function:
          (printf "#define RET_NOTHING_AT_END RET_NOTHING\n")
          
          ;; Declare a temp variable to hold the return value of the indicated type:
          (printf (if callee-restore?
                      "#define DECL_RET_SAVE(type) type __ret__val__;\n"
                      "#define DECL_RET_SAVE(type) /**/\n"))
          
          ;; Value used to initialize pointer variables:
          (printf "#define NULLED_OUT 0\n")
          ;; Macro to initialize a pointer array:
          (printf "#define NULL_OUT_ARRAY(a) memset(a, 0, sizeof(a))\n")
          ;; Annotation that normally disappears:
          (printf "#define GC_CAN_IGNORE /**/\n")
          (printf "#define XFORM_CAN_IGNORE /**/\n")
          (printf "#define __xform_nongcing__ /**/\n")
          (printf "#define __xform_nongcing_nonaliasing__ /**/\n")
          ;; Another annotation to protect against GC conversion:
          (printf "#define HIDE_FROM_XFORM(x) x\n")
          (printf "#define XFORM_HIDE_EXPR(x) x\n")
          (printf "#define HIDE_NOTHING_FROM_XFORM() /**/\n")
          ;; In case a conversion is unnecessary where we have this annotation:
          (printf "#define START_XFORM_SKIP /**/\n")
          (printf "#define END_XFORM_SKIP /**/\n")
          (printf "#define START_XFORM_SUSPEND /**/\n")
          (printf "#define END_XFORM_SUSPEND /**/\n")
          (printf "#define XFORM_START_SKIP /**/\n")
          (printf "#define XFORM_END_SKIP /**/\n")
          (printf "#define XFORM_START_SUSPEND /**/\n")
          (printf "#define XFORM_END_SUSPEND /**/\n")
          (printf "#define XFORM_SKIP_PROC /**/\n")
          (printf "#define XFORM_ASSERT_NO_CONVERSION /**/\n")
          ;; For avoiding warnings:
          (printf "#define XFORM_OK_PLUS +\n")
          (printf "#define XFORM_OK_MINUS -\n")
          (printf "#define XFORM_TRUST_PLUS +\n")
          (printf "#define XFORM_TRUST_MINUS -\n")
          (printf "#define XFORM_OK_ASSIGN /**/\n")
          (printf "\n")

          ;; C++ cupport:
          (printf "#define NEW_OBJ(t) new (UseGC) t\n")
          (printf "#define NEW_ARRAY(t, array) (new (UseGC) t array)\n")
          (printf "#define NEW_ATOM(t) (new (AtomicGC) t)\n")
          (printf "#define NEW_PTR(t) (new (UseGC) t)\n")
          (printf "#define NEW_ATOM_ARRAY(t, array) (new (AtomicGC) t array)\n")
          (printf "#define NEW_PTR_ARRAY(t, array) (new (UseGC) t* array)\n")
          (printf "#define DELETE(x) (delete x)\n")
          (printf "#define DELETE_ARRAY(x) (delete[] x)\n")
          (printf (if callee-restore?
                      "#define XFORM_RESET_VAR_STACK /* empty */\n"
                      "#define XFORM_RESET_VAR_STACK SET_GC_VARIABLE_STACK((void **)__gc_var_stack__[0]);\n"))

          ;; Indirect setjmp support:
          (printf "#define scheme_mz_setjmp_post_xform(s) ((scheme_get_mz_setjmp())(s))\n")
          
          (unless pgc-really?
            (printf "#include \"cgc2.h\"\n"))
          
          (printf "\n"))
        
        (when (and pgc? precompiled-header)
          (printf "#include \"~a\"\n" (let-values ([(base name dir?) (split-path precompiled-header)])
                                        (path->string name))))
        
        (when palm?
          (printf "#include \"segmap.h\"\n"))
        
        ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        ;; Structures and constants
        ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        
        ;; vtype and its substructs describe the shape of a local variable:
        (define-struct vtype ())
        (define-struct (pointer-type vtype) (base stars))
        (define-struct (array-type vtype) (count))
        (define-struct (struc-type vtype) (struct))
        (define-struct (struct-array-type struc-type) (count))
        (define-struct (union-type vtype) ())
        (define-struct (non-pointer-type vtype) (base))
        
        ;; A live-var-info struct is threaded through the conversion process
        ;;  on a function body. It keeps information about which variables
        ;;  are live, which variables were invented along the way, how long
        ;;  the __gc_var_stack__ array needs to be, and so on.
        (define-struct live-var-info (tag 
                                      maxlive 
                                      maxpush
                                      vars
                                      new-vars 
                                      pushed-vars 
                                      num-calls 
                                      num-noreturn-calls
                                      num-empty-calls
                                      nonempty-calls?))
        
        ;; A function prototype record:
        (define-struct prototype (type args static? pointer? pointer?-determined?) #:mutable)
        
        ;; A C++ class record:
        (define-struct c++-class (parent parent-name prototyped top-vars) #:mutable)
        
        ;; Symbol constants:
        (define semi (string->symbol ";"))
        (define START_XFORM_SKIP (string->symbol "START_XFORM_SKIP"))
        (define END_XFORM_SKIP (string->symbol "END_XFORM_SKIP"))
        (define START_XFORM_SUSPEND (string->symbol "START_XFORM_SUSPEND"))
        (define END_XFORM_SUSPEND (string->symbol "END_XFORM_SUSPEND"))
        (define Scheme_Object (string->symbol "Scheme_Object"))
        (define sElF (string->symbol "sElF"))
        (define NULLED_OUT (string->symbol "NULLED_OUT"))
        (define NULL_OUT_ARRAY (string->symbol "NULL_OUT_ARRAY"))
        (define gcMark (string->symbol "gcMark"))
        (define gcFixup (string->symbol "gcFixup"))
        (define gcMARK_TYPED (string->symbol "gcMARK_TYPED"))
        (define gcFIXUP_TYPED (string->symbol "gcFIXUP_TYPED"))
        (define Mark_Proc (string->symbol "Mark_Proc"))
        (define gcBYTES_TO_WORDS (string->symbol "gcBYTES_TO_WORDS"))
        (define GC_cpp_delete (string->symbol "GC_cpp_delete"))
        (define PRE_ALLOCATE (string->symbol "PRE_ALLOCATE"))
        (define NEW_OBJ (string->symbol "NEW_OBJ"))
        (define NEW_ARRAY (string->symbol "NEW_ARRAY"))
        (define NEW_ATOM (string->symbol "NEW_ATOM"))
        (define NEW_PTR (string->symbol "NEW_PTR"))
        (define NEW_ATOM_ARRAY (string->symbol "NEW_ATOM_ARRAY"))
        (define NEW_PTR_ARRAY (string->symbol "NEW_PTR_ARRAY"))
        (define DELETE (string->symbol "DELETE"))
        (define DELETE_ARRAY (string->symbol "DELETE_ARRAY"))
        (define CURRENT_NEW_THIS (string->symbol "CURRENT_NEW_THIS"))
        (define RESTORE_CURRENT_NEW_VAR_STACK (string->symbol "RESTORE_CURRENT_NEW_VAR_STACK"))
        (define XFORM_RESET_VAR_STACK (string->symbol "XFORM_RESET_VAR_STACK"))
        (define END_XFORM_ARITH (string->symbol "END_XFORM_ARITH"))
        (define START_XFORM_ARITH (string->symbol "START_XFORM_ARITH"))
        (define GC_CAN_IGNORE (string->symbol "GC_CAN_IGNORE"))
        (define RET_VALUE_START (string->symbol "RET_VALUE_START"))
        (define RET_VALUE_END (string->symbol "RET_VALUE_END"))
        (define RET_VALUE_EMPTY_START (string->symbol "RET_VALUE_EMPTY_START"))
        (define RET_VALUE_EMPTY_END (string->symbol "RET_VALUE_EMPTY_END"))
        (define RET_NOTHING (string->symbol "RET_NOTHING"))
        (define RET_NOTHING_AT_END (string->symbol "RET_NOTHING_AT_END"))
        (define DECL_RET_SAVE (string->symbol "DECL_RET_SAVE"))
        
        (define __attribute__ (string->symbol "__attribute__"))
        
        (define re:_stk_ (regexp "^_stk_"))
        
        ;; These don't act like functions, but we need to treat them
        ;;  specially:
        (define setjmp-functions
          '(setjmp _setjmp scheme_setjmp scheme_mz_setjmp scheme_mz_setjmp_post_xform))
        
        ;; The non-functions table identifies symbols to ignore when
        ;; finding function calls
        (define non-functions
          '(<= < > >= == != !
               \| \|\| & && |:| ? % + - * / ^ >> << ~ 
               #csXFORM_OK_PLUS #csXFORM_OK_MINUS #csXFORM_TRUST_PLUS #csXFORM_TRUST_MINUS 
               = >>= <<= ^= += *= /= -= %= \|= &= ++ --
               return if for while else switch case XFORM_OK_ASSIGN
               asm __asm __asm__ __volatile __volatile__ volatile __extension__
               __typeof sizeof __builtin_object_size
            
               ;; These don't act like functions:
               setjmp longjmp _longjmp scheme_longjmp_setjmp scheme_mz_longjmp scheme_jit_longjmp
               scheme_jit_setjmp_prepare
               scheme_get_thread_local_variables pthread_getspecific
               __builtin_frame_address

               ;; The following are functions, but they don't trigger GC, and
               ;; they either take one argument or no pointer arguments.
               ;; So we can ignore them:
               
               __get_errno_ptr ; QNX preprocesses errno to __get_errno_ptr
               __getreent ; Cygwin

               strlen cos cosl sin sinl exp expl pow powl log logl sqrt sqrtl atan2 atan2l frexp
               isnan isinf fpclass signbit _signbit _fpclass __fpclassify __fpclassifyf __fpclassifyl
	       _isnan __isfinited __isnanl __isnan __signbit __signbitf __signbitd __signbitl
               __isinff __isinfl isnanf isinff __isinfd __isnanf __isnand __isinf
               __inline_isnanl __inline_isnan __inline_signbit __inline_signbitf __inline_signbitd __inline_signbitl
               __builtin_popcount __builtin_clz __builtin_isnan __builtin_isinf __builtin_signbit
               __builtin_signbitf __builtin_signbitd __builtin_signbitl __builtin_isinf_sign
               _Generic
               __inline_isinff __inline_isinfl __inline_isinfd __inline_isnanf __inline_isnand __inline_isinf
               floor floorl ceil ceill round roundl fmod fmodl modf modfl fabs fabsl __maskrune _errno __errno
               isalpha isdigit isspace tolower toupper
               fread fwrite socket fcntl setsockopt connect send recv close
               __builtin_next_arg __builtin_saveregs 
               __builtin_constant_p __builtin_choose_expr __builtin_types_compatible_p
               __builtin___CFStringMakeConstantString
               __error __errno_location __toupper __tolower ___errno
               __attribute__ __mode__ ; not really functions in gcc
               __iob_func ; VC 8
               __acrt_iob_func ; VC 14.0 (2015)
               |GetStdHandle| |__CFStringMakeConstantString|
               _vswprintf_c
               
               scheme_make_small_bignum scheme_make_small_rational scheme_make_small_complex))
        (define non-functions-table
          (let ([ht (make-hasheq)])
            (for-each (lambda (s)
                        (hash-set! ht s #f))
                      non-functions)
            ht))

	(define args-unevaled '(sizeof __typeof __builtin_object_size))
	(define args-unevaled-table
          (let ([ht (make-hasheq)])
            (for-each (lambda (s)
                        (hash-set! ht s #t))
                      args-unevaled)
            ht))

        (define non-gcing-builtin-functions
          ;; The following don't need wrappers, but we need to check for
          ;;  nested function calls because it takes more than one argument:
          (append
           '(memcpy memmove memcmp memset
		    __builtin___memmove_chk __inline_memmove_chk
		    __builtin___memcpy_chk __inline_memcpy_chk
		    __builtin___memset_chk __inline_memset_chk
		    __builtin___memcmp_chk __inline_memcmp_chk
                    strcmp strcoll strcpy _mzstrcpy strcat
		     __builtin_memset
                    printf sprintf vsprintf vprintf
                    strncmp
                    read write)
           (map
            string->symbol
            '("XTextExtents" "XTextExtents16" 
                             "XDrawImageString16" "XDrawImageString"
                             "XDrawString16" "XDrawString"))))
	(define non-gcing-functions (make-hasheq))
	(for-each (lambda (name)
		    (hash-set! non-gcing-functions name #t))
		  non-gcing-builtin-functions)

        ;; Non-aliasing function may take address of variables as arguments to fill
        ;; them in, but they don't expose those addresses, so taking a variable's
        ;; address for an argument doesn't make it live for the rest of the enclosing
        ;; function.
        (define non-aliasing-functions (make-hasheq))

        (define non-returning-functions
          ;; The following functions never return, so the wrappers
          ;; don't need to push any variables:
          '(exit
            scheme_wrong_type scheme_wrong_number scheme_wrong_syntax
            scheme_wrong_count scheme_wrong_count_m scheme_wrong_rator scheme_read_err
            scheme_wrong_contract scheme_contract_error
            scheme_raise_exn scheme_signal_error
            scheme_raise_out_of_memory
            ))
        
        
        (define non-pointer-typedef-names
          ;; Under Windows, things like HANDLE and HWND, are not
          ;; malloced and could overlap with GCed areas.
          ;; Mac OS X has similar things.
          #cs
          '(HANDLE
            HWND HDC HMENU
            HBITMAP HBRUSH HPEN HFONT HPALETTE HRGN
            HICON HINSTANCE
            GLOBALHANDLE LOCALHANDLE HGLOBAL HLOCAL
            GrafPtr RgnHandle PixMapHandle Handle MenuHandle GDHandle
            WindowPtr DialogPtr ControlRef EventRef EventHandlerCallRef
            CGContextRef))
        
        (define asm-commands
          ;; When outputting, add newline before these syms
          ;; (for __asm blocks in Windows)
          '(mov shl shld shr shrd sar lock setc add))
        
        (define (get-constructor v)
          (cond
            [(creation-parens? v) make-creation-parens]
            [(parens? v) make-parens]
            [(brackets? v) make-brackets]
            [(braces? v) make-braces]))
        
        ;; gets the size of a variable in terms of the number of
        ;; __gc_var_stack__ slots it needs
        (define (get-variable-size vtype)
          (cond
            [(array-type? vtype)
             ;; 1 for "is an array", 1 for array size, 1 for array pointer
             3]
            [(struc-type? vtype)
             (let ([size (let ([m (lookup-struct-def (struc-type-struct vtype))])
                           (apply + (map get-variable-size
                                         (map cdr (cdr m)))))])
               (if (struct-array-type? vtype)
                   (* size
                      (let ([c (struct-array-type-count vtype)])
                        (cond
                         [(eq? c 'unknown)
                          (log-error "[STRUCT ARRAY]: Can't get size of unknown-sized array")
                          1]
                         [else c])))
                   size))]
            [(vtype? vtype) 1]
            [else (error 'get-variable-size "not a vtype: ~e"
                         vtype)]))
        
        (define (replace-live-vars live-vars new-live-vars)
          (make-live-var-info (live-var-info-tag live-vars)
                              (live-var-info-maxlive live-vars)
                              (live-var-info-maxpush live-vars)
                              new-live-vars
                              (live-var-info-new-vars live-vars)
                              (live-var-info-pushed-vars live-vars)
                              (live-var-info-num-calls live-vars)
                              (live-var-info-num-noreturn-calls live-vars)
                              (live-var-info-num-empty-calls live-vars)
                              (live-var-info-nonempty-calls? live-vars)))
        
        (define gentag-count 0)
        
        (define gentag
          (lambda ()
            (set! gentag-count (add1 gentag-count))
            (format "XfOrM~a" gentag-count)))
        
        ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        ;; State
        ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        
        ;; See `used-symbols' above
        
        (define c++-classes null)
        
        ;; list of (cons symbol prototype)
        (define prototyped (make-parameter null))
        ;; list of (cons symbol vtype)
        (define top-vars (make-parameter null))
        
        ;; Accum top-level typedefs for pointers and non-pointers as a list-of-sym:
        (define pointer-types '())
        (define non-pointer-types '(int char long unsigned intptr_t ulong uint uintptr_t void float double |long double| uchar wchar_t))
        ;; Accum top-level struct decls as list of (cons sym (list (cons symbol vtype) ...))
        (define struct-defs '())
        
        ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        ;; Marhsaling and unmarshaling
        ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        
        (define makers (make-hasheq))
        (hash-set! makers 'struct:tok (cons 'make-tok make-tok))
        (hash-set! makers 'struct:sysheader-tok (cons 'make-sysheader-tok make-sysheader-tok))
        (hash-set! makers 'struct:seq (cons 'make-a-seq make-a-seq))
        (hash-set! makers 'struct:parens (cons 'make-parens make-parens))
        (hash-set! makers 'struct:brackets (cons 'make-brackets make-brackets))
        (hash-set! makers 'struct:braces (cons 'make-braces make-braces))
        (hash-set! makers 'struct:callstage-parens (cons 'make-callstage-parens make-callstage-parens))
        (hash-set! makers 'struct:creation-parens (cons 'make-creation-parens make-creation-parens))
        (hash-set! makers 'struct:nosrc-parens (cons 'make-nosrc-parens make-nosrc-parens))
        (hash-set! makers 'struct:call (cons 'make-call make-call))
        (hash-set! makers 'struct:block-push (cons 'make-block-push make-block-push))
        (hash-set! makers 'struct:note (cons 'make-note make-note))
        (hash-set! makers 'struct:vtype (cons 'make-vtype make-vtype))
        (hash-set! makers 'struct:pointer-type (cons 'make-pointer-type make-pointer-type))
        (hash-set! makers 'struct:array-type (cons 'make-array-type make-array-type))
        (hash-set! makers 'struct:struc-type (cons 'make-struc-type make-struc-type))
        (hash-set! makers 'struct:struct-array-type (cons 'make-struct-array-type make-struct-array-type))
        (hash-set! makers 'struct:union-type (cons 'make-union-type make-union-type))
        (hash-set! makers 'struct:non-pointer-type (cons 'make-non-pointer-type make-non-pointer-type))
        (hash-set! makers 'struct:live-var-info (cons 'make-live-var-info make-live-var-info))
        (hash-set! makers 'struct:prototype (cons 'make-prototype make-prototype))
        (hash-set! makers 'struct:c++-class (cons 'make-c++-class make-c++-class))
        
        (define (make-short-tok l) (make-tok l #f #f))
        
        ;; A precompiled header saves the above state variables.
        (when precompiled-header
          (let ([orig (current-namespace)])
            (parameterize ([current-namespace (make-base-empty-namespace)])
	      (namespace-require/copy 'racket/base)
              (namespace-attach-module orig 'racket/base)
              (namespace-require 'racket/base)
              ;; Put constructors into the namespace:
              (hash-for-each makers
                                   (lambda (k v)
                                     (namespace-set-variable-value! (car v) (cdr v))))
              (namespace-set-variable-value! 'make-short-tok make-short-tok)
              ;; Load the pre-compiled-header-as-.zo:
              (let ([l (load (change-suffix precompiled-header #".zo"))])
                (for-each (lambda (x)
                            (hash-set! used-symbols (car x) 
                                             (+
                                              (hash-ref
                                               used-symbols (car x)
                                               (lambda () 0))
                                              (cdr x))))
                          (list-ref l 0))
                
                (set! c++-classes (list-ref l 1))
                (prototyped (list-ref l 2))
                (top-vars (list-ref l 3))
                
                (set! pointer-types (list-ref l 4))
                (set! non-pointer-types (list-ref l 5))
                (set! struct-defs (list-ref l 6))
                
                (set! non-gcing-functions (hash-copy (list-ref l 7)))
                (set! non-aliasing-functions (hash-copy (list-ref l 8)))

                (set! gc-var-stack-mode (list-ref l 9))))))
        
        ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        ;; Pretty-printing output
        ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        
        (define next-indent #f)
        
        (define (newline/indent i)
          (newline)
          (set! next-indent i))
        
        (define (display/indent v s)
          (when next-indent
            (display (make-string next-indent #\space))
            (set! next-indent #f))
          (display s))
        
        (define re:quote-or-backslash (regexp "[\\\"]"))
        
        (define (push-vars l plus comma)
          (let loop ([l l][n 0][comma comma])
            (unless (null? l)
              (loop (cdr l)
                    (let push-var ([full-name (caar l)][vtype (cdar l)][n n][comma comma])
                      (cond
                        [(union-type? vtype)
                         (log-error "[UNION]: Can't push union onto mark stack: ~a." full-name)
                         (printf "~aPUSHUNION(~a, ~a~a)" comma full-name n plus)
                         (add1 n)]
                        [(array-type? vtype)
                         (define c (array-type-count vtype))
                         (when (eq? c 'unknown)
                           (log-error "[ARRAY]: Can't push unknown array size onto mark stack: ~a." full-name))
                         (printf "~aPUSHARRAY(~a, ~a, ~a~a)" comma full-name (if (eq? c 'unknown) 0 c) n plus)
                         (+ 3 n)]
                        [(struc-type? vtype)
                         (let aloop ([array-index 0][n n][comma comma])
                           ;; Push each struct in array (or only struct if not an array)
                           (let loop ([n n][l (cdr (lookup-struct-def (struc-type-struct vtype)))][comma comma])
                             (if (null? l)
                                 (if (and (struct-array-type? vtype)
                                          (< (add1 array-index)
                                             (let ([c (struct-array-type-count vtype)])
                                               (cond
                                                [(eq? c 'unknown)
                                                 (log-error "[STRUCT ARRAY]: Can't push with unknown array size: ~a."
                                                            full-name)
                                                 1]
                                                [else c]))))
                                     ;; Next in array
                                     (aloop (add1 array-index) n comma)
                                     ;; All done
                                     n)
                                 (loop (push-var (format "~a~a.~a"
                                                         full-name 
                                                         (if (struct-array-type? vtype)
                                                             (format "[~a]" array-index)
                                                             "")
                                                         (caar l))
                                                 (cdar l)
                                                 n
                                                 comma)
                                       (cdr l)
                                       ", "))))]
                        [else
                         (printf "~aPUSH(~a, ~a~a)" comma full-name n plus)
                         (+ n 1)]))
                    ", "))))
        
        (define (total-push-size vars)
          (apply + (map (lambda (x)
                          (get-variable-size (cdr x)))
                        vars)))

        (define (extract-src-tok v)
          (cond
           [(tok? v) v]
           [(call? v) (extract-src-tok (call-func v))]
           [else #f]))
        
        (define (print-it e indent semi-newlines? ordered? line file sysheader? keep-lines?)
          (let loop ([e e][prev #f][prevs null][old-line line][old-file file][old-sysheader? sysheader?])
            (if (null? e)
              (values old-line old-file old-sysheader?)
              (let* ([v (car e)]
                     [sv (extract-src-tok v)]
                     [line (if keep-lines?
                               (or (and sv (tok-line sv))
                                   old-line)
                               old-line)]
                     [file (if keep-lines?
                               (or (and sv (tok-file sv))
                                   old-file)
                               old-file)]
                     [sysheader? (if keep-lines?
                                     (if (and sv (tok-file sv))
                                         (sysheader-tok? sv)
                                         old-sysheader?)
                                     old-sysheader?)]
                     [inc-line! (lambda () (set! line (add1 line)))])
                (when keep-lines?
                  (unless (and (equal? line old-line)
                               (equal? file old-file))
                    (if (and (equal? file old-file)
                             (line . > . old-line)
                             ((- line old-line) . < . 10))
                        (display (make-string (- line old-line) #\newline))
                        (printf "\n# ~a \"~a\"~a\n" line file
                                (if sysheader? " 3" "")))
                    (set! next-indent indent)))
                (cond
                  [(pragma? v)
                   (let ([s (format "#pragma ~a" (pragma-s v))])
                     (unless (regexp-match re:boring s)
                       (printf "\n~a\n\n" s)
                       (set! line (+ line 3))))]
                  [(threadlocal-decl? v) (void)]
                  [(seq? v)
                   (define skip-parens?
                     ;; avoid `if ((...))' when "..." is not an assignment, 
                     ;; because that annoys compilers like clang
                     (and prev (tok? prev) (memq (tok-n prev) '(if))
                          (let ([l (seq->list (seq-in v))])
                            (and (pair? l) 
                                 (null? (cdr l))
                                 (parens? (car l))
                                 (let ([l (seq->list (seq-in (car l)))])
                                   (not (ormap (lambda (i) (eq? '= (tok-n i))) 
                                               l)))))))
                   (display/indent v (if skip-parens? "" (tok-n v)))
                   (let ([subindent (if (braces? v)
                                        (begin
                                          (newline/indent (+ indent 2))
                                          (inc-line!)
                                          (+ indent 2))
                                        indent)])
                     (let-values ([(l f s?)
                                   (print-it (seq->list (seq-in v)) subindent
                                             (not (and (parens? v)
                                                       prev
                                                       (tok? prev)
                                                       (memq (tok-n prev) '(for))))
                                             (or (braces? v) (callstage-parens? v))
                                             line file sysheader?
                                             (and keep-lines?
                                                  (not (nosrc-parens? v))))])
                       (set! line l)
                       (set! file f)
                       (set! sysheader? s?))
                     (when (and next-indent (= next-indent subindent))
                       (set! next-indent indent)))
                   (unless skip-parens?
                     (display/indent #f (seq-close v)))
                   (cond
                     [(braces? v)
                      (newline/indent indent)
                      (inc-line!)]
                     [(brackets? v)
                      (display/indent v " ")]
                     [(parens? v)
                      (if (and prev 
			       (tok? prev)
                               (memq (tok-n prev) '(if))
                               (or (null? (cdr e))
                                   (not (braces? (cadr e)))))
                          (begin
                            (newline/indent (+ indent 2))
                            (inc-line!))
                          (display/indent v " "))]
                     [else (error 'xform "unknown brace: ~a" (caar v))])]
                  [(note? v)
                   (display/indent v (note-s v))
                   (newline/indent indent)
                   (inc-line!)]
                  [(call? v)
                   (if (not (call-nonempty? v))
                       (display/indent v "FUNCCALL_EMPTY(")
                       (if (and ordered? (prev-was-funcall? prevs))
                           ;; Do fast version
                           (begin
                             (display/indent v "FUNCCALL_AGAIN("))
                           ;; Do general version
                           (begin
                             (display/indent v (format "FUNCCALL(SETUP_~a(" 
                                                       (call-tag v)))
                             (if show-info?
                                 (begin
                                   (display/indent v (format "(SETUP(~a)" 
                                                             (total-push-size (call-live v))))
                                   (push-vars (call-live v) "" ", ")
                                   (display/indent v ")"))
                                 (display/indent v "_"))
                             (display/indent v "), "))))
                   (let-values ([(l f s?)
                                 (print-it (append (call-func v) (list (call-args v))) 
                                           indent #f #f line file sysheader?
                                           ;; Can't put srcloc within macro call:
                                           #f)])
                     (set! line l)
                     (set! file f)
                     (set! sysheader? s?))
                   (display/indent v ")")]
                  [(block-push? v)
                   (let ([size (total-push-size (block-push-vars v))]
                         [prev-add (if (block-push-super-tag v)
                                       (format "+~a_COUNT" (block-push-super-tag v))
                                       "")]
                         [tag (block-push-tag v)]
                         [tabbing (if (zero? indent)
                                      ""
                                      (make-string (sub1 indent) #\space))])
                     (unless (zero? size)
                       (display/indent v (format "BLOCK_SETUP~a((" (if (block-push-top? v) "_TOP" "")))
                       (push-vars (block-push-vars v) prev-add "")
                       (display/indent v "));")
                       (newline)
                       (inc-line!))
                     (printf "#~adefine ~a_COUNT (~a~a)\n" tabbing tag size prev-add)
                     (inc-line!)
                     (printf "#~adefine SETUP_~a(x) " tabbing tag)
                     (cond
                       [(and (zero? size) (block-push-super-tag v)) 
                        (printf "SETUP_~a(x)" (block-push-super-tag v))]
                       [per-block-push? (printf "SETUP(~a_COUNT)" tag)]
                       [else (printf "x")])
                     (newline/indent indent)
                     (inc-line!))]
                  [(nested-setup? v)
                   (let ([tabbing (if (zero? indent)
                                      ""
                                      (make-string (sub1 indent) #\space))])
                     (case (tok-n v)
                       [(nested)
                        (printf "#~adefine BLOCK_SETUP(x) BLOCK_SETUP_each(x)\n" tabbing)
                        (printf "#~adefine FUNCCALL(s, x) FUNCCALL_each(s, x)\n" tabbing)
                        (printf "#~adefine FUNCCALL_EMPTY(x) FUNCCALL_EMPTY_each(x)\n" tabbing)
                        (printf "#~adefine FUNCCALL_AGAIN(x) FUNCCALL_AGAIN_each(x)\n" tabbing)]
                       [(no-nested)
                        (printf "#~adefine BLOCK_SETUP(x) BLOCK_SETUP_once(x)\n" tabbing)
                        (printf "#~adefine FUNCCALL(s, x) FUNCCALL_once(s, x)\n" tabbing)
                        (printf "#~adefine FUNCCALL_EMPTY(x) FUNCCALL_EMPTY_once(x)\n" tabbing)
                        (printf "#~adefine FUNCCALL_AGAIN(x) FUNCCALL_AGAIN_once(x)\n" tabbing)]
                       [(undefine)
                        (printf "#~aundef BLOCK_SETUP\n" tabbing)
                        (printf "#~aundef FUNCCALL\n" tabbing)
                        (printf "#~aundef FUNCCALL_EMPTY\n" tabbing)
                        (printf "#~aundef FUNCCALL_AGAIN\n" tabbing)])
                     (set! line (+ 4 line)))]
                  [(memq (tok-n v) asm-commands)
                   (newline/indent indent)
                   (inc-line!)
                   (display/indent v (tok-n v))
                   (display/indent v " ")]
                  [(and (or (eq? '|HIDE_FROM_XFORM| (tok-n v))
                            (eq? '|XFORM_HIDE_EXPR| (tok-n v)))
                        (pair? (cdr e))
                        (seq? (cadr e))
                        (null? (seq->list (seq-in (cadr e)))))
                   ;; This handles the case where we were trying to hide
                   ;; something from xform, but the something macro-expanded
                   ;; to nothing.  It happens, for example, in FreeBSD gcc
                   ;; 2.95.x when hiding a va_end() use
                   (display/indent v '|HIDE_NOTHING_FROM_XFORM|)]
                  [else
                   (if (string? (tok-n v))
                       (begin
                         (display/indent v "\"")
                         (display (tok-n v))
                         (display/indent v "\""))
                       (display/indent v (tok-n v)))
                   ;; Don't put a space between L and a string, because without
                   ;; the space it means a long string.
                   (unless (and (eq? '|L| (tok-n v))
                                (pair? (cdr e))
                                (or (string? (tok-n (cadr e)))
				    (character? (tok-n (cadr e))))
                                (not (seq? (tok-n (cadr e)))))
                     (display/indent v " "))
                   (when (and (eq? semi (tok-n v))
                              semi-newlines?)
                     (newline/indent indent)
                     (inc-line!))])
                (loop (cdr e) v (cons v prevs) line file sysheader?)))))
        
        
        ;; prev-was-funcall? implements a last-ditch optimization: if
        ;;  we just did a FUNCALL setup, we can do a FUNCALL_AGAIN setup 
        ;;  this time (which is possibly quicker)
        (define (prev-was-funcall? prevs)
          (letrec ([acall? (lambda (v)
                             (or (call? v)
                                 ;; Maybe nested seq
                                 (and (parens? v)
                                      (let ([p (reverse (seq->list (seq-in v)))])
                                        (and (pair? p)
                                             (call? (car p))
                                             (callseq-prev? (cdr p)))))))]
                   [callseq-prev? (lambda (prevs)
                                    (and (pair? prevs) (pair? (cdr prevs))
					 (tok? (car prevs))
                                         (eq? '|,| (tok-n (car prevs)))
                                         (acall? (cadr prevs))))])
            (or
             ;; Stmt (call or assign=call) sequence
             (let loop ([prevs prevs][semis 0])
               (cond
                 [(and (pair? prevs) 
		       (tok? (car prevs))
                       (eq? semi (tok-n (car prevs))))
                  (or (positive? semis) ;; means that we already found a proc-ending semi
                      (if (and (pair? (cdr prevs))
                               (eq? semi (tok-n (cadr prevs))))
                          ;; Odd extra semi-colon. Skip it and try again.
                          (loop (cdr prevs) semis)
                          ;; Look further...
                          (and (pair? (cdr prevs))
                               (acall? (cadr prevs))
                               (loop (cddr prevs) (add1 semis)))))]
                 [(and (pair? prevs) (pair? (cdr prevs)) (pair? (cddr prevs))
		       (tok? (car prevs))
		       (tok? (cadr prevs))
                       (eq? '= (tok-n (car prevs)))
                       (symbol? (tok-n (cadr prevs)))
                       (eq? semi (tok-n (caddr prevs))))
                  (loop (cddr prevs) semis)]
                 [else #f]))
             ;; Eval sequence
             (callseq-prev? prevs))))
        
        ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        ;; "Parsing"
        ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        
        (define skipping? #f)
        (define suspend-xform 0)
        
        (define re:h (regexp "[.]h$"))
        
        ;; top-level converts the top-level tok list e into
        ;; a new top-level tok list, often collecting info
        ;; (such as function prototypes and typedefs).
        ;; It expects that the tok list e reprsents one "thing",
        ;; which often means that it's terminated with a semicolon.
        (define (top-level e where can-drop-vars?)
          (cond
            [(pragma? (car e))
             (list (car e))]

	    [(compiler-pragma? e)
	     e]
            
            ;; START_XFORM_SKIP and END_XFORM_SKIP:
            [(end-skip? e)
             (set! skipping? #f)
             null]
            [(start-skip? e)
             (set! skipping? #t)
             null]
            [skipping?
             e]
            
            ;; START_XFORM_SUSPEND and END_XFORM_SUSPEND:
            [(end-suspend? e)
             (set! suspend-xform (sub1 suspend-xform))
             null]
            [(start-suspend? e)
             (set! suspend-xform (add1 suspend-xform))
             null]
            
            ;; END_XFORM_ARITH and START_XFORM_ARITH enable and
            ;; re-enable warnings about arithmetic operations
            ;; on pointers
            [(end-arith? e)
             (set! check-arith? #f)
             null]
            [(start-arith? e)
             (set! check-arith? #t)
             null]

            [(threadlocal-decl? e)
             null]
            
            [(access-modifier? e)
             ;; public, private, etc.
             (list* (car e) (cadr e) (top-level (cddr e) where can-drop-vars?))]
            [(friend? e)
             ;; C++ friend annotation
             e]
            
            ;; process 'extern "C"' blocks 
            [(and (>= (length e) 3)
                  (eq? (tok-n (car e)) 'extern)
                  (member (tok-n (cadr e)) '("C" "C++"))
                  (braces? (caddr e)))
             (list* (car e)
                    (cadr e)
                    (let ([body-v (caddr e)])
                      (make-braces
                       (tok-n body-v)
                       (tok-line body-v)
                       (tok-file body-v)
                       (seq-close body-v)
                       (list->seq (process-top-level (seq->list (seq-in body-v)) where can-drop-vars?))))
                    (cdddr e))]
            
            ;; process 'namespace X' blocks; currently, we assume that namespace
            ;;  content is distinct
            [(and (>= (length e) 3)
                  (eq? (tok-n (car e)) 'namespace)
                  (symbol? (tok-n (cadr e)))
                  (braces? (caddr e)))
             (list* (car e)
                    (cadr e)
                    (let ([body-v (caddr e)])
                      (make-braces
                       (tok-n body-v)
                       (tok-line body-v)
                       (tok-file body-v)
                       (seq-close body-v)
                       (list->seq (process-top-level (seq->list (seq-in body-v)) where can-drop-vars?))))
                    (cdddr e))]
            
            [(typedef? e)
             (when show-info?
               (printf "/* TYPEDEF */\n"))
             (if (or (simple-unused-def? e)
                     (unused-struc-typedef? e))
                 null
                 (begin
                   (when pgc?
                     (check-pointer-type e))
                   e))]
            [(proc-prototype? e)
             (let ([name (register-proto-information e)])
               (when (eq? (tok-n (car e)) '__xform_nongcing__)
		 (hash-set! non-gcing-functions name #t))
               (when (eq? (tok-n (car e)) '__xform_nongcing_nonaliasing__)
		 (hash-set! non-gcing-functions name #t)
		 (hash-set! non-aliasing-functions name #t))
	       (when show-info?
                 (printf "/* PROTO ~a */\n" name))
               (if (or precompiling-header?
                       (> (hash-ref used-symbols name) 1)
                       (ormap (lambda (v) (eq? (tok-n v) 'virtual)) e))  ; can't drop virtual methods!
                   (if palm?
                       (add-segment-label name e)
		       (clean-proto e))
                   null))]
            [(struct-decl? e)
             (if (braces? (caddr e))
                 (begin
                   (when pgc?
                     (register-struct e))
                   (when show-info? (printf "/* STRUCT ~a */\n" (tok-n (cadr e)))))
                 (when show-info? (printf "/* STRUCT DECL */\n")))
             e]
            [(class-decl? e)
             (if (or (braces? (caddr e))
                     (eq? '|:| (tok-n (caddr e))))
                 (begin
                   (when show-info? (printf "/* CLASS ~a */\n" (tok-n (cadr e))))
                   (register-class e))
                 (begin
                   (when show-info? (printf "/* CLASS DECL */\n"))
                   (let ([name (tok-n (cadr e))])
                     (if (assoc name c++-classes)
                         ;; we already know this class
                         null
                         e))))]
            [(function? e)
             (let ([name (register-proto-information e)])
               (when (eq? (tok-n (car e)) '__xform_nongcing__)
                 (hash-set! non-gcing-functions name #t))
               (when (eq? (tok-n (car e)) '__xform_nongcing_nonaliasing__)
                 (hash-set! non-gcing-functions name #t)
                 (hash-set! non-aliasing-functions name #t))
               (if (skip-function? e)
                   e
                   (begin
                     (when show-info? (printf "/* FUNCTION ~a */\n" name))
                     (if (or (positive? suspend-xform)
                             (not pgc?)
                             (and where 
                                  (regexp-match re:h where)
                                  (let loop ([e e][prev #f])
                                    (cond
                                     [(null? e) #t]
                                     [(and (eq? '|::| (tok-n (car e)))
                                           prev
                                           (eq? (tok-n prev) (tok-n (cadr e))))
                                      ;; inline constructor: need to convert
                                      #f]
                                     [else (loop (cdr e) (car e))]))))
                         ;; Not pgc, xform suspended,
                         ;; or still in headers and probably a simple inlined function
                         (let ([palm-static? (and palm? (eq? 'static (tok-n (car e))))])
                           (when palm?
                             (fprintf map-port "(~aimpl ~s)\n" 
                                      (if palm-static? "s" "")
                                      name)
                             (call-graph name e))
                           (append
                            (if palm-static?
                                ;; Need to make sure prototype is there for section
                                (add-segment-label
                                 name
                                 (let loop ([e e])
                                   (if (braces? (car e))
                                       (list (make-tok semi #f #f))
                                       (cons (car e) (loop (cdr e))))))
                                null)
                            e))
                         (convert-function e name)))))]
            [(var-decl? e)
             (when show-info? (printf "/* VAR */\n"))
             (if (and can-drop-vars?
                      (simple-unused-def? e))
                 null
                 (begin
                   (when pgc?
                     (unless (eq? (tok-n (car e)) 'static)
                       (let-values ([(pointers non-pointers) (get-vars e "TOPVAR" #f #t)])
                         (top-vars (append pointers non-pointers (top-vars))))))
                   e))]
            
            [(empty-decl? e)
             e]
            
            [else (print-struct #t)
                  (error 'xform "unknown form: ~s" e)]))
        
        (define (empty-decl? e)
          (and (= 1 (length e))
               (eq? '|;| (tok-n (car e)))))

	(define (compiler-pragma? e)
	  ;; MSVC uses __pragma() to control compiler warnings
	  (and (pair? e)
	       (eq? '__pragma (tok-n (car e)))
	       (pair? (cdr e))
	       (parens? (cadr e))))
        
        (define (start-skip? e)
          (and (pair? e)
               (or (eq? START_XFORM_SKIP (tok-n (car e)))
                   (eq? 'XFORM_START_SKIP (tok-n (car e))))))
        
        (define (end-skip? e)
          (and (pair? e)
               (or (eq? END_XFORM_SKIP (tok-n (car e)))
                   (eq? 'XFORM_END_SKIP (tok-n (car e))))))
        
        (define (start-suspend? e)
          (and (pair? e)
               (or (eq? START_XFORM_SUSPEND (tok-n (car e)))
                   (eq? 'XFORM_START_SUSPEND (tok-n (car e))))))
        
        (define (end-suspend? e)
          (and (pair? e)
               (or (eq? END_XFORM_SUSPEND (tok-n (car e)))
                   (eq? 'XFORM_END_SUSPEND (tok-n (car e))))))
        
        (define (start-arith? e)
          (and (pair? e)
               (or (eq? START_XFORM_ARITH (tok-n (car e)))
                   (eq? 'XFORM_END_TRUST_ARITH (tok-n (car e))))))
        
        (define (end-arith? e)
          (and (pair? e)
               (or (eq? END_XFORM_ARITH (tok-n (car e)))
                   (eq? 'XFORM_START_TRUST_ARITH (tok-n (car e))))))

        (define (threadlocal-decl? e)
          (and (pair? e)
               (or (eq? 'XFORM_GC_VARIABLE_STACK_THROUGH_GETSPECIFIC (tok-n (car e)))
                   (eq? 'XFORM_GC_VARIABLE_STACK_THROUGH_FUNCTION (tok-n (car e)))
                   (eq? 'XFORM_GC_VARIABLE_STACK_THROUGH_DIRECT_FUNCTION (tok-n (car e)))
                   (eq? 'XFORM_GC_VARIABLE_STACK_THROUGH_THREAD_LOCAL (tok-n (car e))))))
        
        (define (access-modifier? e)
          (and (memq (tok-n (car e)) '(public private protected))
               (eq? (tok-n (cadr e)) '|:|)))
        
        (define (friend? e)
          (memq (tok-n (car e)) '(friend)))
        
        ;; recognize a function prototype:
        (define (proc-prototype? e)
          (let loop ([l (length e)])
            (and (> l 2)
                 ;; Ends in semicolon
                 (eq? semi (tok-n (list-ref e (sub1 l))))
                 (let loop ([l l])
                   (or (and
                        (> l 2)
                        ;; next-to-last is parens
                        (parens? (list-ref e (- l 2)))
                        ;; Symbol before parens, not '= or '__attribute__
                        (let ([s (tok-n (list-ref e (- l 3)))])
                          (and (symbol? s)
                               (not (eq? '= s))
                               (not (eq? '__attribute__ s)))))
                       (and
                        ;; next-to-last is 0, then =, then parens
                        (eq? 0 (tok-n (list-ref e (- l 2))))
                        (eq? '= (tok-n (list-ref e (- l 3))))
                        (loop (- l 2)))
                       (and
                        ;; next-to-last is 0, then =, then parens
                        (eq? '__attribute__ (tok-n (list-ref e (- l 3))))
                        (loop (- l 2))))))))
        
        ;; recognize a typedef:
        (define (typedef? e)
          (or (eq? 'typedef (tok-n (car e)))
              (and (eq? '__extension__ (tok-n (car e)))
                   (pair? (cdr e))
                   (eq? 'typedef (tok-n (cadr e))))))
        
        ;; Sometimes, we know that a declaration is unused because
        ;; the tokenizer saw the defined symbol only once. (This
        ;;  doesn't work if we're pre-compiling a header for later.)
        (define (simple-unused-def? e)
          (and (not precompiling-header?)
               (andmap (lambda (x) (and (symbol? (tok-n x))
                                   (not (eq? '|,| (tok-n x)))))
                       e)
               (= 1 (hash-ref used-symbols
			      (let loop ([e e])
				(if (or (null? (cddr e))
					(and (pair? (cdr e))
					     (eq? '= (tok-n (cadr e)))
					     (= (length e) 4)))
				    (tok-n (car e))
				    (loop (cdr e))))))))
        
        ;; See `simple-unused-def?'. The `struct' case is more
        ;; complex, because multiple names might be assigned
        ;; in the same declaration.
        (define (unused-struc-typedef? e)
          (let ([once (lambda (s)
                        (and (not precompiling-header?)
                             (= 1 (hash-ref used-symbols 
                                                  (tok-n s)))))]
                [seps (list '|,| '* semi)])
            (let ([e (if (eq? '__extension__ (car e))
                         (cdr e)
                         e)])
              (and (eq? (tok-n (cadr e)) 'struct)
                   (brackets? (cadddr e))
                   (once (caddr e))
                   (let loop ([e (cddddr e)])
                     (cond
                       [(null? e) #t]
                       [(or (memq (tok-n (car e)) seps)
                            (braces? (car e))
                            (once (car e)))
                        (loop (cdr e))]
                       [else #f]))))))
        
        (define (struct-decl? e)
          (and (memq (tok-n (car e)) '(struct enum))
               (ormap braces? (cdr e))))
        
        (define (class-decl? e)
          (memq (tok-n (car e)) '(class)))
        
        ;; Recognize a function (as opposed to a prototype):
        (define (function? e)
          (let ([l (length e)])
            (and (> l 2)
                 (let* ([_n (tok-n (list-ref e (sub1 l)))]
                        [ll (if (eq? _n semi)
                                (- l 2)
                                (sub1 l))])
                   (let ([v (list-ref e ll)])
                     (and (braces? v)
                          (let ([v (list-ref e (sub1 ll))])
                            (or (parens? v)
                                (eq? (tok-n v) 'XFORM_SKIP_PROC)
                                (eq? (tok-n v) 'XFORM_ASSERT_NO_CONVERSION)
                                ;; `const' can appear between the arg parens
                                ;;  and the function body; this happens in the
                                ;;  OS X headers
                                (and (eq? 'const (tok-n v))
                                     (positive? (sub1 ll))
                                     (parens? (list-ref e (- ll 2))))))))))))

        (define (skip-function? e)
          (ormap (lambda (v) (eq? (tok-n v) 'XFORM_SKIP_PROC)) e))
        
        ;; Recognize a top-level variable declaration:
        (define (var-decl? e)
          (let ([l (length e)])
            (and (> l 2)
                 (eq? semi (tok-n (list-ref e (sub1 l)))))))
        
        (define (skip-static-line? e)
          ;; We want to skip the really-big static declaration for
          ;;  the inlined bytecodes in GRacket
          (let loop ([e e][l '(static unsigned char expr)])
            (cond
              [(null? l) #t]
              [(null? e) #f]
              [(eq? (tok-n (car e)) (car l))
               (loop (cdr e) (cdr l))]
              [else #f])))

	(define (clean-proto e)
	  ;; Strip __declspec(deprecated(...))
	  (if (and (eq? '__declspec (tok-n (car e)))
		   (parens? (cadr e))
		   (let ([l (seq->list (seq-in (cadr e)))])
		     (and (= 2 (length l))
			  (eq? 'deprecated (tok-n (car l)))
			  (parens? (cadr l)))))
	      ;; Drop __declspec
	      (cddr e)
	      ;; Nothing to drop
	      e))
        
        ;; e has been determined to be a function prototype.
        ;; Remember the information needed to convert calls
        ;; to e (especially the return type).
        (define (register-proto-information e)
          (parse-proto-information
           e
           (lambda (name class-name type args static?)
             (unless class-name
               (prototyped (cons (cons name (make-prototype 
                                             type
                                             (seq->list (seq-in args))
                                             static? #f #f))
                                 (prototyped))))
             name)))
        
        (define (parse-proto-information e k)
          (let loop ([e e][type null])
            (cond
              [(eq? '__declspec (tok-n (car e)))
               (loop (cddr e) type)]
              [(eq? '__attribute__ (tok-n (car e)))
               (loop (cddr e) type)]
              [(parens? (cadr e))
               (let ([name (tok-n (let ([p (car e)])
				    (if (parens? p)
					(car (seq->list (seq-in p)))
					p)))]
                     [type (let loop ([t (reverse type)])
                             (if (pair? t)
                                 (if (or (memq (tok-n (car t)) '(extern static virtual __stdcall __cdecl 
                                                                        inline _inline __inline __inline__
                                                                        __xform_nongcing__
                                                                        __xform_nongcing_nonaliasing__))
                                         (equal? "C" (tok-n (car t))))
                                     (loop (cdr t))
                                     (cons (car t) (loop (cdr t))))
                                 t))]
                     [static? (ormap (lambda (t) (eq? (tok-n t) 'static)) type)])
                 ;; Clean type if we find a method/constructor/destructor
                 (let-values ([(type class-name)
                               (if (and (list? type)
                                        ((length type) . >= . 2))
                                   (let ([rev-type (reverse type)])
                                     (cond
                                       [(eq? '|::| (tok-n (car rev-type)))
                                        (values (reverse (cddr rev-type)) (cadr rev-type))]
                                       [(and ((length type) . >= . 3)
                                             (eq? '~ (tok-n (car rev-type)))
                                             (eq? '|::| (tok-n (cadr rev-type))))
                                        (values (reverse (cdddr rev-type)) (caddr rev-type))]
                                       [else (values type #f)]))
                                   (values type #f))])
                   (k name
                      class-name
                      type
                      (cadr e)
                      static?)))]
              [else
               (loop (cdr e) (cons (car e) type))])))
        
        ;; prototype-for-pointer? : (cons sym prototype) -> bool
        ;; Returns #t if the prototype declares a function that returns
        ;; a pointer. This information is computed (based on the declaration)
        ;; the first time it is needed, and then cached.
        (define (prototype-for-pointer? m)
          (let ([name (car m)]
                [proto (cdr m)])
            (unless (prototype-pointer?-determined? proto)
              ;; We want to use `get-pointer-vars' to figure out the
              ;; answer, so invent a fake declaration and check it:
              (let ([e (append (prototype-type proto)
                               (list (make-tok name #f #f)
                                     (make-tok semi #f #f)))])
                (let ([vars (get-pointer-vars e "PROTODEF" #f #t)])
                  (set-prototype-pointer?! proto (not (null? vars)))
                  (set-prototype-pointer?-determined?! proto #t))))
            (prototype-pointer? proto)))
        
        (define (lookup-non-pointer-type t)
          (memq t non-pointer-types))
        (define (lookup-pointer-type t)
          (assq t pointer-types))
        (define (lookup-struct-def t)
          (assq t struct-defs))
        
        ;; e is a typedef; drop the "typedef" keyword and
        ;; parse it as a variable declaration using `get-vars', then extend
        ;; `pointer-types' and `non-pointer-types' based on the result.
        (define (check-pointer-type e)
          (let*-values ([(pointers non-pointers)
                         (get-vars ((if (eq? '__extension__ (car e))
                                        cddr
                                        cdr)
                                    e)
                                   "PTRDEF" #t #t)]
                        ;; Remove things like HANDLE and HWND, which are not
                        ;; malloced and could overlap with GCed areas:
                        [(pointers non-pointers)
                         (let ([l (filter (lambda (p)
                                            (memq (car p) non-pointer-typedef-names))
                                          pointers)])
                           (if (null? l)
                               (values pointers non-pointers)
                               (values (filter (lambda (p)
                                                 (not (memq (car p) non-pointer-typedef-names)))
                                               pointers)
                                       (append l non-pointers))))])
            (set! pointer-types (append pointers pointer-types))
            (set! non-pointer-types (append (map car non-pointers) non-pointer-types))))
        
        ;; get-vars : tok-list str bool bool -> (values list-of-(cons sym vtype) list-of-(cons sym vtype))
        ;; Parses a declaration  of one line (which may have multiple, comma-separated variables).
        ;; Returns a list of pointer declarations and a list of non-pointer declarations.
        (define (get-vars e comment union-ok? empty-array-is-ptr?)
          (let* ([e   (if (or (eq? GC_CAN_IGNORE (tok-n (car e)))
                              (eq? 'XFORM_CAN_IGNORE (tok-n (car e))))
                          (list (make-tok semi #f #f)) ; drop everything
                          (filter (lambda (x) (not (memq (tok-n x) '(volatile __volatile__ __volatile const)))) e))]
                 [base (tok-n (car e))]
                 [base-is-ptr?
                  (lookup-pointer-type base)]
                 [base-struct
                  (and (eq? base 'struct)
                       (if (or (braces? (cadr e)) (braces? (caddr e)))
                           (register-struct e)
                           (let ([m (lookup-struct-def (tok-n (cadr e)))])
                             (and m (car m)))))]
                 [minpos (if (or (eq? base 'struct)
                                 (eq? base 'union))
                             1
                             0)]
                 [non-ptr-base (cond
                                 [(eq? 'unsigned  (tok-n (car e)))
                                  (if (memq (tok-n (cadr e))
					    '(int long char intptr_t))
                                      (list 'unsigned (tok-n (cadr e)))
				      (void))]
                                 [(lookup-non-pointer-type (tok-n (car e)))
                                  (list (tok-n (car e)))]
                                 [else #f])])
            (let loop ([l (- (length e) 2)][array-size #f][pointers null][non-pointers null])
              (if (<= l minpos)
                  (values pointers non-pointers)
                  ;; Look back for "=" before comma:
                  (let ([skip (let loop ([l (sub1 l)])
                                (cond
                                  [(or (<= l minpos) 
                                       (eq? '|,| (tok-n (list-ref e l))))
                                   #f]
                                  [(eq? '= (tok-n (list-ref e l)))
                                   (sub1 l)]
                                  [else (loop (sub1 l))]))])
                    (if skip
                        ;; Skip assignment RHS:
                        (loop skip #f pointers non-pointers)
                        ;; Not assignment RHS:
                        (let ([v (list-ref e l)])
                          (cond
                            [(seq? v)
                             ;; Array? Struct?
                             (cond
                               [(brackets? v)
                                ;; Array decl:
                                (loop (sub1 l)
                                      (let ([inner (seq->list (seq-in (list-ref e l)))])
                                        (cond
                                         [(null? inner)
                                          (if empty-array-is-ptr?
                                              'pointer
                                              0)]
                                         [(= 1 (length inner))
                                          (tok-n (car inner))]
                                         [else 'unknown]))
                                      pointers non-pointers)]
                               [(braces? v) 
                                ;; No more variable declarations
                                (values pointers non-pointers)]
                               [else
                                ;; End of function ptr
                                ;; (and we don't care about func ptrs)
                                (values pointers non-pointers)])]
                            [(memq (tok-n v) '(int long char unsigned intptr_t void ulong uint uintptr_t))
                             ;; No more variable declarations
                             (values pointers non-pointers)]
                            [(memq (tok-n v) '(|,| * |:| 1))
                             (loop (sub1 l) #f pointers non-pointers)]
                            [else (let* ([name (tok-n v)]
                                         [pointer? (or (eq? 'pointer array-size)
                                                       (eq? '* (tok-n (list-ref e (sub1 l)))))]
                                         [star-count (+ (if (eq? 'pointer array-size)
                                                            1 
                                                            0)
                                                        (let loop ([l (sub1 l)])
                                                          (if (eq? '* (tok-n (list-ref e l)))
                                                              (add1 (loop (sub1 l)))
                                                              0)))]
                                         [base-struct (or base-struct
                                                          (and base-is-ptr?
                                                               (struc-type? (cdr base-is-ptr?))
                                                               (struc-type-struct (cdr base-is-ptr?))))]
                                         [union? (or (eq? base 'union)
                                                     (and base-is-ptr?
                                                          (union-type? (cdr base-is-ptr?))))]
                                         [struct-array? (or (and base-struct (not pointer?) (number? array-size))
                                                            (and base-is-ptr? (struct-array-type? (cdr base-is-ptr?))))]
                                         [array-size (if (or (number? array-size) (eq? array-size 'unknown))
                                                         array-size
                                                         (and struct-array?
                                                              (struct-array-type-count (cdr base-is-ptr?))))])
                                    (when (and struct-array?
                                               (not union-ok?)
                                               (and (number? array-size)
                                                    (> array-size 16)))
                                      (log-error "[SIZE] ~a in ~a: Large array of structures at ~a."
                                                 (tok-line v) (tok-file v) name))
                                    (when (and (not union-ok?)
                                               (not pointer?)
                                               (or union?
                                                   (and base-struct
                                                        (let has-union? ([base base-struct])
                                                          (let ([v (cdr (lookup-struct-def base))])
                                                            (ormap
                                                             (lambda (v)
                                                               (or (union-type? v)
                                                                   (and (struc-type? v)
                                                                        (has-union? (struc-type-struct v)))))
                                                             v))))))
                                      (log-warning "[UNION] ~a in ~a: Can't handle union or record with union, ~a."
                                                   (tok-line v) (tok-file v) name))
                                    (if (and (or pointer?
                                                 base-is-ptr?
                                                 base-struct
                                                 union?)
                                             ; Ignore these variables, for one reason or another:
                                             (not (memq name '(tcp_connect_dest_addr
                                                               tcp_listen_addr
                                                               tcp_here_addr
                                                               tcp_there_addr
                                                               tcp_accept_addr))))
                                        (begin
                                          (when show-info?
                                            (printf "/* ~a: ~a ~a*/\n" 
                                                    comment name
                                                    (cond
                                                      [struct-array?
                                                       (format "struct ~a[~a] " base-struct array-size)]
                                                      [(or (number? array-size) (eq? array-size 'unknown))
                                                       (format "[~a] " array-size)]
                                                      [(and base-struct (not pointer?))
                                                       (format "struct ~a " base-struct)]
                                                      [(and union? (not pointer?)) "union "]
                                                      [else (format "~a ~a* " (or (and base (list base))
                                                                                  non-ptr-base)
                                                                    star-count)])))
                                          (loop (sub1 l) #f 
                                                (cons (cons name
                                                            (cond
                                                              [struct-array?
                                                               (make-struct-array-type base-struct array-size)]
                                                              [(or (number? array-size) (eq? array-size 'unknown))
                                                               (make-array-type array-size)]
                                                              [pointer? (make-pointer-type (or (and base (list base))
                                                                                               non-ptr-base)
                                                                                           star-count)]
                                                              [base-struct
                                                               (make-struc-type base-struct)]
                                                              [union?
                                                               (make-union-type)]
                                                              [else
                                                               (make-pointer-type (or (and base (list base))
                                                                                      non-ptr-base)
                                                                                  star-count)]))
                                                      pointers)
                                                non-pointers))
                                        (begin
                                          (when (and base (find-c++-class base #f))
                                            (log-error "[INST] ~a in ~a: Static instance of class ~a."
                                                       (tok-line (car e)) (tok-file (car e)) base))
                                          (when show-info?
                                            (printf "/* NP ~a: ~a */\n" 
                                                    comment name))
                                          (loop (sub1 l) #f pointers (cons (cons name 
                                                                                 (make-non-pointer-type non-ptr-base)) 
                                                                           non-pointers)))))]))))))))
        
        (define (get-pointer-vars e comment union-ok? empty-array-is-ptr?)
          (let-values ([(pointers non-pointers)
                        (get-vars e comment union-ok? empty-array-is-ptr?)])
            pointers))
        
        (define (get-pointer-vars-from-seq body comment comma-sep?)
          (let-values ([(pragmas el) (body->lines body comma-sep?)])
            (apply
             append
             (map (lambda (e)
                    (get-pointer-vars e comment #t #f))
                  el))))
        
        ;; e is a struct decl; parse it an remember the results
        (define (register-struct e)
          (let ([body (seq->list (seq-in (if (braces? (cadr e))
                                             (cadr e)
                                             (caddr e))))]
                [name (if (braces? (cadr e))
                          (gensym 'Anonymous)
                          (tok-n (cadr e)))])
            (let ([l (get-pointer-vars-from-seq body "PTRFIELD" #f)])
              (and (not (null? l))
                   (begin
                     (set! struct-defs (cons (cons name l) struct-defs))
                     name)))))
        
        ;; This is for PalmOS conversion with SEGOF decls.
        (define (add-segment-label name e)
          (let loop ([e e])
            (cond
              [(null? (cdr e))
               (fprintf map-port "(decl ~s)\n" name)
               (list (make-tok (string->symbol (format "SEGOF_~a" name))
                               #f #f)
                     (car e))]
              [(memq (tok-n (car e)) (list __attribute__))
               ;; No segment wanted
               e]
              [else
               (cons (car e) (loop (cdr e)))])))
        
        
        ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        ;; Transformations
        ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        
        ;; type->decl : vtype tok[for errs] -> seq list
        ;; Creates a type declaration based on a type struct (without the name of
        ;; a declared variable).
        (define (type->decl x where-v)
          (cond
            [(and (non-pointer-type? x)
                  (non-pointer-type-base x))
             (map (lambda (x) (make-tok x #f #f)) (non-pointer-type-base x))]
            [(and (pointer-type? x) (pointer-type-base x))
             (append (map (lambda (x) (make-tok x #f #f)) (pointer-type-base x))
                     (let loop ([n (pointer-type-stars x)])
                       (if (zero? n)
                           null
                           (cons (make-tok '* #f #f) (loop (sub1 n))))))]
            [else (log-error "[TYPE] ~a in ~a: Can't render type declaration for ~a"
                             (tok-line where-v) (tok-file where-v)
                             x)
                  (list (make-tok '??? #f #f))]))
        
        ;; Takes a class-decl, parses it, and records the information.
        ;; The basic strategy is to parse the class body as a top-level
        ;; sequence, and then move the collected info into the class
        ;; record. As the same time, we re-arrange the constructor
        ;; and put the work into a gcInit_ method to be called explicitly.
        ;; We also manufactor the gcMark and gcFixup methods.
        (define (register-class e)
          (let ([name (tok-n (cadr e))]
                [body-pos (if (eq? '|:| (tok-n (caddr e)))
                              (if (memq (tok-n (cadddr e)) '(public private))
                                  5
                                  4)
                              2)])
            (unless (braces? (list-ref e body-pos))
              (error 'xform "Confused by form of class declaration at line ~a in ~a"
                     (tok-line (car e))
                     (tok-file (car e))))
            (let* ([super (if (> body-pos 2)
                              (tok-n (list-ref e (sub1 body-pos)))
                              #f)]
                   [cl (make-c++-class super
                                       (if (or super (eq? name 'gc))
                                           super
                                           'gc)
                                       null
                                       null)]
                   [pt (prototyped)]
                   [vs (top-vars)])
              (set! c++-classes (cons (cons name cl) c++-classes))
              (prototyped null)
              (top-vars null)
              (let* ([body-v (list-ref e body-pos)]
                     [body-e (process-top-level (seq->list (seq-in body-v)) ".h" #f)]
                     [methods (prototyped)])
                ;; Save prototype list, but remove constructor and statics:
                (set-c++-class-prototyped! cl (filter (lambda (x)
                                                        (not (or (eq? (car x) name)
                                                                 (prototype-static? (cdr x)))))
                                                      methods))
                (set-c++-class-top-vars! cl (top-vars))
                (prototyped pt)
                (top-vars vs)
                (if (not (or (eq? 'gc (tok-n (caddr e)))
                             (assoc 'gc c++-classes)))
                    ;; primitive class, before `gc' defn
                    e
                    ;; normal class:
                    (let loop ([e e][p body-pos])
                      (if (zero? p)
                          (append
                           (if (or super (eq? name 'gc))
                               null
                               (list
                                (make-tok '|:| #f #f)
                                (make-tok 'public #f #f)
                                (make-tok 'gc #f #f)))
                           (cons (make-braces
                                  (tok-n body-v)
                                  (tok-line body-v)
                                  (tok-file body-v)
                                  (seq-close body-v)
                                  (list->seq
                                   (append
                                    
                                    ;; Replace constructors names with gcInit_ names
                                    (let loop ([e body-e][did-one? #f])
                                      (cond
                                        [(null? e) (if did-one?
                                                       null
                                                       ;; Need an explicit gcInit_ method:
                                                       (list
                                                        (make-tok 'inline #f #f)
                                                        (make-tok 'void #f #f)
                                                        (make-gc-init-tok name)
                                                        (make-parens "(" #f #f ")" (seqce))
                                                        (make-braces "{" #f #f "}" 
                                                                     (if super
                                                                         (seqce
                                                                          (make-tok 'this #f #f)
                                                                          (make-tok '-> #f #f)
                                                                          (make-gc-init-tok super)
                                                                          (make-parens "(" #f #f ")" (seqce))
                                                                          (make-tok semi #f #f))
                                                                         (seqce)))))]
                                        [(eq? (tok-n (car e)) '~)
                                         ;; destructor
                                         (cons (car e) (cons (cadr e) (loop (cddr e) did-one?)))]
                                        [(and (eq? (tok-n (car e)) name)
                                              (parens? (cadr e)))
                                         ;; constructor
                                         (cons (make-tok 'void #f #f)
                                               (cons (make-gc-init-tok (tok-n (car e)))
                                                     (loop (cdr e) #t)))]
                                        [else (cons (car e) (loop (cdr e) did-one?))]))
                                    
                                    (if (or (eq? name 'gc)
                                            (assq gcMark (c++-class-prototyped cl)))
                                        ;; Don't add to gc or to a class that has it
                                        null
                                        
                                        ;; Add gcMark and gcFixup methods:
                                        (let ([mk-proc
                                               (lambda (name marker)
                                                 (list
                                                  (make-tok 'inline #f #f)
                                                  (make-tok 'void #f #f)
                                                  (make-tok name #f #f)
                                                  (make-parens
                                                   "(" #f #f ")"
                                                   (seqce))
                                                  (make-braces
                                                   "{" #f #f "}"
                                                   (list->seq
                                                    (make-mark-body name marker
                                                                    (or super 'gc)
                                                                    (c++-class-top-vars cl)
                                                                    (car e))))))])
                                          (append
                                           (list
                                            (make-tok 'public #f #f)
                                            (make-tok '|:| #f #f))
                                           ;; gcMark method:
                                           (mk-proc gcMark gcMARK_TYPED)
                                           ;; gcFixup method:
                                           (mk-proc gcFixup gcFIXUP_TYPED)))))))
                                 (cdr e)))
                          
                          (cons (car e) (loop (cdr e) (sub1 p))))))))))
        
        ;; Builds the body of a gcMark or gcFixup method
        (define (make-mark-body name marker super vars where-v)
          (let ([pointers (append
                           (filter (lambda (x)
                                     (not (non-pointer-type? (cdr x))))
                                   vars))])
            (append
             (list
              (make-tok super #f #f)
              (make-tok '|::| #f #f)
              (make-tok name #f #f)
              (make-parens
               "(" #f #f ")"
               (seqce))
              (make-tok semi #f #f))
             (if (null? pointers)
                 null
                 (apply
                  append
                  (map (lambda (x)
                         (list
                          (make-tok marker #f #f)
                          (make-parens
                           "(" #f #f ")"
                           (list->seq
                            (append
                             (type->decl (cdr x) where-v)
                             (list (make-tok '|,| #f #f)
                                   (make-tok (car x) #f #f)))))
                          (make-tok semi #f #f)))
                       pointers))))))
        
        (define (find-c++-class class-name report-err?)
          (and class-name
               (let ([m (assoc class-name c++-classes)])
                 (if m
                     (cdr m)
                     (begin
                       (when report-err?
                         (log-error "[CLASS]: Unknown class ~a."
                                    class-name))
                       #f)))))
        
        (define (get-c++-class-member var c++-class c++-class-members)
          (and c++-class
               (let ([m (assoc var (c++-class-members c++-class))])
                 (or m
                     (let ([parent (c++-class-parent c++-class)])
                       (and parent
                            (if (c++-class? parent)
                                (get-c++-class-member var parent c++-class-members)
                                (let ([parent (find-c++-class parent #t)])
                                  (set-c++-class-parent! c++-class parent)
                                  (get-c++-class-member var parent c++-class-members)))))))))
        
        (define (get-c++-class-var var c++-class)
          (get-c++-class-member var c++-class c++-class-top-vars))
        
        (define (get-c++-class-method var c++-class)
          (get-c++-class-member var c++-class c++-class-prototyped))
        
        ;; Temporary state used during a conversion:
        (define used-self? #f)
        (define important-conversion? #f)
	(define saw-gcing-call #f)
        
        (define (new-vars->decls vars)
          (apply
           append
           (map
            (lambda (tv)
              (list (make-tok (car tv) #f #f)
                    (make-tok '* #f #f)
                    (make-tok (cdr tv) #f #f)
                    (make-tok semi #f #f)))
            vars)))
        
        (define (make-gc-init-tok s)
          (make-tok (string->symbol (format "gcInit_~a" s)) #f #f))
        
        ;; e is a function definition. Convert its body (if necessary)
        ;; to register locals with the GC. Do a little special work
        ;; for constructors, detected by a '|:| outside the body.
        (define (convert-function e name)
          (let*-values ([(body-v len) (let* ([len (sub1 (length e))]
                                             [v (list-ref e len)])
                                        ;; Function may have trailing semicolon:
                                        (if (eq? semi (tok-n v))
                                            (values (list-ref e (sub1 len)) (sub1 len))
                                            (values v len)))]
                        [(assert-no-conversion?)
                         (eq? (tok-n (list-ref e (sub1 len)))
                              'XFORM_ASSERT_NO_CONVERSION)]
                        [(body-e) (seq->list (seq-in body-v))]
                        [(class-name function-name func-pos) 
                         (let loop ([e e][p 0])
                           (cond
                             [(null? e) (values #f #f #f)]
                             [(null? (cdr e)) (values #f #f #f)]
                             [(eq? '|::| (tok-n (cadr e)))
                              (values (tok-n (car e))
                                      (tok-n (caddr e))
                                      (+ p 2))]
                             [else (loop (cdr e) (add1 p))]))]
                        [(args-e) (seq->list (seq-in (list-ref e (if (and func-pos
                                                                          (eq? class-name function-name))
                                                                     (add1 func-pos)
                                                                     (if assert-no-conversion?
                                                                         (- len 2)
                                                                         (sub1 len))))))]
                        [(arg-vars all-arg-vars) 
                         (let-values ([(arg-pragmas arg-decls) (body->lines (append
                                                                             args-e
                                                                             (list (make-tok '|,| #f #f)))
                                                                            #t)])
                           (unless (null? arg-pragmas)
                             (error 'arg-decls "unexpected pragmas"))
                           (let loop ([l arg-decls][arg-vars null][all-arg-vars null])
                             (if (null? l)
                                 (values arg-vars all-arg-vars) 
                                 (let-values ([(ptrs non-ptrs) (get-vars (car l) "PTRARG" #f #t)])
                                   (loop (cdr l) (append arg-vars ptrs) (append all-arg-vars ptrs non-ptrs))))))]
                        [(c++-class) (let ([c++-class (find-c++-class class-name #t)])
                                       (and c++-class
                                            (or (get-c++-class-method function-name c++-class)
                                                (eq? function-name class-name)
                                                (eq? function-name '~))
                                            c++-class))]
                        [(initializers) (let loop ([e e][len len])
                                          (cond
                                            [(zero? len) #f]
                                            [(eq? (tok-n (car e)) '|:|)
                                             (cons (cadr e) (caddr e))]
                                            [else (loop (cdr e) (sub1 len))]))])
            (append
             
             ;; Build all of the function declaration up to the body:
             (let loop ([e e][len len][need-void? #t])
               (cond
                 [(zero? len)
                  null]
                 [(eq? (tok-n (car e)) '|:|)
                  ;; skip initializers
                  null]
                 [(and function-name
                       (eq? function-name class-name)
                       (eq? (tok-n (car e)) class-name)
                       (parens? (cadr e)))
                  ;; Replace constructor name with gcInit_ name:
                  (cons (make-gc-init-tok (tok-n (car e)))
                        (loop (cdr e) (sub1 len) #f))]
                 [(eq? (tok-n (car e)) 'inline)
                  ;; Don't want 'void before 'inline
                  (cons (car e) (loop (cdr e) (sub1 len) need-void?))]
                 [else
                  (if (and need-void?
                           function-name
                           (eq? function-name class-name))
                      (cons (make-tok 'void #f #f)
                            (loop e len #f))
                      (cons (car e) 
                            (loop (cdr e) (sub1 len) #f)))]))
             (list
              (make-braces
               (tok-n body-v)
               (tok-line body-v)
               (tok-file body-v)
               (seq-close body-v)
               (let-values ([(orig-body-e) (begin
                                             (set! important-conversion? #f)
                                             (set! saw-gcing-call #f)
                                             body-e)]
                            [(body-e live-vars)
                             ;; convert-body does most of the conversion work, and also
                             ;; introduces the PREPARE_VAR_STACK decl, since the last arg
                             ;; provided here is not #f.
                             (convert-body (if c++-class
                                               (let* ([new-vars-box (box null)]
                                                      [e (begin
                                                           (set! used-self? #f)
                                                           (convert-class-vars body-e all-arg-vars c++-class new-vars-box))])
                                                 (append
                                                  ;; If sElF is used, add its declaration.
                                                  (if (or used-self?
                                                          (and function-name
                                                               (eq? class-name function-name)))
                                                      (list
                                                       (make-tok class-name #f #f)
                                                       (make-tok '* #f #f)
                                                       (make-tok sElF #f #f)
                                                       (make-tok '= #f #f)
                                                       (make-tok 'this #f #f)
                                                       (make-tok semi #f #f))
                                                      null)
                                                  ;; New vars for obj creation:
                                                  (new-vars->decls (unbox new-vars-box))
                                                  ;; The main body:
                                                  e))
                                               
                                               ;; Do any conversion?
                                               (if source-is-c++?
                                                   (let* ([new-vars-box (box null)]
                                                          [e (convert-class-vars body-e all-arg-vars #f new-vars-box)])
                                                     (append
                                                      (new-vars->decls (unbox new-vars-box))
                                                      e))
                                                   body-e))
                                           arg-vars arg-vars #f
                                           c++-class 
                                           ;; Moved initializers, if constructor
                                           (if (and function-name
                                                    (eq? class-name function-name))
                                               (let ([super-type (if initializers
                                                                     (tok-n (car initializers))
                                                                     (c++-class-parent-name c++-class))]
                                                     [super-args (if initializers
                                                                     (cdr initializers)
                                                                     (make-parens "(" #f #f ")" (seqce)))])
                                                 (list (list (make-tok sElF #f #f)
                                                             (make-tok '-> #f #f)
                                                             (make-gc-init-tok super-type)
                                                             super-args
                                                             (make-tok semi #f #f))))
                                               null)
                                           (lambda () null)
                                           ;; Initially, no live vars, no introduiced vars, etc.:
                                           (make-live-var-info #f -1 0 null null null 0 0 0 #f) 
                                           ;; Add PREPARE_VAR_STACK and ensure result return:
                                           (parse-proto-information
                                            e
                                            (lambda (name class-name type args static?)
                                              type)))])
		 (if (hash-ref non-gcing-functions name #f)
		     (when saw-gcing-call
		       (log-error "[GCING] ~a in ~a: Function ~a declared __xform_nongcing__, but includes a function call at ~s."
				  (tok-line saw-gcing-call) (tok-file saw-gcing-call)
				  name
                                  (tok-n saw-gcing-call)))
		     (unless saw-gcing-call
		       '
		       (eprintf "[SUGGEST] Consider declaring ~a as __xform_nongcing__.\n"
				name)))
                 (if (and (not important-conversion?)
                          (not (and function-name
                                    (eq? class-name function-name)))
			  (or (not saw-gcing-call)
			      (and
			       (null? (live-var-info-new-vars live-vars))
			       (zero? (live-var-info-maxpush live-vars))
			       (or (<= (live-var-info-num-calls live-vars) 1)
				   (= (live-var-info-num-calls live-vars)
				      (+ (live-var-info-num-empty-calls live-vars)
                                         (live-var-info-num-noreturn-calls live-vars)))))))
                     ;; No conversion necessary. (Lack of `call' records means no GC-setup
                     ;; work when printing out the function.)
                     (list->seq
                      (cons
                       (make-note 'note #f #f "/* No conversion */")
                       orig-body-e))
                     (begin
                       (when assert-no-conversion?
                         (log-error "[CONVERSION] ~a in ~a: Function ~a declared XFORM_ASSERT_NO_CONVERSION, but requires conversion."
                                    (tok-line (car e)) (tok-file (car e))
                                    name))
                       (list->seq body-e)))))))))

        (define (convert-class-vars body-e arg-vars c++-class new-vars-box)
          (when c++-class
            (let-values ([(pragmas el) (body->lines body-e #f)])
              (let-values ([(decls body) (split-decls el)])
                (for-each (lambda (e) 
                            (let-values ([(pointers non-pointers) (get-vars e "CVTLOCAL" #f #t)])
                              (for-each
                               (lambda (var)
                                 (when (get-c++-class-var (car var) c++-class)
                                   (log-error "[SHADOW++] ~a in ~a: Class variable ~a shadowed in decls."
                                              (tok-line (caar decls)) (tok-file (caar decls))
                                              (car var))))
                               (append pointers non-pointers))))
                          decls))))
          (let loop ([e body-e][can-convert? #t][paren-arrows? #t])
            (cond
             [(null? e) null]
             [(skip-static-line? e)
              ;; Jump to semicolon:
              (let jloop ([e e])
                (if (eq? semi (tok-n (car e)))
                    (loop e can-convert? paren-arrows?)
                    (cons (car e) (jloop (cdr e)))))]
             [(and can-convert?
                   c++-class
                   (pair? (cdr e))
                   (eq? (tok-n (cadr e)) '|::|)
                   (find-c++-class (tok-n (car e)) #f))
              ;; Maybe class-qualified method invocation. See
              ;;  what happens if we remove the qualification
              (let ([rest (loop (cddr e) #t paren-arrows?)])
                (if (eq? sElF (tok-n (car rest)))
                    (list* (car rest)
                           (cadr rest)
                           (car e)
                           (cadr e)
                           (cddr rest))
                    (list* (car e)
                           (cadr e)
                           rest)))]
             [else
              (let ([v (car e)])
                (cond
                 [(pragma? v)
                  (cons v (loop (cdr e) can-convert? paren-arrows?))]
                 [(memq (tok-n v) '(|.| -> |::|))
                  ;; Don't check next as class member
                  (cons v (loop (cdr e) #f paren-arrows?))]
                 [(eq? (tok-n v) 'delete)
                  ;; Make `delete' expression look like a function call
                  (let ([arr? (brackets? (cadr e))])
                    (loop (list*
                           (make-tok (if arr? DELETE_ARRAY DELETE)
                                     (tok-line v) (tok-file v))
                           (make-parens
                            "(" (tok-line v) (tok-file v) ")"
                            (seqce ((if arr? caddr cadr) e)))
                           ((if arr? cdddr cddr) e))
                          #t
                          paren-arrows?))]
                 [(eq? (tok-n v) 'delete_wxobject)
                  ;; replace with call to GC_cpp_delete()
                  (set! important-conversion? #t)
                  (when (brackets? (cadr e))
                    (log-error "[DELOBJ] ~a in ~a: bad use of delete_wxobject"
                               (tok-line v) (tok-file v)))
                  (loop (list*
                         (make-tok GC_cpp_delete (tok-line v) (tok-file v))
                         (make-parens
                          "(" (tok-line v) (tok-file v) ")"
                          (seqce (cadr e)))
                         (cddr e))
                        #t
                        paren-arrows?)]
                 [(eq? (tok-n v) 'new)
                  ;; Make `new' expression look like a function call
                  (set! important-conversion? #t)
                  (let* ([t (cadr e)]
                         [obj? (find-c++-class (tok-n t) #f)]
                         [atom? (lookup-non-pointer-type (tok-n t))])
                    (unless (or obj? atom?)
                      (log-error "[NEW] ~a in ~a: New used on non-class"
                                 (tok-line (car e)) (tok-file (car e))))
                    
                    (cond
                     [(and (pair? (cddr e))
                           (eq? '* (tok-n (caddr e)))
                           (pair? (cdddr e))
                           (brackets? (cadddr e)))
                      ;; Array of pointers
                      (loop (list*
                             (make-tok NEW_PTR_ARRAY
                                       (tok-line v) (tok-file v))
                             (make-parens
                              "(" (tok-line v) (tok-file v) ")"
                              (seqce (cadr e) 
                                     (make-tok '|,| #f #f) 
                                     (cadddr e)))
                             (cddddr e))
                            #t
                            paren-arrows?)]
                     [(and (pair? (cddr e))
                           (eq? '* (tok-n (caddr e))))
                      ;; A pointer
                      (loop (list*
                             (make-tok NEW_PTR 
                                       (tok-line v) (tok-file v))
                             (make-parens
                              "(" (tok-line v) (tok-file v) ")"
                              (seqce (cadr e) (caddr e)))
                             (cdddr e))
                            #t
                            paren-arrows?)]
                     [(and (pair? (cddr e))
                           (brackets? (caddr e)))
                      ;; An array of objects
                      (unless (or atom? (eq? #cs'wxPoint (tok-n t)))
                        (log-warning "[ARRAY] ~a in ~a: array of ~a objects, allocating as array of atomic."
                                     (tok-line t) (tok-file t)
                                     (tok-n t)))
                      (loop (list*
                             (make-tok (if atom? 
                                           NEW_ATOM_ARRAY 
                                           NEW_ARRAY)
                                       #f #f)
                             (make-parens
                              "(" (tok-line v) (tok-file v) ")"
                              (seqce (cadr e) 
                                     (make-tok '|,| #f #f) 
                                     (caddr e)))
                             (cdddr e))
                            #t
                            paren-arrows?)]
                     [(or (and (pair? (cddr e))
                               (parens? (caddr e)))
                          (not atom?))
                      ;; An object with init argument
                      (when atom?
                        (log-error "[CONFUSED] ~a in ~a: atomic type with initializers?"
                                   (tok-line v) (tok-file v)))
                      (let ([args? (and (pair? (cddr e))
                                        (parens? (caddr e)))]
                            [line (tok-line v)]
                            [file (tok-file v)]
                            [new-var (string->symbol (format "~a_created" (tok-n (cadr e))))])
                        (unless (assq (tok-n (cadr e)) (unbox new-vars-box))
                          (set-box! new-vars-box (cons (cons (tok-n (cadr e)) new-var)
                                                       (unbox new-vars-box))))
                        (loop (list*
                               (make-creation-parens
                                "(" line file ")"
                                (seqce
                                 (make-tok new-var line file) 
                                 (make-tok '= line file) 
                                 (make-tok NEW_OBJ line file)
                                 (make-parens
                                  "(" line file ")"
                                  (seqce (cadr e)))
                                 (make-tok '|,| line file) 
                                 (make-tok new-var line file) 
                                 (make-tok '-> line file) 
                                 (make-gc-init-tok (tok-n (cadr e)))
                                 (if args?
                                     (caddr e)
                                     (make-parens
                                      "(" line file ")"
                                      (seqce)))
                                 (make-tok '|,| line file) 
                                 (make-tok new-var line file)))
                               ((if args? cdddr cddr) e))
                              #t
                              paren-arrows?))]
                     [else
                      ;; An atom
                      (loop (list*
                             (make-tok NEW_ATOM (tok-line v) (tok-file v))
                             (make-parens
                              "(" (tok-line v) (tok-file v) ")"
                              (seqce (cadr e)))
                             (cddr e))
                            #t
                            paren-arrows?)]))]
                 [(and can-convert?
                       c++-class
                       (pair? (cdr e))
                       (parens? (cadr e))
                       (get-c++-class-method (tok-n v) c++-class))
                  ;; method call:
                  (set! used-self? #t)
                  (list*
                   (make-tok sElF (tok-line v) (tok-file v))
                   (make-tok '-> (tok-line v) (tok-file v))
                   v
                   (loop (cdr e) #t paren-arrows?))]
                 [(and paren-arrows?
                       (>= (length e) 3)
                       (eq? '-> (tok-n (cadr e)))
                       (or (null? (cdddr e))
                           (not (or (parens? (cadddr e))
                                    (eq? '|::| (tok-n (cadddr e)))))))
                  (loop (cons (make-parens
                               "(" #f #f ")"
                               (seqce (car e) (cadr e) (caddr e)))
                              (cdddr e))
                        can-convert?
                        #t)]
                 [else
                  ;; look for conversion
                  (cons
                   (cond
                    [(braces? v)
                     (make-braces
                      "{" (tok-line v) (tok-file v) "}"
                      (list->seq (convert-class-vars (seq->list (seq-in v)) arg-vars c++-class new-vars-box)))]
                    [(seq? v)
                     ((get-constructor v)
                      (tok-n v) (tok-line v) (tok-file v) (seq-close v)
                      (list->seq (loop (seq->list (seq-in v)) #t #f)))]
                    [(and can-convert? (eq? (tok-n v) 'this))
                     (set! used-self? #t)
                     (make-tok sElF (tok-line v) (tok-file v))]
                    [(and can-convert?
                          c++-class
                          (not (assq (tok-n v) arg-vars))
                          (get-c++-class-var (tok-n v) c++-class))
                     (set! used-self? #t)
                     (make-parens
                      "(" (tok-line v) (tok-file v) ")"
                      (seqce (make-tok sElF (tok-line v) (tok-file v))
                             (make-tok '-> (tok-line v) (tok-file v))
                             v))]
                    [else v])
                   (loop (cdr e) #t paren-arrows?))]))])))
        
        (define re:funcarg (regexp "^__funcarg"))
        (define (is-generated? x)
          (regexp-match re:funcarg (symbol->string (car x))))
        
        ;; body-e is something in {} or (). Convert the body with
        ;; `convert-function-calls' (which does the actual statement-level
        ;; inspection), and add appropriate body headers.  Some of the
        ;; work here is distinguishing decls from body code.
        ;; The result is two values: converted body, and a new live-vars
        ;; record.
        (define (convert-body body-e extra-vars pushable-vars &-vars c++-class initializers after-vars-thunk live-vars setup-stack-return-type)
          (let-values ([(&-vars) (or &-vars (find-&-vars body-e))]
                       [(pragmas el) (body->lines body-e #f)])
            (let-values ([(decls body) (split-decls el)])
              (let* ([local-vars 
                      (apply
                       append
                       (map (lambda (e) 
                              (if (eq? (tok-n (car e)) 'static)
                                  null
                                  (get-pointer-vars e "PTRLOCAL" #f #t)))
                            decls))]
                     [vars (begin
                             (ormap (lambda (var)
                                      (when (assq (car var) extra-vars)
                                        (log-error "[SHADOW] ~a in ~a: Pointerful variable ~a shadowed in decls."
                                                   (tok-line (caar decls)) (tok-file (caar decls))
                                                   (car var))))
                                    
                                    local-vars)
                             (append extra-vars local-vars))])
                ;; Convert calls and body (recusively)
                (let-values ([(orig-maxlive) (live-var-info-maxlive live-vars)]
                             [(orig-maxpush) (live-var-info-maxpush live-vars)]
                             [(orig-tag) (live-var-info-tag live-vars)]
                             [(body-x live-vars)
                              (let loop ([body (append initializers body)])
                                (cond
                                  [(null? body)
                                   ;; Starting live-vars record for this block:
                                   ;;  Create new tag
                                   ;;  Locally-defined arrays, records, and & variables, are always live.
                                   ;;  Start with -1 maxlive in case we want to check whether anything
                                   ;;   was pushed in the block.
                                   (values null (make-live-var-info (gentag)
                                                                    -1
                                                                    0
                                                                    (append
								     (let loop ([vars extra-vars])
								       (cond
									[(null? vars) null]
									[(memq (caar vars) &-vars)
									 (cons (car vars) (loop (cdr vars)))]
									[else (loop (cdr vars))]))
                                                                     (let loop ([vars local-vars])
                                                                       (cond
                                                                         [(null? vars) null]
                                                                         [(or (array-type? (cdar vars))
                                                                              (struc-type? (cdar vars))
									      (memq (caar vars) &-vars))
                                                                          (cons (car vars) (loop (cdr vars)))]
                                                                         [else (loop (cdr vars))]))
                                                                     (live-var-info-vars live-vars))
                                                                    (live-var-info-new-vars live-vars)
                                                                    (live-var-info-pushed-vars live-vars)
                                                                    (live-var-info-num-calls live-vars)
                                                                    (live-var-info-num-noreturn-calls live-vars)
                                                                    (live-var-info-num-empty-calls live-vars)
                                                                    (live-var-info-nonempty-calls? live-vars)))]
                                  [(memq (tok-n (caar body)) '(START_XFORM_SKIP XFORM_START_SKIP))
                                   (let skip-loop ([body (cdr body)])
                                     (let*-values ([(end?) (memq (tok-n (caar body)) '(END_XFORM_SKIP XFORM_START_SKIP))]
                                                   [(rest live-vars) ((if end?
                                                                          loop
                                                                          skip-loop)
                                                                      (cdr body))])
                                       (values (if end? rest (cons (car body) rest)) live-vars)))]
                                  [(eq? (tok-n (caar body)) XFORM_RESET_VAR_STACK)
                                   (let-values ([(rest live-vars) (loop (cdr body))])
                                     (values (cons (car body) rest) live-vars))]
                                  [else
                                   (when (body-var-decl? (car body))
                                     (let ([type (tok-n (caar body))]
                                           [var (let loop ([e (car body)])
                                                  (if (or (null? (cdr e))
                                                          (eq? semi (tok-n (cadr e))))
                                                      (tok-n (car e))
                                                      (loop (cdr e))))])
                                       (unless (or (eq? '|::| type) (eq? '|::| (tok-n (cadar body)))) ;; $patch vs2008 - goetter
                                         (log-error "[DECL] ~a in ~a: Variable declaration (~a ~a) not at the beginning of a block."
                                                    (tok-line (caar body)) (tok-file (caar body))
                                                    type var))))
                                   (let*-values ([(rest live-vars) (loop (cdr body))]
                                                 [(e live-vars)
                                                  (if (skip-static-line? (car body))
                                                      (values (car body) live-vars)
                                                      ;; Here's the main body work:
                                                      (convert-function-calls (car body)
                                                                              vars
                                                                              &-vars
                                                                              c++-class
                                                                              live-vars
                                                                              #f #f #f))])
                                     (values (cons e rest) live-vars))]))])
                  ;; Collect live vars and look for function calls in decl section.
                  (let ([live-vars
                         (let loop ([decls decls][live-vars live-vars])
                           (if (null? decls)
                               live-vars
                               (let dloop ([el (let-values ([(pragmas el) (body->lines (car decls) #t)])
                                                 el)]
                                           [live-vars live-vars])
                                 (if (null? el)
                                     (loop (cdr decls) live-vars)
                                     (let-values ([(_ live-vars)
                                                   ;; We're not really interested in the conversion.
                                                   ;; We just want to get live vars and
                                                   ;; complain about function calls in the decl area:
                                                   (convert-function-calls (car el) extra-vars &-vars c++-class live-vars "decls" #f #t)])
                                       (dloop (cdr el) live-vars))))))])
                    ;; Calculate vars to push in this block. Make sure there are no duplicates.
                    (let ([newly-pushed (let ([ht (make-hasheq)])
                                          (for-each (lambda (x)
                                                      (when (or (assq (car x) local-vars)
                                                                (assq (car x) pushable-vars)
                                                                (and setup-stack-return-type
                                                                     (is-generated? x)))
                                                        (hash-set! ht (car x) x)))
                                                    (live-var-info-pushed-vars live-vars))
                                          (hash-map ht (lambda (k v) v)))])
                      (values (apply
                               append
                               pragmas
                               (append
                                decls
                                (list (after-vars-thunk))
                                (list (let* ([vs-size (if per-block-push?
                                                          (+ (total-push-size newly-pushed)
                                                             (live-var-info-maxpush live-vars))
                                                          (live-var-info-maxlive live-vars))]
                                             [once? (and setup-stack-return-type
                                                         (= (total-push-size newly-pushed) vs-size))])
                                        (append (if show-info?
                                                    (list (make-note 'note #f #f (format "/* PTRVARS: ~a */" (map car vars))))
                                                    null)
                                                (if setup-stack-return-type
                                                    (apply append (live-var-info-new-vars live-vars))
                                                    null)
                                                (if (and setup-stack-return-type
                                                         ;; Look for RET_VALUE_START anywhere:
                                                         (let loop ([e body-x])
                                                           (cond
                                                             [(list? e) (ormap loop e)]
                                                             [(pair? e) (or (loop (cdr e))
                                                                            (loop (car e)))]
                                                             [(seq? e) (ormap loop (seq->list (seq-in e)))]
                                                             [(and (tok? e) (eq? RET_VALUE_START (tok-n e)))
                                                              #t]
                                                             [else #f])))
                                                    (list (make-tok DECL_RET_SAVE #f #f)
                                                          (make-nosrc-parens
                                                           "(" #f #f ")"
                                                           (list->seq setup-stack-return-type)))
                                                    null)
                                                (if (and setup-stack-return-type (not (negative? (live-var-info-maxlive live-vars))))
                                                    (list (make-note 'note #f #f 
                                                                     (format "PREPARE_VAR_STACK~a(~a);" 
                                                                             (if once?
                                                                                 "_ONCE"
                                                                                 "")
                                                                             vs-size)))
                                                    
                                                    null)
                                                (if (negative? (live-var-info-maxlive live-vars))
                                                    null
                                                    (list (make-block-push
                                                           "block push"
                                                           #f #f
                                                           newly-pushed (live-var-info-tag live-vars) orig-tag
                                                           setup-stack-return-type)))
                                                (if setup-stack-return-type
                                                    (if once?
                                                        (list no-nested-pushable)
                                                        (list nested-pushable))
                                                    null))))
                                ;; Null out local vars:
                                (map (lambda (var)
                                       ;; Check that the variable isn't specifically initialized:
                                       (if (let loop ([decls decls])
                                             (and (pair? decls)
                                                  (or (let loop ([e (car decls)])
                                                        (and (pair? e)
                                                             (pair? (cdr e))
                                                             (or (and (eq? (car var) (tok-n (car e)))
                                                                      (eq? '= (tok-n (cadr e))))
                                                                 (loop (cdr e)))))
                                                      (loop (cdr decls)))))
                                           null
                                           (let null-var ([full-name (car var)][vtype (cdr var)])
                                             (cond
                                               [(or (union-type? vtype)
                                                    (non-pointer-type? vtype))
                                                null]
                                               [(array-type? vtype)
                                                (let ([c (array-type-count vtype)])
                                                  (when (eq? c 'unknown)
                                                    (log-error "[ARRAY]: Can't initialize array of unknown: ~a." full-name))
                                                  (if (and (number? c) (<= c 3))
                                                      (let loop ([n 0])
                                                        (if (= n c)
                                                            null
                                                            (append
                                                             (null-var (string->symbol
                                                                        (format "~a[~a]" full-name n))
                                                                       #f)
                                                             (loop (add1 n)))))
                                                      (list (make-tok NULL_OUT_ARRAY #f #f)
                                                            (make-parens "(" #f #f ")"
                                                                         (seqce (make-tok full-name #f #f)))
                                                            (make-tok semi #f #f))))]
                                               [(struc-type? vtype)
                                                (let aloop ([array-index 0])
                                                  ;; Push each struct in array (or only struct if not an array)
                                                  (let loop ([l (cdr (lookup-struct-def (struc-type-struct vtype)))])
                                                    (if (null? l)
                                                        (if (and (struct-array-type? vtype)
                                                                 (< (add1 array-index)
                                                                    (let ([c (struct-array-type-count vtype)])
                                                                      (cond
                                                                       [(eq? c 'unknown)
                                                                        (log-error "[STRUCT ARRAY]: Can't initialize with unknown array size: ~a."
                                                                                   full-name)
                                                                        1]
                                                                       [else c]))))
                                                            ;; Next in array
                                                            (aloop (add1 array-index))
                                                            ;; All done
                                                            null)
                                                        (append
                                                         (null-var (string->symbol
                                                                    (format "~a~a.~a"
                                                                            full-name 
                                                                            (if (struct-array-type? vtype)
                                                                                (format "[~a]" array-index)
                                                                                "")
                                                                            (caar l)))
                                                                   (cdar l))
                                                         (loop (cdr l))))))]
                                               [else
                                                (list (make-tok full-name #f #f)
                                                      (make-tok '= #f #f)
                                                      (make-tok NULLED_OUT #f #f)
                                                      (make-tok semi #f #f))]))))
                                     local-vars)
                                body-x
                                (if setup-stack-return-type
                                    (list (append
                                           (if (or (null? setup-stack-return-type)
                                                   (and (= 1 (length setup-stack-return-type))
                                                        (eq? 'void (tok-n (car setup-stack-return-type)))))
                                               (list (make-tok RET_NOTHING_AT_END #f #f)
                                                     (make-tok semi #f #f))
                                               null)
                                           (list undefine-nested-pushable)))
                                    null)))
                              ;; Restore original tag and union max live vars:
                              (let ([total-pushed (total-push-size newly-pushed)])
                                (make-live-var-info orig-tag
                                                    (max orig-maxlive
                                                         (live-var-info-maxlive live-vars))
                                                    (max orig-maxpush
                                                         (+ total-pushed
                                                            (live-var-info-maxpush live-vars)))
                                                    (live-var-info-vars live-vars)
                                                    (live-var-info-new-vars live-vars)
                                                    (live-var-info-pushed-vars live-vars)
                                                    (live-var-info-num-calls live-vars)
                                                    (live-var-info-num-noreturn-calls live-vars)
                                                    (live-var-info-num-empty-calls live-vars)
                                                    (live-var-info-nonempty-calls? live-vars)))))))))))
        
        (define (body-var-decl? e)
          (and (pair? e)
               (or (lookup-non-pointer-type (tok-n (car e)))
                   (lookup-pointer-type (tok-n (car e)))
                   (assq (tok-n (car e)) c++-classes))))
        
        (define (looks-like-call? e- nf?)
          ;; e- is a reversed expression
          (and (pair? e-)
               (parens? (car e-))
               ;; Something precedes
               (not (null? (cdr e-)))
               ;; Not an assignment, sizeof, if, string
               (or nf? (hash-ref non-functions-table (tok-n (cadr e-)) #t))
               (not (string? (tok-n (cadr e-))))
               ;; Look back one more for if, etc. if preceding is paren
               (not (and (parens? (cadr e-))
                         (not (null? (cddr e-)))
                         (memq (tok-n (caddr e-)) '(if while for))))))
        
        (define (ignored-stuff? e-)
          ;; e- is a reversed expression
          (and (pair? e-)
               (parens? (car e-))
               ;; Something precedes
               (not (null? (cdr e-)))
               (memq (tok-n (cadr e-)) '(|HIDE_FROM_XFORM| |XFORM_HIDE_EXPR|))))
        
        (define (cast-or-call e- cast-k call-k)
          ;; Looks like a function call, although we don't know the
          ;; function yet.  (The parens may be preceded by an
          ;; unparenthesized expression.) And it could be a cast (which
          ;; requires parens).
          (let ([pre (cadr e-)])
            ;; Look for cast:
            (if (and (parens? pre)
                     (let ([prel (seq->list (seq-in pre))])
                       (or 
                        ;; Assume we never have (func)(args, ...)
                        (= 1 (length prel))
                        ;; trailing * is a give-away
                        (eq? '* (tok-n (list-ref prel (sub1 (length prel)))))
                        ;; leading `struct' is a giveaway:
                        (eq? 'struct (tok-n (car prel))))))
                ;; Cast
                (cast-k)
                ;; Call
                (call-k))))
        
        (define (resolve-indirection v get-c++-class-member c++-class locals)
          (and (parens? v)
               (let ([seql (seq->list (seq-in v))])
                 (and (= 3 (length seql))
                      (eq? '-> (tok-n (cadr seql)))
                      (let ([lhs (car seql)])
                        (cond
                          [(eq? sElF (tok-n lhs))
                           (get-c++-class-member (tok-n (caddr seql)) c++-class)]
                          [(or (resolve-indirection lhs get-c++-class-var c++-class locals)
                               (assq (tok-n lhs) locals)
                               (assq (tok-n lhs) (top-vars)))
                           => (lambda (m)
                                (let ([type (cdr m)])
                                  (and (pointer-type? type)
                                       (= 1 (pointer-type-stars type))
                                       (= 1 (length (pointer-type-base type))))
                                  (let ([c++-class (find-c++-class (car (pointer-type-base type)) #f)])
                                    (and c++-class
                                         (get-c++-class-member (tok-n (caddr seql)) c++-class)))))]
                          [else #f]))))))
        
        (define (extract-resolvable-record-var v)
          (and (parens? v)
               (let ([seql (seq->list (seq-in v))])
                 (= 3 (length seql))
                 (eq? '-> (tok-n (cadr seql)))
                 (if (parens? (car seql))
                     (extract-resolvable-record-var (car seql))
                     (car seql)))))
        
        ;; Found a sequance of argument expressions where function calls
        ;; are not allowed. Lift out the calls, inventing temporary variables
        ;; as necessary.
        (define (lift-out-calls args live-vars c++-class locals)
          (let ([e (seq->list (seq-in args))])
            (if (null? e)
                (values null args null null live-vars)
                (let-values ([(pragmas el) (body->lines e #t)])
                  (unless (null? pragmas)
                    (error 'lift-out-calls "unexpected pragma"))
                  (let loop ([el el]
                             [new-args null][setups null][new-vars null]
                             [ok-calls null][must-convert? #t][live-vars live-vars])
                    (letrec ([lift-one?
                              (lambda (e)
                                (let ([e- (let ([e- (reverse e)])
                                            (if (null? (cdr el))
                                                e-
                                                (cdr e-)))]) ; skip comma
                                  (and (looks-like-call? e- #f)
                                       (cast-or-call e- 
                                                     (lambda () #f) 
                                                     (lambda () 
                                                       (lambda (wrap)
                                                         (lift-one (cons e
                                                                         (cons (or (and (null? (cddr e-)) 
                                                                                        (cadr e-))
                                                                                   (and (= 3 (length (cdr e-)))
                                                                                        (eq? '-> (tok-n (caddr e-)))
                                                                                        (make-parens
                                                                                         "(" #f #f ")"
                                                                                         (list->seq (reverse (cdr e-))))))
                                                                               (car e-)))
                                                                   wrap)))))))]
                             [lift-one
                              (lambda (call-form wrap)
                                (let* ([call (car call-form)]
                                       [call-func (cadr call-form)]
                                       [call-args (cddr call-form)]
                                       [p-m (and must-convert?
                                                 call-func
                                                 (if (parens? call-func)
                                                     (resolve-indirection call-func get-c++-class-method c++-class locals)
                                                     (assq (tok-n call-func) (prototyped))))])
                                  (if p-m
                                      (let ([new-var (gensym '__funcarg)])
                                        (loop (cdr el)
                                              (cons (append
                                                     (wrap (list (make-tok new-var #f #f)))
                                                     (if (null? (cdr el))
                                                         null
                                                         (list (make-tok '|,| #f #f))))
                                                    new-args)
                                              (cons (if (null? (cdr el))
                                                        ;; Add comma
                                                        (append call (list (make-tok '|,| #f #f)))
                                                        call)
                                                    setups)
                                              (cons (cons new-var (prototype-for-pointer? p-m))
                                                    new-vars)
                                              ok-calls
                                              #t
                                              (make-live-var-info
                                               (live-var-info-tag live-vars)
                                               (live-var-info-maxlive live-vars)
                                               (live-var-info-maxpush live-vars)
                                               (live-var-info-vars live-vars)
                                               ;; Add newly-created vars for lifting to declaration set
                                               (cons (append (prototype-type (cdr p-m))
                                                             (list
                                                              (make-tok new-var #f #f))
                                                             (if (prototype-for-pointer? p-m)
                                                                 (list (make-tok '= #f #f)
                                                                       (make-tok NULLED_OUT #f #f))
                                                                 null)
                                                             (list
                                                              (make-tok semi #f #f)))
                                                     (live-var-info-new-vars live-vars))
                                               (live-var-info-pushed-vars live-vars)
                                               (live-var-info-num-calls live-vars)
                                               (live-var-info-num-noreturn-calls live-vars)
                                               (live-var-info-num-empty-calls live-vars)
                                               (live-var-info-nonempty-calls? live-vars))))
                                      (loop (cdr el) (cons (wrap e) new-args) setups new-vars 
                                            (if must-convert?
                                                ok-calls
                                                (cons call-args ok-calls))
                                            #t
                                            live-vars))))]
                             [lift-in-arithmetic?
                              (lambda (e)
                                (and (pair? e)
                                     (cond
                                       ;; look for: ! <liftable>
                                       [(eq? '! (tok-n (car e)))
                                        (let ([k (lift-in-arithmetic? (cdr e))])
                                          (and k
                                               (lambda (wrap)
                                                 (k (lambda (x) 
                                                      (wrap
                                                       (cons (car e) x)))))))]
                                       ;; look for: (<liftable>)
                                       [(and (parens? (car e))
                                             (null? (cdr e)))
                                        (let ([k (lift-in-arithmetic? (seq->list (seq-in (car e))))])
                                          (and k
                                               (lambda (wrap)
                                                 (k (lambda (x) 
                                                      (wrap (list
                                                             (make-parens
                                                              "(" #f #f ")"
                                                              (list->seq x)))))))))]
                                       ;; look for: n op <liftable>
                                       [(and (>= (length e) 3)
                                             (let ([n (tok-n (car e))])
                                               (or (number? n) (symbol? n)))
                                             (memq (tok-n (cadr e)) '(+ - * / 
                                                                        #csXFORM_OK_PLUS #csXFORM_OK_MINUS
                                                                        #csXFORM_TRUST_PLUS #csXFORM_TRUST_MINUS)))
                                        (let ([k (lift-in-arithmetic? (cddr e))])
                                          (and k
                                               (lambda (wrap)
                                                 (k (lambda (x) 
                                                      (wrap
                                                       (list* (car e) (cadr e) x)))))))]
                                       ;; look for: <liftable> op n
                                       [(let ([len (if (null? el)
                                                       (length e)
                                                       (sub1 (length e)))]) ; skip comma
                                          (and (>= len 3)
                                               (let ([n (tok-n (list-ref e (sub1 len)))])
                                                 (or (number? n) (symbol? n)))
                                               (memq (tok-n (list-ref e (- len 2))) '(+ - * / 
                                                                                        #csXFORM_OK_PLUS #csXFORM_OK_MINUS
                                                                                        #csXFORM_TRUST_PLUS #csXFORM_TRUST_MINUS))))
                                        (let* ([last? (null? el)]
                                               [len (if last?
                                                        (length e)
                                                        (sub1 (length e)))])
                                          (let ([k (lift-in-arithmetic? (let loop ([e e])
                                                                          (if (null? ((if last?
                                                                                          cddr 
                                                                                          cdddr)
                                                                                      e))
                                                                              (if last?
                                                                                  null
                                                                                  (cddr e))
                                                                              (cons (car e) (loop (cdr e))))))])
                                            (and k
                                                 (lambda (wrap)
                                                   (k (lambda (x) 
                                                        (wrap
                                                         (append x 
                                                                 (list
                                                                  (list-ref e (- len 2))
                                                                  (list-ref e (- len 1)))
                                                                 (if last?
                                                                     (list (list-ref e len))
                                                                     null)))))))))]
                                       [(lift-one? e) => values]
                                       [else #f])))])
                      (cond
                        [(null? el)
                         (if (null? new-vars)
                             (values null args null ok-calls live-vars)
                             (values
                              setups
                              (make-parens
                               "(" (tok-line args) (tok-file args) ")"
                               (list->seq (apply append (reverse new-args))))
                              new-vars
                              ok-calls
                              live-vars))]
                        [(lift-in-arithmetic? (car el)) => (lambda (k) (k values))]
                        [(and (= (length (car el)) 2)
                              (or (string? (tok-n (caar el)))
                                  (number? (tok-n (caar el)))))
                         ;; Constant => still no need to lift other args..
                         (loop (cdr el) (cons (car el) new-args) setups new-vars ok-calls must-convert? live-vars)]
                        [else
                         (loop (cdr el) (cons (car el) new-args) setups new-vars ok-calls #t live-vars)])))))))
        
        (define (check-special-live-vars rest- vars live-vars)
          (cond
            [(and (pair? rest-)
                  (eq? '= (tok-n (car rest-)))
                  (pair? (cdr rest-))
                  (extract-resolvable-record-var (cadr rest-)))
             => (lambda (v)
                  (if (and (assq (tok-n v) vars)
                           (not (assq (tok-n v) (live-var-info-vars live-vars))))
                      ;; Add a live variable:
                      (replace-live-vars live-vars 
                                         (cons (assq (tok-n v) vars)
                                               (live-var-info-vars live-vars)))
                      ;; Already there, or not pushable:
                      live-vars))]
            [else live-vars]))
        
        ;; Inspect an expression sequence statement-by-statement to convert
        ;; function calls with the GC-registration wrappers, expose temps,
        ;; etc. Some conversions require generating local variables, as
        ;; accumulated in a `live-vars' record. The result is two values:
        ;; converted body, and a new live-vars record.  (This function is
        ;; Mutually recursive with convert-body.)
        (define (convert-function-calls e vars &-vars c++-class live-vars complain-not-in memcpy? braces-are-aggregates?)
          ;; e is a single statement
          ;; Reverse to calculate live vars as we go.
          ;; Also, it's easier to look for parens and then inspect preceding
          ;;  to find function calls.
          ;; complain-not-in is ither #f [function calls are ok], a string [not ok, string describes way],
          ;;  or (list ok-exprs ...)) [in a rator position, only ok-expr calls are allowed,
          ;;  because they're blessed by the lifter]
          (let ([e- (reverse e)]
                [orig-num-calls (live-var-info-num-calls live-vars)])
            (let loop ([e- e-][result null][live-vars live-vars][converted-sub? #f])
              (cond
                [(null? e-) (values result live-vars)]
                [(ignored-stuff? e-)
                 (loop (cdr e-) (cons (car e-) result) live-vars converted-sub?)]
                [(eq? 'return (tok-n (car e-)))
                 ;; Look forward in result to semicolon, and wrap that:
                 (let rloop ([result result][l null])
                   (cond
                     [(null? result)
                      (error 'xform "odd return at ~a:~a" (tok-file (car e-)) (tok-line (car e-)))]
                     [(eq? (tok-n (car result)) semi)
                      (loop (cdr e-) 
                            (if (null? l)
                                (cons (make-tok RET_NOTHING (tok-line (car e-)) (tok-file (car e-)))
                                      result)
                                (let ([has-empty-funccall? 
                                       ;; All calls must be empty calls, otherwise
                                       ;; the result might not depend on the empty call
                                       ;; (e.g., f() && empty(f()) )
                                       (let loop ([l l][one? #f])
                                         (cond
                                          [(null? l) one?]
                                          [(call? (car l))
                                           (if (null? (call-live (car l)))
                                               (loop (cdr l) #t)
                                               #f)]
                                          [(seq? (car l))
                                           (and (loop (seq->list (seq-in (car l))) one?)
                                                (loop (cdr l) one?))]
                                          [else #f]))])
                                  (list* (make-tok (if has-empty-funccall?
                                                       RET_VALUE_EMPTY_START 
                                                       RET_VALUE_START)
                                                   (tok-line (car e-)) (tok-file (car e-))) 
                                         (make-parens
                                          "(" (tok-line (car e-)) (tok-file (car e-)) ")"
                                          (list->seq (reverse l)))
                                         (make-tok (if has-empty-funccall?
                                                       RET_VALUE_EMPTY_END 
                                                       RET_VALUE_END)
                                                   (tok-line (car e-)) (tok-file (car e-)))
                                         result)))
                            live-vars
                            converted-sub?)]
                     [else (rloop (cdr result) (cons (car result) l))]))]
                [(looks-like-call? e- #f)
                 ;; Looks like a function call, maybe a cast:
                 (cast-or-call
                  e-
                  (lambda ()
                    ;; It's a cast:
                    (let-values ([(v live-vars)
                                  (convert-paren-interior (car e-) vars &-vars c++-class live-vars complain-not-in memcpy?)])
                      (loop (cddr e-)
                            (list* (cadr e-) v result)
                            live-vars
                            #t)))
                  (lambda ()
                    ;; It's a function call; find the start
                    (let-values ([(args) (car e-)]
                                 [(func rest-)
                                  (let loop ([e- (cdr e-)])
                                    (cond
                                      [(null? e-)
                                       (values null null)]
                                      [(null? (cdr e-))
                                       (values e- null)]
                                      [(parens? (car e-))
                                       (values (list (car e-)) (cdr e-))]
                                      [(brackets? (car e-))
                                       ;; Array access
                                       (let-values ([(func rest-) (loop (cdr e-))])
                                         (values (cons (car e-) func) rest-))]
                                      ;; Assignment to result of top-level call
                                      [(and (pair? (cddr e-))
                                            (eq? (tok-n (cadr e-)) '|::|)
                                            (eq? (tok-n (caddr e-)) '=))
                                       (values (list (car e-) (cadr e-)) (cddr e-))]
                                      ;; Struct reference, class-specified:
                                      [(memq (tok-n (cadr e-)) '(-> |.| |::|))
                                       ;; In '|::| case, check for 'return or parens that might mean "if"
                                       (if (and (eq? '|::| (tok-n (cadr e-)))
                                                (pair? (cddr e-))
                                                (or (eq? 'return (tok-n (caddr e-)))
                                                    (seq? (caddr e-))))
                                           (values (list (car e-) (cadr e-)) (cddr e-))
                                           (let-values ([(func rest-) (loop (cddr e-))])
                                             (values (list* (car e-) (cadr e-) func) rest-)))]
                                      [else (values (list (car e-)) (cdr e-))]))])
                      (when (and complain-not-in
                                 (or (not (pair? complain-not-in))
                                     (not (memq args complain-not-in))))
                        (log-error "[CALL] ~a in ~a: Bad place for function call~a, starting tok is ~s."
                                   (tok-line (car func)) (tok-file (car func))
                                   (if (list? complain-not-in)
                                       ""
                                       (format " (in ~a)" complain-not-in))
                                   (tok-n (car func))))
                      ;; Lift out function calls as arguments. (Can re-order code.
                      ;; Racket source code must live with this change to C's semantics.)
                      ;; Calls are replaced by varaibles, and setup code generated that
                      ;; assigns to the variables.
                      (let*-values ([(live-vars)
                                     ;; Check for special form (XXX -> ivar) = call, which will
                                     ;; get re-arranged to (newvar = call, (XXX -> ivar) = newvar)
                                     (check-special-live-vars rest- vars live-vars)]
                                    [(orig-live-vars) live-vars]
                                    [(setups args new-vars ok-calls live-vars)
                                     ;; Split args into setup (calls) and args.
                                     ;; List newly-created vars (in order) in new-vars.
                                     ;; Make sure each setup ends with a comma.
                                     (lift-out-calls args live-vars c++-class vars)]
                                    [(sub-memcpy?)
                                     ;; memcpy, etc. call?
                                     (and (pair? (cdr e-))
                                          (hash-ref non-gcing-functions (tok-n (cadr e-)) #f))]
                                    [(args live-vars)
                                     (convert-paren-interior args vars &-vars
                                                             c++-class
                                                             (replace-live-vars 
                                                              live-vars
                                                              (append (map (lambda (x)
                                                                             (cons (car x) (make-vtype)))
                                                                           (filter (lambda (x)
                                                                                     (cdr x))
                                                                                   new-vars))
                                                                      (live-var-info-vars live-vars)))
                                                             ok-calls
                                                             sub-memcpy?)]
                                    [(func live-vars)
                                     (convert-function-calls (reverse func) vars &-vars c++-class live-vars "rator" #f #f)]
                                    ;; Process lifted-out function calls:
                                    [(setups live-vars)
                                     (let loop ([setups setups][new-vars new-vars][result null][live-vars live-vars])
                                       (if (null? setups)
                                           (values result live-vars)
                                           (let-values ([(setup live-vars)
                                                         (convert-function-calls (car setups) vars &-vars
                                                                                 c++-class
                                                                                 ;; Remove var for this one:
                                                                                 (replace-live-vars
                                                                                  live-vars
                                                                                  (remove (caar new-vars)
                                                                                          (live-var-info-vars live-vars)
                                                                                          (lambda (a b)
                                                                                            (eq? a (car b)))))
                                                                                 #f #f #f)])
                                             (loop (cdr setups)
                                                   (cdr new-vars)
                                                   (cons (list* (make-tok (caar new-vars) #f #f)
                                                                (make-tok '= #f #f)
                                                                setup)
                                                         result)
                                                   live-vars))))])
                        ;; Put everything back together. Lifted out calls go into a sequence
                        ;;  before the main function call.
                        (let* ([non-returning? (and 
                                                ;; call declared to not return, or after a `return'
                                                (or (and (null? (cdr func))
                                                         (memq (tok-n (car func)) non-returning-functions))
                                                    (and (pair? rest-)
                                                         (eq? 'return (tok-n (car rest-)))
                                                         (not converted-sub?)))
                                                ;; no arrays of pointers in this scope, or addresses of
                                                ;; local vars taken in the function.
                                                (not (or (ormap (lambda (var)
                                                                  (and (array-type? (cdr var))
                                                                       '(eprintf "Optwarn [return] ~a in ~a: tail-push blocked by ~s[].\n"
                                                                                 (tok-line (car func)) (tok-file (car func))
                                                                                 (car var))))
                                                                (live-var-info-vars live-vars))
                                                         (ormap (lambda (&-var)
                                                                  (and (assq &-var vars)
                                                                       '(eprintf "Optwarn [return] ~a in ~a: tail-push blocked by &~s.\n"
                                                                                 (tok-line (car func)) (tok-file (car func))
                                                                                 &-var)))
                                                                &-vars))))]
                               [pushed-vars (cond
                                              [non-returning?
                                               ;; non-returning -> don't need to push vars
                                               null]
                                              [else
                                               (live-var-info-vars orig-live-vars)])]
                               [this-nonempty?
                                (and (not non-returning?)
                                     (or (pair? pushed-vars)
                                         (live-var-info-nonempty-calls? live-vars)))])
			  (let ([non-gcing-call?
				 (and (null? (cdr func))
				      (hash-ref non-gcing-functions (tok-n (car func)) (lambda () #f)))]
                                [setjmp-call?
                                 (memq (tok-n (car func)) setjmp-functions)])
			    (loop rest-
				  (let ([call (if (or non-gcing-call?
                                                      setjmp-call?)
						  ;; Call without pointer pushes
						  (make-parens
						   "(" #f #f ")"
						   (list->seq (append func (list args))))
						  ;; Call with pointer pushes
						  (begin
						    (set! saw-gcing-call (car func))
						    (make-call
						     "func call"
						     #f #f
						     func
						     args
						     pushed-vars
						     (live-var-info-tag orig-live-vars)
						     this-nonempty?)))])
				    (cons (if (null? setups)
					      call
					      (make-callstage-parens
					       "(" #f #f ")"
					       (list->seq
						(append
						 (apply append setups)
						 (list call)))))
					  result))
				  (make-live-var-info (live-var-info-tag live-vars)
						      ;; maxlive is either size for this push or old maxlive:
						      (max (if non-gcing-call?
							       0
							       (total-push-size (live-var-info-vars orig-live-vars)))
							   (live-var-info-maxlive live-vars))
						      ;; note: maxpush calculated at block level
						      (live-var-info-maxpush live-vars)
						      (live-var-info-vars live-vars)
						      (live-var-info-new-vars live-vars)
						      ;; Add newly-pushed variable to pushed set:
						      (let* ([old-pushed (live-var-info-pushed-vars live-vars)]
							     [new-pushed (if non-gcing-call?
									     null
									     (filter (lambda (x) (not (assq (car x) old-pushed))) 
										     pushed-vars))])
							(append new-pushed old-pushed))
						      (+ (if (or non-gcing-call? setjmp-call?) 0 1)
							 (live-var-info-num-calls live-vars))
						      (+ (if (or non-gcing-call? setjmp-call?) 0 (if non-returning? 1 0))
							 (live-var-info-num-noreturn-calls live-vars))
						      (+ (if (or non-gcing-call? non-returning? setjmp-call?) 0 (if this-nonempty? 0 1))
							 (live-var-info-num-empty-calls live-vars))
						      (or (and this-nonempty? (not (or non-gcing-call? setjmp-call?)))
							  (live-var-info-nonempty-calls? live-vars)))
				  (or converted-sub?
				      (null? rest-)
				      (not (memq (tok-n (car rest-)) '(return else)))))))))))]
		[(and (looks-like-call? e- #t)
		      (hash-ref args-unevaled-table (tok-n (cadr e-)) #f))
		 (loop (cddr e-) (cons (cadr e-) (cons (car e-) result)) live-vars converted-sub?)]
                [(eq? 'goto (tok-n (car e-)))
                 ;; Goto - assume all vars are live
                 (loop (cdr e-) (cons (car e-) result) 
                       (replace-live-vars live-vars vars)
                       #t)]
                [(eq? '= (tok-n (car e-)))
                 ;; Check for assignments where the LHS can move due to
                 ;; a function call on the RHS. [Note that special support
                 ;;  in the function call case is necessary.]
                 (if (> (live-var-info-num-calls live-vars) orig-num-calls)
                     (let ([assignee (cdr e-)])
                       ;; Special case: (YYY -> ivar) = XXX;
                       (let ([special-case-type (and (not (null? assignee))
                                                     (null? (cdr assignee))
                                                     (= 2 (length result))
                                                     (or (call? (car result))
                                                         (creation-parens? (car result)))
                                                     (eq? semi (tok-n (cadr result)))
                                                     (let ([m (resolve-indirection (car assignee) get-c++-class-var c++-class vars)])
                                                       (and m (cdr m))))])
                         (if (and special-case-type
                                  (or (non-pointer-type? special-case-type)
                                      (pointer-type? special-case-type)))
                             ;; Change to (newvar = XXX, (YYY -> ivar) = newvar)
                             (let ([new-var (gensym '__assign)]
                                   [v (car e-)])
                               (loop null
                                     (list
                                      (make-parens
                                       "(" (tok-line v) (tok-file v) ")"
                                       (seqce (make-tok new-var #f #f)
                                              (make-tok '= #f #f)
                                              (car result)
                                              (make-tok '|,| #f #f)
                                              (car assignee)
                                              v
                                              (make-tok new-var (tok-line v) (tok-file v))))
                                      (cadr result)) ; semicolon
                                     ;; Add new variable to the list:
                                     (make-live-var-info
                                      (live-var-info-tag live-vars)
                                      (live-var-info-maxlive live-vars)
                                      (live-var-info-maxpush live-vars)
                                      (live-var-info-vars live-vars)
                                      ;; Add newly-created vars for lifting to declaration set
                                      (cons (append (type->decl special-case-type v)
                                                    (list
                                                     (make-tok new-var #f #f)
                                                     (make-tok semi #f #f)))
                                            (live-var-info-new-vars live-vars))
                                      (live-var-info-pushed-vars live-vars)
                                      (live-var-info-num-calls live-vars)
                                      (live-var-info-num-noreturn-calls live-vars)
                                      (live-var-info-num-empty-calls live-vars)
                                      (live-var-info-nonempty-calls? live-vars))
                                     #t))
                             (begin
                               (when (and (not (null? assignee))
                                          (or (if (brackets? (car assignee))
                                                  (or (not (or (null? (cddr assignee))
                                                               (eq? '|:| (tok-n (caddr assignee)))))
                                                      (let ([v (cadr assignee)])
                                                        (or (not (symbol? (tok-n v)))
                                                            ;; Assignment to locally-declared array is fine:
                                                            (let ([m (assq (tok-n v) vars)])
                                                              (and m
                                                                   (not (or (array-type? (cdr m))
                                                                            (struct-array-type? (cdr m)))))))))
                                                  (and (not (symbol? (tok-n (car assignee))))
                                                       ;; as below, ok if preceded by XFORM_OK_ASSIGN
                                                       (or (not (pair? (cdr assignee)))
                                                           (not (eq? (tok-n (cadr assignee)) 'XFORM_OK_ASSIGN)))))
                                              (and (symbol? (tok-n (car assignee)))
                                                   (not (null? (cdr assignee)))
                                                   ;; ok if name starts with "_stk_"
                                                   (not (regexp-match re:_stk_ (symbol->string (tok-n (car assignee)))))
                                                   ;; ok if preceding is else or label terminator
                                                   (not (memq (tok-n (cadr assignee)) '(else |:|)))
                                                   ;; assignment to field in record is ok
                                                   (not (and (eq? (tok-n (cadr assignee)) '|.|)
                                                             (pair? (cddr assignee))
                                                             (symbol? (tok-n (caddr assignee)))
                                                             (null? (cdddr assignee))))
                                                   ;; ok if preceded by XFORM_OK_ASSIGN
                                                   (not (eq? (tok-n (cadr assignee)) 'XFORM_OK_ASSIGN))
                                                   ;; ok if preceding is `if', `until', etc.
                                                   (not (and (parens? (cadr assignee))
                                                             (pair? (cddr assignee))
                                                             (memq (tok-n (caddr assignee)) '(if while for until))))))
                                          (not (eq? 'exn_table (tok-n (car (last-pair e-))))))
                                 (log-warning "[ASSIGN] ~a in ~a: suspicious assignment with a function call, LHS ends ~s."
                                              (tok-line (car e-)) (tok-file (car e-))
                                              (tok-n (cadr e-))))
                               (loop (cdr e-) (cons (car e-) result) live-vars #t)))))
                     (loop (cdr e-) (cons (car e-) result) live-vars #t))]
                [(and (braces? (car e-)) (not braces-are-aggregates?))
                 (let*-values ([(v) (car e-)]
                               ;; do/while/for: we'll need a fixpoint for live-vars
                               ;;  (We'll get the fixpoint by poing things twice)
                               [(do?) (and (not (null? (cdr e-)))
                                           (memq (tok-n (cadr e-)) '(do)))]
                               [(while?) (and (not (null? (cdr e-)))
                                              (parens? (cadr e-))
                                              (not (null? (cddr e-)))
                                              (memq (tok-n (caddr e-)) '(for while)))]
                               [(orig-new-vars) (live-var-info-new-vars live-vars)]
                               [(orig-pushed-vars) (live-var-info-pushed-vars live-vars)]
                               ;; Proc to convert body once
                               [(convert-brace-body) 
                                (lambda (live-vars)
                                  (convert-body (seq->list (seq-in v)) vars null &-vars c++-class null (lambda () null) live-vars #f))]
                               ;; First conversion
                               [(e live-vars) (convert-brace-body live-vars)]
                               ;; Proc to filter live and pushed vars, dropping vars no longer in scope:
                               [(filter-live-vars)
                                (lambda (live-vars)
                                  (let* ([not-declared (lambda (x) (assq (car x) vars))]
                                         [new-live-vars (filter
                                                         not-declared
                                                         (live-var-info-vars live-vars))]
                                         [new-pushed-vars (filter
                                                           (lambda (x) (or (not-declared x)
                                                                           (is-generated? x)))
                                                           (live-var-info-pushed-vars live-vars))])
                                    (make-live-var-info (live-var-info-tag live-vars)
                                                        (live-var-info-maxlive live-vars)
                                                        (live-var-info-maxpush live-vars)
                                                        new-live-vars
                                                        (live-var-info-new-vars live-vars)
                                                        new-pushed-vars
                                                        (live-var-info-num-calls live-vars)
                                                        (live-var-info-num-noreturn-calls live-vars)
                                                        (live-var-info-num-empty-calls live-vars)
                                                        (live-var-info-nonempty-calls? live-vars))))]
                               [(restore-new-vars)
                                (lambda (live-vars)
                                  (make-live-var-info (live-var-info-tag live-vars)
                                                      (live-var-info-maxlive live-vars)
                                                      (live-var-info-maxpush live-vars)
                                                      (live-var-info-vars live-vars)
                                                      orig-new-vars
                                                      orig-pushed-vars
                                                      (live-var-info-num-calls live-vars)
                                                      (live-var-info-num-noreturn-calls live-vars)
                                                      (live-var-info-num-empty-calls live-vars)
                                                      (live-var-info-nonempty-calls? live-vars)))]
                               [(e live-vars rest extra)
                                (cond
                                  [(and do? (not exit-with-error?))
                                   (let-values ([(e live-vars)
                                                 (convert-brace-body (restore-new-vars live-vars))])
                                     (values e live-vars (cdr e-) #f))]
                                  [(and while? (not exit-with-error?))
                                   ;; Run test part. We don't filter live-vars, but maybe we should:
                                   (let-values ([(v live-vars)
                                                 (convert-seq-interior (cadr e-) #t vars &-vars
                                                                       c++-class
                                                                       (restore-new-vars live-vars)
                                                                       #f #f)])
                                     ;; Now run body again:
                                     (let-values ([(e live-vars)
                                                   (convert-brace-body (restore-new-vars live-vars))])
                                       ;; Finally, run test again:
                                       (let-values ([(v live-vars)
                                                     (convert-seq-interior (cadr e-) #t vars &-vars
                                                                           c++-class
                                                                           live-vars
                                                                           #f #f)])
                                         (values e live-vars (cddr e-) v))))]
                                  [else
                                   (values e live-vars (cdr e-) #f)])])
                   (loop rest
                         (append
                          (if extra
                              (list extra)
                              null)
                          (list (make-braces
                                 (tok-n v)
                                 (tok-line v)
                                 (tok-file v)
                                 (seq-close v)
                                 (list->seq e)))
                          result)
                         (filter-live-vars live-vars)
                         #t))]
                [(seq? (car e-))
                 ;; Do nested body.
                 ;; For (v = new x, ...) parens, check for special conversion
                 ;;  on (XXX -> ivar) = (v = new x, ...)
                 (let ([live-vars (if (creation-parens? (car e-))
                                      (check-special-live-vars (cdr e-) vars live-vars)
                                      live-vars)])
                   (let-values ([(v live-vars)
                                 (convert-seq-interior (car e-) (parens? (car e-)) 
                                                       vars &-vars c++-class live-vars 
                                                       (or complain-not-in 
                                                           (and (brackets? (car e-))
                                                                "array access"))
                                                       memcpy?)])
                     (loop (cdr e-) (cons v result) live-vars #t)))]
                [(and (assq (tok-n (car e-)) vars)
                      (not (assq (tok-n (car e-)) (live-var-info-vars live-vars))))
                 ;; Add a live variable:
                 (loop (cdr e-)
                       (cons (car e-) result)
                       (replace-live-vars live-vars 
                                          (cons (assq (tok-n (car e-)) vars)
                                                (live-var-info-vars live-vars)))
                       #t)]
                [(and (memq (tok-n (car e-)) '(while do for))
                      (case (tok-n (car e-))
                        [(do)
                         (not (braces? (car result)))]
                        [(for)
                         (not (braces? (cadr result)))]
                        [(while)
                         (not (or (eq? semi (tok-n (cadr result)))
                                  (braces? (cadr result))))]))
                 (log-error "[LOOP] ~a in ~a: while/do/for with body not in braces."
                            (tok-line (car e-)) (tok-file (car e-)))
                 (loop (cdr e-) (cons (car e-) result) live-vars #t)]
                [else 
                 (when (and check-arith? (not memcpy?)
                            (positive? (live-var-info-num-calls live-vars)))
                   (when (and (memq (tok-n (car e-)) '(+ - ++ -- += -=))
                              (let ([assignee (cdr e-)])
                                (or (and (not (null? assignee))
                                         (assq (tok-n (car assignee)) vars))
                                    ;; Special case: (YYY -> ivar) + ...;
                                    (let ([special-case-type (and (not (null? assignee))
                                                                  (let ([m (resolve-indirection (car assignee) get-c++-class-var c++-class vars)])
                                                                    (and m (cdr m))))])
                                      (and special-case-type
                                           (pointer-type? special-case-type))))))
                     ;; __u comes from memset in some variants of gcc
                     (unless (eq? '__u (tok-n (cadr e-)))
                       (log-warning "[ARITH] ~a in ~a: suspicious arithmetic, LHS ends ~s."
                                    (tok-line (car e-)) (tok-file (car e-))
                                    (tok-n (cadr e-))))))
                 (loop (cdr e-) (cons (car e-) result) live-vars converted-sub?)]))))
        
        (define (convert-seq-interior v comma-sep? vars &-vars c++-class live-vars complain-not-in memcpy?)
          (let ([e (seq->list (seq-in v))])
            (let-values ([(pragmas el) (body->lines e comma-sep?)])
              (unless (null? pragmas)
                (error 'convert-seq-interior "unexpected pragmas"))
              (let-values ([(el live-vars)
                            (let loop ([el el])
                              (if (null? el)
                                  (values null live-vars)
                                  (let-values ([(rest live-vars) (loop (cdr el))])
                                    (let-values ([(e live-vars)
                                                  (convert-function-calls (car el) vars &-vars c++-class live-vars complain-not-in memcpy? #f)])
                                      (values (cons e rest) live-vars)))))])
                (values ((get-constructor v)
                         (tok-n v)
                         (tok-line v)
                         (tok-file v)
                         (seq-close v)
                         (list->seq (apply append el)))
                        live-vars)))))
        
        (define (convert-paren-interior v vars &-vars c++-class live-vars complain-not-in memcpy?)
          (convert-seq-interior v #t vars &-vars c++-class live-vars complain-not-in memcpy?))
        
        (define (find-&-vars e)
          (let loop ([e e])
            (cond
              [(null? e)
               null]
              [(pragma? (car e))
               (loop (cdr e))]
              [(and (pair? (cdr e))
                    (parens? (cadr e))
                    (hash-ref non-aliasing-functions (tok-n (car e)) #f))
               ;; A call to a non-aliasing function: drop immediate '&'s on args:
               (define (drop-&s now? e)
                 (cond
                  [(null? e) null]
                  [(and now? (eq? '& (tok-n (car e))))
                   (drop-&s #f (cdr e))]
                  [(eq? '|,| (tok-n (car e)))
                   (cons (car e) (drop-&s #t (cdr e)))]
                  [else
                   (cons (car e) (drop-&s #f (cdr e)))]))
               (append (loop (drop-&s #t (seq->list (seq-in (cadr e)))))
                       (loop (cddr e)))]
              [(eq? '& (tok-n (car e)))
               (if (null? (cdr e))
                   null
                   (let ([next (let loop ([next (cadr e)])
                                 (cond
                                   [(symbol? (tok-n next)) next]
                                   [(seq? (tok-n next))
                                    (let ([l (seq->list (seq-in next))])
                                      (if (null? l)
                                          #f
                                          (loop (car l))))]
                                   [else #f]))])
                     (if next
                         (cons (tok-n next) (loop (cdr e)))
                         (loop (cdr e)))))]
              [(seq? (car e))
               (append (find-&-vars (seq->list (seq-in (car e))))
                       (loop (cdr e)))]
              [else (loop (cdr e))])))
        
        ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        ;; Palm call-graph
        ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        
        (define (call-graph name e)
          (let ([body-v (let* ([len (sub1 (length e))]
                               [v (list-ref e len)])
                          ;; Function may have trailing semicolon:
                          (if (eq? semi (tok-n v))
                              (list-ref e (sub1 len))
                              v))])
            (call-graph/body name (seq->list (seq-in body-v)))))
        
        (define (call-graph/body name body-e)
          (let-values ([(pragmas el) (body->lines body-e #f)])
            (for-each
             (lambda (v)
               (call-graph/stmt name v))
             el)))
        
        (define (call-graph/stmt name e)
          ;; e is a single statement
          (for-each
           (lambda (v)
             (cond
               [(seq? v)
                (call-graph/body name (seq->list (seq-in v)))]
               [(assq (tok-n v) (prototyped))
                (fprintf map-port
                         "(call ~s ~s)\n"
                         name (tok-n v))]
               [else (void)]))
           e))
        
        ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        ;; More "parsing", main loop
        ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        
        (define (body->lines e comma-sep?)
          (let loop ([e e][pragmas null])
            (if (or (null? e)
                    (not (pragma? (car e))))
                (values
                 (reverse pragmas)
                 (reverse
                  (foldl-statement
                   e
                   comma-sep?
                   (lambda (sube l)
                     (cons sube l))
                   null)))
                (loop (cdr e)
                      (cons (car e) pragmas)))))
        
        (define (split-decls el)
          (let loop ([el el][decls null])
            (if (null? el)
                (values (reverse decls) null)
                (let ([e (car el)])
                  (if (or 
                       ;; These keywords appear only in decls:
                       (memq (tok-n (car e)) '(union struct static))
                       ;; Otherwise try harder:
                       (and
                        ;; Decl needs at least three parts:
                        (< 2 (length e))
                        ;; Decl ends in seimicolon
                        (eq? semi (tok-n (list-ref e (sub1 (length e)))))
                        ;; Doesn't start with a star, decrement, increment, or global call
                        (not (memq (tok-n (car e)) '(* -- ++ |::|)))
                        ;; Not an assignemnt
                        (not (memq (tok-n (cadr e)) '(= += -=)))
                        ;; Not a return, case, new, or delete
                        (not (memq (tok-n (car e)) '(return case new delete delete_wxobject)))
                        ;; Not a label, field lookup, pointer deref, class-specific
                        (not (memq (tok-n (cadr e)) '(|:| |.| -> |::|)))
                        ;; No parens/braces in first two parts, except __typeof
                        (not (seq? (car e)))
                        (or (not (seq? (cadr e)))
                            (eq? '__typeof (tok-n (car e))))))
                      ;; Looks like a decl
                      (loop (cdr el) (cons e decls))
                      ;; Not a decl
                      (values (reverse decls) el))))))
        
        (define (get-one e comma-sep?)
          (let loop ([e e][result null][first #f][second #f])
            (cond
              [(null? e) (values (reverse result) null)]
              [(pragma? (car e)) 
               (unless (null? result)
                 (error 'pragma "unexpected pragma: ~a at: ~a:~a" 
                        (pragma-s (car e))
                        (pragma-file (car e)) (pragma-line (car e))))
               (values (list (car e)) (cdr e))]
              [(compiler-pragma? e)
               (unless (null? result)
                 (error 'pragma "unexpected MSVC compiler pragma"))
               (values (list (car e) (cadr e)) (cddr e))]
              [(eq? semi (tok-n (car e)))
               (values (reverse (cons (car e) result)) (cdr e))]
              [(and (eq? '|,| (tok-n (car e))) comma-sep?)
               (values (reverse (cons (car e) result)) (cdr e))]
              [(and (braces? (car e))
                    (not (memq first '(typedef enum)))
                    (or (not (memq first '(static extern const struct union)))
                        (equal? second "C") ; => extern "C" ...
                        (equal? second "C++") ; => extern "C++" ...
                        (ormap parens? result))) ; => function prototype
               (let ([rest (cdr e)])
                 (if (or (null? rest)
                         (pragma? (car rest))
                         (not (eq? semi (tok-n (car rest)))))
                     (values (reverse (cons (car e) result)) rest)
                     (values (reverse (list* (car rest) (car e) result)) (cdr rest))))]
              [else (loop (cdr e) (cons (car e) result)
                          (or first (let ([s (tok-n (car e))])
                                      (if (memq s '(__extension__))
                                          #f ; skip over annotation when deciding shape
                                          s)))
                          (or second (and first (tok-n (car e)))))])))
        
        (define (foldl-statement e comma-sep? f a-init)
          (let loop ([e e][a a-init])
            (cond
              [(null? e) a]
              [else
               (let-values ([(sube e) (get-one e comma-sep?)])
                 (loop e (f sube a)))])))
        
        ; (print-it e 0 #t) (exit)
        
        (define (process-top-level e init-file can-drop-vars?)
          (foldl-statement
           e
           #f
           (lambda (sube l)
             (let* ([sube (top-level sube init-file can-drop-vars?)])
               (append l sube)))
           null))
        
        ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        
        (let* ([e e-raw]
               [line -inf.0]
               [file #f]
               [sysheader? #f])
          (set! e-raw #f) ;; to allow GC
          (foldl-statement
           e
           #f
           (lambda (sube where)
             (let* ([where (if (pragma? (car sube))
                               where
                               (or (tok-file (car sube))
                                   where))]
                    [sube (top-level sube where #t)])
               (let-values ([(l f s?) (print-it sube 0 #t #f line file sysheader? keep-lines?)])
                 (set! line l)
                 (set! file f)
                 (set! sysheader? s?))
               where))
           #f))
        
        
        (define (marshall v)
          (let loop ([v v])
            (cond
              [(struct? v) (let ([vec (struct->vector v)])
                             (if (eq? 'struct:tok (vector-ref vec 0))
                                 (list 'make-short-tok (loop (vector-ref vec 1)))
                                 (cons
                                  (car (hash-ref makers (vector-ref vec 0)))
                                  (map loop (cdr (vector->list vec))))))]
              [(list? v) (cons 'list (map loop v))]
              [(pair? v) (list 'cons (loop (car v)) (loop (cdr v)))]
              [(vector? v)
               (cons 'vector (map loop (vector->list v)))]
              [(symbol? v) (list 'quote v)]
              [else v])))
        
        (when precompiling-header?
          (parameterize ([current-inspector power-inspector]
                         [print-struct #t])
            (let ([e
                   (list
                    'list
                    
                    (list 'quote (hash-map used-symbols cons))
                    
                    (marshall c++-classes)
                    (marshall (prototyped))
                    (marshall (top-vars))
                    
                    (marshall pointer-types)
                    (marshall non-pointer-types)
                    (marshall struct-defs)
		    non-gcing-functions
		    non-aliasing-functions
                    (list 'quote gc-var-stack-mode))])
              (with-output-to-file (change-suffix file-out #".zo")
                (lambda ()
                  (let ([orig (current-namespace)])
                    (parameterize ([current-namespace (make-base-namespace)])
		      (namespace-require/copy 'racket/base)
                      (namespace-attach-module orig 'racket/base)
                      (namespace-require 'racket/base)
		      (namespace-require '(for-syntax racket/base))
		      (namespace-require/copy '(for-syntax racket/base))
                      (eval #'(define-syntaxes (#%top-interaction) (lambda (stx) (cdr (syntax-e stx)))))
                      (write (compile e)))))
                #:exists 'truncate))))
        
        (when precompiling-header?
          (let loop ([i 1])
            (unless (i . > . gentag-count)
              (printf "#undef XfOrM~a_COUNT\n" i)
              (printf "#undef SETUP_XfOrM~a\n" i)
              (loop (add1 i)))))
        
        (close-output-port (current-output-port))
        
        (when exit-with-error?
          (error 'xform "Errors converting"))
        
        (when output-depends-info?
          (with-output-to-file (change-suffix file-out #".sdep")
            (lambda ()
              (write (hash-map depends-files (lambda (k v) k)))
              (newline))
            #:exists 'truncate/replace))))))
