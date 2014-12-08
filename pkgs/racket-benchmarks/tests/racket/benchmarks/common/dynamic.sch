; This benchmark was obtained from Andrew Wright,
; based on Fritz Henglein's code.
; 970215 / wdc Removed most i/o and added dynamic-benchmark.
; 990707 / lth Added a quote and changed the call to run-benchmark.
; 010404 / wdc Changed the input file path name to "dynamic-input.sch".

;; Fritz's dynamic type inferencer, set up to run on itself
;; (see the end of this file).

;----------------------------------------------------------------------------
; Environment management
;----------------------------------------------------------------------------

;; environments are lists of pairs, the first component being the key

;; general environment operations
;;
;; empty-env: Env
;; gen-binding: Key x Value -> Binding
;; binding-key: Binding -> Key
;; binding-value: Binding -> Value
;; binding-show: Binding -> Symbol*
;; extend-env-with-binding: Env x Binding -> Env
;; extend-env-with-env: Env x Env -> Env
;; lookup: Key x Env -> (Binding + False)
;; env->list: Env -> Binding*
;; env-show: Env -> Symbol*


; bindings

(define gen-binding cons)
; generates a type binding, binding a symbol to a type variable

(define binding-key car)
; returns the key of a type binding

(define binding-value cdr)
; returns the tvariable of a type binding

(define (key-show key)
  ; default show procedure for keys
  key)

(define (value-show value)
  ; default show procedure for values
  value)

(define (binding-show binding)
  ; returns a printable representation of a type binding
  (cons (key-show (binding-key binding))
	(cons ': (value-show (binding-value binding)))))


; environments

(define dynamic-empty-env '())
; returns the empty environment

(define (extend-env-with-binding env binding)
  ; extends env with a binding, which hides any other binding in env
  ; for the same key (see dynamic-lookup)
  ; returns the extended environment
  (cons binding env))

(define (extend-env-with-env env ext-env)
  ; extends environment env with environment ext-env 
  ; a binding for a key in ext-env hides any binding in env for
  ; the same key (see dynamic-lookup)
  ; returns the extended environment
  (append ext-env env))

(define dynamic-lookup (lambda (x l) (assv x l)))
; returns the first pair in env that matches the key; returns #f
; if no such pair exists

(define (env->list e)
  ; converts an environment to a list of bindings
  e)

(define (env-show env)
  ; returns a printable list representation of a type environment
  (map binding-show env))
;----------------------------------------------------------------------------
;	Parsing for Scheme
;----------------------------------------------------------------------------


;; Needed packages: environment management

;(load "env-mgmt.ss")
;(load "pars-act.ss")

;; Lexical notions

(define syntactic-keywords
  ;; source: IEEE Scheme, 7.1, <expression keyword>, <syntactic keyword>
  '(lambda if set! begin cond and or case let let* letrec do
	  quasiquote else => define unquote unquote-splicing))


;; Parse routines

; Datum

; dynamic-parse-datum: parses nonterminal <datum>

(define (dynamic-parse-datum e)
  ;; Source: IEEE Scheme, sect. 7.2, <datum>
  ;; Note: "'" is parsed as 'quote, "`" as 'quasiquote, "," as
  ;; 'unquote, ",@" as 'unquote-splicing (see sect. 4.2.5, p. 18)
  ;; ***Note***: quasi-quotations are not permitted! (It would be
  ;; necessary to pass the environment to dynamic-parse-datum.)
  (cond
   ((null? e)
    (dynamic-parse-action-null-const))
   ((boolean? e)
    (dynamic-parse-action-boolean-const e))
   ((char? e)
    (dynamic-parse-action-char-const e))
   ((number? e)
    (dynamic-parse-action-number-const e))
   ((string? e)
    (dynamic-parse-action-string-const e))
   ((symbol? e)
    (dynamic-parse-action-symbol-const e))
   ((vector? e)
    (dynamic-parse-action-vector-const (map dynamic-parse-datum (vector->list e))))
   ((pair? e)
    (dynamic-parse-action-pair-const (dynamic-parse-datum (car e))
			     (dynamic-parse-datum (cdr e))))
   (else (error 'dynamic-parse-datum "Unknown datum: ~s" e))))


; VarDef

; dynamic-parse-formal: parses nonterminal <variable> in defining occurrence position

(define (dynamic-parse-formal f-env e)
  ; e is an arbitrary object, f-env is a forbidden environment;
  ; returns: a variable definition (a binding for the symbol), plus
  ; the value of the binding as a result
  (if (symbol? e)
      (cond
       ((memq e syntactic-keywords)
	(error 'dynamic-parse-formal "Illegal identifier (keyword): ~s" e))
       ((dynamic-lookup e f-env)
	(error 'dynamic-parse-formal "Duplicate variable definition: ~s" e))
       (else (let ((dynamic-parse-action-result (dynamic-parse-action-var-def e)))
	       (cons (gen-binding e dynamic-parse-action-result)
		     dynamic-parse-action-result))))
      (error 'dynamic-parse-formal "Not an identifier: ~s" e)))

; dynamic-parse-formal*

(define (dynamic-parse-formal* formals)
  ;; parses a list of formals and returns a pair consisting of generated
  ;; environment and list of parsing action results
  (letrec
      ((pf*
	(lambda (f-env results formals)
	  ;; f-env: "forbidden" environment (to avoid duplicate defs)
	  ;; results: the results of the parsing actions
	  ;; formals: the unprocessed formals
	  ;; Note: generates the results of formals in reverse order!
	  (cond
	   ((null? formals)
	    (cons f-env results))
	   ((pair? formals)
	    (let* ((fst-formal (car formals))
		   (binding-result (dynamic-parse-formal f-env fst-formal))
		   (binding (car binding-result))
		   (var-result (cdr binding-result)))
	      (pf*
	       (extend-env-with-binding f-env binding)
	       (cons var-result results)
	       (cdr formals))))
	   (else (error 'dynamic-parse-formal* "Illegal formals: ~s" formals))))))
    (let ((renv-rres (pf* dynamic-empty-env '() formals)))
      (cons (car renv-rres) (reverse (cdr renv-rres))))))


; dynamic-parse-formals: parses <formals>

(define (dynamic-parse-formals formals)
  ;; parses <formals>; see IEEE Scheme, sect. 7.3
  ;; returns a pair: env and result
  (letrec ((pfs (lambda (f-env formals)
		  (cond
		   ((null? formals)
		    (cons dynamic-empty-env (dynamic-parse-action-null-formal)))
		   ((pair? formals)
		    (let* ((fst-formal (car formals))
			   (rem-formals (cdr formals))
			   (bind-res (dynamic-parse-formal f-env fst-formal))
			   (bind (car bind-res))
			   (res (cdr bind-res))
			   (nf-env (extend-env-with-binding f-env bind))
			   (renv-res* (pfs nf-env rem-formals))
			   (renv (car renv-res*))
			   (res* (cdr renv-res*)))
		      (cons
		       (extend-env-with-binding renv bind)
		       (dynamic-parse-action-pair-formal res res*))))
		   (else
		    (let* ((bind-res (dynamic-parse-formal f-env formals))
			   (bind (car bind-res))
			   (res (cdr bind-res)))
		      (cons
		       (extend-env-with-binding dynamic-empty-env bind)
		       res)))))))
    (pfs dynamic-empty-env formals)))


; Expr

; dynamic-parse-expression: parses nonterminal <expression>

(define (dynamic-parse-expression env e)
  (cond
   ((symbol? e)
    (dynamic-parse-variable env e))
   ((pair? e)
    (let ((op (car e)) (args (cdr e)))
      (case op
	((quote) (dynamic-parse-quote env args))
	((lambda) (dynamic-parse-lambda env args))
	((if) (dynamic-parse-if env args))
	((set!) (dynamic-parse-set env args))
	((begin) (dynamic-parse-begin env args))
	((cond) (dynamic-parse-cond env args))
	((case) (dynamic-parse-case env args))
	((and) (dynamic-parse-and env args))
	((or) (dynamic-parse-or env args))
	((let) (dynamic-parse-let env args))
	((let*) (dynamic-parse-let* env args))
	((letrec) (dynamic-parse-letrec env args))
	((do) (dynamic-parse-do env args))
	((quasiquote) (dynamic-parse-quasiquote env args))
        (else (dynamic-parse-procedure-call env op args)))))
   (else (dynamic-parse-datum e))))

; dynamic-parse-expression*

(define (dynamic-parse-expression* env exprs)
  ;; Parses lists of expressions (returns them in the right order!)
  (letrec ((pe*
	    (lambda (results es)
	      (cond
	       ((null? es) results)
	       ((pair? es) (pe* (cons (dynamic-parse-expression env (car es)) results) (cdr es)))
	       (else (error 'dynamic-parse-expression* "Not a list of expressions: ~s" es))))))
    (reverse (pe* '() exprs))))


; dynamic-parse-expressions

(define (dynamic-parse-expressions env exprs)
  ;; parses lists of arguments of a procedure call
  (cond
   ((null? exprs) (dynamic-parse-action-null-arg))
   ((pair? exprs) (let* ((fst-expr (car exprs))
			 (rem-exprs (cdr exprs))
			 (fst-res (dynamic-parse-expression env fst-expr))
			 (rem-res (dynamic-parse-expressions env rem-exprs)))
		    (dynamic-parse-action-pair-arg fst-res rem-res)))
   (else (error 'dynamic-parse-expressions "Illegal expression list: ~s"
		exprs))))


; dynamic-parse-variable: parses variables (applied occurrences)

(define (dynamic-parse-variable env e)
  (if (symbol? e)
      (if (memq e syntactic-keywords)
	  (error 'dynamic-parse-variable "Illegal identifier (keyword): ~s" e)
	  (let ((assoc-var-def (dynamic-lookup e env)))
	    (if assoc-var-def
		(dynamic-parse-action-variable (binding-value assoc-var-def))
		(dynamic-parse-action-identifier e))))
      (error 'dynamic-parse-variable "Not an identifier: ~s" e)))


; dynamic-parse-procedure-call

(define (dynamic-parse-procedure-call env op args)
  (dynamic-parse-action-procedure-call
   (dynamic-parse-expression env op)
   (dynamic-parse-expressions env args)))


; dynamic-parse-quote

(define (dynamic-parse-quote env args)
  (if (list-of-1? args)
      (dynamic-parse-datum (car args))
      (error 'dynamic-parse-quote "Not a datum (multiple arguments): ~s" args)))


; dynamic-parse-lambda

(define (dynamic-parse-lambda env args)
  (if (pair? args)
      (let* ((formals (car args))
	     (body (cdr args))
	     (nenv-fresults (dynamic-parse-formals formals))
	     (nenv (car nenv-fresults))
	     (fresults (cdr nenv-fresults)))
	(dynamic-parse-action-lambda-expression
	 fresults
	 (dynamic-parse-body (extend-env-with-env env nenv) body)))
      (error 'dynamic-parse-lambda "Illegal formals/body: ~s" args)))


; dynamic-parse-body

(define (dynamic-parse-body env body)
  ; <body> = <definition>* <expression>+
  (define (def-var* f-env body)
    ; finds the defined variables in a body and returns an 
    ; environment containing them
    (if (pair? body)
        (let ((n-env (def-var f-env (car body))))
          (if n-env
              (def-var* n-env (cdr body))
              f-env))
        f-env))
  (define (def-var f-env clause)
    ; finds the defined variables in a single clause and extends
    ; f-env accordingly; returns false if it's not a definition
    (if (pair? clause)
        (case (car clause)
          ((define) (if (pair? (cdr clause))
                        (let ((pattern (cadr clause)))
                          (cond
			   ((symbol? pattern)
			    (extend-env-with-binding 
			     f-env 
			     (gen-binding pattern
					  (dynamic-parse-action-var-def pattern))))
			   ((and (pair? pattern) (symbol? (car pattern)))
			    (extend-env-with-binding
			     f-env
			     (gen-binding (car pattern)
					  (dynamic-parse-action-var-def 
					   (car pattern)))))
			   (else f-env)))
                        f-env))
          ((begin) (def-var* f-env (cdr clause)))
          (else #f))
        #f))
  (if (pair? body)
      (dynamic-parse-command* (def-var* env body) body)
      (error 'dynamic-parse-body "Illegal body: ~s" body)))

; dynamic-parse-if

(define (dynamic-parse-if env args)
  (cond
   ((list-of-3? args)
    (dynamic-parse-action-conditional
     (dynamic-parse-expression env (car args))
     (dynamic-parse-expression env (cadr args))
     (dynamic-parse-expression env (caddr args))))
   ((list-of-2? args)
    (dynamic-parse-action-conditional
     (dynamic-parse-expression env (car args))
     (dynamic-parse-expression env (cadr args))
     (dynamic-parse-action-empty)))
   (else (error 'dynamic-parse-if "Not an if-expression: ~s" args))))


; dynamic-parse-set

(define (dynamic-parse-set env args)
  (if (list-of-2? args)
      (dynamic-parse-action-assignment
       (dynamic-parse-variable env (car args))
       (dynamic-parse-expression env (cadr args)))
      (error 'dynamic-parse-set "Not a variable/expression pair: ~s" args)))


; dynamic-parse-begin

(define (dynamic-parse-begin env args)
  (dynamic-parse-action-begin-expression
   (dynamic-parse-body env args)))


; dynamic-parse-cond

(define (dynamic-parse-cond env args)
  (if (and (pair? args) (list? args))
      (dynamic-parse-action-cond-expression
       (map (lambda (e)
	      (dynamic-parse-cond-clause env e))
	    args))
      (error 'dynamic-parse-cond "Not a list of cond-clauses: ~s" args)))

; dynamic-parse-cond-clause

(define (dynamic-parse-cond-clause env e)
  ;; ***Note***: Only (<test> <sequence>) is permitted!
  (if (pair? e)
      (cons
       (if (eqv? (car e) 'else)
	   (dynamic-parse-action-empty)
	   (dynamic-parse-expression env (car e)))
       (dynamic-parse-body env (cdr e)))
      (error 'dynamic-parse-cond-clause "Not a cond-clause: ~s" e)))


; dynamic-parse-and

(define (dynamic-parse-and env args)
  (if (list? args)
      (dynamic-parse-action-and-expression
       (dynamic-parse-expression* env args))
      (error 'dynamic-parse-and "Not a list of arguments: ~s" args)))


; dynamic-parse-or

(define (dynamic-parse-or env args)
  (if (list? args)
      (dynamic-parse-action-or-expression
       (dynamic-parse-expression* env args))
      (error 'dynamic-parse-or "Not a list of arguments: ~s" args)))


; dynamic-parse-case

(define (dynamic-parse-case env args)
  (if (and (list? args) (> (length args) 1))
      (dynamic-parse-action-case-expression
       (dynamic-parse-expression env (car args))
       (map (lambda (e)
	       (dynamic-parse-case-clause env e))
	     (cdr args)))
      (error 'dynamic-parse-case "Not a list of clauses: ~s" args)))

; dynamic-parse-case-clause

(define (dynamic-parse-case-clause env e)
  (if (pair? e)
      (cons
       (cond
	((eqv? (car e) 'else)
	 (list (dynamic-parse-action-empty)))
	((list? (car e))
	 (map dynamic-parse-datum (car e)))
	(else (error 'dynamic-parse-case-clause "Not a datum list: ~s" (car e))))
       (dynamic-parse-body env (cdr e)))
      (error 'dynamic-parse-case-clause "Not case clause: ~s" e)))


; dynamic-parse-let

(define (dynamic-parse-let env args)
  (if (pair? args)
      (if (symbol? (car args))
	  (dynamic-parse-named-let env args)
	  (dynamic-parse-normal-let env args))
      (error 'dynamic-parse-let "Illegal bindings/body: ~s" args)))


; dynamic-parse-normal-let

(define (dynamic-parse-normal-let env args)
  ;; parses "normal" let-expressions
  (let* ((bindings (car args))
	 (body (cdr args))
	 (env-ast (dynamic-parse-parallel-bindings env bindings))
	 (nenv (car env-ast))
	 (bresults (cdr env-ast)))
    (dynamic-parse-action-let-expression
     bresults
     (dynamic-parse-body (extend-env-with-env env nenv) body))))

; dynamic-parse-named-let

(define (dynamic-parse-named-let env args)
  ;; parses a named let-expression
  (if (pair? (cdr args))
      (let* ((variable (car args))
	     (bindings (cadr args))
	     (body (cddr args))
	     (vbind-vres (dynamic-parse-formal dynamic-empty-env variable))
	     (vbind (car vbind-vres))
	     (vres (cdr vbind-vres))
	     (env-ast (dynamic-parse-parallel-bindings env bindings))
	     (nenv (car env-ast))
	     (bresults (cdr env-ast)))
	(dynamic-parse-action-named-let-expression
	 vres bresults
	 (dynamic-parse-body (extend-env-with-env 
		      (extend-env-with-binding env vbind)
		      nenv) body)))
      (error 'dynamic-parse-named-let "Illegal named let-expression: ~s" args)))


; dynamic-parse-parallel-bindings

(define (dynamic-parse-parallel-bindings env bindings)
  ; returns a pair consisting of an environment
  ; and a list of pairs (variable . asg)
  ; ***Note***: the list of pairs is returned in reverse unzipped form!
  (if (list-of-list-of-2s? bindings)
      (let* ((env-formals-asg
	     (dynamic-parse-formal* (map car bindings)))
	    (nenv (car env-formals-asg))
	    (bresults (cdr env-formals-asg))
	    (exprs-asg
	     (dynamic-parse-expression* env (map cadr bindings))))
	(cons nenv (cons bresults exprs-asg)))
      (error 'dynamic-parse-parallel-bindings
	     "Not a list of bindings: ~s" bindings)))


; dynamic-parse-let*

(define (dynamic-parse-let* env args)
  (if (pair? args)
      (let* ((bindings (car args))
	     (body (cdr args))
	     (env-ast (dynamic-parse-sequential-bindings env bindings))
	     (nenv (car env-ast))
	     (bresults (cdr env-ast)))
	(dynamic-parse-action-let*-expression
	 bresults
	 (dynamic-parse-body (extend-env-with-env env nenv) body)))
      (error 'dynamic-parse-let* "Illegal bindings/body: ~s" args)))

; dynamic-parse-sequential-bindings

(define (dynamic-parse-sequential-bindings env bindings)
  ; returns a pair consisting of an environment
  ; and a list of pairs (variable . asg)
  ;; ***Note***: the list of pairs is returned in reverse unzipped form!
  (letrec
      ((psb
	(lambda (f-env c-env var-defs expr-asgs binds)
	  ;; f-env: forbidden environment
	  ;; c-env: constructed environment
	  ;; var-defs: results of formals
	  ;; expr-asgs: results of corresponding expressions
	  ;; binds: reminding bindings to process
	  (cond
	   ((null? binds)
	    (cons f-env (cons var-defs expr-asgs)))
	   ((pair? binds)
	    (let ((fst-bind (car binds)))
	      (if (list-of-2? fst-bind)
		  (let* ((fbinding-bres
			  (dynamic-parse-formal f-env (car fst-bind)))
			 (fbind (car fbinding-bres))
			 (bres (cdr fbinding-bres))
			 (new-expr-asg
			  (dynamic-parse-expression c-env (cadr fst-bind))))
		    (psb
		     (extend-env-with-binding f-env fbind)
		     (extend-env-with-binding c-env fbind)
		     (cons bres var-defs)
		     (cons new-expr-asg expr-asgs)
		     (cdr binds)))
		  (error 'dynamic-parse-sequential-bindings
			 "Illegal binding: ~s" fst-bind))))
	   (else (error 'dynamic-parse-sequential-bindings
			"Illegal bindings: ~s" binds))))))
    (let ((env-vdefs-easgs (psb dynamic-empty-env env '() '() bindings)))
      (cons (car env-vdefs-easgs)
	    (cons (reverse (cadr env-vdefs-easgs))
		  (reverse (cddr env-vdefs-easgs)))))))


; dynamic-parse-letrec

(define (dynamic-parse-letrec env args)
  (if (pair? args)
      (let* ((bindings (car args))
	     (body (cdr args))
	     (env-ast (dynamic-parse-recursive-bindings env bindings))
	     (nenv (car env-ast))
	     (bresults (cdr env-ast)))
	(dynamic-parse-action-letrec-expression
	  bresults
	  (dynamic-parse-body (extend-env-with-env env nenv) body)))
      (error 'dynamic-parse-letrec "Illegal bindings/body: ~s" args)))

; dynamic-parse-recursive-bindings

(define (dynamic-parse-recursive-bindings env bindings)
  ;; ***Note***: the list of pairs is returned in reverse unzipped form!
  (if (list-of-list-of-2s? bindings)
      (let* ((env-formals-asg
	      (dynamic-parse-formal* (map car bindings)))
	     (formals-env
	      (car env-formals-asg))
	     (formals-res
	      (cdr env-formals-asg))
	     (exprs-asg
	      (dynamic-parse-expression*
	       (extend-env-with-env env formals-env)
	       (map cadr bindings))))
	(cons
	 formals-env
	 (cons formals-res exprs-asg)))
      (error 'dynamic-parse-recursive-bindings "Illegal bindings: ~s" bindings)))


; dynamic-parse-do

(define (dynamic-parse-do env args)
  ;; parses do-expressions
  ;; ***Note***: Not implemented!
  (error 'dynamic-parse-do "Nothing yet..."))

; dynamic-parse-quasiquote

(define (dynamic-parse-quasiquote env args)
  ;; ***Note***: Not implemented!
  (error 'dynamic-parse-quasiquote "Nothing yet..."))


;; Command

; dynamic-parse-command

(define (dynamic-parse-command env c)
  (if (pair? c)
      (let ((op (car c))
	    (args (cdr c)))
	(case op
	 ((define) (dynamic-parse-define env args))
;	 ((begin) (dynamic-parse-command* env args))  ;; AKW
	 ((begin) (dynamic-parse-action-begin-expression (dynamic-parse-command* env args)))
	 (else (dynamic-parse-expression env c))))
      (dynamic-parse-expression env c)))


; dynamic-parse-command*

(define (dynamic-parse-command* env commands)
  ;; parses a sequence of commands
  (if (list? commands)
      (map (lambda (command) (dynamic-parse-command env command)) commands)
      (error 'dynamic-parse-command* "Invalid sequence of commands: ~s" commands)))


; dynamic-parse-define

(define (dynamic-parse-define env args)
  ;; three cases -- see IEEE Scheme, sect. 5.2
  ;; ***Note***: the parser admits forms (define (x . y) ...)
  ;; ***Note***: Variables are treated as applied occurrences!
  (if (pair? args)
      (let ((pattern (car args))
	    (exp-or-body (cdr args)))
	(cond
	 ((symbol? pattern)
	  (if (list-of-1? exp-or-body)
	      (dynamic-parse-action-definition
	       (dynamic-parse-variable env pattern)
	       (dynamic-parse-expression env (car exp-or-body)))
	      (error 'dynamic-parse-define "Not a single expression: ~s" exp-or-body)))
	 ((pair? pattern)
	  (let* ((function-name (car pattern))
		 (function-arg-names (cdr pattern))
		 (env-ast (dynamic-parse-formals function-arg-names))
		 (formals-env (car env-ast))
		 (formals-ast (cdr env-ast)))
	    (dynamic-parse-action-function-definition
	     (dynamic-parse-variable env function-name)
	     formals-ast
	     (dynamic-parse-body (extend-env-with-env env formals-env) exp-or-body))))
	 (else (error 'dynamic-parse-define "Not a valid pattern: ~s" pattern))))
      (error 'dynamic-parse-define "Not a valid definition: ~s" args)))

;; Auxiliary routines

; forall?

(define (forall? pred list)
  (if (null? list)
      #t
      (and (pred (car list)) (forall? pred (cdr list)))))

; list-of-1?

(define (list-of-1? l)
  (and (pair? l) (null? (cdr l))))

; list-of-2?

(define (list-of-2? l)
  (and (pair? l) (pair? (cdr l)) (null? (cddr l))))

; list-of-3?

(define (list-of-3? l)
  (and (pair? l) (pair? (cdr l)) (pair? (cddr l)) (null? (cdddr l))))

; list-of-list-of-2s?

(define (list-of-list-of-2s? e)
  (cond
   ((null? e)
    #t)
   ((pair? e)
    (and (list-of-2? (car e)) (list-of-list-of-2s? (cdr e))))
   (else #f)))


;; File processing

; dynamic-parse-from-port

(define (dynamic-parse-from-port port)
  (let ((next-input (read port)))
    (if (eof-object? next-input)
	'()
	(dynamic-parse-action-commands
	 (dynamic-parse-command dynamic-empty-env next-input)
	 (dynamic-parse-from-port port)))))

; dynamic-parse-file

(define (dynamic-parse-file file-name)
  (let ((input-port (open-input-file file-name)))
    (dynamic-parse-from-port input-port)))
;----------------------------------------------------------------------------
; Implementation of Union/find data structure in Scheme
;----------------------------------------------------------------------------

;; for union/find the following attributes are necessary: rank, parent 
;; (see Tarjan, "Data structures and network algorithms", 1983)
;; In the Scheme realization an element is represented as a single
;; cons cell; its address is the element itself; the car field contains 
;; the parent, the cdr field is an address for a cons
;; cell containing the rank (car field) and the information (cdr field)


;; general union/find data structure
;; 
;; gen-element: Info -> Elem
;; find: Elem -> Elem
;; link: Elem! x Elem! -> Elem
;; asymm-link: Elem! x Elem! -> Elem
;; info: Elem -> Info
;; set-info!: Elem! x Info -> Void


(define (gen-element info)
  ; generates a new element: the parent field is initialized to '(),
  ; the rank field to 0
  (cons '() (cons 0 info)))

(define info (lambda (l) (cddr l)))
  ; returns the information stored in an element

(define (set-info! elem info)
  ; sets the info-field of elem to info
  (set-cdr! (cdr elem) info))

; (define (find! x)
;   ; finds the class representative of x and sets the parent field 
;   ; directly to the class representative (a class representative has
;   ; '() as its parent) (uses path halving)
;   ;(display "Find!: ")
;   ;(display (pretty-print (info x)))
;   ;(newline)
;   (let ((px (car x)))
;     (if (null? px)
; 	x
; 	(let ((ppx (car px)))
; 	  (if (null? ppx)
; 	      px
; 	      (begin
; 		(set-car! x ppx)
; 		(find! ppx)))))))

(define (find! elem)
  ; finds the class representative of elem and sets the parent field 
  ; directly to the class representative (a class representative has
  ; '() as its parent)
  ;(display "Find!: ")
  ;(display (pretty-print (info elem)))
  ;(newline)
  (let ((p-elem (car elem)))
    (if (null? p-elem)
	elem
	(let ((rep-elem (find! p-elem)))
	  (set-car! elem rep-elem)
	  rep-elem))))

(define (link! elem-1 elem-2)
  ; links class elements by rank
  ; they must be distinct class representatives
  ; returns the class representative of the merged equivalence classes
  ;(display "Link!: ")
  ;(display (pretty-print (list (info elem-1) (info elem-2))))
  ;(newline)
  (let ((rank-1 (cadr elem-1))
	(rank-2 (cadr elem-2)))
    (cond
     ((= rank-1 rank-2)
      (set-car! (cdr elem-2) (+ rank-2 1))
      (set-car! elem-1 elem-2)
      elem-2)
     ((> rank-1 rank-2)
      (set-car! elem-2 elem-1)
      elem-1)
     (else
      (set-car! elem-1 elem-2)
      elem-2))))

(define asymm-link! (lambda (l x) (set-car! l x)))

;(define (asymm-link! elem-1 elem-2)
  ; links elem-1 onto elem-2 no matter what rank; 
  ; does not update the rank of elem-2 and does not return a value
  ; the two arguments must be distinct
  ;(display "AsymmLink: ")
  ;(display (pretty-print (list (info elem-1) (info elem-2))))
  ;(newline)
  ;(set-car! elem-1 elem-2))

;----------------------------------------------------------------------------
; Type management
;----------------------------------------------------------------------------

; introduces type variables and types for Scheme,


;; type TVar (type variables)
;;
;; gen-tvar:          () -> TVar
;; gen-type:          TCon x TVar* -> TVar
;; dynamic:           TVar
;; tvar-id:           TVar -> Symbol
;; tvar-def:          TVar -> Type + Null
;; tvar-show:         TVar -> Symbol*
;;
;; set-def!:          !TVar x TCon x TVar* -> Null
;; equiv!:            !TVar x !TVar -> Null
;;
;;
;; type TCon (type constructors)
;;
;; ...
;;
;; type Type (types)
;;
;; gen-type:          TCon x TVar* -> Type
;; type-con:          Type -> TCon
;; type-args:         Type -> TVar*
;;
;; boolean:           TVar
;; character:         TVar
;; null:              TVar
;; pair:              TVar x TVar -> TVar
;; procedure:         TVar x TVar* -> TVar
;; charseq:           TVar
;; symbol:            TVar
;; array:             TVar -> TVar


; Needed packages: union/find

;(load "union-fi.so")

; TVar

(define counter 0)
; counter for generating tvar id's

(define (gen-id)
  ; generates a new id (for printing purposes)
  (set! counter (+ counter 1))
  counter)

(define (gen-tvar)
  ; generates a new type variable from a new symbol
  ; uses union/find elements with two info fields
  ; a type variable has exactly four fields:
  ; car:     TVar (the parent field; initially null)
  ; cadr:    Number (the rank field; is always nonnegative)
  ; caddr:   Symbol (the type variable identifier; used only for printing)
  ; cdddr:   Type (the leq field; initially null)
  (gen-element (cons (gen-id) '())))

(define (gen-type tcon targs)
  ; generates a new type variable with an associated type definition
  (gen-element (cons (gen-id) (cons tcon targs))))

(define dynamic (gen-element (cons 0 '())))
; the special type variable dynamic
; Generic operations

(define (tvar-id tvar)
  ; returns the (printable) symbol representing the type variable
  (car (info tvar)))

(define (tvar-def tvar)
  ; returns the type definition (if any) of the type variable
  (cdr (info tvar)))

(define (set-def! tvar tcon targs)
  ; sets the type definition part of tvar to type
  (set-cdr! (info tvar) (cons tcon targs))
  '())

(define (reset-def! tvar)
  ; resets the type definition part of tvar to nil
  (set-cdr! (info tvar) '()))

(define type-con (lambda (l) (car l)))
; returns the type constructor of a type definition

(define type-args (lambda (l) (cdr l)))
; returns the type variables of a type definition

(define (tvar->string tvar)
  ; converts a tvar's id to a string
  (if (eqv? (tvar-id tvar) 0)
      "Dynamic"
      (string-append "t#" (number->string (tvar-id tvar) 10))))

(define (tvar-show tv)
  ; returns a printable list representation of type variable tv
  (let* ((tv-rep (find! tv))
	 (tv-def (tvar-def tv-rep)))
    (cons (tvar->string tv-rep)
	  (if (null? tv-def)
	      '()
	      (cons 'is (type-show tv-def))))))

(define (type-show type)
  ; returns a printable list representation of type definition type
  (cond
   ((eqv? (type-con type) ptype-con)
    (let ((new-tvar (gen-tvar)))
      (cons ptype-con
	    (cons (tvar-show new-tvar)
		  (tvar-show ((type-args type) new-tvar))))))
   (else
    (cons (type-con type)
	  (map (lambda (tv)
		 (tvar->string (find! tv)))
	       (type-args type))))))



; Special type operations

; type constructor literals

(define boolean-con 'boolean)
(define char-con 'char)
(define null-con 'null)
(define number-con 'number)
(define pair-con 'pair)
(define procedure-con 'procedure)
(define string-con 'string)
(define symbol-con 'symbol)
(define vector-con 'vector)

; type constants and type constructors

(define (null)
  ; ***Note***: Temporarily changed to be a pair!
  ; (gen-type null-con '())
  (pair (gen-tvar) (gen-tvar)))
(define (boolean)
  (gen-type boolean-con '()))
(define (character)
  (gen-type char-con '()))
(define (number)
  (gen-type number-con '()))
(define (charseq)
  (gen-type string-con '()))
(define (symbol)
  (gen-type symbol-con '()))
(define (pair tvar-1 tvar-2)
  (gen-type pair-con (list tvar-1 tvar-2)))
(define (array tvar)
  (gen-type vector-con (list tvar)))
(define (procedure arg-tvar res-tvar)
  (gen-type procedure-con (list arg-tvar res-tvar)))


; equivalencing of type variables

(define (equiv! tv1 tv2)
  (let* ((tv1-rep (find! tv1))
	 (tv2-rep (find! tv2))
	 (tv1-def (tvar-def tv1-rep))
	 (tv2-def (tvar-def tv2-rep)))
    (cond
     ((eqv? tv1-rep tv2-rep)
      '())
     ((eqv? tv2-rep dynamic)
      (equiv-with-dynamic! tv1-rep))
     ((eqv? tv1-rep dynamic)
      (equiv-with-dynamic! tv2-rep))
     ((null? tv1-def)
      (if (null? tv2-def)
	  ; both tv1 and tv2 are distinct type variables
	  (link! tv1-rep tv2-rep)
	  ; tv1 is a type variable, tv2 is a (nondynamic) type
	  (asymm-link! tv1-rep tv2-rep)))
     ((null? tv2-def)
      ; tv1 is a (nondynamic) type, tv2 is a type variable
      (asymm-link! tv2-rep tv1-rep))
     ((eqv? (type-con tv1-def) (type-con tv2-def))
      ; both tv1 and tv2 are (nondynamic) types with equal numbers of
      ; arguments
      (link! tv1-rep tv2-rep)
      (map equiv! (type-args tv1-def) (type-args tv2-def)))
     (else
      ; tv1 and tv2 are types with distinct type constructors or different
      ; numbers of arguments
      (equiv-with-dynamic! tv1-rep)
      (equiv-with-dynamic! tv2-rep))))
  '())

(define (equiv-with-dynamic! tv)
  (let ((tv-rep (find! tv)))
    (if (not (eqv? tv-rep dynamic))
	(let ((tv-def (tvar-def tv-rep)))
	  (asymm-link! tv-rep dynamic)
	  (if (not (null? tv-def))
	      (map equiv-with-dynamic! (type-args tv-def))))))
  '())
;----------------------------------------------------------------------------
; Polymorphic type management
;----------------------------------------------------------------------------

; introduces parametric polymorphic types


;; forall: (Tvar -> Tvar) -> TVar
;; fix: (Tvar -> Tvar) -> Tvar
;;  
;; instantiate-type: TVar -> TVar

; type constructor literal for polymorphic types

(define ptype-con 'forall)

(define (forall tv-func)
  (gen-type ptype-con tv-func))

(define (forall2 tv-func2)
  (forall (lambda (tv1)
	    (forall (lambda (tv2)
		      (tv-func2 tv1 tv2))))))

(define (forall3 tv-func3)
  (forall (lambda (tv1)
	    (forall2 (lambda (tv2 tv3)
		       (tv-func3 tv1 tv2 tv3))))))

(define (forall4 tv-func4)
  (forall (lambda (tv1)
	    (forall3 (lambda (tv2 tv3 tv4)
		       (tv-func4 tv1 tv2 tv3 tv4))))))

(define (forall5 tv-func5)
  (forall (lambda (tv1)
	    (forall4 (lambda (tv2 tv3 tv4 tv5)
		       (tv-func5 tv1 tv2 tv3 tv4 tv5))))))


; (polymorphic) instantiation

(define (instantiate-type tv)
  ; instantiates type tv and returns a generic instance
  (let* ((tv-rep (find! tv))
	 (tv-def (tvar-def tv-rep)))
    (cond 
     ((null? tv-def)
      tv-rep)
     ((eqv? (type-con tv-def) ptype-con)
      (instantiate-type ((type-args tv-def) (gen-tvar))))
     (else
      tv-rep))))

(define (fix tv-func)
  ; forms a recursive type: the fixed point of type mapping tv-func
  (let* ((new-tvar (gen-tvar))
	 (inst-tvar (tv-func new-tvar))
	 (inst-def (tvar-def inst-tvar)))
    (if (null? inst-def)
	(error 'fix "Illegal recursive type: ~s"
	       (list (tvar-show new-tvar) '= (tvar-show inst-tvar)))
	(begin
	  (set-def! new-tvar 
		    (type-con inst-def)
		    (type-args inst-def))
	  new-tvar))))

  
;----------------------------------------------------------------------------
;	Constraint management 
;----------------------------------------------------------------------------


; constraints

(define gen-constr (lambda (a b) (cons a b)))
; generates an equality between tvar1 and tvar2

(define constr-lhs (lambda (c) (car c)))
; returns the left-hand side of a constraint

(define constr-rhs (lambda (c) (cdr c)))
; returns the right-hand side of a constraint

(define (constr-show c)
  (cons (tvar-show (car c)) 
	(cons '= 
	      (cons (tvar-show (cdr c)) '()))))


; constraint set management

(define global-constraints '())

(define (init-global-constraints!)
  (set! global-constraints '()))

(define (add-constr! lhs rhs)
  (set! global-constraints
	(cons (gen-constr lhs rhs) global-constraints))
  '())

(define (glob-constr-show) 
  ; returns printable version of global constraints
  (map constr-show global-constraints))


; constraint normalization

; Needed packages: type management

;(load "typ-mgmt.so")

(define (normalize-global-constraints!) 
  (normalize! global-constraints)
  (init-global-constraints!))

(define (normalize! constraints)
  (map (lambda (c)
	 (equiv! (constr-lhs c) (constr-rhs c))) constraints))
; ----------------------------------------------------------------------------
; Abstract syntax definition and parse actions
; ----------------------------------------------------------------------------

; Needed packages: ast-gen.ss
;(load "ast-gen.ss")

;; Abstract syntax
;;
;; VarDef
;;
;; Identifier =		Symbol - SyntacticKeywords
;; SyntacticKeywords =	{ ... } (see Section 7.1, IEEE Scheme Standard)
;;
;; Datum
;;
;; null-const:		Null		-> Datum
;; boolean-const:	Bool		-> Datum
;; char-const:		Char		-> Datum
;; number-const:	Number		-> Datum
;; string-const:	String		-> Datum
;; vector-const:	Datum*		-> Datum
;; pair-const:		Datum x Datum	-> Datum
;;
;; Expr
;;
;; Datum < 		Expr
;;
;; var-def:             Identifier              -> VarDef
;; variable:		VarDef			-> Expr
;; identifier:		Identifier		-> Expr
;; procedure-call:	Expr x Expr*		-> Expr
;; lambda-expression:	Formals x Body		-> Expr
;; conditional:		Expr x Expr x Expr 	-> Expr
;; assignment:		Variable x Expr		-> Expr
;; cond-expression:	CondClause+	        -> Expr
;; case-expression:	Expr x CaseClause* 	-> Expr
;; and-expression:	Expr* 			-> Expr
;; or-expression:	Expr* 			-> Expr
;; let-expression:	(VarDef* x Expr*) x Body -> Expr
;; named-let-expression: VarDef x (VarDef* x Expr*) x Body -> Expr
;; let*-expression:	(VarDef* x Expr*) x Body -> Expr
;; letrec-expression:	(VarDef* x Expr*) x Body -> Expr
;; begin-expression:	Expr+ 			-> Expr
;; do-expression:	IterDef* x CondClause x Expr* -> Expr
;; empty:					-> Expr
;;
;; VarDef* <		Formals
;;
;; simple-formal:	VarDef			-> Formals
;; dotted-formals:	VarDef* x VarDef	-> Formals
;;
;; Body =		Definition* x Expr+	(reversed)
;; CondClause =		Expr x Expr+
;; CaseClause =		Datum* x Expr+
;; IterDef =		VarDef x Expr x Expr
;;
;; Definition
;;
;; definition:		Identifier x Expr	-> Definition
;; function-definition: Identifier x Formals x Body -> Definition
;; begin-command:	Definition*		-> Definition
;;
;; Expr <		Command
;; Definition <		Command
;;
;; Program =		Command*


;; Abstract syntax operators

; Datum

(define null-const 0)
(define boolean-const 1)
(define char-const 2)
(define number-const 3)
(define string-const 4)
(define symbol-const 5)
(define vector-const 6)
(define pair-const 7)

; Bindings

(define var-def 8)
(define null-def 29)
(define pair-def 30)

; Expr

(define variable 9)
(define identifier 10)
(define procedure-call 11)
(define lambda-expression 12)
(define conditional 13)
(define assignment 14)
(define cond-expression 15)
(define case-expression 16)
(define and-expression 17)
(define or-expression 18)
(define let-expression 19)
(define named-let-expression 20)
(define let*-expression 21)
(define letrec-expression 22)
(define begin-expression 23)
(define do-expression 24)
(define empty 25)
(define null-arg 31)
(define pair-arg 32)

; Command

(define definition 26)
(define function-definition 27)
(define begin-command 28)


;; Parse actions for abstract syntax construction

(define (dynamic-parse-action-null-const)
  ;; dynamic-parse-action for '()
  (ast-gen null-const '()))

(define (dynamic-parse-action-boolean-const e)
  ;; dynamic-parse-action for #f and #t
  (ast-gen boolean-const e))

(define (dynamic-parse-action-char-const e)
  ;; dynamic-parse-action for character constants
  (ast-gen char-const e))

(define (dynamic-parse-action-number-const e)
  ;; dynamic-parse-action for number constants
  (ast-gen number-const e))

(define (dynamic-parse-action-string-const e)
  ;; dynamic-parse-action for string literals
  (ast-gen string-const e))

(define (dynamic-parse-action-symbol-const e)
  ;; dynamic-parse-action for symbol constants
  (ast-gen symbol-const e))

(define (dynamic-parse-action-vector-const e)
  ;; dynamic-parse-action for vector literals
  (ast-gen vector-const e))

(define (dynamic-parse-action-pair-const e1 e2)
  ;; dynamic-parse-action for pairs
  (ast-gen pair-const (cons e1 e2)))

(define (dynamic-parse-action-var-def e)
  ;; dynamic-parse-action for defining occurrences of variables;
  ;; e is a symbol
  (ast-gen var-def e))

(define (dynamic-parse-action-null-formal)
  ;; dynamic-parse-action for null-list of formals
  (ast-gen null-def '()))

(define (dynamic-parse-action-pair-formal d1 d2)
  ;; dynamic-parse-action for non-null list of formals;
  ;; d1 is the result of parsing the first formal,
  ;; d2 the result of parsing the remaining formals
  (ast-gen pair-def (cons d1 d2)))

(define (dynamic-parse-action-variable e)
  ;; dynamic-parse-action for applied occurrences of variables
  ;; ***Note***: e is the result of a dynamic-parse-action on the
  ;; corresponding variable definition!
  (ast-gen variable e))

(define (dynamic-parse-action-identifier e)
  ;; dynamic-parse-action for undeclared identifiers (free variable
  ;; occurrences)
  ;; ***Note***: e is a symbol (legal identifier)
  (ast-gen identifier e))
 
(define (dynamic-parse-action-null-arg)
  ;; dynamic-parse-action for a null list of arguments in a procedure call
  (ast-gen null-arg '()))

(define (dynamic-parse-action-pair-arg a1 a2)
  ;; dynamic-parse-action for a non-null list of arguments in a procedure call
  ;; a1 is the result of parsing the first argument, 
  ;; a2 the result of parsing the remaining arguments
  (ast-gen pair-arg (cons a1 a2)))

(define (dynamic-parse-action-procedure-call op args)
  ;; dynamic-parse-action for procedure calls: op function, args list of arguments
  (ast-gen procedure-call (cons op args)))

(define (dynamic-parse-action-lambda-expression formals body)
  ;; dynamic-parse-action for lambda-abstractions
  (ast-gen lambda-expression (cons formals body)))

(define (dynamic-parse-action-conditional test then-branch else-branch)
  ;; dynamic-parse-action for conditionals (if-then-else expressions)
  (ast-gen conditional (cons test (cons then-branch else-branch))))

(define (dynamic-parse-action-empty)
  ;; dynamic-parse-action for missing or empty field
  (ast-gen empty '()))

(define (dynamic-parse-action-assignment lhs rhs)
  ;; dynamic-parse-action for assignment
  (ast-gen assignment (cons lhs rhs)))

(define (dynamic-parse-action-begin-expression body)
  ;; dynamic-parse-action for begin-expression
  (ast-gen begin-expression body))

(define (dynamic-parse-action-cond-expression clauses)
  ;; dynamic-parse-action for cond-expressions
  (ast-gen cond-expression clauses))

(define (dynamic-parse-action-and-expression args)
  ;; dynamic-parse-action for and-expressions
  (ast-gen and-expression args))

(define (dynamic-parse-action-or-expression args)
  ;; dynamic-parse-action for or-expressions
  (ast-gen or-expression args))

(define (dynamic-parse-action-case-expression key clauses)
  ;; dynamic-parse-action for case-expressions
  (ast-gen case-expression (cons key clauses)))

(define (dynamic-parse-action-let-expression bindings body)
  ;; dynamic-parse-action for let-expressions
  (ast-gen let-expression (cons bindings body)))

(define (dynamic-parse-action-named-let-expression variable bindings body)
  ;; dynamic-parse-action for named-let expressions
  (ast-gen named-let-expression (cons variable (cons bindings body))))

(define (dynamic-parse-action-let*-expression bindings body)
  ;; dynamic-parse-action for let-expressions
  (ast-gen let*-expression (cons bindings body)))

(define (dynamic-parse-action-letrec-expression bindings body)
  ;; dynamic-parse-action for let-expressions
  (ast-gen letrec-expression (cons bindings body)))

(define (dynamic-parse-action-definition variable expr)
  ;; dynamic-parse-action for simple definitions
  (ast-gen definition (cons variable expr)))

(define (dynamic-parse-action-function-definition variable formals body)
  ;; dynamic-parse-action for function definitions
  (ast-gen function-definition (cons variable (cons formals body))))


(define dynamic-parse-action-commands (lambda (a b) (cons a b)))
;; dynamic-parse-action for processing a command result followed by a the
;; result of processing the remaining commands


;; Pretty-printing abstract syntax trees

(define (ast-show ast)
  ;; converts abstract syntax tree to list representation (Scheme program)
  ;; ***Note***: check translation of constructors to numbers at the top of the file
  (let ((syntax-op (ast-con ast))
	(syntax-arg (ast-arg ast)))
    (case syntax-op
      ((0 1 2 3 4 8 10) syntax-arg)
      ((29 31) '())
      ((30 32) (cons (ast-show (car syntax-arg)) (ast-show (cdr syntax-arg))))
      ((5) (list 'quote syntax-arg))
      ((6) (list->vector (map ast-show syntax-arg)))
      ((7) (list 'cons (ast-show (car syntax-arg)) (ast-show (cdr syntax-arg))))
      ((9) (ast-arg syntax-arg))
      ((11) (cons (ast-show (car syntax-arg)) (ast-show (cdr syntax-arg))))
      ((12) (cons 'lambda (cons (ast-show (car syntax-arg)) 
				(map ast-show (cdr syntax-arg)))))
      ((13) (cons 'if (cons (ast-show (car syntax-arg))
			    (cons (ast-show (cadr syntax-arg))
				  (let ((alt (cddr syntax-arg)))
				    (if (eqv? (ast-con alt) empty)
					'()
					(list (ast-show alt))))))))
      ((14) (list 'set! (ast-show (car syntax-arg)) (ast-show (cdr syntax-arg))))
      ((15) (cons 'cond
		  (map (lambda (cc)
			 (let ((guard (car cc))
			       (body (cdr cc)))
			   (cons
			    (if (eqv? (ast-con guard) empty)
				'else
				(ast-show guard))
			    (map ast-show body))))
		       syntax-arg)))
      ((16) (cons 'case
		  (cons (ast-show (car syntax-arg))
			(map (lambda (cc)
			       (let ((data (car cc)))
				 (if (and (pair? data)
					  (eqv? (ast-con (car data)) empty))
				     (cons 'else
					   (map ast-show (cdr cc)))
				     (cons (map datum-show data)
					   (map ast-show (cdr cc))))))
			     (cdr syntax-arg)))))
      ((17) (cons 'and (map ast-show syntax-arg)))
      ((18) (cons 'or (map ast-show syntax-arg)))
      ((19) (cons 'let
		  (cons (map
			 (lambda (vd e)
			   (list (ast-show vd) (ast-show e)))
			 (caar syntax-arg)
			 (cdar syntax-arg))
			(map ast-show (cdr syntax-arg)))))
      ((20) (cons 'let
		  (cons (ast-show (car syntax-arg))
			(cons (map
			       (lambda (vd e)
				 (list (ast-show vd) (ast-show e)))
			       (caadr syntax-arg)
			       (cdadr syntax-arg))
			      (map ast-show (cddr syntax-arg))))))
      ((21) (cons 'let*
		  (cons (map
			 (lambda (vd e)
			   (list (ast-show vd) (ast-show e)))
			 (caar syntax-arg)
			 (cdar syntax-arg))
			(map ast-show (cdr syntax-arg)))))
      ((22) (cons 'letrec
		  (cons (map
			 (lambda (vd e)
			   (list (ast-show vd) (ast-show e)))
			 (caar syntax-arg)
			 (cdar syntax-arg))
			(map ast-show (cdr syntax-arg)))))
      ((23) (cons 'begin
		  (map ast-show syntax-arg)))
      ((24) (error 'ast-show "Do expressions not handled! (~s)" syntax-arg))
      ((25) (error 'ast-show "This can't happen: empty encountered!"))
      ((26) (list 'define
		  (ast-show (car syntax-arg))
		  (ast-show (cdr syntax-arg))))
      ((27) (cons 'define
		  (cons
		   (cons (ast-show (car syntax-arg))
			 (ast-show (cadr syntax-arg)))
		   (map ast-show (cddr syntax-arg)))))
      ((28) (cons 'begin
		  (map ast-show syntax-arg)))
      (else (error 'ast-show "Unknown abstract syntax operator: ~s"
		   syntax-op)))))


;; ast*-show

(define (ast*-show p)
  ;; shows a list of abstract syntax trees
  (map ast-show p))


;; datum-show

(define (datum-show ast)
  ;; prints an abstract syntax tree as a datum
  (case (ast-con ast)
    ((0 1 2 3 4 5) (ast-arg ast))
    ((6) (list->vector (map datum-show (ast-arg ast))))
    ((7) (cons (datum-show (car (ast-arg ast))) (datum-show (cdr (ast-arg ast)))))
    (else (error 'datum-show "This should not happen!"))))

; write-to-port

(define (write-to-port prog port)
  ; writes a program to a port
  (for-each
   (lambda (command)
     (write command port)
     (newline port))
   prog)
  '())

; write-file 

(define (write-to-file prog filename)
  ; write a program to a file
  (let ((port (open-output-file filename)))
    (write-to-port prog port)
    (close-output-port port)
    '()))

; ----------------------------------------------------------------------------
; Typed abstract syntax tree management: constraint generation, display, etc.
; ----------------------------------------------------------------------------


;; Abstract syntax operations, incl. constraint generation

(define (ast-gen syntax-op arg)
  ; generates all attributes and performs semantic side effects
  (let ((ntvar
	 (case syntax-op
	   ((0 29 31) (null))
	   ((1) (boolean))
	   ((2) (character))
	   ((3) (number))
	   ((4) (charseq))
	   ((5) (symbol))
	   ((6) (let ((aux-tvar (gen-tvar)))
		  (for-each (lambda (t)
			      (add-constr! t aux-tvar))
			    (map ast-tvar arg))
		  (array aux-tvar)))
	   ((7 30 32) (let ((t1 (ast-tvar (car arg)))
			    (t2 (ast-tvar (cdr arg))))
			(pair t1 t2)))
	   ((8) (gen-tvar))
	   ((9) (ast-tvar arg))
	   ((10) (let ((in-env (dynamic-lookup arg dynamic-top-level-env)))
		   (if in-env
		       (instantiate-type (binding-value in-env))
		       (let ((new-tvar (gen-tvar)))
			 (set! dynamic-top-level-env (extend-env-with-binding
					      dynamic-top-level-env
					      (gen-binding arg new-tvar)))
			 new-tvar))))
	   ((11) (let ((new-tvar (gen-tvar)))
		   (add-constr! (procedure (ast-tvar (cdr arg)) new-tvar)
				(ast-tvar (car arg)))
		   new-tvar))
	   ((12) (procedure (ast-tvar (car arg))
			    (ast-tvar (tail (cdr arg)))))
	   ((13) (let ((t-test (ast-tvar (car arg)))
		       (t-consequent (ast-tvar (cadr arg)))
		       (t-alternate (ast-tvar (cddr arg))))
		   (add-constr! (boolean) t-test)
		   (add-constr! t-consequent t-alternate)
		   t-consequent))
	   ((14) (let ((var-tvar (ast-tvar (car arg)))
		       (exp-tvar (ast-tvar (cdr arg))))
		   (add-constr! var-tvar exp-tvar)
		   var-tvar))
	   ((15) (let ((new-tvar (gen-tvar)))
		   (for-each (lambda (body)
			       (add-constr! (ast-tvar (tail body)) new-tvar))
			     (map cdr arg))
		   (for-each (lambda (e)
			       (add-constr! (boolean) (ast-tvar e)))
			     (map car arg))
		   new-tvar))
	   ((16) (let* ((new-tvar (gen-tvar))
			(t-key (ast-tvar (car arg)))
			(case-clauses (cdr arg)))
		   (for-each (lambda (exprs)
			       (for-each (lambda (e)
					   (add-constr! (ast-tvar e) t-key))
					 exprs))
			     (map car case-clauses))
		   (for-each (lambda (body)
			       (add-constr! (ast-tvar (tail body)) new-tvar))
			     (map cdr case-clauses))
		   new-tvar))
	   ((17 18) (for-each (lambda (e)
				(add-constr! (boolean) (ast-tvar e)))
			      arg)
		    (boolean))
	   ((19 21 22) (let ((var-def-tvars (map ast-tvar (caar arg)))
			     (def-expr-types (map ast-tvar (cdar arg)))
			     (body-type (ast-tvar (tail (cdr arg)))))
			 (for-each add-constr! var-def-tvars def-expr-types)
			 body-type))
	   ((20) (let ((var-def-tvars (map ast-tvar (caadr arg)))
		       (def-expr-types (map ast-tvar (cdadr arg)))
		       (body-type (ast-tvar (tail (cddr arg))))
		       (named-var-type (ast-tvar (car arg))))
		   (for-each add-constr! var-def-tvars def-expr-types)
		   (add-constr! (procedure (convert-tvars var-def-tvars) body-type)
				named-var-type)
		   body-type))
	   ((23) (ast-tvar (tail arg)))
	   ((24) (error 'ast-gen
			"Do-expressions not handled! (Argument: ~s) arg"))
	   ((25) (gen-tvar))
	   ((26) (let ((t-var (ast-tvar (car arg)))
		       (t-exp (ast-tvar (cdr arg))))
		   (add-constr! t-var t-exp)
		   t-var))
	   ((27) (let ((t-var (ast-tvar (car arg)))
		       (t-formals (ast-tvar (cadr arg)))
		       (t-body (ast-tvar (tail (cddr arg)))))
		   (add-constr! (procedure t-formals t-body) t-var)
		   t-var))
	   ((28) (gen-tvar))
	   (else (error 'ast-gen "Can't handle syntax operator: ~s" syntax-op)))))
    (cons syntax-op (cons ntvar arg))))

(define ast-con car)
;; extracts the ast-constructor from an abstract syntax tree

(define ast-arg cddr)
;; extracts the ast-argument from an abstract syntax tree

(define ast-tvar cadr)
;; extracts the tvar from an abstract syntax tree


;; tail

(define (tail l)
  ;; returns the tail of a nonempty list
  (if (null? (cdr l))
      (car l)
      (tail (cdr l))))

; convert-tvars

(define (convert-tvars tvar-list)
  ;; converts a list of tvars to a single tvar
  (cond
   ((null? tvar-list) (null))
   ((pair? tvar-list) (pair (car tvar-list)
			    (convert-tvars (cdr tvar-list))))
   (else (error 'convert-tvars "Not a list of tvars: ~s" tvar-list))))


;; Pretty-printing abstract syntax trees

(define (tast-show ast)
  ;; converts abstract syntax tree to list representation (Scheme program)
  (let ((syntax-op (ast-con ast))
        (syntax-tvar (tvar-show (ast-tvar ast)))
	(syntax-arg (ast-arg ast)))
    (cons
     (case syntax-op
       ((0 1 2 3 4 8 10) syntax-arg)
       ((29 31) '())
       ((30 32) (cons (tast-show (car syntax-arg))
		      (tast-show (cdr syntax-arg))))
       ((5) (list 'quote syntax-arg))
       ((6) (list->vector (map tast-show syntax-arg)))
       ((7) (list 'cons (tast-show (car syntax-arg))
		  (tast-show (cdr syntax-arg))))
       ((9) (ast-arg syntax-arg))
       ((11) (cons (tast-show (car syntax-arg)) (tast-show (cdr syntax-arg))))
       ((12) (cons 'lambda (cons (tast-show (car syntax-arg))
				 (map tast-show (cdr syntax-arg)))))
       ((13) (cons 'if (cons (tast-show (car syntax-arg))
			     (cons (tast-show (cadr syntax-arg))
				   (let ((alt (cddr syntax-arg)))
				     (if (eqv? (ast-con alt) empty)
					 '()
					 (list (tast-show alt))))))))
       ((14) (list 'set! (tast-show (car syntax-arg))
		   (tast-show (cdr syntax-arg))))
       ((15) (cons 'cond
		   (map (lambda (cc)
			  (let ((guard (car cc))
				(body (cdr cc)))
			    (cons
			     (if (eqv? (ast-con guard) empty)
				 'else
				 (tast-show guard))
			     (map tast-show body))))
			syntax-arg)))
       ((16) (cons 'case
		   (cons (tast-show (car syntax-arg))
			 (map (lambda (cc)
				(let ((data (car cc)))
				  (if (and (pair? data)
					   (eqv? (ast-con (car data)) empty))
				      (cons 'else
					    (map tast-show (cdr cc)))
				      (cons (map datum-show data)
					    (map tast-show (cdr cc))))))
			      (cdr syntax-arg)))))
       ((17) (cons 'and (map tast-show syntax-arg)))
       ((18) (cons 'or (map tast-show syntax-arg)))
       ((19) (cons 'let
		   (cons (map
			  (lambda (vd e)
			    (list (tast-show vd) (tast-show e)))
			  (caar syntax-arg)
			  (cdar syntax-arg))
			 (map tast-show (cdr syntax-arg)))))
       ((20) (cons 'let
		   (cons (tast-show (car syntax-arg))
			 (cons (map
				(lambda (vd e)
				  (list (tast-show vd) (tast-show e)))
				(caadr syntax-arg)
				(cdadr syntax-arg))
			       (map tast-show (cddr syntax-arg))))))
       ((21) (cons 'let*
		   (cons (map
			  (lambda (vd e)
			    (list (tast-show vd) (tast-show e)))
			  (caar syntax-arg)
			  (cdar syntax-arg))
			 (map tast-show (cdr syntax-arg)))))
       ((22) (cons 'letrec
		   (cons (map
			  (lambda (vd e)
			    (list (tast-show vd) (tast-show e)))
			  (caar syntax-arg)
			  (cdar syntax-arg))
			 (map tast-show (cdr syntax-arg)))))
       ((23) (cons 'begin
		   (map tast-show syntax-arg)))
       ((24) (error 'tast-show "Do expressions not handled! (~s)" syntax-arg))
       ((25) (error 'tast-show "This can't happen: empty encountered!"))
       ((26) (list 'define
		   (tast-show (car syntax-arg))
		   (tast-show (cdr syntax-arg))))
       ((27) (cons 'define
		   (cons
		    (cons (tast-show (car syntax-arg))
			  (tast-show (cadr syntax-arg)))
		    (map tast-show (cddr syntax-arg)))))
       ((28) (cons 'begin
		   (map tast-show syntax-arg)))
       (else (error 'tast-show "Unknown abstract syntax operator: ~s"
		    syntax-op)))
     syntax-tvar)))

;; tast*-show

(define (tast*-show p)
  ;; shows a list of abstract syntax trees
  (map tast-show p))


;; counters for tagging/untagging

(define untag-counter 0)
(define no-untag-counter 0)
(define tag-counter 0)
(define no-tag-counter 0)
(define may-untag-counter 0)
(define no-may-untag-counter 0)

(define (reset-counters!)
  (set! untag-counter 0)
  (set! no-untag-counter 0)
  (set! tag-counter 0)
  (set! no-tag-counter 0)
  (set! may-untag-counter 0)
  (set! no-may-untag-counter 0))

(define (counters-show)
  (list
   (cons tag-counter no-tag-counter)
   (cons untag-counter no-untag-counter)
   (cons may-untag-counter no-may-untag-counter)))  


;; tag-show

(define (tag-show tvar-rep prog)
  ; display prog with tagging operation
  (if (eqv? tvar-rep dynamic)
      (begin
	(set! tag-counter (+ tag-counter 1))
	(list 'tag prog))
      (begin
	(set! no-tag-counter (+ no-tag-counter 1))
	(list 'no-tag prog))))


;; untag-show

(define (untag-show tvar-rep prog)
  ; display prog with untagging operation
  (if (eqv? tvar-rep dynamic)
      (begin
	(set! untag-counter (+ untag-counter 1))
	(list 'untag prog))
      (begin
	(set! no-untag-counter (+ no-untag-counter 1))
	(list 'no-untag prog))))

(define (may-untag-show tvar-rep prog)
  ; display possible untagging in actual arguments
  (if (eqv? tvar-rep dynamic)
      (begin
	(set! may-untag-counter (+ may-untag-counter 1))
	(list 'may-untag prog))
      (begin
	(set! no-may-untag-counter (+ no-may-untag-counter 1))
	(list 'no-may-untag prog))))


;; tag-ast-show

(define (tag-ast-show ast)
  ;; converts typed and normalized abstract syntax tree to
  ;; a Scheme program with explicit tagging and untagging operations
  (let ((syntax-op (ast-con ast))
        (syntax-tvar (find! (ast-tvar ast)))
	(syntax-arg (ast-arg ast)))
    (case syntax-op
      ((0 1 2 3 4)
       (tag-show syntax-tvar syntax-arg))
      ((8 10) syntax-arg)
      ((29 31) '())
      ((30) (cons (tag-ast-show (car syntax-arg))
                  (tag-ast-show (cdr syntax-arg))))
      ((32) (cons (may-untag-show (find! (ast-tvar (car syntax-arg)))
                              (tag-ast-show (car syntax-arg)))
                  (tag-ast-show (cdr syntax-arg))))
      ((5) (tag-show syntax-tvar (list 'quote syntax-arg)))
      ((6) (tag-show syntax-tvar (list->vector (map tag-ast-show syntax-arg))))
      ((7) (tag-show syntax-tvar (list 'cons (tag-ast-show (car syntax-arg))
				       (tag-ast-show (cdr syntax-arg)))))
      ((9) (ast-arg syntax-arg))
      ((11) (let ((proc-tvar (find! (ast-tvar (car syntax-arg)))))
	      (cons (untag-show proc-tvar 
				(tag-ast-show (car syntax-arg)))
		    (tag-ast-show (cdr syntax-arg)))))
      ((12) (tag-show syntax-tvar
		      (cons 'lambda (cons (tag-ast-show (car syntax-arg))
					  (map tag-ast-show (cdr syntax-arg))))))
      ((13) (let ((test-tvar (find! (ast-tvar (car syntax-arg)))))
	      (cons 'if (cons (untag-show test-tvar
					  (tag-ast-show (car syntax-arg)))
			      (cons (tag-ast-show (cadr syntax-arg))
				    (let ((alt (cddr syntax-arg)))
				      (if (eqv? (ast-con alt) empty)
					  '()
					  (list (tag-ast-show alt)))))))))
      ((14) (list 'set! (tag-ast-show (car syntax-arg))
                  (tag-ast-show (cdr syntax-arg))))
      ((15) (cons 'cond
		  (map (lambda (cc)
			 (let ((guard (car cc))
			       (body (cdr cc)))
			   (cons
			    (if (eqv? (ast-con guard) empty)
				'else
				(untag-show (find! (ast-tvar guard))
					    (tag-ast-show guard)))
			    (map tag-ast-show body))))
		       syntax-arg)))
      ((16) (cons 'case
		  (cons (tag-ast-show (car syntax-arg))
			(map (lambda (cc)
			       (let ((data (car cc)))
				 (if (and (pair? data)
					  (eqv? (ast-con (car data)) empty))
				     (cons 'else
					   (map tag-ast-show (cdr cc)))
				     (cons (map datum-show data)
					   (map tag-ast-show (cdr cc))))))
			     (cdr syntax-arg)))))
      ((17) (cons 'and (map
			(lambda (ast)
			  (let ((bool-tvar (find! (ast-tvar ast))))
			    (untag-show bool-tvar (tag-ast-show ast))))
			syntax-arg)))
      ((18) (cons 'or (map
		       (lambda (ast)
			 (let ((bool-tvar (find! (ast-tvar ast))))
			   (untag-show bool-tvar (tag-ast-show ast))))
		       syntax-arg)))
      ((19) (cons 'let
		  (cons (map
			 (lambda (vd e)
			   (list (tag-ast-show vd) (tag-ast-show e)))
			 (caar syntax-arg)
			 (cdar syntax-arg))
			(map tag-ast-show (cdr syntax-arg)))))
      ((20) (cons 'let
		  (cons (tag-ast-show (car syntax-arg))
			(cons (map
			       (lambda (vd e)
				 (list (tag-ast-show vd) (tag-ast-show e)))
			       (caadr syntax-arg)
			       (cdadr syntax-arg))
			      (map tag-ast-show (cddr syntax-arg))))))
      ((21) (cons 'let*
		  (cons (map
			 (lambda (vd e)
			   (list (tag-ast-show vd) (tag-ast-show e)))
			 (caar syntax-arg)
			 (cdar syntax-arg))
			(map tag-ast-show (cdr syntax-arg)))))
      ((22) (cons 'letrec
		  (cons (map
			 (lambda (vd e)
			   (list (tag-ast-show vd) (tag-ast-show e)))
			 (caar syntax-arg)
			 (cdar syntax-arg))
			(map tag-ast-show (cdr syntax-arg)))))
      ((23) (cons 'begin
		  (map tag-ast-show syntax-arg)))
      ((24) (error 'tag-ast-show "Do expressions not handled! (~s)" syntax-arg))
      ((25) (error 'tag-ast-show "This can't happen: empty encountered!"))
      ((26) (list 'define
		  (tag-ast-show (car syntax-arg))
		  (tag-ast-show (cdr syntax-arg))))
      ((27) (let ((func-tvar (find! (ast-tvar (car syntax-arg)))))
	      (list 'define
		    (tag-ast-show (car syntax-arg))
		    (tag-show func-tvar
			      (cons 'lambda
				    (cons (tag-ast-show (cadr syntax-arg))
					  (map tag-ast-show (cddr syntax-arg))))))))
      ((28) (cons 'begin
		  (map tag-ast-show syntax-arg)))
      (else (error 'tag-ast-show "Unknown abstract syntax operator: ~s"
		   syntax-op)))))


; tag-ast*-show

(define (tag-ast*-show p)
  ; display list of commands/expressions with tagging/untagging
  ; operations
  (map tag-ast-show p))
; ----------------------------------------------------------------------------
; Top level type environment
; ----------------------------------------------------------------------------


; Needed packages: type management (monomorphic and polymorphic)

;(load "typ-mgmt.ss")
;(load "ptyp-mgm.ss")


; type environment for miscellaneous

(define misc-env
  (list
   (cons 'quote (forall (lambda (tv) tv)))
   (cons 'eqv? (forall (lambda (tv) (procedure (convert-tvars (list tv tv))
					       (boolean)))))
   (cons 'eq? (forall (lambda (tv) (procedure (convert-tvars (list tv tv))
					      (boolean)))))
   (cons 'equal? (forall (lambda (tv) (procedure (convert-tvars (list tv tv))
						 (boolean)))))
   ))

; type environment for input/output

(define io-env
  (list
   (cons 'open-input-file (procedure (convert-tvars (list (charseq))) dynamic))
   (cons 'eof-object? (procedure (convert-tvars (list dynamic)) (boolean)))
   (cons 'read (forall (lambda (tv)
			 (procedure (convert-tvars (list tv)) dynamic))))
   (cons 'write (forall (lambda (tv)
			  (procedure (convert-tvars (list tv)) dynamic))))
   (cons 'display (forall (lambda (tv)
			    (procedure (convert-tvars (list tv)) dynamic))))
   (cons 'newline (procedure (null) dynamic))
   (cons 'pretty-print (forall (lambda (tv)
				 (procedure (convert-tvars (list tv)) dynamic))))))


; type environment for Booleans

(define boolean-env
  (list
   (cons 'boolean? (forall (lambda (tv)
			     (procedure (convert-tvars (list tv)) (boolean)))))
   ;(cons #f (boolean))
   ; #f doesn't exist in Chez Scheme, but gets mapped to null!
   (cons #t (boolean))
   (cons 'not (procedure (convert-tvars (list (boolean))) (boolean)))
   ))


; type environment for pairs and lists

(define (list-type tv)
  (fix (lambda (tv2) (pair tv tv2))))

(define list-env
  (list
   (cons 'pair? (forall2 (lambda (tv1 tv2)
			   (procedure (convert-tvars (list (pair tv1 tv2)))
				      (boolean)))))
   (cons 'null? (forall2 (lambda (tv1 tv2)
			   (procedure (convert-tvars (list (pair tv1 tv2)))
				      (boolean)))))
   (cons 'list? (forall2 (lambda (tv1 tv2)
			   (procedure (convert-tvars (list (pair tv1 tv2)))
				      (boolean)))))
   (cons 'cons (forall2 (lambda (tv1 tv2)
			  (procedure (convert-tvars (list tv1 tv2))
				     (pair tv1 tv2)))))
   (cons 'car (forall2 (lambda (tv1 tv2)
			 (procedure (convert-tvars (list (pair tv1 tv2)))
				    tv1))))
   (cons 'cdr (forall2 (lambda (tv1 tv2)
			 (procedure (convert-tvars (list (pair tv1 tv2)))
				    tv2))))
   (cons 'set-car! (forall2 (lambda (tv1 tv2)
			      (procedure (convert-tvars (list (pair tv1 tv2)
							      tv1))
					 dynamic))))
   (cons 'set-cdr! (forall2 (lambda (tv1 tv2)
			      (procedure (convert-tvars (list (pair tv1 tv2)
							      tv2))
					 dynamic))))
   (cons 'caar (forall3 (lambda (tv1 tv2 tv3)
			  (procedure (convert-tvars
				      (list (pair (pair tv1 tv2) tv3)))
				     tv1))))
   (cons 'cdar (forall3 (lambda (tv1 tv2 tv3)
			  (procedure (convert-tvars
				      (list (pair (pair tv1 tv2) tv3)))
				     tv2))))

   (cons 'cadr (forall3 (lambda (tv1 tv2 tv3)
			  (procedure (convert-tvars
				      (list (pair tv1 (pair tv2 tv3))))
				     tv2))))
   (cons 'cddr (forall3 (lambda (tv1 tv2 tv3)
			  (procedure (convert-tvars
				      (list (pair tv1 (pair tv2 tv3))))
				     tv3))))
   (cons 'caaar (forall4
		 (lambda (tv1 tv2 tv3 tv4)
		   (procedure (convert-tvars
			       (list (pair (pair (pair tv1 tv2) tv3) tv4)))
			      tv1))))
   (cons 'cdaar (forall4
		 (lambda (tv1 tv2 tv3 tv4)
		   (procedure (convert-tvars
			       (list (pair (pair (pair tv1 tv2) tv3) tv4)))
			      tv2))))
   (cons 'cadar (forall4
		 (lambda (tv1 tv2 tv3 tv4)
		   (procedure (convert-tvars
			       (list (pair (pair tv1 (pair tv2 tv3)) tv4)))
			      tv2))))
   (cons 'cddar (forall4
		 (lambda (tv1 tv2 tv3 tv4)
		   (procedure (convert-tvars
			       (list (pair (pair tv1 (pair tv2 tv3)) tv4)))
			      tv3))))
   (cons 'caadr (forall4
		 (lambda (tv1 tv2 tv3 tv4)
		   (procedure (convert-tvars
			       (list (pair tv1 (pair (pair tv2 tv3) tv4))))
			      tv2))))
   (cons 'cdadr (forall4
		 (lambda (tv1 tv2 tv3 tv4)
		   (procedure (convert-tvars
			       (list (pair tv1 (pair (pair tv2 tv3) tv4))))
			      tv3))))
   (cons 'caddr (forall4
		 (lambda (tv1 tv2 tv3 tv4)
		   (procedure (convert-tvars
			       (list (pair tv1 (pair tv2 (pair tv3 tv4)))))
			      tv3))))
   (cons 'cdddr (forall4
		 (lambda (tv1 tv2 tv3 tv4)
		   (procedure (convert-tvars
			       (list (pair tv1 (pair tv2 (pair tv3 tv4)))))
			      tv4))))
   (cons 'cadddr
         (forall5 (lambda (tv1 tv2 tv3 tv4 tv5)
                    (procedure (convert-tvars
				(list (pair tv1
					    (pair tv2
						  (pair tv3
							(pair tv4 tv5))))))
			       tv4))))
   (cons 'cddddr
         (forall5 (lambda (tv1 tv2 tv3 tv4 tv5)
                    (procedure (convert-tvars
				(list (pair tv1
					    (pair tv2
						  (pair tv3
							(pair tv4 tv5))))))
			       tv5))))
   (cons 'list (forall (lambda (tv)
			 (procedure tv tv))))
   (cons 'length (forall (lambda (tv)
			   (procedure (convert-tvars (list (list-type tv)))
				      (number)))))
   (cons 'append (forall (lambda (tv)
			   (procedure (convert-tvars (list (list-type tv)
							   (list-type tv)))
				      (list-type tv)))))
   (cons 'reverse (forall (lambda (tv)
			    (procedure (convert-tvars (list (list-type tv)))
				       (list-type tv)))))
   (cons 'list-ref (forall (lambda (tv)
			     (procedure (convert-tvars (list (list-type tv)
							     (number)))
					tv))))
   (cons 'memq (forall (lambda (tv)
			 (procedure (convert-tvars (list tv
							 (list-type tv)))
				    (boolean)))))
   (cons 'memv (forall (lambda (tv)
			 (procedure (convert-tvars (list tv
							 (list-type tv)))
				    (boolean)))))
   (cons 'member (forall (lambda (tv)
			   (procedure (convert-tvars (list tv
							   (list-type tv)))
				      (boolean)))))
   (cons 'assq (forall2 (lambda (tv1 tv2)
			  (procedure (convert-tvars
				      (list tv1
					    (list-type (pair tv1 tv2))))
				     (pair tv1 tv2)))))
   (cons 'assv (forall2 (lambda (tv1 tv2)
			  (procedure (convert-tvars
				      (list tv1
					    (list-type (pair tv1 tv2))))
				     (pair tv1 tv2)))))
   (cons 'assoc (forall2 (lambda (tv1 tv2)
			   (procedure (convert-tvars
				       (list tv1
					     (list-type (pair tv1 tv2))))
				      (pair tv1 tv2)))))
   ))


(define symbol-env
  (list
   (cons 'symbol? (forall (lambda (tv)
			    (procedure (convert-tvars (list tv)) (boolean)))))
   (cons 'symbol->string (procedure (convert-tvars (list (symbol))) (charseq)))
   (cons 'string->symbol (procedure (convert-tvars (list (charseq))) (symbol)))
   ))

(define number-env
  (list
   (cons 'number? (forall (lambda (tv)
			    (procedure (convert-tvars (list tv)) (boolean)))))
   (cons '+ (procedure (convert-tvars (list (number) (number))) (number)))
   (cons '- (procedure (convert-tvars (list (number) (number))) (number)))
   (cons '* (procedure (convert-tvars (list (number) (number))) (number)))
   (cons '/ (procedure (convert-tvars (list (number) (number))) (number)))
   (cons 'number->string (procedure (convert-tvars (list (number))) (charseq)))
   (cons 'string->number (procedure (convert-tvars (list (charseq))) (number)))
   ))

(define char-env
  (list
   (cons 'char? (forall (lambda (tv)
			  (procedure (convert-tvars (list tv)) (boolean)))))
   (cons 'char->integer (procedure (convert-tvars (list (character)))
                                   (number)))
   (cons 'integer->char (procedure (convert-tvars (list (number)))
                                   (character)))
   ))

(define string-env
  (list
   (cons 'string? (forall (lambda (tv)
			    (procedure (convert-tvars (list tv)) (boolean)))))
   ))

(define vector-env
  (list
   (cons 'vector? (forall (lambda (tv)
			    (procedure (convert-tvars (list tv)) (boolean)))))
   (cons 'make-vector (forall (lambda (tv)
				(procedure (convert-tvars (list (number)))
					   (array tv)))))
   (cons 'vector-length (forall (lambda (tv)
				  (procedure (convert-tvars (list (array tv)))
					     (number)))))
   (cons 'vector-ref (forall (lambda (tv)
			       (procedure (convert-tvars (list (array tv)
							       (number)))
					  tv))))
   (cons 'vector-set! (forall (lambda (tv)
				(procedure (convert-tvars (list (array tv)
								(number)
								tv))
					   dynamic))))
   ))

(define procedure-env
  (list
   (cons 'procedure? (forall (lambda (tv)
			       (procedure (convert-tvars (list tv)) (boolean)))))
   (cons 'map (forall2 (lambda (tv1 tv2)
			 (procedure (convert-tvars
				     (list (procedure (convert-tvars
						       (list tv1)) tv2)
					   (list-type tv1)))
				    (list-type tv2)))))
   (cons 'foreach (forall2 (lambda (tv1 tv2)
			     (procedure (convert-tvars
					 (list (procedure (convert-tvars
							   (list tv1)) tv2)
					       (list-type tv1)))
					(list-type tv2)))))
   (cons 'call-with-current-continuation
	 (forall2 (lambda (tv1 tv2) 
		   (procedure (convert-tvars
			       (list (procedure
				      (convert-tvars
				       (list (procedure (convert-tvars
							 (list tv1)) tv2)))
				      tv2)))
			      tv2))))
   ))


; global top level environment

(define (global-env)
  (append misc-env
	  io-env
	  boolean-env
	  symbol-env
	  number-env
	  char-env
	  string-env
	  vector-env
	  procedure-env
	  list-env))

(define dynamic-top-level-env (global-env))

(define (init-dynamic-top-level-env!)
  (set! dynamic-top-level-env (global-env))
  '())

(define (dynamic-top-level-env-show)
  ; displays the top level environment
  (map (lambda (binding)
	 (cons (key-show (binding-key binding))
	       (cons ': (tvar-show (binding-value binding)))))
       (env->list dynamic-top-level-env)))
; ----------------------------------------------------------------------------
; Dynamic type inference for Scheme
; ----------------------------------------------------------------------------

; Needed packages:

(define (ic!) (init-global-constraints!))
(define (pc) (glob-constr-show))
(define (lc) (length global-constraints))
(define (n!) (normalize-global-constraints!))
(define (pt) (dynamic-top-level-env-show))
(define (it!) (init-dynamic-top-level-env!))
(define (io!) (set! tag-ops 0) (set! no-ops 0))
(define (i!) (ic!) (it!) (io!) '())

(define tag-ops 0)
(define no-ops 0)


; This wasn't intended to be an i/o benchmark,
; so let's read the file just once.

(define *forms*
  (call-with-input-file
      "dynamic-input.txt"
    (lambda (port)
      (define (loop forms)
        (let ((form (read port)))
          (if (eof-object? form)
              (reverse forms)
              (loop (cons form forms)))))
      (loop '()))))

(define (dynamic-parse-forms forms)
  (if (null? forms)
      '()
      (let ((next-input (car forms)))
	(dynamic-parse-action-commands
	 (dynamic-parse-command dynamic-empty-env next-input)
	 (dynamic-parse-forms (cdr forms))))))

(define doit 
  (lambda ()
    (i!)
    (let ((foo (dynamic-parse-forms *forms*)))
      (normalize-global-constraints!)
      (reset-counters!)
      (tag-ast*-show foo)
      (counters-show))))

(time (doit))
