; Module header is generated automatically
#cs(module myenv mzscheme
(require (lib "defmacro.ss"))

;; $Id: myenv.ss,v 1.14 2002/03/28 22:23:06 nwv Exp $
;; $Source: /home/nwv/cvsroot/projects/ssax-plt/myenv.ss,v $
;; [ssax-plt] This is a modified version of "official/lib/myenv.scm".
 ;(module myenv mzscheme
 ;  (require (lib "defmacro.ss"))
 ;  (require (rename (lib "pretty.ss") pp pretty-print))

; 		   My Standard Scheme "Prelude"
;
; This version of the prelude contains several forms and procedures
; that are specific to a Gambit-C 3.0 system.
; See myenv-scm.scm, myenv-bigloo.scm, etc. for versions
; of this prelude that are tuned to other Scheme systems.
;
; Id: myenv.scm,v 1.2 2001/09/21 19:53:30 oleg Exp


; assert the truth of an expression (or of a sequence of expressions)
;
; syntax: assert ?expr ?expr ... [report: ?r-exp ?r-exp ...]
;
; If (and ?expr ?expr ...) evaluates to anything but #f, the result
; is the value of that expression.
; If (and ?expr ?expr ...) evaluates to #f, an error is reported.
; The error message will show the failed expressions, as well
; as the values of selected variables (or expressions, in general).
; The user may explicitly specify the expressions whose
; values are to be printed upon assertion failure -- as ?r-exp that
; follow the identifier 'report:'
; Typically, ?r-exp is either a variable or a string constant.
; If the user specified no ?r-exp, the values of variables that are
; referenced in ?expr will be printed upon the assertion failure.

(define-macro (assert expr . others)
			; given the list of expressions or vars,
			; make the list appropriate for cerr
  (define (make-print-list prefix lst)
    (cond
     ((null? lst) '())
     ((symbol? (car lst))
      (cons #\newline
	(cons (list 'quote (car lst))
	  (cons ": " (cons (car lst) (make-print-list #\newline (cdr lst)))))))
     (else 
      (cons prefix (cons (car lst) (make-print-list "" (cdr lst)))))))

			; return the list of all unique "interesting"
			; variables in the expr. Variables that are certain
			; to be bound to procedures are not interesting.
  (define (vars-of expr)
    (let loop ((expr expr) (vars '()))
      (cond
       ((not (pair? expr)) vars)	; not an application -- ignore
       ((memq (car expr) 
	      '(quote let let* letrec let-values* lambda cond quasiquote
		      case define do assert))
	vars)				; won't go there
       (else				; ignore the head of the application
	(let inner ((expr (cdr expr)) (vars vars))
	  (cond 
	   ((null? expr) vars)
	   ((symbol? (car expr))
	    (inner (cdr expr)
		   (if (memq (car expr) vars) vars (cons (car expr) vars))))
	   (else
	    (inner (cdr expr) (loop (car expr) vars)))))))))

  (cond
   ((null? others)		; the most common case
    `(or ,expr (begin (cerr "failed assertion: " ',expr nl "bindings"
			    ,@(make-print-list #\newline (vars-of expr)) nl)
		      (error "assertion failure"))))
   ((eq? (car others) 'report:) ; another common case
    `(or ,expr (begin (cerr "failed assertion: " ',expr
			    ,@(make-print-list #\newline (cdr others)) nl)
		      (error "assertion failure"))))
   ((not (memq 'report: others))
    `(or (and ,expr ,@others)
	 (begin (cerr "failed assertion: " '(,expr ,@others) nl "bindings"
		      ,@(make-print-list #\newline
			 (vars-of (cons 'and (cons expr others)))) nl)
		      (error "assertion failure"))))
   (else			; report: occurs somewhere in 'others'
    (let loop ((exprs (list expr)) (reported others))
      (cond
       ((eq? (car reported) 'report:)
	`(or (and ,@(reverse exprs))
	     (begin (cerr "failed assertion: " ',(reverse exprs)
			  ,@(make-print-list #\newline (cdr reported)) nl)
		    (error "assertion failure"))))
       (else (loop (cons (car reported) exprs) (cdr reported)))))))
)
    
(define-macro (assure exp error-msg) `(assert ,exp report: ,error-msg))

;; [ssax-plt] Different definition of `identify-error'.

;(define (identify-error msg args . disposition-msgs)
;  (##identify-error "ERROR" #f #f msg args disposition-msgs))

(define identify-error
  (let ((display-list (lambda (lst)
                        (for-each (lambda (arg)
                                    (display " ")
                                    (display arg))
                                  lst))))
    (lambda (msg args . disposition-msgs)
      (parameterize ((current-output-port (current-error-port)))
        (newline)
        (display "ERROR: ")
        (display msg)
        (display-list args)
        (unless (null? disposition-msgs)
          (newline)
          (display "ERROR DISPOSITION:")
          (display-list disposition-msgs))
        (newline)))))

; like cout << arguments << args
; where argument can be any Scheme object. If it's a procedure
; (without args) it's executed rather than printed (like newline)

(define (cout . args)
  (for-each (lambda (x)
              (if (procedure? x) (x) (display x)))
            args))

;; [ssax-plt] In `cerr', `##stderr' replaced with `(current-error-port)'.

(define (cerr . args)
  (for-each (lambda (x)
              (if (procedure? x)
                  (x (current-error-port))
                  (display x (current-error-port))))
            args))

;(##define-macro (nl) '(newline))
(define nl (string #\newline))

;; [ssax-plt] `##fixnum.' prefix removed.

; Some useful increment/decrement operators
; Note, ##fixnum prefix is Gambit-specific, it means that the
; operands assumed FIXNUM (as they ought to be anyway).
; This perfix could be safely removed: it'll leave the code just as
; correct, but more portable (and less efficient)

				; Mutable increment
(define-macro (++! x) `(set! ,x (+ 1 ,x)))

				; Read-only increment
(define-macro (++ x) `(+ 1 ,x))

				; Mutable decrement
(define-macro (--! x) `(set! ,x (- ,x 1)))

				; Read-only decrement
(define-macro (-- x) `(- ,x 1))


; Some useful control operators

;; [ssax-plt] PLT defines `when'.

			; if condition is true, execute stmts in turn
			; and return the result of the last statement
			; otherwise, return #f
;(define-macro (when condition . stmts)
;  `(and ,condition (begin ,@stmts)))

  
			; if condition is false execute stmts in turn
			; and return the result of the last statement
			; otherwise, return #t
			; This primitive is often called 'unless'
(define-macro (whennot condition . stmts)
  `(or ,condition (begin ,@stmts)))


;; [ssax-plt] PLT defines `begin0'.

			; Execute a sequence of forms and return the
			; result of the _first_ one. Like PROG1 in Lisp.
			; Typically used to evaluate one or more forms with
			; side effects and return a value that must be
			; computed before some or all of the side effects
			; happen.
;(define-macro (begin0 form . forms)
;  (let ((var (gensym)))
;    `(let ((,var ,form)) ,@forms ,var)))

  
			; Prepend an ITEM to a LIST, like a Lisp macro PUSH
			; an ITEM can be an expression, but ls must be a VAR
(define-macro (push! item ls)
  `(set! ,ls (cons ,item ,ls)))

; DL: Defined in SRFI-13
;			; Is str the empty string?
;			; string-null? str -> bool
;			; See Olin Shiver's Underground String functions
;(define-macro (string-null? str) `(zero? (string-length ,str)))



; Support for multiple-values and let-values* form
; Multiple values are not present natively in Gambit.
; What follows is an _approximation_: it is not very good in case
; of continuations captured while evaluating an argument expression of
; values. Note that the only meaningful way to use 'values' procedure is
; in conjunction with call-with-values or let-values*

;; [ssax-plt] PLT defines `values' and `call-with-values'.

; (define values list)

; (define (call-with-values producer consumer)
;   (apply consumer (producer)))

;; [ssax-plt] We use the `let-values*' from Kirill Lisovsky's port of SSAX 4.9
;;            instead of the current SSAX one.

; Like let* but allowing for multiple-value bindings
;(define-macro (let-values* bindings . body)
;  (if (null? bindings) (cons 'begin body)
;      (apply (lambda (vars initializer)
;	 (let ((cont 
;		(cons 'let-values* 
;		      (cons (cdr bindings) body))))
;	   (cond
;	    ((not (pair? vars))		; regular let case, a single var
;	     `(let ((,vars ,initializer)) ,cont))
;	    ((null? (cdr vars))		; single var, see the prev case
;	     `(let ((,(car vars) ,initializer)) ,cont))
;	    ((null? (cddr vars))	; two variables
;	     (let ((val (gensym)))
;	       `(let* ((,val ,initializer)
;		       (,(car vars) (car ,val))
;		       (,(cadr vars) (cadr ,val))) ,cont)))
;	   (else			; the most generic case
;	    `(apply (lambda ,vars ,cont) ,initializer)))))
;       (car bindings))))

(define-macro let-values* (lambda (bindings . body)
  (if (null? bindings) (cons 'begin body)
      (apply (lambda (vars initializer)
	 (let ((cont 
		(cons 'let-values* 
		      (cons (cdr bindings) body))))
	   (cond
	    ((not (pair? vars))		; regular let case, a single var
	     `(let ((,vars ,initializer)) ,cont))
	    ((null? (cdr vars))		; single var, see the prev case
	     `(let ((,(car vars) ,initializer)) ,cont))
	   (else			; the most generic case
	    `(call-with-values (lambda () ,initializer)
	      (lambda ,vars ,cont))))))
       (car bindings)))))


			; assoc-primitives with a default clause
			; If the search in the assoc list fails, the
			; default action argument is returned. If this
			; default action turns out to be a thunk,
			; the result of its evaluation is returned.
			; If the default action is not given, an error
			; is signaled

(define-macro (assq-def key alist . default-action-arg)
  (let ((default-action
        (if (null? default-action-arg)
          `(error "failed to assq key '" ,key "' in a list " ,alist)
          (let ((defact-symb (gensym)))
	    `(let ((,defact-symb ,(car default-action-arg)))
               (if (procedure? ,defact-symb) (,defact-symb) ,defact-symb))))))
    `(or (assq ,key ,alist) ,default-action)))

(define-macro (assv-def key alist . default-action-arg)
  (let ((default-action
        (if (null? default-action-arg)
          `(error "failed to assv key '" ,key "' in a list " ,alist)
          (let ((defact-symb (gensym)))
	    `(let ((,defact-symb ,(car default-action-arg)))
               (if (procedure? ,defact-symb) (,defact-symb) ,defact-symb))))))
    `(or (assv ,key ,alist) ,default-action)))

(define-macro (assoc-def key alist . default-action-arg)
  (let ((default-action
        (if (null? default-action-arg)
          `(error "failed to assoc key '" ,key "' in a list " ,alist)
          (let ((defact-symb (gensym)))
	    `(let ((,defact-symb ,(car default-action-arg)))
               (if (procedure? ,defact-symb) (,defact-symb) ,defact-symb))))))
    `(or (assoc ,key ,alist) ,default-action)))


			; Convenience macros to avoid quoting of symbols
			; being deposited/looked up in the environment
(define-macro (env.find key) `(%%env.find ',key))
(define-macro (env.demand key) `(%%env.demand ',key))
(define-macro (env.bind key value) `(%%env.bind ',key ,value))

			; Implementation of SRFI-0
			; Only feature-identifiers srfi-0 and gambit
			; assumed predefined
(define-macro (cond-expand . clauses)
  (define feature-ids '(plt srfi-0))
  (define (feature-req-satisfies? fr) ; does feature-request satisfies?
    (cond
     ((memq fr feature-ids) #t)
     ((not (pair? fr)) #f)
     ((eq? 'and (car fr))
      (let loop ((clauses (cdr fr)))
	(or (null? clauses)
	    (and (feature-req-satisfies? (car clauses))
		 (loop (cdr clauses))))))
     ((eq? 'or (car fr))
      (let loop ((clauses (cdr fr)))
	(and (pair? clauses)
	     (or (feature-req-satisfies? (car clauses))
		 (loop (cdr clauses))))))
     ((eq? 'not (car fr))
      (not (feature-req-satisfies? (and (pair? (cdr fr)) (cadr fr)))))
     (else #f)))
  (let loop ((clauses clauses))
    (if (null? clauses) '(error "Unfulfilled cond-expand")
	(let* ((feature-req (if (pair? (car clauses)) (caar clauses)
				(error "<cond-expand clause> is not a list")))
	       (cmd-or-defs* (cons 'begin (cdar clauses))))
	  (cond
	   ((and (eq? 'else feature-req) (null? (cdr clauses)))
	    cmd-or-defs*)
	   ((feature-req-satisfies? feature-req)
	    cmd-or-defs*)
	   (else (loop (cdr clauses))))))))

;; [ssax-plt] Begin misc. other definitions needed by other modules.

(define (call-with-input-string str proc)
  (proc (open-input-string str)))

; I guess there's only one way to write this... :)
;(define (string-index str chr)
;  (let ((len (string-length str)))
;    (let search ((i 0))
;      (cond ((= i len)                       #f)
;            ((char=? chr (string-ref str i)) i)
;            (else                            (search (+ i 1)))))))
 
(define (with-input-from-string str thunk)
  (parameterize ((current-input-port (open-input-string str)))
    (thunk)))

;; [ssax-plt] End misc. other definitions needed by other modules.

;; [ssax-plt] Finish module.
;(provide (all-defined) pp))


;==============================================================================
; DL: this piece of code is taken from the previous version of "myenv.scm"
; Stubs

(define-macro (inc x) `(+ 1 ,x))
(define-macro (dec x) `(- ,x 1))

(define (cons* a1 a2 . rest)
  (if (null? rest)
      (cons a1 a2)
      (cons a1 (apply cons* (cons a2 rest)))))

;; Gambit's include and declare are disabled
(define-macro include (lambda (file) #f))
(define-macro declare (lambda x #f))

(provide (all-defined)))
