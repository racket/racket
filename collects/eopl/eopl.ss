(module eopl mzscheme
  (require "datatype.ss"
	   "private/sllgen.ss"
	   (lib "trace.ss")
	   (lib "pretty.ss"))
  (require-for-syntax "private/slldef.ss")

  (provide define-datatype
	   cases)

  ;; Special def that saves a quoted value at compile time in case
  ;; it's needed for `sllgen:make-define-datatypes':
  (define-syntax (eopl-define stx)
    (syntax-case stx (quote)
      [(_ name (quote def))
       (identifier? (syntax name))
       (begin
	 (hash-table-put! sllgen-def
			  (syntax-e (syntax name))
			  (syntax def))
	 (syntax/loc stx (define name (quote def))))]
      [(_ . rest)
       (syntax/loc stx (define . rest))]))

  (provide (rename eopl-define define))

  (provide (all-from "private/sllgen.ss"))

  (provide (rename error eopl:error)
	   (rename printf eopl:printf)
	   (rename pretty-print eopl:pretty-print))

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  ;; Ugly: 
  ;;
  ;;   1) `eopl:error-stop' has to be a top-level binding to be
  ;;      mutated by client programs --- actually, the test harness ---
  ;;      for exception handling.
  ;;   2) Exception jumps by the test harness are performed through
  ;;      call/cc, not call/ec.
  ;;
  ;; Solution: use `namespace-variable-value', and create an escape
  ;; continuation for each nested continuation.

  (define esc-cont-mark-key (gensym))
  (define detect-tail-key (gensym))
  (define recovering-from-error (make-parameter #f))
  (define (mk-k k ek)
    (lambda args 
      (apply (if (recovering-from-error) ek k) args)))

  (define (eopl:call-with-current-continuation f)
    (unless (and (procedure? f)
		 (procedure-arity-includes? f 1))
      ;; let call/cc report the error:
      (call/cc f))
    (let/cc k
      (let ([v (gensym)]
	    [orig-marks (continuation-mark-set->list
			 (continuation-marks k)
			 detect-tail-key)])
	(with-continuation-mark detect-tail-key v
	  (let ([new-marks (continuation-mark-set->list
			    (current-continuation-marks)
			    detect-tail-key)])
	    (if (or (null? orig-marks)
		    (and (pair? (cdr new-marks))
			 (eq? (car orig-marks) (cadr new-marks))))
		;; Old mark surived => not tail wrt old call.
		;; Create an escape continuation to use for
		;; error escapes. Of course, we rely on the fact
		;; that continuation marks are not visible to EoPL
		;; programs.
		(let/ec ek
		  (with-continuation-mark esc-cont-mark-key ek
		    (with-continuation-mark detect-tail-key (gensym)
		      (f (mk-k k ek)))))
		;; Old mark replaced => tail wrt old call.
		;; To preserve tail semantics for all but the first call
		;; reuse `mark' instead of creating a new escape continuation:
		(let ([mark (car (continuation-mark-set->list
				  (continuation-marks k)
				  esc-cont-mark-key))])
		  (f (mk-k k mark)))))))))
  
  (namespace-set-variable-value! 'eopl:error-stop #f #t)
  (define (install-eopl-exception-handler)
    (current-exception-handler 
     (let ([eh (current-exception-handler)]
	   [orig-namespace (current-namespace)])
       (lambda (x)
	 (let ([v (with-handlers ([void (lambda (x) #f)])
		    (parameterize ([current-namespace orig-namespace])
		      (namespace-variable-value 'eopl:error-stop)))])
	   (if v
	       (parameterize ([recovering-from-error #t])
		 (v))
	       (eh x)))))))
  
  (provide install-eopl-exception-handler)
  
  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (provide always? list-of)

  (define always?
    (lambda (x) #t))
  
  (define list-of
    (lambda (pred . l)
      (let ((all-preds (cons pred l)))
	(lambda (obj)
	  (let loop ((obj obj) (preds '()))
	    (or 
	     ;; if list is empty, preds should be, too
	     (and (null? obj) (null? preds))
	     (if (null? preds)
		 ;; if preds is empty, but list isn't, then recycle
		 (loop obj all-preds)
		 ;; otherwise check and element and recur.
		 (and (pair? obj)
		      ((car preds) (car obj))
		      (loop (cdr obj) (cdr preds))))))))))

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (define empty null)

  (provide time            ;; useful to compare implementations
	   collect-garbage ;; useful with `time'
	   empty           ;; for constructor-based printing
	   trace untrace   ;; debugging
           require module  ;; we allow full use of modules
	   provide)        ;; in case someone wants to use a module

  ;; Unfortunately, most of the rest is cut-and-pasted from R5RS:

  ;; values
  (provide car cdr caar cadr cdar cddr
	   caaar caadr cadar caddr cdaar cdadr cddar cdddr
	   caaaar caaadr caadar caaddr cadaar cadadr caddar cadddr
	   cdaaar cdaadr cdadar cdaddr cddaar cddadr cdddar cddddr
	   map = < > <= >= max min + - * / 
	   abs gcd lcm exp log sin cos tan not eq?
	   (rename eopl:call-with-current-continuation call-with-current-continuation)
	   make-string
	   symbol->string string->symbol make-rectangular 
	   exact->inexact inexact->exact number->string string->number 
	   rationalize output-port? current-input-port current-output-port current-error-port 
	   open-input-file open-output-file close-input-port close-output-port
	   with-output-to-file transcript-on transcript-off flush-output
	   string-length string-ci<=? string-ci>=? string-append 
	   string->list list->string string-fill! 
	   vector-length vector->list list->vector vector-fill!
	   char-alphabetic? char-numeric? char-whitespace? 
	   char-upper-case? char-lower-case? char->integer integer->char char-downcase
	   call-with-output-file call-with-input-file with-input-from-file
	   apply for-each symbol? pair? cons set-car! set-cdr! null? list? list length append reverse
	   list-tail list-ref memq memv member assq assv assoc procedure?
	   number? complex? real? rational? integer? exact? inexact? zero?
	   positive?  negative? odd? even? 
	   quotient remainder modulo floor ceiling truncate round 
	   numerator denominator asin acos atan sqrt
	   expt make-polar real-part imag-part angle magnitude input-port?
	   read read-char peek-char eof-object?
	   char-ready? write display newline write-char load 
	   string? string string-ref string-set! string=? substring string-copy
	   string-ci=? string<? string>? string<=? string>=? string-ci<? string-ci>?
	   vector? make-vector vector vector-ref vector-set! 
	   char? char=? char<? char>? char<=? char>=? 
	   char-ci=? char-ci<? char-ci>? char-ci<=? char-ci>=? 
	   char-upcase boolean? eqv? equal? force
	   call-with-values values eval port? scheme-report-environment null-environment 
	   interaction-environment dynamic-wind)

  ;; syntax
  (provide quasiquote unquote unquote-splicing 
	   if let and or cond case delay do
	   letrec let* begin lambda quote set!
	   define-syntax

	   ;; We have to include the following MzScheme-isms to do anything,
	   ;; but they're not legal R5RS names, anyway.
	   #%app #%datum #%top 
	   (rename synrule-in-stx-module-begin #%module-begin))

  (define-syntax synrule-in-stx-module-begin
    (lambda (stx)
      (datum->syntax-object
       (quote-syntax here)
       (list* (quote-syntax #%plain-module-begin)
	      (quote-syntax 
	       (require-for-syntax (rename mzscheme syntax-rules syntax-rules)))
	      (cdr (syntax-e stx)))
       stx))))
