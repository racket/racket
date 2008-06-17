(module eopl mzscheme
  (require "datatype.ss"
	   "private/sllgen.ss"
	   mzlib/trace
	   mzlib/pretty
           (rename r5rs r5rs:define define)
           (rename r5rs r5rs:quote quote))
  (require-for-syntax "private/slldef.ss")

  (provide define-datatype
	   cases)

  ;; Special def that saves a quoted value at compile time in case
  ;; it's needed for `sllgen:make-define-datatypes':
  (define-syntax (eopl-define stx)
    (syntax-case stx (r5rs:quote)
      [(_ name (r5rs:quote def))
       (identifier? (syntax name))
       (syntax/loc stx
	 (begin
	   (begin-for-syntax (hash-table-put! sllgen-def 'name (quote-syntax def)))
	   (define name (r5rs:quote def))))]
      [(_ . rest)
       (syntax/loc stx (r5rs:define . rest))]))

  (provide (rename eopl-define define))

  (provide (all-from "private/sllgen.ss"))

  (provide (rename error eopl:error)
	   (rename printf eopl:printf)
	   (rename pretty-print eopl:pretty-print)
           (rename eopl:call-with-current-continuation call-with-current-continuation))

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
    (uncaught-exception-handler 
     (let ([eh (uncaught-exception-handler)]
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
		 (and (mpair? obj)
		      ((car preds) (mcar obj))
		      (loop (mcdr obj) (cdr preds))))))))))

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (define empty null)

  (provide time            ;; useful to compare implementations
	   collect-garbage ;; useful with `time'
	   empty           ;; for constructor-based printing
	   trace untrace   ;; debugging
           require module  ;; we allow full use of modules
	   provide)        ;; in case someone wants to use a module

  (define-syntax r5rs-out
    (syntax-rules ()
      [(_) (begin
             (require (all-except r5rs 
                                  define
                                  call-with-current-continuation))
             (provide (all-from-except r5rs 
                                       r5rs:define)))]))
  (r5rs-out))
