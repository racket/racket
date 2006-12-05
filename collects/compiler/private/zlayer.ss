;; Zodiac interface and library routines
;; (c)1996-1997 Sebastian Good
;; (c)1997-2001 PLT

(module zlayer mzscheme
  (require (lib "unit.ss")
	  (lib "list.ss")
	  (lib "etc.ss"))
  
  (require (lib "zodiac-sig.ss" "syntax"))

  (require "../sig.ss")
  (require "sig.ss")
  
  (provide zlayer@)
  (define-unit zlayer@
      (import (prefix compiler:option: compiler:option^)
	      (prefix zodiac: zodiac^)
	      compiler:cstructs^
	      compiler:driver^)
      (export compiler:zlayer^)

      ;;----------------------------------------------------------------------------
      ;; ANNOTATIONS
      ;;
      ;; zodiac:* AST notes are annotated using set-annotation!, and
      ;; the annotations are extracted using get-annotation. Every
      ;; AST node has a single annotation, but the type of the annotation
      ;; depends on the type of the AST node.
      
      ;; This is the default annotation value, used before the annotation
      ;; is set for an AST node
      (define compiler:empty-annotation (gensym 'mzc-default-annotation))

      ;; Create a new back-box for a new zodiac AST node
      (define (make-empty-box) (zodiac:make-empty-back-box))

      ;; Manipulating annotations:
      ;; NOTE: Zodiac must be invoked before this unit
      (define-values (get-annotation set-annotation!)
	(let-values ([(getter setter)
		      (zodiac:register-client 'compiler 
					      (lambda ()
						compiler:empty-annotation))])
	  (values
	   (lambda (ast)
	     (getter (zodiac:parsed-back ast)))
	   (lambda (ast obj)
	     (setter (zodiac:parsed-back ast) obj)))))
      (define (annotated? ast)
	(not (eq? (get-annotation ast)
		  compiler:empty-annotation)))
      (define (remove-annotation! ast)
	(set-annotation! ast compiler:empty-annotation))

      ;;----------------------------------------------------------------------------
      ;; Error handling

      (define compiler:escape-on-error (make-parameter #f))

      ;; initialize zodiac-error procedures
      (define zodiac-error-template
	(lambda (c s)
	  (lambda (where fmt-spec . args)
	    (c where 
	       (string-append 
		s
		(apply format (cons fmt-spec args))))
	    (when (compiler:escape-on-error)
	      (error 'compiler "parsing error")))))
      
      (define (call-compiler:fatal-error . args)
	(apply compiler:fatal-error args))
      
      (define static-error 
	(zodiac-error-template call-compiler:fatal-error "(syntax) "))
      (define internal-error
	(zodiac-error-template call-compiler:fatal-error "(elaboration) "))
      (define dynamic-error
	(zodiac-error-template call-compiler:fatal-error "(parser dynamic) "))
      

      ;;----------------------------------------------------------------------------
      ;; BEGIN0-FORM
      ;;
      ;;  maintain the illusion of a two slot begin0-form
      
      (define zodiac:begin0-form-first
	(compose car zodiac:begin0-form-bodies))
      (define zodiac:begin0-form-rest
	(compose cadr zodiac:begin0-form-bodies))
      (define zodiac:set-begin0-form-first!
	(lambda (ast v)
	  (set-car! (zodiac:begin0-form-bodies ast) v)))
      (define zodiac:set-begin0-form-rest!
	(lambda (ast v)
	  (set-car! (cdr (zodiac:begin0-form-bodies ast)) v)))
      
      ;;----------------------------------------------------------------------------
      ;; SPECIAL CONSTANTS
      ;;
      ;; some constants we don't know how to write, like #<void>
      ;;

      (define undefined (letrec ([x x]) x))

      (define (undefined? x) (eq? x undefined))

      (define self_modidx (let ()
			    (define-struct self_modidx ())
			    (make-self_modidx)))

      (define zodiac:make-special-constant
	;; make-quote, make-constant
	(lambda (text)
	  (let ([stx (case text
		       [(void) (datum->syntax-object #f (void) #f)]
		       [(null) (datum->syntax-object #f null)]
		       [(undefined) (datum->syntax-object #f undefined)]
		       [(self_modidx) (datum->syntax-object #f self_modidx)]
		       [else (compiler:internal-error 'make-special-constant "bad type")])])
	    (zodiac:make-quote-form 
	     stx (make-empty-box)
	     (zodiac:make-zread stx)))))
      
      ;;-----------------------------------------------------------------------------
      ;; BINDING->LEXICAL-VARREF
      ;;
      ;; creates a zodiac:lexical-varref from a zodiac:binding
      ;;
      
      (define zodiac:binding->lexical-varref
	(lambda (ast)
	  (let ([v (zodiac:make-lexical-varref (zodiac:zodiac-stx ast)
					       (make-empty-box)
					       (zodiac:binding-var ast)
					       ast)])
	    (set-annotation! v (varref:empty-attributes))
	    v)))

      ;;----------------------------------------------------------------------------
      ;; POSITION REPORTING
      
      (define main-source-file (make-parameter #f))
      
      (define zodiac:print-start!
	(lambda (port ast)
	  (let ([bad (lambda () (fprintf port " [?,?]: "))])
	    (if (and ast (zodiac:zodiac? ast))
		(let* ([start (zodiac:zodiac-start ast)]
		       [good (lambda ()
			       (fprintf port " ~a[~a,~a]: "
					(if (equal? (main-source-file) (zodiac:location-file start))
					    ""
					    (format "~s " (zodiac:location-file start)))
					(zodiac:location-line start)
					(zodiac:location-column start)))])
		  (good))
		(bad)))))

      ;;----------------------------------------------------------------------
      ;; Debugging: AST to annotated S-expression
      (define zodiac->sexp/annotate
	(lambda (ast)
	  (zodiac->sexp ast)))
      
      (define zodiac->sexp
	(lambda (ast)

	  (cond 
	   [(zodiac:quote-form? ast) 
	    (syntax-object->datum (zodiac:zodiac-stx ast))]

	   [(zodiac:binding? ast)
	    (zodiac:binding-var ast)]

	   [(zodiac:varref? ast)
	    (zodiac:varref-var ast)]
	   
	   ;; compound sexps
	   [(zodiac:define-values-form? ast)
	    `(define-values ,(map zodiac->sexp (zodiac:define-values-form-vars ast))
	       ,(zodiac->sexp/annotate (zodiac:define-values-form-val ast)))]
	   
	   [(zodiac:app? ast)
	    `(,(zodiac->sexp/annotate (zodiac:app-fun ast))
	      ,@(map zodiac->sexp/annotate (zodiac:app-args ast)))]
	   
	   [(zodiac:set!-form? ast)
	    `(set! ,(zodiac->sexp (zodiac:set!-form-var ast))
		   ,(zodiac->sexp/annotate (zodiac:set!-form-val ast)))]
	   
	   [(zodiac:case-lambda-form? ast)
	    `(case-lambda
	      ,@(map
		 (lambda (args body)
		   `(,(let ([vars (zodiac:arglist-vars args)])
			(cond
			 [(zodiac:sym-arglist? args) (zodiac->sexp (car vars))]
			 [(zodiac:list-arglist? args) (map zodiac->sexp vars)]
			 [(zodiac:ilist-arglist? args) (let loop ([args vars])
							 (if (null? (cdr args))
							     (zodiac->sexp (car args))
							     (cons (zodiac->sexp (car args))
								   (loop (cdr args)))))]))
		     ,(zodiac->sexp/annotate body)))
		 (zodiac:case-lambda-form-args ast)
		 (zodiac:case-lambda-form-bodies ast)))]

	   [(zodiac:begin-form? ast)
	    `(begin ,@(map zodiac->sexp/annotate (zodiac:begin-form-bodies ast)))]

	   [(zodiac:begin0-form? ast)
	    `(begin0 ,@(map zodiac->sexp/annotate (zodiac:begin0-form-bodies ast)))]

	   [(zodiac:let-values-form? ast)
	    `(let-values
		 ,(map list
		       (map (lambda (l) (map zodiac->sexp l)) (zodiac:let-values-form-vars ast))
		       (map zodiac->sexp/annotate (zodiac:let-values-form-vals ast)))
	       ,(zodiac->sexp/annotate (zodiac:let-values-form-body ast)))]
	   
	   [(zodiac:letrec-values-form? ast)
	    `(letrec-values
		 ,(map list
		       (map (lambda (l) (map zodiac->sexp l)) (zodiac:letrec-values-form-vars ast))
		       (map zodiac->sexp/annotate (zodiac:letrec-values-form-vals ast)))
	       ,(zodiac->sexp/annotate (zodiac:letrec-values-form-body ast)))]

	   [(zodiac:if-form? ast)
	    `(if ,(zodiac->sexp/annotate (zodiac:if-form-test ast))
		 ,(zodiac->sexp/annotate (zodiac:if-form-then ast))
		 ,(zodiac->sexp/annotate (zodiac:if-form-else ast)))]
	   
	   [(zodiac:with-continuation-mark-form? ast)
	    `(with-continuation-mark 
	      ,(zodiac->sexp/annotate (zodiac:with-continuation-mark-form-key ast))
	      ,(zodiac->sexp/annotate (zodiac:with-continuation-mark-form-val ast))
	      ,(zodiac->sexp/annotate (zodiac:with-continuation-mark-form-body ast)))]

	   [(zodiac:require/provide-form? ast)
	    `(require/provide ...)]

	   [(zodiac:module-form? ast)
	    `(module ... ,(zodiac->sexp/annotate (zodiac:module-form-body ast)))]

	   [else
	    (error 'zodiac->sexp/annotate "unsupported ~s" ast)])))))
