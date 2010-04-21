
;;----------------------------------------------------------------------
;; #%define : define and define-syntax

(module define '#%kernel
  (#%require (for-syntax '#%kernel 
                         "letstx-scheme.rkt" "stxcase-scheme.rkt" "stx.rkt" "qqstx.rkt"
                         "norm-define.rkt"))

  (#%provide define define-syntax define-for-syntax begin-for-syntax)

  (define-syntaxes (define define-syntax define-for-syntax)
    (let ([go
	   (lambda (define-values-stx stx)
             (let-values ([(id rhs)
                           (normalize-definition stx #'lambda #t #f)])
               (quasisyntax/loc stx
                 (#,define-values-stx (#,id) #,rhs))))])
      (values (lambda (stx) (go #'define-values stx))
	      (lambda (stx) (go #'define-syntaxes stx))
              (lambda (stx) (go #'define-values-for-syntax stx)))))

  (define-syntaxes (begin-for-syntax)
    (lambda (stx)
      (let ([ctx (syntax-local-context)])
	(unless (memq ctx '(module module-begin top-level))
	  (raise-syntax-error #f "allowed only at the top-level or a module top-level" stx))
	(syntax-case stx ()
	  [(_) #'(begin)]
	  [(_ elem)
	   (not (eq? ctx 'module-begin))
	   (let ([e (local-transformer-expand/capture-lifts
		     #'elem
		     ctx
		     (syntax->list
		      #'(begin
			  define-values
			  define-syntaxes
			  define-values-for-syntax
			  set!
			  let-values
			  let*-values
			  letrec-values
			  lambda
			  case-lambda
			  if
			  quote
			  letrec-syntaxes+values
			  fluid-let-syntax
			  with-continuation-mark
                          #%expression
                          #%variable-reference
			  #%app
			  #%top
                          #%provide 
                          #%require)))])
	     (syntax-case* e (begin define-values define-syntaxes require require-for-template) 
			   free-transformer-identifier=?
	       [(begin (begin v ...))
		#'(begin-for-syntax v ...)]
	       [(begin (define-values (id ...) expr))
		#'(define-values-for-syntax (id ...) expr)]
	       [(begin (require v ...))
		#'(require (for-syntax v ...))]
	       [(begin (define-syntaxes (id ...) expr))
		(raise-syntax-error
		 #f
		 "syntax definitions not allowed within begin-for-syntax"
		 #'elem)]
               [(begin other)
		#'(define-values-for-syntax () (begin other (values)))]
	       [(begin v ...)
                #'(begin-for-syntax v ...)]))]
	  [(_ elem ...)
	   ;; We split up the elems so that someone else can
	   ;;  worry about the fact that properly expanding the second
	   ;;  things might depend somehow on the first thing.
	   ;; This also avoids a problem when `begin-for-syntax' is the
	   ;;  only thing in a module body, and `module' has to expand
	   ;;  it looking for #%module-begin.
	   (syntax/loc stx (begin (begin-for-syntax elem) ...))])))))
