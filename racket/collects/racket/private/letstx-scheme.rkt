
;;----------------------------------------------------------------------
;; #%stxcase-scheme: adds let-syntax, letrec-syntax, etc.

(module letstx-scheme '#%kernel
  (#%require "small-scheme.rkt"
             (for-syntax '#%kernel "stxcase.rkt" 
                         "with-stx.rkt" "stxloc.rkt"))
  
  (-define-syntax letrec-syntaxes
    (lambda (stx)
      (syntax-case stx ()
	[(_ ([(id ...) expr] ...) body1 body ...)
	 (syntax/loc stx
	     (letrec-syntaxes+values ([(id ...) expr] ...)
				     ()
	       body1 body ...))])))

  (-define-syntax letrec-syntax
    (lambda (stx)
      (syntax-case stx ()
	[(_ ([id expr] ...) body1 body ...)
	 (syntax/loc stx
	     (letrec-syntaxes+values ([(id) expr] ...)
				     ()
	       body1 body ...))])))

  (-define-syntax let-syntaxes
    (lambda (stx)
      (syntax-case stx ()
	[(_ ([(id ...) expr] ...) body1 body ...)
	 (with-syntax ([((tmp ...) ...) 
			(map
			 generate-temporaries 
			 (syntax->list (syntax ((id ...) ...))))])
	   (syntax/loc stx
	       (letrec-syntaxes+values ([(tmp ...) expr] ...) ()
		 (letrec-syntaxes+values ([(id ...) (values
						     (make-rename-transformer (quote-syntax tmp))
						     ...)] ...)
					 ()
		   body1 body ...))))])))

  (-define-syntax let-syntax
    (lambda (stx)
      (syntax-case stx ()
	[(_ ([id expr] ...) body1 body ...)
	 (syntax/loc stx
	     (let-syntaxes ([(id) expr] ...)
	       body1 body ...))])))

  (#%provide (all-from "small-scheme.rkt")
             letrec-syntaxes letrec-syntax let-syntaxes let-syntax))
