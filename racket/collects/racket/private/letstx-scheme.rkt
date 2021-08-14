
;;----------------------------------------------------------------------
;; #%stxcase-scheme: adds let-syntax, letrec-syntax, etc.

(module letstx-scheme '#%kernel
  (#%require "define-et-al.rkt" "qq-and-or.rkt" "cond.rkt"
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
           (with-syntax ([let-syntaxes-body/loc
                          (syntax/loc stx
                            (letrec-syntaxes+values ([(id ...)
                                                      (values
                                                       (make-rename-transformer (quote-syntax tmp))
                                                       ...)] ...)
                                                    ()
                              body1 body ...))])
             (syntax/loc stx
               (letrec-syntaxes+values ([(tmp ...) expr] ...) ()
                 let-syntaxes-body/loc))))])))

  (-define-syntax let-syntax
    (lambda (stx)
      (syntax-case stx ()
	[(_ ([id expr] ...) body1 body ...)
	 (syntax/loc stx
	     (let-syntaxes ([(id) expr] ...)
	       body1 body ...))])))

  (#%provide (all-from "define-et-al.rkt") (all-from "qq-and-or.rkt") (all-from "cond.rkt")
             letrec-syntaxes letrec-syntax let-syntaxes let-syntax))
