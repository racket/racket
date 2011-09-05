
;;----------------------------------------------------------------------
;; #%define : define and define-syntax

(module define '#%kernel
  (#%require (for-syntax '#%kernel 
                         "letstx-scheme.rkt" "stxcase-scheme.rkt" "stx.rkt" "qqstx.rkt"
                         "norm-define.rkt"))

  (#%provide define 
             define-syntax 
             define-values-for-syntax
             define-for-syntax)

  (define-syntaxes (define-values-for-syntax)
    (lambda (stx)
      (syntax-case stx ()
        [(_ (id ...) expr)
         (begin
           (for-each (lambda (x)
                       (unless (identifier? x)
                         (raise-syntax-error #f "not an identifier" x stx)))
                     (syntax->list #'(id ...)))
           #'(begin-for-syntax
              (define-values (id ...) expr)))])))

  (define-syntaxes (define define-syntax define-for-syntax)
    (let ([go
	   (lambda (define-values-stx stx)
             (let-values ([(id rhs)
                           (normalize-definition stx #'lambda #t #f)])
               (quasisyntax/loc stx
                 (#,define-values-stx (#,id) #,rhs))))])
      (values (lambda (stx) (go #'define-values stx))
	      (lambda (stx) (go #'define-syntaxes stx))
              (lambda (stx) (go #'define-values-for-syntax stx))))))
