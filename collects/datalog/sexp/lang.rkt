#lang racket
(require (for-syntax syntax/parse)
         "../eval.rkt"
         "../ast.rkt")

(define-syntax (top stx)
  (syntax-parse 
   stx
   [(_ . sym:id)
    (quasisyntax/loc stx
      (constant #'#,stx 'sym))]))

(define-syntax (unquote stx)
  (syntax-parse 
   stx
   [(_ sym:id)
    (quasisyntax/loc stx
      (variable #'#,stx 'sym))]))

(define-syntax (datum stx)
  (syntax-parse 
   stx
   [(_ . sym:str)
    (quasisyntax/loc stx
      (constant #'#,stx 'sym))]))

(define-syntax (->literal stx)
  (syntax-parse 
   stx
   [(_ sym:id)
    (quasisyntax/loc stx
      (literal #'#,stx 'sym empty))]
   [(_ (sym:id e ...))
    (quasisyntax/loc stx
      (literal #'#,stx 'sym (list e ...)))]))

(define-syntax (->simple-clause stx)
  (syntax-case stx ()
    [(_ e)
     (quasisyntax/loc stx
       (clause #'#,stx (->literal e) empty))]))

(define-syntax (:- stx)
  (syntax-case stx ()
    [(_ head body ...)
     (quasisyntax/loc stx
       (eval-top-level-statement 
        (assertion #'#,stx 
                   (clause #'#,stx (->literal head) 
                           (list (->literal body) ...)))))]))

(define-syntax-rule (define-paren-stx op struct)
  (define-syntax (op stx)
    (syntax-case stx ()
      [(_ c)
       (quasisyntax/loc stx
         (eval-top-level-statement (struct #'#,stx (->simple-clause c))))])))

(define-paren-stx ! assertion)
(define-paren-stx ~ retraction)

(define-syntax (? stx)
  (syntax-case stx ()
    [(_ c)
     (quasisyntax/loc stx
       (eval-top-level-statement (query #'#,stx (->literal c))))]))

(provide (rename-out [top #%top]
                     [datum #%datum]
                     #;[module-begin #%module-begin]
                     #;[top-interaction #%top-interaction])
         
         #%module-begin
         ! ~ ?
         :-
         unquote)