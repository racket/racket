#lang racket
(require (for-syntax syntax/parse)
         racket/stxparam
         "../eval.rkt"
         "../ast.rkt")

(define-syntax (:- stx)
  (raise-syntax-error ':- "only allowed inside ! and ~" stx))

(define-syntax-parameter top
  (λ (stx) (raise-syntax-error '#%top "undefined identifier" stx)))
(define-syntax-parameter unquote
  (λ (stx) (raise-syntax-error 'unquote "only allowed inside literals" stx)))
(define-syntax-parameter datum
  (λ (stx) (raise-syntax-error '#%datum "only allowed inside literals" stx)))

(define-syntax (literal-top stx)
  (syntax-parse 
   stx
   [(_ . sym:id)
    (quasisyntax/loc stx
      (constant #'#,stx 'sym))]))

(define-syntax (literal-unquote stx)
  (syntax-parse 
   stx
   [(_ sym:id)
    (quasisyntax/loc stx
      (variable #'#,stx 'sym))]))

(define-syntax (literal-datum stx)
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
      (literal #'#,stx 'sym 
               (syntax-parameterize ([top (make-rename-transformer #'literal-top)]
                                     [datum (make-rename-transformer #'literal-datum)]
                                     [unquote (make-rename-transformer #'literal-unquote)])
                                    (list e ...))))]))

(define-syntax (->simple-clause stx)
  (syntax-case stx (:-)
    [(_ (:- head body ...))
     (quasisyntax/loc stx
       (clause #'#,stx (->literal head) 
               (list (->literal body) ...)))]
    [(_ e)
     (quasisyntax/loc stx
       (clause #'#,stx (->literal e) empty))]))

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

(define-syntax (= stx)
  (quasisyntax/loc stx
    (constant #'#,stx '=)))

(provide (rename-out [top #%top]
                     [datum #%datum])
         #%top-interaction
         #%module-begin
         ! ~ ?
         :- =
         unquote)