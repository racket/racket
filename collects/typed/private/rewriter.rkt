#lang racket/base
(require (for-syntax syntax/parse racket/base syntax/id-table racket/dict
                     unstable/debug))

(define-for-syntax code-insp
  (variable-reference->module-declaration-inspector
   (#%variable-reference)))

(define-for-syntax (rewrite stx tbl from)
  (define (rw stx)
     (syntax-parse (syntax-disarm stx code-insp) #:literal-sets (kernel-literals)
       [i:identifier
        (dict-ref tbl #'i #'i)]
       ;; no expressions here
       [((~or (~literal #%top) (~literal quote) (~literal quote-syntax)) . _) stx]
       [(#%plain-lambda formals expr ...)
        (quasisyntax/loc stx (#%plain-lambda formals #,@(map rw (syntax->list #'(expr ...)))))]
       [(case-lambda [formals expr ...] ...)
        (with-syntax ([((expr* ...) ...) (for*/list ([exprs (in-list (syntax->list #'((expr ...) ...)))]
                                                     [e (in-list (syntax->list exprs))])
                                           (rw e))])
          (quasisyntax/loc stx (case-lambda [formals expr* ...] ...)))]
       [(let-values ([(id ...) rhs] ...) expr ...)
        (with-syntax ([(rhs* ...) (map rw (syntax->list #'(rhs ...)))]
                      [(expr* ...) (map rw (syntax->list #'(expr ...)))])
          (quasisyntax/loc stx (let-values ([(id ...) rhs*] ...) expr* ...)))]
       [(letrec-values ([(id ...) rhs] ...) expr ...)
        (with-syntax ([(rhs* ...) (map rw (syntax->list #'(rhs ...)))]
                      [(expr* ...) (map rw (syntax->list #'(expr ...)))])
          (quasisyntax/loc stx (letrec-values ([(id ...) rhs*] ...) expr* ...)))]
       [(letrec-syntaxes+values ([(sid ...) srhs] ...) ([(id ...) rhs] ...) expr ...)
        (with-syntax ([(srhs* ...) (map rw (syntax->list #'(srhs ...)))]
                      [(rhs* ...) (map rw (syntax->list #'(rhs ...)))]
                      [(expr* ...) (map rw (syntax->list #'(expr ...)))])
          (quasisyntax/loc stx (letrec-syntaxes+values ([(sid ...) srhs*] ...) ([(id ...) rhs*] ...) expr* ...)))]
       [((~and kw
               (~or if begin begin0 set! #%plain-app #%expression
                    #%variable-reference with-continuation-mark))
         expr ...)
        (quasisyntax/loc stx (#,#'kw #,@(map rw (syntax->list #'(expr ...)))))]))
  (rw stx))

(define-syntax (define-rewriter stx)
  (syntax-case stx ()
    [(_ orig-name new-name [from to] ...)
     #'(begin
         (define-for-syntax from-list (list #'from ...))
         (define-for-syntax tbl (make-immutable-free-id-table (map cons from-list (list #'to ...))))
         (define-syntax (new-name stx)
           (syntax-case stx ()
             [(_ . args)
              (let ([result (local-expand (syntax/loc stx (orig-name . args)) (syntax-local-context) null)])
                (rewrite result tbl from-list))])))]))

(provide define-rewriter)
#;(define-syntax-rule (m x) (+ x 7))

#;(define-rewriter m n [+ -])

#;(n 77)
