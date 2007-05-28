(module mark-lambda mzscheme
  (require-for-template mzscheme)
  (require (lib "kerncase.ss" "syntax")
           "util.ss")
  (provide mark-lambda
           marked-lambda?)
  
  (define lambda-key 'marked-lambda)
  
  (define (marked-lambda? stx)
    (kernel-syntax-case stx #f
      [(lambda formals be ...)
       (syntax-property stx lambda-key)]
      [(case-lambda [formals be ...] ...)
       (syntax-property stx lambda-key)]
      [_ #f]))
  
  (define (mark-lambda stx)  
    (recertify
     stx
     (kernel-syntax-case 
         stx #f 
       [(begin be ...)
        (with-syntax ([(be ...) (map mark-lambda (syntax->list #'(be ...)))])
          (syntax/loc stx
            (begin be ...)))]
       [(begin0 be ...)
        (with-syntax ([(be ...) (map mark-lambda (syntax->list #'(be ...)))])
          (syntax/loc stx
            (begin0 be ...)))]
       [(define-values (v ...) ve)
        (with-syntax ([ve (mark-lambda #'ve)])
          (syntax/loc stx
            (define-values (v ...) ve)))]
       [(define-syntaxes (v ...) ve)
        (with-syntax ([ve (mark-lambda #'ve)])
          (syntax/loc stx
            (define-values (v ...) ve)))]
       [(define-values-for-syntax (v ...) ve)
        (with-syntax ([ve (mark-lambda #'ve)])
          (syntax/loc stx 
            (define-values-for-syntax (v ...) ve)))]
       [(set! v ve)
        (with-syntax ([ve (mark-lambda #'ve)])
          (syntax/loc stx
            (set! v ve)))]
       [(let-values ([(v ...) ve] ...) be ...)
        (with-syntax ([(ve ...) (map mark-lambda (syntax->list #'(ve ...)))]
                      [(be ...) (map mark-lambda (syntax->list #'(be ...)))])
          (syntax/loc stx
            (let-values ([(v ...) ve] ...) be ...)))]
       [(letrec-values ([(v ...) ve] ...) be ...)
        (with-syntax ([(ve ...) (map mark-lambda (syntax->list #'(ve ...)))]
                      [(be ...) (map mark-lambda (syntax->list #'(be ...)))])
          (syntax/loc stx
            (letrec-values ([(v ...) ve] ...) be ...)))]
       [(lambda formals be ...)
        (syntax-property 
         (with-syntax ([(be ...) (map mark-lambda (syntax->list #'(be ...)))])
           (syntax/loc stx
             (lambda formals be ...)))
         lambda-key #t)]
       [(case-lambda [formals be ...] ...)
        (syntax-property
         (with-syntax ([((be ...) ...) (map mark-lambda (syntax->list #'((be ...) ...)))])
           (syntax/loc stx
             (case-lambda [formals be ...] ...)))
         lambda-key #t)]
       [(if te ce ae)
        (with-syntax ([te (mark-lambda #'te)]
                      [ce (mark-lambda #'ce)]
                      [ae (mark-lambda #'ae)])
          (syntax/loc stx
            (if te ce ae)))]
       [(if te ce)
        (mark-lambda (syntax/loc stx (if te ce (#%app void))))]
       [(quote datum)
        stx]
       [(quote-syntax datum)
        stx]
       [(letrec-syntaxes+values ([(sv ...) se] ...)
          ([(vv ...) ve] ...)
          be ...)
        (with-syntax ([(se ...) (map mark-lambda (syntax->list #'(se ...)))]
                      [(ve ...) (map mark-lambda (syntax->list #'(ve ...)))]
                      [(be ...) (map mark-lambda (syntax->list #'(be ...)))])
          (syntax/loc stx
            (letrec-syntaxes+values ([(sv ...) se] ...)
              ([(vv ...) ve] ...)
              be ...)))]
       [(with-continuation-mark ke me be)
        (with-syntax ([ke (mark-lambda #'ke)]
                      [me (mark-lambda #'me)]
                      [be (mark-lambda #'be)])
          (syntax/loc stx
            (with-continuation-mark ke me be)))]
       [(#%expression . d)
        stx]
       [(#%app e ...)
        (with-syntax ([(e ...) (map mark-lambda (syntax->list #'(e ...)))])
          (syntax/loc stx
            (#%app e ...)))]
       [(#%top . v)
        stx]
       [(#%datum . d)
        stx]
       [(#%variable-reference . v)
        stx]       
       [id (identifier? #'id)
           stx]     
       [_
        (raise-syntax-error 'kerncase "Dropped through:" stx)]))))