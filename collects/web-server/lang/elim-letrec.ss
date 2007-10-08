(module elim-letrec mzscheme
  (require-for-template mzscheme
                        "../lang/abort-resume.ss")
  (require-for-syntax "../lang/abort-resume.ss")
  (require (lib "kerncase.ss" "syntax")
           (lib "etc.ss")
           (lib "list.ss")
           "util.ss")
  (provide (all-defined))
  
  ; elim-letrec : (listof identifier-syntax?)[3] -> syntax?[2] -> syntax?[3]
  ; Eliminates letrec-values from syntax[2] and correctly handles references to 
  ; letrec-bound variables [3] therein. 
  (define ((elim-letrec ids) stx)  
    (recertify
     stx
     (kernel-syntax-case
         stx (transformer?)
       [(begin be ...)
        (with-syntax ([(be ...) (map (elim-letrec ids) (syntax->list #'(be ...)))])
          (syntax/loc stx
            (begin be ...)))]
       [(begin0 be ...)
        (with-syntax ([(be ...) (map (elim-letrec ids) (syntax->list #'(be ...)))])
          (syntax/loc stx
            (begin0 be ...)))]
       [(define-values (v ...) ve)
        (with-syntax ([ve ((elim-letrec ids) #'ve)])
          (syntax/loc stx
            (define-values (v ...) ve)))]
       [(define-syntaxes (v ...) ve)
        stx]
       [(define-values-for-syntax (v ...) ve)       
        stx]
       [(set! v ve)
        (with-syntax ([ve ((elim-letrec ids) #'ve)])
          (if (bound-identifier-member? #'id ids)
              (syntax/loc stx (#%app set-box! id ve))
              (syntax/loc stx (set! id ve))))]
       [(let-values ([(v ...) ve] ...) be ...)
        (with-syntax ([(ve ...) (map (elim-letrec ids) (syntax->list #'(ve ...)))]
                      [(be ...) (map (elim-letrec ids) (syntax->list #'(be ...)))])
          (syntax/loc stx
            (let-values ([(v ...) ve] ...) be ...)))]
       [(letrec-values ([(v ...) ve] ...) be ...)
        (let ([new-ids (apply append ids (map syntax->list (syntax->list #'((v ...) ...))))])
          (with-syntax ([((nv ...) ...) (map (compose generate-temporaries syntax->list) (syntax->list #'((v ...) ...)))]
                        [((nv-box ...) ...) (map (lambda (nvs)
                                                   (map (lambda (x) (syntax/loc x (#%app box the-undef)))
                                                        (syntax->list nvs)))
                                                 (syntax->list #`((v ...) ...)))]
                        [(ve ...) (map (elim-letrec new-ids) (syntax->list #'(ve ...)))]
                        [(be ...) (map (elim-letrec new-ids) (syntax->list #'(be ...)))])
            ; XXX Optimize special case of one nv
            (syntax/loc stx
              (let-values ([(v ...)
                            (#%app values nv-box ...)] ...)
                (begin (#%app call-with-values
                              (lambda () ve)
                              (lambda (nv ...)
                                (#%app set-box! v nv) ...))
                       ...
                       be ...)))))]
       [(lambda formals be ...)
        (with-syntax ([(be ...) (map (elim-letrec ids) (syntax->list #'(be ...)))])
          (syntax/loc stx
            (lambda formals be ...)))]
       [(case-lambda [formals be ...] ...)
        (with-syntax ([((be ...) ...) (map (elim-letrec ids) (syntax->list #'((be ...) ...)))])
          (syntax/loc stx
            (case-lambda [formals be ...] ...)))]
       [(if te ce ae)
        (with-syntax ([te ((elim-letrec ids) #'te)]
                      [ce ((elim-letrec ids) #'ce)]
                      [ae ((elim-letrec ids) #'ae)])
          (syntax/loc stx
            (if te ce ae)))]
       [(if te ce)
        ((elim-letrec ids) 
         (syntax/loc stx
           (if te ce (#%app (#%top . void)))))]
       [(quote datum)
        stx]
       [(quote-syntax datum)
        stx]
       [(letrec-syntaxes+values ([(sv ...) se] ...)
          ([(vv ...) ve] ...)
          be ...)
        (let ([new-ids (apply append ids (map syntax->list (syntax->list #'((vv ...) ...))))])
          (with-syntax ([((nvv ...) ...) (map (compose generate-temporaries syntax->list) (syntax->list #'((vv ...) ...)))]
                        [((nvv-box ...) ...) (map (lambda (nvs)
                                                    (map (lambda (x) (syntax/loc x (#%app box the-undef)))
                                                         (syntax->list nvs)))
                                                  (syntax->list #`((vv ...) ...)))]
                        [(se ...) (map (elim-letrec new-ids) (syntax->list #'(se ...)))]
                        [(ve ...) (map (elim-letrec new-ids) (syntax->list #'(ve ...)))]
                        [(be ...) (map (elim-letrec new-ids) (syntax->list #'(be ...)))])
            ; XXX Optimize special case of one nv
            (syntax/loc stx
              (let-values ([(vv ...)
                            (#%app values nvv-box ...)] ...)
                ; This is okay, because we've already expanded the syntax.
                (let-syntaxes 
                 ([(sv ...) se] ...)
                 (begin (#%app call-with-values
                               (lambda () ve)
                               (lambda (nvv ...)
                                 (#%app set-box! vv nvv) ...))
                        ...
                        be ...))))))]
       [(with-continuation-mark ke me be)
        (with-syntax ([ke ((elim-letrec ids) #'ke)]
                      [me ((elim-letrec ids) #'me)]
                      [be ((elim-letrec ids) #'be)])
          (syntax/loc stx
            (with-continuation-mark ke me be)))]
       [(#%expression d)
        (quasisyntax/loc stx (#%expression #,((elim-letrec ids) #'d)))]
       [(#%app e ...)
        (with-syntax ([(e ...) (map (elim-letrec ids) (syntax->list #'(e ...)))])
          (syntax/loc stx
            (#%app e ...)))]
       [(#%top . v)
        stx]
       [(#%datum . d)
        stx]
       [(#%variable-reference . v)
        stx]       
       [id (identifier? #'id)
           (if (bound-identifier-member? #'id ids)
               (syntax/loc stx (#%app unbox id))
               #'id)]
       [_
        (raise-syntax-error 'elim-letrec "Dropped through:" stx)])))
  
  (define elim-letrec-term (elim-letrec empty)))