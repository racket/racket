(module util mzscheme
  (require-for-template mzscheme)
  (require (lib "kerncase.ss" "syntax")
           (lib "list.ss"))
  (provide (all-defined-except template))
  
  (define transformer? (make-parameter #f))
  
  (define (recertify old-expr expr)
    (syntax-recertify expr old-expr (current-code-inspector) #f))
  
  (define (recertify* old-expr exprs)
    (map (lambda (expr)
           (syntax-recertify expr old-expr (current-code-inspector) #f))
         exprs))
  
  (define (recertify/new-defs old-expr thunk)
    (call-with-values
     thunk
     (lambda (expr new-defs)
       (values (recertify old-expr expr)
               (recertify* old-expr new-defs)))))
  
  (define current-code-labeling
    (make-parameter
     (lambda (stx)
       (datum->syntax-object stx 'error))))
  
  (define (generate-formal sym-name)
    (let ([name (datum->syntax-object #f (gensym sym-name))])
      (with-syntax ([(lambda (formal) ref-to-formal)
                     (if (syntax-transforming?)
                         (local-expand #`(lambda (#,name) #,name) 'expression empty)
                         #`(lambda (#,name) #,name))])
        (values #'formal #'ref-to-formal))))
  
  (define (formals-list stx)
    (syntax-case stx ()
      [v (identifier? #'v)
         (list #'v)]
      [(v ...)
       (syntax->list #'(v ...))]
      [(v ... . rv)
       (list* #'rv (syntax->list #'(v ...)))]))
  
  (define ((make-define-case inner) stx)
    (recertify 
     stx
     (syntax-case stx (define-values define-syntaxes define-values-for-syntax)
       [(define-values (v ...) ve)
        (with-syntax ([ve (inner #'ve)])
          (syntax/loc stx
            (define-values (v ...) ve)))]
       [(define-syntaxes (v ...) ve)
        (parameterize ([transformer? #t])
          (with-syntax ([ve (inner #'ve)])
            (syntax/loc stx
              (define-syntaxes (v ...) ve))))]      
       [(define-values-for-syntax (v ...) ve)
        (parameterize ([transformer? #t])
          (with-syntax ([ve (inner #'ve)])
            (syntax/loc stx
              (define-values-for-syntax (v ...) ve))))]
       [_
        (raise-syntax-error 'define-case "Dropped through:" stx)])))
  
  (define ((make-define-case/new-defs inner) stx)
    (let-values ([(nstx defs) (inner stx)])
      (append defs (list nstx))))
  
  (define ((make-module-case/new-defs inner) stx)
    (recertify*
     stx
     (syntax-case* stx (require provide require-for-syntax require-for-template) module-identifier=?
       [(require spec ...)
        (list stx)]
       [(provide spec ...)
        (list stx)]
       [(require-for-syntax spec ...)
        (list stx)]
       [(require-for-template spec ...)
        (list stx)]
       [_
        (inner stx)])))
  
  (define ((make-module-case inner) stx)
    (recertify
     stx
     (syntax-case* stx (require provide require-for-syntax require-for-template) module-identifier=?
       [(require spec ...)
        stx]
       [(provide spec ...)
        stx]
       [(require-for-syntax spec ...)
        stx]
       [(require-for-template spec ...)
        stx]
       [_
        (inner stx)])))
  
  (define ((make-lang-module-begin make-labeling transform) stx)
    (recertify
     stx
     (syntax-case stx ()                     
       ((mb forms ...)
        (with-syntax ([(pmb rfs0 body ...)
                       (local-expand (quasisyntax/loc stx
                                       (#%plain-module-begin 
                                        #,(syntax-local-introduce #'(require-for-syntax mzscheme))
                                        forms ...))
                                     'module-begin 
                                     empty)])
          (let ([base-labeling (make-labeling (string->bytes/utf-8 (format "~a" (syntax-object->datum stx))))])
            (parameterize ([current-code-labeling
                            (lambda (stx)
                              (datum->syntax-object stx (base-labeling)))])
              (let ([new-defs (apply append (map transform (syntax->list #'(body ...))))])
                (quasisyntax/loc stx
                  (pmb rfs0
                       #,@new-defs))))))))))
  
  (define (bound-identifier-member? id ids)
    (ormap
     (lambda (an-id)
       (bound-identifier=? id an-id))
     ids))
  
  ;; Kernel Case Template
  (define (template stx)  
    (recertify
     stx
     (kernel-syntax-case 
         stx (transformer?)
       [(begin be ...)
        (with-syntax ([(be ...) (map template (syntax->list #'(be ...)))])
          (syntax/loc stx
            (begin be ...)))]
       [(begin0 be ...)
        (with-syntax ([(be ...) (map template (syntax->list #'(be ...)))])
          (syntax/loc stx
            (begin0 be ...)))]
       [(define-values (v ...) ve)
        (with-syntax ([ve (template #'ve)])
          (syntax/loc stx
            (define-values (v ...) ve)))]
       [(define-syntaxes (v ...) ve)
        (parameterize ([transformer? #t])
          (with-syntax ([ve (template #'ve)])
            (syntax/loc stx
              (define-syntaxes (v ...) ve))))]
       [(define-values-for-syntax (v ...) ve)
        (parameterize ([transformer? #t])
          (with-syntax ([ve (template #'ve)])
            (syntax/loc stx 
              (define-values-for-syntax (v ...) ve))))]
       [(set! v ve)
        (with-syntax ([ve (template #'ve)])
          (syntax/loc stx
            (set! v ve)))]
       [(let-values ([(v ...) ve] ...) be ...)
        (with-syntax ([(ve ...) (map template (syntax->list #'(ve ...)))]
                      [(be ...) (map template (syntax->list #'(be ...)))])
          (syntax/loc stx
            (let-values ([(v ...) ve] ...) be ...)))]
       [(letrec-values ([(v ...) ve] ...) be ...)
        (with-syntax ([(ve ...) (map template (syntax->list #'(ve ...)))]
                      [(be ...) (map template (syntax->list #'(be ...)))])
          (syntax/loc stx
            (letrec-values ([(v ...) ve] ...) be ...)))]
       [(lambda formals be ...)
        (with-syntax ([(be ...) (map template (syntax->list #'(be ...)))])
          (syntax/loc stx
            (lambda formals be ...)))]
       [(case-lambda [formals be ...] ...)
        (with-syntax ([((be ...) ...) (map template (syntax->list #'((be ...) ...)))])
          (syntax/loc stx
            (case-lambda [formals be ...] ...)))]
       [(if te ce ae)
        (with-syntax ([te (template #'te)]
                      [ce (template #'ce)]
                      [ae (template #'ae)])
          (syntax/loc stx
            (if te ce ae)))]
       [(if te ce)
        (template (syntax/loc stx (if te ce (#%app void))))]
       [(quote datum)
        stx]
       [(quote-syntax datum)
        stx]
       [(letrec-syntaxes+values ([(sv ...) se] ...)
          ([(vv ...) ve] ...)
          be ...)
        (with-syntax ([(se ...) (map template (syntax->list #'(se ...)))]
                      [(ve ...) (map template (syntax->list #'(ve ...)))]
                      [(be ...) (map template (syntax->list #'(be ...)))])
          (syntax/loc stx
            (letrec-syntaxes+values ([(sv ...) se] ...)
              ([(vv ...) ve] ...)
              be ...)))]
       [(with-continuation-mark ke me be)
        (with-syntax ([ke (template #'ke)]
                      [me (template #'me)]
                      [be (template #'be)])
          (syntax/loc stx
            (with-continuation-mark ke me be)))]
       [(#%expression . d)
        stx]
       [(#%app e ...)
        (with-syntax ([(e ...) (map template (syntax->list #'(e ...)))])
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