#lang scheme/base
(require (for-template scheme/base)         
         syntax/kerncase
         mzlib/etc
         mzlib/list
         (for-template "../lang/abort-resume.ss")
         "util.ss")
(provide (all-defined-out))

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
     [(set! v ve)
      (with-syntax ([ve ((elim-letrec ids) #'ve)])
        (if (bound-identifier-member? #'id ids)
            (syntax/loc stx (#%plain-app set-box! id ve))
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
                                                 (map (lambda (x) (syntax/loc x (#%plain-app box the-undef)))
                                                      (syntax->list nvs)))
                                               (syntax->list #`((v ...) ...)))]
                      [(ve ...) (map (elim-letrec new-ids) (syntax->list #'(ve ...)))]
                      [(be ...) (map (elim-letrec new-ids) (syntax->list #'(be ...)))])
          ; XXX Optimize special case of one nv
          (syntax/loc stx
            (let-values ([(v ...)
                          (#%plain-app values nv-box ...)] ...)
              (begin (#%plain-app call-with-values
                                  (#%plain-lambda () ve)
                                  (#%plain-lambda (nv ...)
                                                  (#%plain-app set-box! v nv) ...))
                     ...
                     be ...)))))]
     [(#%plain-lambda formals be ...)
      (with-syntax ([(be ...) (map (elim-letrec ids) (syntax->list #'(be ...)))])
        (syntax/loc stx
          (#%plain-lambda formals be ...)))]
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
     [(quote datum)
      stx]
     [(quote-syntax datum)
      stx]     
     [(with-continuation-mark ke me be)
      (with-syntax ([ke ((elim-letrec ids) #'ke)]
                    [me ((elim-letrec ids) #'me)]
                    [be ((elim-letrec ids) #'be)])
        (syntax/loc stx
          (with-continuation-mark ke me be)))]     
     [(#%plain-app e ...)
      (with-syntax ([(e ...) (map (elim-letrec ids) (syntax->list #'(e ...)))])
        (syntax/loc stx
          (#%plain-app e ...)))]
     [(#%top . v)
      stx]
     [(#%variable-reference . v)
      stx]            
     [id (identifier? #'id)
         (if (bound-identifier-member? #'id ids)
             (syntax/loc stx (#%plain-app unbox id))
             #'id)]
     [(letrec-syntaxes+values ([(sv ...) se] ...)
        ([(vv ...) ve] ...)
        be ...)
      ((elim-letrec ids)
       (syntax/loc stx
         (letrec-values ([(vv ...) ve] ...) be ...)))]
     [(#%expression d)
      (quasisyntax/loc stx (#%expression #,((elim-letrec ids) #'d)))]
     [_
      (raise-syntax-error 'elim-letrec "Dropped through:" stx)])))

(define elim-letrec-term (elim-letrec empty))
