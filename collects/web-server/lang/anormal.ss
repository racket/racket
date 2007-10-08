(module anormal mzscheme
  (require-for-template mzscheme)
  (require (lib "kerncase.ss" "syntax")
           (lib "list.ss")
           (lib "plt-match.ss")
           "util.ss")
  (provide make-anormal-term)
  
  ; A-Normal Form
  (define (id x) x)
  
  ;; a context is either
  ;;    frame
  ;;    (compose context frame)
  
  ;; a frame is either
  ;;    w -> target-redex
  ;;    (listof w) -> target-redex
  
  ;; compose: (w -> target-expr) (alpha -> target-redex) -> (alpha -> target-expr)
  ;; compose a context with a frame
  (define (compose ctxt frame)
    (if (eq? ctxt id) 
        frame
        (lambda (val)
          (let-values ([(x ref-to-x) (generate-formal 'x)])
            #`(#%app (lambda (#,x) #,(ctxt ref-to-x)) #,(frame val))))))
  
  (define (make-anormal-term elim-letrec-term)
    (define (anormal-term stx)
      (anormal id stx))
    
    (define (anormal ctxt stx)
      (recertify
       stx
       (kernel-syntax-case
           stx (transformer?)
         [(begin)
          (anormal ctxt (syntax/loc stx (#%app (#%top . void))))]
         [(begin lbe)
          (anormal ctxt (syntax/loc stx lbe))]
         [(begin fbe be ...)
          (anormal ctxt 
                   (syntax/loc stx 
                     (#%app call-with-values
                            (lambda () fbe)
                            (lambda throw-away
                              (begin be ...)))))]
         [(begin0)
          (anormal ctxt (syntax/loc stx (#%app (#%top . void))))]
         [(begin0 lbe)
          (anormal ctxt (syntax/loc stx lbe))]
         [(begin0 fbe be ...)
          (let-values ([(save ref-to-save) (generate-formal 'save)])
            (anormal ctxt 
                     (quasisyntax/loc stx 
                       (#%app call-with-values
                              (lambda () fbe)
                              (lambda #,save
                                (begin be ... 
                                       (#%app apply values #,ref-to-save)))))))]
         [(define-values (v ...) ve)
          (with-syntax ([ve (anormal-term #'ve)])
            (syntax/loc stx 
              (define-values (v ...) ve)))]
         [(define-syntaxes (v ...) ve)
          stx]
         [(define-values-for-syntax (v ...) ve)
          stx]
         [(set! v ve)
          (anormal
           (compose ctxt
                    (lambda (val)
                      (quasisyntax/loc stx (set! v #,val))))
           #'ve)]
         [(let-values () be)
          (anormal ctxt (syntax/loc stx be))]
         [(let-values ([(v) ve]) be)
          (anormal ctxt
                   (syntax/loc stx 
                     (#%app (lambda (v) be)
                            ve)))]
         [(let-values ([(v ...) ve]) be)
          (anormal ctxt
                   (syntax/loc stx 
                     (#%app call-with-values
                            (lambda () ve)
                            (lambda (v ...) be))))]
         [(let-values ([(fv ...) fve] [(v ...) ve] ...) be)
          (anormal ctxt
                   (syntax/loc stx 
                     (let-values ([(fv ...) fve])
                       (let-values ([(v ...) ve] ...)
                         be))))]
         [(let-values ([(v ...) ve] ...) be ...)
          (anormal ctxt
                   (syntax/loc stx 
                     (let-values ([(v ...) ve] ...)
                       (begin be ...))))]
         [(letrec-values ([(v ...) ve] ...) be ...)
          (anormal ctxt
                   (elim-letrec-term stx))]
         [(lambda formals be ...)
          (with-syntax ([nbe (anormal-term (syntax/loc stx (begin be ...)))])
            (ctxt (syntax/loc stx (lambda formals nbe))))]
         [(case-lambda [formals be] ...)
          (with-syntax ([(be ...) (map anormal-term (syntax->list #'(be ...)))])
            (ctxt (syntax/loc stx (case-lambda [formals be] ...))))]
         [(case-lambda [formals be ...] ...)
          (anormal ctxt
                   (syntax/loc stx (case-lambda [formals (begin be ...)] ...)))]
         [(if te ce ae)
          (anormal
           (compose ctxt
                    (lambda (val)
                      (quasisyntax/loc stx 
                        (if #,val
                            #,(anormal-term #'ce)
                            #,(anormal-term #'ae)))))
           #'te)]
         [(if te ce)
          (anormal ctxt (syntax/loc stx (if te ce (#%app void))))]
         [(quote datum)
          (ctxt stx)]
         [(quote-syntax datum)
          (ctxt stx)]
         [(letrec-syntaxes+values ([(sv ...) se] ...)
            ([(vv ...) ve] ...)
            be ...)
          (anormal ctxt
                   (elim-letrec-term stx))]
         [(with-continuation-mark ke me be)
          (anormal
           (compose ctxt
                    (lambda (kev)
                      (anormal 
                       (lambda (mev)
                         (quasisyntax/loc stx 
                           (with-continuation-mark #,kev #,mev
                             #,(anormal-term #'be))))
                       #'me)))
           #'ke)]
         [(#%expression d)
          (anormal
           (compose ctxt
                    (lambda (d)
                       (quasisyntax/loc stx (#%expression #,d))))
           #'d)]
         [(#%app fe e ...)
          (anormal
           (lambda (val0)
             (anormal*
              (compose ctxt
                       (lambda (rest-vals)
                         (quasisyntax/loc stx 
                           (#%app #,val0 #,@rest-vals))))
              (syntax->list #'(e ...))))
           #'fe)]
         [(#%top . v)
          (ctxt stx)]
         [(#%datum . d)
          (ctxt stx)]
         [(#%variable-reference . v)
          (ctxt stx)]
         [id (identifier? #'id)
             (ctxt #'id)]
         [_
          (raise-syntax-error 'anormal "Dropped through:" stx)])))
    
    ;; anormal*: ((listof w) -> target-expr) (listof source-expr) -> target-expr
    ;; normalize an expression given as a context and list of sub-expressions
    (define (anormal* multi-ctxt exprs)
      (match exprs
        [(list) 
         (multi-ctxt '())]
        [(list-rest fe re)
         (anormal
          (lambda (val)
            (anormal*
             (lambda (rest-vals)
               (multi-ctxt (list* val rest-vals)))
             re))
          fe)]))
    
    anormal-term))