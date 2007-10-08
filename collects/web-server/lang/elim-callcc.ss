(module elim-callcc mzscheme
  (require-for-template mzscheme
                        "../lang/abort-resume.ss")
  (require-for-syntax "../lang/abort-resume.ss")
  (require (lib "kerncase.ss" "syntax")
           "util.ss")
  (provide elim-callcc)
  
  (define (id x) x)
  
  ;; mark-lambda-as-safe: w -> w
  ;; If w is a lambda-expression then add #t to the safety mark, otherwise no mark
  (define (mark-lambda-as-safe w)
    (recertify
     w
     (syntax-case w (lambda case-lambda)
       [(lambda formals be ...)
        (syntax/loc w
          (lambda formals
            (with-continuation-mark safe-call? '(#t (lambda formals))
              be ...)))]
       [(case-lambda [formals be ...] ...)
        (syntax/loc w
          (case-lambda [formals 
                        (with-continuation-mark safe-call? '(#t (case-lambda formals ...))
                          be ...)] ...))]
       [_else w])))
  
  (define (elim-callcc stx)
    (elim-callcc/mark id stx))
  
  (define (elim-callcc/mark markit stx)  
    (recertify
     stx
     (kernel-syntax-case*
      stx (transformer?) (call/cc call-with-values)
      [(begin be ...)
       (raise-syntax-error 'elim-callcc/mark "Not in ANF" stx)]
      [(begin0 be ...)
       (raise-syntax-error 'elim-callcc/mark "Not in ANF" stx)]
      [(define-values (v ...) ve)
       (with-syntax ([ve (mark-lambda-as-safe (elim-callcc #'ve))])
         (syntax/loc stx
           (define-values (v ...) ve)))]
      [(define-syntaxes (v ...) ve)
       stx]
      [(define-values-for-syntax (v ...) ve)
       stx]
      [(set! v ve)
       (with-syntax ([ve (elim-callcc #'ve)])
         (syntax/loc stx (set! v ve)))]
      [(let-values ([(v ...) ve] ...) be ...)
       (raise-syntax-error 'elim-callcc/mark "Not in ANF" stx)]
      [(letrec-values ([(v ...) ve] ...) be ...)
       (raise-syntax-error 'elim-callcc/mark "Not in ANF" stx)]
      [(lambda formals be)
       (with-syntax ([be (elim-callcc #'be)])
         (syntax/loc stx
           (lambda formals be)))]
      [(case-lambda [formals be] ...)
       (with-syntax ([(be ...) (map elim-callcc (syntax->list #'(be ...)))])
         (syntax/loc stx
           (case-lambda [formals be] ...)))]
      [(if te ce ae)
       (with-syntax ([te (elim-callcc #'te)]
                     [ce (elim-callcc #'ce)]
                     [ae (elim-callcc #'ae)])
         (markit (syntax/loc stx (if te ce ae))))]
      [(if te ce)
       (raise-syntax-error 'elim-callcc/mark "Not in ANF" stx)]
      [(quote datum)
       stx]
      [(quote-syntax datum)
       stx]
      [(letrec-syntaxes+values ([(sv ...) se] ...)
         ([(vv ...) ve] ...)
         be ...)
       (raise-syntax-error 'elim-callcc/mark "Not in ANF" stx)]
      [(with-continuation-mark ke me be)
       (let* ([ke-prime (elim-callcc #'ke)]
              [me-prime (elim-callcc #'me)]
              [be-prime (elim-callcc #'be)])
         ; XXX Could be dangerous to evaluate ke-prime and me-prime twice
         (markit 
          (quasisyntax/loc stx
            (with-continuation-mark #,ke-prime #,me-prime
              (with-continuation-mark 
                  the-save-cm-key 
                (#%app current-saved-continuation-marks-and #,ke-prime #,me-prime)
                #,be-prime)))))]
      [(#%expression d)
       (markit (quasisyntax/loc stx (#%expression #,(elim-callcc #'d))))]
      [(#%app call/cc w)
       (let-values ([(cm ref-to-cm) (generate-formal 'current-marks)]
                    [(x ref-to-x) (generate-formal 'x)])
         (markit 
          (quasisyntax/loc stx
            (#%app #,(elim-callcc #'w)
                   (#%app (lambda (#,cm)
                            (lambda #,x
                              (#%app abort
                                     (lambda () (#%app resume #,ref-to-cm #,ref-to-x)))))
                          (#%app activation-record-list))))))]
      [(#%app call-with-values (lambda () prod) cons)
       (let ([cons-prime (datum->syntax-object #f (gensym 'cons))])
         (quasisyntax/loc stx
           (let-values ([(#,cons-prime) #,(mark-lambda-as-safe (elim-callcc #'cons))])
             #,(markit
                (quasisyntax/loc stx
                  (#%app call-with-values 
                         #,(mark-lambda-as-safe
                            (quasisyntax/loc stx
                              (lambda ()
                                #,(elim-callcc/mark
                                   (lambda (x)
                                     (quasisyntax/loc stx
                                       (with-continuation-mark the-cont-key #,cons-prime #,x)))
                                   #'prod))))
                         #,cons-prime))))))]
      [(#%app w (#%app . stuff))
       (with-syntax ([e #'(#%app . stuff)])
         (syntax-case #'w (lambda case-lambda)
           [(lambda formals body)
            (let ([w-prime (datum->syntax-object #f (gensym 'l))])
              (quasisyntax/loc stx
                (let-values ([(#,w-prime) #,(elim-callcc #'w)])
                  #,(markit
                     (quasisyntax/loc stx
                       (#%app #,w-prime
                              #,(elim-callcc/mark
                                 (lambda (x)
                                   (quasisyntax/loc stx
                                     (with-continuation-mark the-cont-key #,w-prime #,x)))
                                 #'e)))))))]
           [(case-lambda [formals body] ...)
            (let ([w-prime (datum->syntax-object #f (gensym 'cl))])
              (quasisyntax/loc stx
                (let-values ([(#,w-prime) #,(elim-callcc #'w)])
                  #,(markit
                     (quasisyntax/loc stx
                       (#%app #,w-prime
                              #,(elim-callcc/mark
                                 (lambda (x)
                                   (quasisyntax/loc stx
                                     (with-continuation-mark the-cont-key #,w-prime #,x)))
                                 #'e)))))))]
           [_else
            (let ([w-prime (elim-callcc #'w)])
              (markit
               (quasisyntax/loc stx
                 (#%app #,w-prime
                        #,(elim-callcc/mark
                           (lambda (x)
                             #`(with-continuation-mark the-cont-key #,w-prime #,x))
                           #'e)))))]))]
      [(#%app w rest ...)
       (markit
        (quasisyntax/loc stx
          (with-continuation-mark safe-call? '(#f stx)
            (#%app #,(mark-lambda-as-safe (elim-callcc #'w))
                   #,@(map 
                       (lambda (an-expr)
                         (mark-lambda-as-safe
                          (elim-callcc
                           an-expr)))
                       (syntax->list #'(rest ...)))))))]
      [(#%top . v)
       stx]
      [(#%datum . d)
       stx]
      [(#%variable-reference . v)
       stx]       
      [id (identifier? #'id)
          stx]
      [_
       (raise-syntax-error 'elim-callcc "Dropped through:" stx)]))))