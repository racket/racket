
;;----------------------------------------------------------------------
;; cond

(module cond '#%kernel
  (#%require (for-syntax '#%kernel "stx.rkt"))

  (define-syntaxes (=>)
    (lambda (stx)
      (raise-syntax-error #f "arrow not allowed as an expression" stx)))

  (define-syntaxes (else)
    (lambda (stx)
      (raise-syntax-error #f "not allowed as an expression" stx)))

  ;; old-cond is like cond, but uses unbound `=>' and `else'

  (define-syntaxes (cond old-cond)
    (let-values ([(go)
                  (let-values ([(here) (quote-syntax here)])
                    (lambda (in-form =>-stx else-stx track-disappeared-uses?)
                      (if (identifier? in-form)
                          (raise-syntax-error #f "bad syntax" in-form)
                          (void))
                      (let-values
                          ([(expansion disappeared-uses)
                            (let-values ([(form) (stx-cdr in-form)]
                                         [(serror)
                                          (lambda (msg at)
                                            (raise-syntax-error #f msg in-form at))])
                              (letrec-values ([(loop)
                                               (lambda (tests)

                                                 (if (stx-null? tests)
                                                     (values (quote-syntax (void)) '())
                                                     (if (not (stx-pair? tests))
                                                         (serror
                                                          "bad syntax (body must contain a list of pairs)"
                                                          tests)
                                                         (let-values ([(line) (stx-car tests)]
                                                                      [(rest) (stx-cdr tests)])
                                                           (if (not (stx-pair? line))
                                                               (serror
                                                                "bad syntax (clause is not a test-value pair)"
                                                                line)
                                                               (letrec-values ([(test) (stx-car line)]
                                                                               [(value) (stx-cdr line)]
                                                                               [(else?) (if (identifier? test)
                                                                                            (free-identifier=? test else-stx)
                                                                                            #f)])
                                                                 (if (if else? (stx-pair? rest) #f)
                                                                     (serror "bad syntax (`else' clause must be last)" line)
                                                                     (void))
                                                                 (if (if (not else?)
                                                                         (if (stx-pair? value)
                                                                             (if (identifier? (stx-car value))
                                                                                 (free-identifier=? (stx-car value) =>-stx)
                                                                                 #f)
                                                                             #f)
                                                                         #f)
                                                                     (if (if (stx-pair? (stx-cdr value))
                                                                             (stx-null? (stx-cdr (stx-cdr value)))
                                                                             #f)
                                                                         (let-values ([(exp d-u) (loop rest)])
                                                                           (let-values ([(gen) 'cond-val])
                                                                             (values
                                                                              (list (quote-syntax let-values)
                                                                                    (list (list (list gen) test))
                                                                                    (list (quote-syntax if)
                                                                                          gen
                                                                                          (list (stx-car (stx-cdr value)) gen)
                                                                                          exp))
                                                                              (cons (syntax-local-introduce (stx-car value))
                                                                                    d-u))))
                                                                         (serror
                                                                          "bad syntax (bad clause form with =>)"
                                                                          line))
                                                                     (if else?
                                                                         (if (stx-null? value)
                                                                             (serror
                                                                              "missing expressions in `else' clause"
                                                                              line)
                                                                             (values (list* (quote-syntax let-values)
                                                                                            (quote-syntax ())
                                                                                            value)
                                                                                     (list (syntax-local-introduce test))))
                                                                         (let-values ([(exp d-u) (loop rest)])
                                                                           (values
                                                                            (if (stx-null? value)
                                                                                (let-values ([(gen) 'cond-val])
                                                                                  (list (quote-syntax let-values)
                                                                                        (list (list (list gen) test))
                                                                                        (list (quote-syntax if)
                                                                                              gen gen exp)))
                                                                                (list
                                                                                 (quote-syntax if) test
                                                                                 (list* (quote-syntax let-values)
                                                                                        (quote-syntax ())
                                                                                        value)
                                                                                 exp))
                                                                            d-u)))))))))
                                                 )])
                                (loop form)))])
                        (let-values ([(expansion-stx) (datum->syntax here expansion in-form)])
                          (if (if (not track-disappeared-uses?) #t (null? disappeared-uses))
                              expansion-stx
                              (syntax-property expansion-stx 'disappeared-use disappeared-uses))))))])
      (values
       (lambda (stx) (go stx (quote-syntax =>) (quote-syntax else) #t))
       (lambda (stx) (go stx (datum->syntax #f '=>) (datum->syntax #f 'else) #f)))))
  
  (#%provide cond old-cond else =>))
