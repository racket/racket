
;;----------------------------------------------------------------------
;; cond

(module cond '#%kernel
  (#%require (for-syntax "stx.rkt" "qq-and-or.rkt" '#%kernel))

  (define-syntaxes (=>)
    (lambda (stx)
      (raise-syntax-error #f "arrow not allowed as an expression" stx)))

  (define-syntaxes (else)
    (lambda (stx)
      (raise-syntax-error #f "not allowed as an expression" stx)))

  ;; old-cond is like cond, but uses unbound `=>' and `else'

  (define-syntaxes (cond old-cond)
    (let ([go
           (let ([here (quote-syntax here)])
             (lambda (in-form =>-stx else-stx)
               (if (identifier? in-form)
                   (raise-syntax-error #f "bad syntax" in-form)
                   (void))
               (datum->syntax
                here
                (let ([form (stx-cdr in-form)]
                      [serror
                       (lambda (msg at)
                         (raise-syntax-error #f msg in-form at))])
                  (let loop ([tests form][first? #t])
                    (if (stx-null? tests)
                        (quote-syntax (void))
                        (if (not (stx-pair? tests))
                            (serror
                             "bad syntax (body must contain a list of pairs)"
                             tests)
                            (let ([line (stx-car tests)]
                                  [rest (stx-cdr tests)])
                              (if (not (stx-pair? line))
                                  (serror
                                   "bad syntax (clause is not a test-value pair)"
                                   line)
                                  (let* ([test (stx-car line)]
                                         [value (stx-cdr line)]
                                         [else? (and (identifier? test)
                                                     (free-identifier=? test else-stx))])
                                    (if (and else? (stx-pair? rest))
                                        (serror "bad syntax (`else' clause must be last)" line)
                                        (void))
                                    (if (and (not else?)
                                             (stx-pair? value)
                                             (identifier? (stx-car value))
                                             (free-identifier=? (stx-car value) =>-stx))
                                        (if (and (stx-pair? (stx-cdr value))
                                                 (stx-null? (stx-cdr (stx-cdr value))))
                                            (let ([gen (gen-temp-id 'c)])
                                              `(,(quote-syntax let-values) ([(,gen) ,test])
                                                (,(quote-syntax if) ,gen
                                                 (,(stx-car (stx-cdr value)) ,gen)
                                                 ,(loop rest #f))))
                                            (serror
                                             "bad syntax (bad clause form with =>)"
                                             line))
                                        (if else?
                                            (if (stx-null? value)
                                                (serror
                                                 "missing expressions in `else' clause"
                                                 line)
                                                (list* (quote-syntax let-values) (quote-syntax ()) value))
                                            (if (stx-null? value)
                                                (let ([gen (gen-temp-id 'c)])
                                                  `(,(quote-syntax let-values) ([(,gen) ,test])
                                                    (,(quote-syntax if) ,gen ,gen ,(loop rest #f))))
                                                (list
                                                 (quote-syntax if) test
                                                 (list* (quote-syntax let-values) (quote-syntax ()) value)
                                                 (loop rest #f))))))))))))
                in-form)))])
      (values
       (lambda (stx) (go stx (quote-syntax =>) (quote-syntax else)))
       (lambda (stx) (go stx (datum->syntax #f '=>) (datum->syntax #f 'else))))))
  
  (#%provide cond old-cond else =>))
