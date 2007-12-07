;; A #%module-begin that wraps each module-level expression with 
;;  `print-value'.

(module modbeg '#%kernel
  (#%require (for-syntax '#%kernel))

  (#%provide module-begin)

  (define-values (print-values)
    (lambda vs (for-each (current-print) vs)))

  (define-syntaxes (module-begin)
    (lambda (stx)
      (if (eq? 'module-begin (syntax-local-context))
          (void)
          (raise-syntax-error
           #f
           "allowed only around a module body"
           stx))
      (if (symbol? (syntax-e stx))
          (raise-syntax-error
           #f
           "bad syntax" 
           stx)
          (void))
      (datum->syntax
       stx
       (list (quote-syntax #%module-begin)
             (cons (quote-syntax printing-module-begin)
                   (cdr (syntax-e stx))))
       stx
       stx)))

  (define-syntaxes (printing-module-begin)
    (lambda (stx)
      (let-values ([(r) (cdr (syntax-e stx))])
        (let-values ([(r) (if (syntax? r)
                              (syntax-e r)
                              r)])
          (if (null? r)
              (quote-syntax (void))
              (let-values ([(e) (local-expand (car r)
                                              'module
                                              (syntax->list
                                               (quote-syntax 
                                                (quote 
                                                 quote-syntax #%top
                                                 lambda case-lambda
                                                 let-values letrec-values
                                                 begin begin0 set!
                                                 with-continuation-mark
                                                 if #%app #%expression
                                                 define-values define-syntaxes define-values-for-syntax
                                                 module 
                                                 #%module-begin 
                                                 #%require #%provide 
                                                 #%variable-reference))))])
                ;; `begin' is special...
                (if (let-values ([(p) (syntax-e e)])
                      (if (pair? p)
                          (if (symbol? (syntax-e (car p)))
                              (if (free-identifier=? (car p) (quote-syntax begin))
                                  (syntax->list e)
                                  #f)
                              #f)
                          #f))
                    ;; splice `begin'
                    (datum->syntax
                     stx
                     (cons (quote-syntax printing-module-begin)
                           (append (let-values ([(l) (syntax->list e)])
                                     (map (lambda (elem)
                                            (syntax-track-origin elem e (car l)))
                                          (cdr l)))
                                   (cdr r)))
                     stx)
                    ;; no need to splice
                    (let-values ([(wrap?)
                                  (let-values ([(e) (syntax-e e)])
                                    (if (pair? e)
                                        (let-values ([(a) (car e)])
                                          (if (symbol? (syntax-e a))
                                              (if (ormap (lambda (i)
                                                           (free-identifier=? i a))
                                                         (syntax->list
                                                          (quote-syntax 
                                                           (define-values define-syntaxes define-values-for-syntax
                                                             module 
                                                             #%module-begin 
                                                             #%require #%provide))))
                                                  #f
                                                  #t)
                                              #t))
                                        #t))])
                      (let-values ([(e) (if wrap?
                                            (datum->syntax
                                             e
                                             (list (quote-syntax #%app)
                                                   (quote-syntax call-with-values)
                                                   (list (quote-syntax lambda)
                                                         '()
                                                         e)
                                                   (quote-syntax print-values))
                                             e)
                                            e)])
                        (datum->syntax
                         stx
                         (list (quote-syntax begin)
                               e
                               (cons (quote-syntax printing-module-begin)
                                     (cdr r)))
                         stx)))))))))))
