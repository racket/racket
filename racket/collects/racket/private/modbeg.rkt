;; A #%module-begin that wraps each module-level expression with 
;;  `print-value'.

(module modbeg '#%kernel
  (#%require syntax/wrap-modbeg
             (for-syntax '#%kernel))

  (#%provide module-begin)

  (define-values (print-values)
    (lambda vs 
      (for-each (current-print) vs)
      (apply values vs)))

  (define-syntaxes (module-begin)
    (make-wrapping-module-begin (quote-syntax print-result)))

  (define-syntaxes (print-result)
    (lambda (stx)
      (let-values ([(e) (cadr (syntax->list stx))])
        (datum->syntax
         (quote-syntax here)
         (list (quote-syntax #%app)
               (quote-syntax call-with-values)
               (list (quote-syntax lambda)
                     '()
                     e)
               (quote-syntax print-values))
         e)))))
