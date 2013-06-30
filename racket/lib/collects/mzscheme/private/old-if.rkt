(module old-if '#%kernel
  (#%require (for-syntax '#%kernel))
  (#%provide if*)

  (define-syntaxes (if*)
    (lambda (stx)
      (let-values ([(if-stx) (quote-syntax if)])
        (if (pair? (syntax-e stx))
            (let-values ([(l) (syntax->list stx)])
              (if (if l
                      (if (= (length l) 3)
                          #t
                          #f)
                      #f)
                  (datum->syntax
                   stx
                   (list if-stx
                         (cadr l)
                         (caddr l)
                         (quote-syntax (void)))
                   stx
                   stx)
                  (datum->syntax
                   stx
                   (cons if-stx
                         (cdr (syntax-e stx)))
                   stx
                   stx)))
            (datum->syntax
             if-stx
             'if
             stx))))))
