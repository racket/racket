
(module old-rp '#%kernel
  (#%require (for-syntax '#%kernel "stx.rkt" "small-scheme.rkt" "stxcase-scheme.rkt"))

  (#%provide require require-for-syntax require-for-template require-for-label
             provide provide-for-syntax provide-for-label)

  (begin-for-syntax
   (define-values (rebuild-elem)
     (lambda (stx elem sub pos loop ids)
       ;; For sub-forms, we loop and reconstruct:
       (for-each (lambda (id)
                   (unless (identifier? id)
                     (raise-syntax-error
                      #f
                      "expected an identifier"
                      stx
                      id)))
                 (syntax->list ids))
       (let rloop ([elem elem][pos pos])
         (if (syntax? elem)
             (datum->syntax elem
                            (rloop (syntax-e elem) pos)
                            elem
                            elem)
             (if (zero? pos)
                 (cons (loop (car elem))
                       (cdr elem))
                 (cons (car elem)
                       (rloop (cdr elem) (sub1 pos)))))))))


  (define-syntaxes (require require-for-syntax require-for-template require-for-label)
    (let ([mk
           (lambda (for-stx)
             (lambda (stx)
               (syntax-case stx ()
                 [(_ elem ...)
                  (if for-stx
                      (with-syntax ([for for-stx])
                        (syntax/loc stx
                          (#%require (for-meta for (just-meta 0 elem ...)))))
                      (syntax/loc stx
                        (#%require elem ...)))])))])
      (values (mk #f)
              (mk #'1)
              (mk #'-1)
              (mk #'#f))))

  (define-syntaxes (provide provide-for-syntax provide-for-label)
    (let ([mk
           (lambda (for-stx)
             (lambda (stx)
               (syntax-case stx ()
                 [(_ elem ...)
                  (if for-stx
                      (with-syntax ([for for-stx])
                        (syntax/loc stx
                          (#%provide (for elem ...))))
                      (syntax/loc stx
                        (#%provide elem ...)))])))])
      (values (mk #f)
              (mk #'for-syntax)
              (mk #'for-label)))))
