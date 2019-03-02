(compile-compressed #f)

(define-values (src dest machine)
  (let loop ([args (command-line-arguments)])
    (cond
      [(and (pair? args)
            (equal? (car args) "--compress"))
       (compile-compressed #t)
       (loop (cdr args))]
      [(and (pair? args)
            (equal? (car args) "--xpatch")
            (pair? (cdr args)))
       (load (cadr args))
       (loop (cddr args))]
      [(null? args)
       (error 'convert-to-boot "missing file arguments")]
      [(null? (cdr args))
       (error 'convert-to-boot "missing destination-file argument")]
      [(null? (cddr args))
       (error 'convert-to-boot "missing machine argument")]
      [(pair? (cdddr args))
       (error 'convert-to-boot "extra arguments after files")]
      [else
       (values (car args) (cadr args) (caddr args))])))

(#%$make-boot-file dest (string->symbol machine) '("petite" "scheme") src)
