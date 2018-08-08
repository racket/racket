(compile-compressed #f)

(define-values (src dest)
  (let loop ([args (command-line-arguments)])
    (cond
      [(and (pair? args)
            (equal? (car args) "--compress"))
       (compile-compressed #t)
       (loop (cdr args))]
      [(null? args)
       (error 'convert-to-boot "missing file arguments")]
      [(and (pair? (cdr args)) (null? (cddr args)))
       (values (car args) (cadr args))]
      [else
       (error 'convert-to-boot "extra arguments after files")])))

(make-boot-file dest '("petite" "scheme") src)
