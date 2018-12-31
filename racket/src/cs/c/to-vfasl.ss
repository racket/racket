(compile-compressed #f)

(define-values (src dest deps)
  (let loop ([args (command-line-arguments)])
    (cond
      [(and (pair? args)
            (equal? (car args) "--compress"))
       (compile-compressed #t)
       (loop (cdr args))]
      [(null? args)
       (error 'to-vfasl "missing src argument")]
      [(null? (cdr args))
       (error 'to-vfasl "missing dest argument")]
      [else
       (values (car args) (cadr args) (cddr args))])))

(vfasl-convert-file src dest deps)
