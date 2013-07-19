(module runtime-config '#%kernel

  (#%provide configure)

  (define-values (configure)
    (lambda (config)
      (print-as-expression #f))))
