(module runtime-config '#%kernel
  (#%declare #:require=define)

  (#%provide configure)

  (define-values (configure)
    (lambda (config)
      (print-as-expression #t))))
