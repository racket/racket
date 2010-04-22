(module runtime-config '#%kernel

  (#%provide configure)

  (define-values (configure)
    (lambda (config)
      (current-prompt-read  (lambda ()
                              (printf "> ")
                              (read)))
      (print-as-quasiquote #t))))
