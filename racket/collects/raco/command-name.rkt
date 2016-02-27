;; The `setup' tool loads this module with bytecode files disabled,
;; so minimize its dependencies

(module command-name '#%kernel
  (#%provide current-command-name
             program+command-name
             short-program+command-name)

  (define-values (current-command-name) (make-parameter #f))

  (define-values (program+command-name)
    (lambda ()
      (define-values (p) (find-system-path 'run-file))
      (define-values (n) (current-command-name))
      (if n (format "~a ~a" p n) p)))

  (define-values (short-program+command-name)
    (lambda ()
      (define-values (p) (find-system-path 'run-file))
      (define-values (n) (current-command-name))
      (define-values (base name dir?) (split-path p))
      (define-values (converted-name) (convert-name name))
      (if n
          (format "~a ~a" converted-name n)
          (path->string converted-name))))

  (define-values (convert-name)
    (lambda (name)
      (if (eq? (system-type) 'windows)
          (string->path-element
           (regexp-replace #rx"(?i:[.]exe)$"
                           (path-element->string name)
                           ""))
          name))))
