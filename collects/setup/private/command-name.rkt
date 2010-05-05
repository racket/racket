;; The `setup' tool loads this module with bytecode files disabled,
;; so minimize its dependencies

(module command-name '#%kernel
  (#%require raco/command-name)
  (#%provide get-names)

  (define-values (get-names)
    (lambda ()
      (let-values ([(p) (find-system-path 'run-file)])
        (let-values ([(base name dir?) (split-path p)])
          (if (current-command-name)
              (values (format "~a ~a" name (current-command-name))
                      (program+command-name))
              ;; Hack for bootstrapping, if the program name is "raco",
              ;; then claim to be the "setup" command:
              (if (equal? (path->string name) "raco")
                  (values (format "~a setup" name)
                          (format "~a setup" p))
                  (values (path->string name) p))))))))
