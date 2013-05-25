;; The `setup' tool loads this module with bytecode files disabled,
;; so minimize its dependencies

(module command-name '#%kernel
  (#%provide current-command-name
             program+command-name
             short-program+command-name)

  (define-values (current-command-name) (make-parameter #f))

  (define-values (program+command-name)
    (lambda ()
      (let-values ([(p) (find-system-path 'run-file)]
                   [(n) (current-command-name)])
        (if n
            (format "~a ~a" p n)
            p))))
  
  (define-values (short-program+command-name)
    (lambda ()
      (let-values ([(p) (find-system-path 'run-file)]
                   [(n) (current-command-name)])
        (let-values ([(base name dir?) (split-path p)])
          (if n
              (format "~a ~a" name n)
              (path->string name)))))))

