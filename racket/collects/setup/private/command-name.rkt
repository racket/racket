;; The `setup' tool loads this module with bytecode files disabled,
;; so minimize its dependencies

(module command-name '#%kernel
  (#%require raco/command-name '#%utils)
  (#%provide get-names)

  (define-values (get-names)
    (lambda ()
      (let-values ([(p) (find-system-path 'run-file)])
        (let-values ([(p) (if (eq? (system-type) 'windows)
                              (path-replace-extension p #"")
                              p)])
          (let-values ([(base name dir?) (split-path p)])
            (if (current-command-name)
                (values (format "~a ~a" name (current-command-name))
                        (program+command-name)
                        #t)
                ;; Hack for bootstrapping, if the program name is "raco",
                ;; then claim to be the "setup" command:
                ;; if the program name is "racket", assume that there's a "racket -l setup"
                ;; going on in there and also claim to be the "raco setup" command
                (if (if (regexp-match? #rx"^raco(?i:|3m|cgc|cs)$" (path->string name))
                        #t
                        (regexp-match? #rx"^racket(?i:|3m|cgc|cs)$" (path->string name)))
                    (values "raco setup"
                            (string-append (regexp-replace*
                                            #rx"racket$"
                                            (format "~a" p)
                                            "raco")
                                           " setup")
                            #t)
                    (values (path->string name) p #f)))))))))

