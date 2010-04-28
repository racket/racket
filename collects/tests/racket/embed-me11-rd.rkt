(module embed-me11-rd mzscheme
  (provide (rename *read-syntax read-syntax)
           (rename *read read))

  (define (*read port)
    `(module embed-me11 mzscheme
       (with-output-to-file "stdout"
         (lambda () 
           (printf ,(read port) 
                   ;; Use `getenv' at read time!!!
                   ,(getenv "ELEVEN")))
         'append)))

  (define (*read-syntax src port)
    (*read port)))
