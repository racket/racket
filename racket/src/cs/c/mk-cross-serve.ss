(let ([args (command-line-arguments)])
  (load (caddr args))
  (let ([dest (string-append (current-directory) "/" "cross-serve.so")])
    (parameterize ([current-directory (car args)]
                   ;; setting unsafe mode now comfigures a certain
                   ;; kinds of unsafety when compiling Racket code:
                   [optimize-level 3])
      (compile-file (cadr args) dest))))
