
(let ([dir (build-path (collection-path "mzlib")
                       "private")])
  (with-input-from-file (build-path dir "class-internal.ss")
    (lambda ()
      (parameterize ([current-load-relative-directory dir])
        (let ([s (read-syntax)])
          (time (compile s)))))))


                     