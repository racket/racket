
(let ([dir (build-path (collection-path "scheme")
                       "private")])
  (with-input-from-file (build-path dir "class-internal.ss")
    (lambda ()
      (parameterize ([current-load-relative-directory dir]
		     [read-accept-reader #t])
        (let ([s (read-syntax)])
          (time (compile s)))))))
