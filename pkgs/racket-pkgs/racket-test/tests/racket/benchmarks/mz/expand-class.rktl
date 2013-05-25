
(let ([dir (build-path (collection-path "racket")
                       "private")])
  (with-input-from-file (build-path dir "class-internal.rkt")
    (lambda ()
      (parameterize ([current-load-relative-directory dir]
		     [read-accept-reader #t])
        (let ([s (read-syntax)])
          (time (compile s)))))))
