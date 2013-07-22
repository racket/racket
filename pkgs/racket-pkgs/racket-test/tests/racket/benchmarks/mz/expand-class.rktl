
(let ([path (collection-file-path "class-internal.rkt" "racket/private")])
  (define-values (dir name dir?) (split-path path))
  (with-input-from-file path
    (lambda ()
      (parameterize ([current-load-relative-directory dir]
		     [read-accept-reader #t])
        (let ([s (read-syntax)])
          (time (compile s)))))))
