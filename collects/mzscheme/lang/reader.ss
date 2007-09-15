(module reader mzscheme
  (provide (rename *read read)
           (rename *read-syntax read-syntax))

  (define (*read in)
    (wrap in read))

  (define (*read-syntax src in)
    (wrap in (lambda (in)
               (read-syntax src in))))

  (define (wrap port read)
    (let ([body
           (let loop ([a null])
             (let ([v (read port)])
               (if (eof-object? v)
                   (reverse a)
                   (loop (cons v a)))))])
      (let* ([p-name (object-name port)]
             [name (if (path? p-name)
                       (let-values ([(base name dir?) (split-path p-name)])
                         (string->symbol (path->string (path-replace-suffix name #""))))
                       'page)]
             [id 'doc])
        `(module ,name mzscheme
           . ,body)))))
