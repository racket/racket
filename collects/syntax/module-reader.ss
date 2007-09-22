(module module-reader mzscheme
  (provide provide-module-reader)

  (define-syntax provide-module-reader
    (syntax-rules ()
      [(_ lib)
       (begin
         (provide (rename *read read)
                  (rename *read-syntax read-syntax))
         
         (define (*read in)
           (wrap 'lib in read))
         
         (define (*read-syntax src in)
           (wrap 'lib in (lambda (in)
                           (read-syntax src in)))))]))

  (define (wrap lib port read)
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
        `(module ,name ,lib
           . ,body)))))

