
(module defreader mzscheme
  (require (prefix scribble: "reader.ss")
           (lib "kw.ss"))

  (provide (rename *read read)
           (rename *read-syntax read-syntax))

  (define/kw (*read #:optional [inp (current-input-port)])
    (wrap inp (scribble:read-inside inp)))

  (define/kw (*read-syntax #:optional src [port (current-input-port)])
    (wrap port (scribble:read-inside-syntax src port)))

  (define (wrap port body)
    (let* ([p-name (object-name port)]
	   [name (if (path? p-name)
		     (let-values ([(base name dir?) (split-path p-name)])
		       (string->symbol (path->string (path-replace-suffix name #""))))
		     'page)]
           [id 'doc])
      `(module ,name (lib "lang.ss" "big")
         . ,body))))

