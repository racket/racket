
(module docreader mzscheme
  (require (prefix scribble: "reader.ss")
           (lib "kw.ss"))

  (provide (rename *read read)
	   (rename *read-syntax read-syntax))

  (define (call-with-scribble-params t)
    (parameterize ([scribble:read-accept-=-keyword #f]
                   [scribble:read-insert-indents #f])
      (t)))

  (define/kw (*read #:optional [inp (current-input-port)])
    (call-with-scribble-params
     (lambda ()
       (wrap inp (scribble:read-inside inp)))))
     
  (define/kw (*read-syntax #:optional src [port (current-input-port)])
    (call-with-scribble-params
     (lambda ()
       (wrap port (scribble:read-inside-syntax src port)))))

  (define (wrap port body)
    (let* ([p-name (object-name port)]
	   [name (if (path? p-name)
		     (let-values ([(base name dir?) (split-path p-name)])
		       (string->symbol (path->string (path-replace-suffix name #""))))
		     'page)]
           [id 'doc])
      `(module ,name (lib "doclang.ss" "scribble")
         (#%module-begin
          ,id ()
          . ,body)))))
