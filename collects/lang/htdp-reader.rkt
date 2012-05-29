(module htdp-reader mzscheme
  (require mzlib/etc)
  (provide make-read-syntax
           make-read)
  
  (define (make-read spec)
    (let ([read
           (opt-lambda ([port (current-input-port)])
             (syntax-object->datum ((make-read-syntax spec) 'whatever port)))])
      read))
  
  (define (get-all-exps source-name port)
    (parameterize ([read-accept-lang #f])
      (let loop ()
        (let ([exp (read-syntax source-name port)])
          (cond
           [(eof-object? exp) null]
           [else (cons exp (loop))])))))
  
  (define (lookup key table)
    (let ([ans (assoc key table)])
      (unless ans
        (error 'special-reader "couldn't find ~s in table ~s"
               key table))
      (cadr ans)))
  
  (define (make-read-syntax spec)
    (let ([read-syntax
           (opt-lambda ([source-name #f]
                        [port (current-input-port)])
             (let* ([table (read port)]
                    [path (object-name port)]
                    [modname 
                     (if (or (path? path)
                             (and (string? path)
                                  (path-string? path)))
                         (let-values ([(base name dir) (split-path path)])
                           (string->symbol (path->string (path-replace-suffix name #""))))
                         (lookup 'modname table))])
               (datum->syntax-object
                #f
                `(module ,modname ,spec
                   (#%module-begin
                    ,@(map (λ (x) `(require ,x))
                           (lookup 'teachpacks table))
                    ,@(parameterize ([read-case-sensitive (lookup 'read-case-sensitive table)]
                                     [read-decimal-as-inexact #f]
                                     [read-accept-dot #f])
                        (get-all-exps source-name port)))))))])
      read-syntax)))
