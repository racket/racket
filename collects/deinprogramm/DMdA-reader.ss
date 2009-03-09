#lang scheme/base

  (require mzlib/etc)
  (provide make-read-syntax
           make-read)
  
  (define (make-read spec)
    (let ([read
           (opt-lambda ([port (current-input-port)])
             (syntax->datum ((make-read-syntax spec) 'whatever port)))])
      read))
  
  (define (get-all-exps source-name port)
    (let loop ()
      (let ([exp (read-syntax source-name port)])
        (cond
          [(eof-object? exp) null]
          [else (cons exp (loop))]))))
  
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
                     (if (path-string? path)
                         (let-values ([(base name dir) (split-path path)])
                           (string->symbol (path->string (path-replace-suffix name #""))))
                         (lookup 'modname table))])
               (datum->syntax
                #f
                `(module ,modname ,spec
                   ,@(map (lambda (x) `(require ,x))
                          (lookup 'teachpacks table))
                   ,@(parameterize ([read-case-sensitive (lookup 'read-case-sensitive table)])
                       (get-all-exps source-name port))))))])
      read-syntax))
