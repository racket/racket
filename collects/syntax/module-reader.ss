(module module-reader scheme/base

;; (require scheme/class) ; avoid requiring too much to the reader level

(provide (rename-out [provide-module-reader #%module-begin]
                     [wrap wrap-read-all]))

(define-syntax provide-module-reader
  (syntax-rules ()
    [(_ lib)
     (#%module-begin
      (#%provide (rename *read read)
                 (rename *read-syntax read-syntax))

      (define (*read in modpath line col pos)
        (wrap 'lib in read modpath #f line col pos))

      (define (*read-syntax src in modpath line col pos)
        (wrap 'lib in (lambda (in) (read-syntax src in))
              modpath src line col pos)))]))

(define (wrap lib port read modpath src line col pos)
  (let* ([body (let loop ([a null])
                 (let ([v (read port)])
                   (if (eof-object? v)
                     (reverse a)
                     (loop (cons v a)))))]
         [p-name (object-name port)]
         #;
         [p-name (let ([i (and (object? p-name) (object-interface p-name))])
                   (cond [(and i (method-in-interface? 'get-port-name i))
                          (send p-name get-port-name)]
                         [(and i (method-in-interface? 'get-filename i))
                          (send p-name get-filename)]
                         [else p-name]))]
         [name (if (path? p-name)
                 (let-values ([(base name dir?) (split-path p-name)])
                   (string->symbol
                    (path->string (path-replace-suffix name #""))))
                 'page)]
         [tag-src (lambda (v)
                    (if (syntax? modpath)
                      (datum->syntax #f v
                                     (vector src line col pos
                                             (- (or (syntax-position modpath)
                                                    (add1 pos))
                                                pos)))
                      v))]
         [lib-src (lambda (v)
                    (if (syntax? modpath)
                      (datum->syntax #f lib modpath modpath)
                      v))])
    `(,(tag-src 'module) ,(tag-src name) ,(lib-src lib)
      . ,body)))

)
