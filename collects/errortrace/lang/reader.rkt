(module reader racket/base
  (require syntax/module-reader)

  (provide (rename-out [et-read read]
                       [et-read-syntax read-syntax]
                       [et-get-info get-info]))

  (define (wrap-reader p)
    (lambda args
      (let ([r (apply p args)])
        ;; Re-write module to use `errortrace':
        (if (syntax? r)
            (syntax-case r ()
              [(mod name lang . body)
               (quasisyntax/loc r
                 (mod name errortrace/lang/body (#,(datum->syntax #f '#%module-begin) lang . body)))])
            `(,(car r) ,(cadr r) errortrace/lang/body (#%module-begin . ,(cddr r)))))))

  (define-values (et-read et-read-syntax et-get-info)
    (make-meta-reader
     'errortrace
     "language path"
     (lambda (bstr)
       (let* ([str (bytes->string/latin-1 bstr)]
              [sym (string->symbol str)])
         (and (module-path? sym)
              (vector
               ;; try submod first:
               `(submod ,sym reader)
               ;; fall back to /lang/reader:
               (string->symbol (string-append str "/lang/reader"))))))
     wrap-reader
     wrap-reader
     values)))
