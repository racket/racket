(module reader scheme/base
  (require syntax/module-reader)

  (provide (rename-out [planet-read read]
                       [planet-read-syntax read-syntax]
                       [planet-get-info get-info]))

  (define-values (planet-read planet-read-syntax planet-get-info)
    (make-meta-reader
     'planet
     "planet path"
     (lambda (str)
       (let ([str (bytes->string/latin-1 str)])
         (if (module-path? `(planet ,(string->symbol str)))
           `(planet ,(string->symbol (string-append str "/lang/reader")))
           #f)))
     values
     values
     values)))
