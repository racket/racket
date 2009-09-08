(module reader syntax/module-reader #:language (lambda (p) (read-syntax (object-name p) p)))
