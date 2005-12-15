(module filename-version mzscheme

  ;; this module provides the string that should replace xxxxxxx's in file names

  (provide filename-version-part)
  (define filename-version-part
    (cond [(regexp-match #rx"^([0-9]+(?:p[0-9])?)(?:[.]([0-9]+))?$"
                         (version))
           => (lambda (m)
                (let ([major (cadr m)] [minor (or (caddr m) "")])
                  (string-append major "_"
                                 (make-string (- (string-length "xxxxxxx")
                                                 1
                                                 (string-length major)
                                                 (string-length minor))
                                              #\0)
                                 minor)))]
          [else (error 'filename-version-part
                       "unexpected version string: ~s"
                       (version))])))
