
(define-syntax (include-generated stx)
  (syntax-case stx ()
    [(inc file)
     (let* ([dir (or (getenv "COMPILED_SCM_DIR")
                     "compiled/")]
            [file (#%datum->syntax #'inc (string-append dir (#%syntax->datum #'file)))])
       (#%datum->syntax #'inc `(include ,file)))]))
