(module mred-doc mzscheme

  (define mr-eval? (getenv "MREVAL"))

  (define-syntax bounce
    (syntax-rules ()
      [(_ id)
       (begin
         (provide id)
         (define id (if mr-eval?
                        (dynamic-require '(lib "mred.ss" "mred") 'id)
                        #f)))]
      [(_ id ...)
       (begin (bounce id) ...)]))

  (bounce frame% canvas%
          bitmap% bitmap-dc%
          color%))
