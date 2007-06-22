(module slideshow-code-doc mzscheme
  (require (only "slideshow-doc.ss"))

  (define mr-eval? (getenv "MREVAL"))

  (define-syntax bounce
    (syntax-rules ()
      [(_ id)
       (begin
         (provide id)
         (define id (if mr-eval?
                        (dynamic-require '(lib "code.ss" "slideshow") 'id)
                        #f)))]
      [(_ id ...)
       (begin (bounce id) ...)]))

  (bounce typeset-code))

