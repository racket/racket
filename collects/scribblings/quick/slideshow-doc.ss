(module slideshow-doc mzscheme

  (define mr-eval? (getenv "MREVAL"))

  (when mr-eval?
    (parameterize ([current-command-line-arguments #()])
      (dynamic-require '(lib "slideshow.ss" "slideshow") #f)))

  (define-syntax bounce
    (syntax-rules ()
      [(_ id)
       (begin
         (provide id)
         (define id (if mr-eval?
                        (dynamic-require '(lib "slideshow.ss" "slideshow") 'id)
                        #f)))]
      [(_ id ...)
       (begin (bounce id) ...)]))

  (bounce circle
          rectangle
          hc-append
          filled-rectangle
          vc-append
          colorize
          scale
          bitmap
          make-pict-drawer

          pict? pict-width pict-height))
