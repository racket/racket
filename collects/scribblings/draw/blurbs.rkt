#readerscribble/reader
(module blurbs scheme/base
  (require scribble/struct
           scribble/manual
           scribble/scheme
           scribble/decode
           (for-label racket/draw
                      scheme/base)
           (for-syntax scheme/base))

  (provide (all-defined-out))

  (define (p . l)
    (decode-paragraph l))

  (define PrintNote
    (make-splice
     (list
      @p{Be sure to use the following methods to start/end drawing:}
      @itemize[@item{@method[dc<%> start-doc]}
               @item{@method[dc<%> start-page]}
               @item{@method[dc<%> end-page]}
               @item{@method[dc<%> end-doc]}]
      @p{Attempts to use a drawing method outside of an active page raises an exception.})))

  (define reference-doc '(lib "scribblings/reference/reference.scrbl"))

  (define SeeMzParam @elem{(see @secref[#:doc reference-doc "parameters"])})
  
  (define DrawSizeNote "")

  (define MismatchExn @elem{an @racket[exn:fail:contract] exception is raised})

  (define (colorName name name2 r g b)
    (make-element #f
                  (list (make-element `(bg-color ,r ,g ,b)
                                      (list (hspace 5)))
                        (hspace 1)
                        (bytes->string/latin-1 name))))

  (define (slant . s)
    (make-element "slant" (decode-content s)))

  (define (res-sym s)
    (string->symbol (string-append "GRacket:" s)))

  (define (boxisfill which what)
    @elem{The @|which| box is filled with @|what|.})
  (define (boxisfillnull which what)
    @elem{The @|which| box is filled with @|what|, unless @|which| is @racket[#f].})

  )

