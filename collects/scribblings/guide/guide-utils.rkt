(module guide-utils racket/base
  (require scribble/manual
           scribble/struct
           scribble/decode
           scribble/eval
           "../icons.ss")

  (require (for-label racket/base))
  (provide (for-label (all-from-out racket/base)))

  (provide Quick Racket HtDP
           tool
           moreguide
           guideother
           refalso
           refdetails
           refdetails/gory
           refsecref
           ext-refsecref)

  (define Quick
    (other-manual '(lib "scribblings/quick/quick.scrbl")))

  (define HtDP
    (italic (link "http://www.htdp.org" "How to Design Programs")))

  (define (tool name . desc)
    (apply item (bold name) ", " desc))

  (define (moreguide tag . s)
    (apply margin-note
           (decode-content (append
                            (list
                             finger (secref tag) " (later in this guide)"
                             " explains more about ")
                            s
                            (list ".")))))

  (define (guideother . s)
    (apply margin-note
           (cons finger (decode-content s))))

  (define (refdetails* tag what . s)
    (apply margin-note
           (decode-content (append (list magnify (ext-refsecref tag))
                                   (list what)
                                   s
                                   (list ".")))))

  (define (refdetails tag . s)
    (apply refdetails* tag " provides more on " s))

  (define (refalso tag . s)
    (apply refdetails* tag " also documents " s))

  (define (refdetails/gory tag . s)
    (apply refdetails* tag " documents the fine points of " s))

  (define (refsecref s)
    (secref #:doc '(lib "scribblings/reference/reference.scrbl") s))

  (define (ext-refsecref s)
    (make-element #f (list (refsecref s) " in " Racket)))

  (define Racket (other-manual '(lib "scribblings/reference/reference.scrbl"))))
