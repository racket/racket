(module guide-utils (lib "lang.ss" "big")
  (require (lib "manual.ss" "scribble")
           (lib "struct.ss" "scribble")
           (lib "decode.ss" "scribble")
           (lib "kw.ss")
           (lib "eval.ss" "scribble")
           "../icons.ss")

  (require-for-label (lib "lang.ss" "big"))
  (provide-for-label (all-from (lib "lang.ss" "big")))

  (provide Quick MzScheme HtDP
           tool
           moreguide
           guideother
           refalso
           refdetails
           refdetails/gory
           refsecref
           ext-refsecref)

  (define Quick
    (italic (secref #:doc '(lib "quick.scrbl" "scribblings" "quick") "top")))

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
    (secref #:doc '(lib "reference.scrbl" "scribblings" "reference") s))

  (define (ext-refsecref s)
    (make-element #f (list (refsecref s) " in " MzScheme)))

  (define MzScheme
    (italic (refsecref "top"))))
