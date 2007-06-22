(module guide-utils (lib "new-lambda.ss" "scribblings")
  (require (lib "manual.ss" "scribble")
           (lib "struct.ss" "scribble")
           (lib "decode.ss" "scribble")
           (lib "kw.ss")
           (lib "eval.ss" "scribble"))

  (provide Quick MzScheme HtDP
           tool
           refdetails
           refdetails/gory
           refsecref)

  (define Quick
    (italic (link "../quick/index.html" "An Introduction to PLT Scheme with Pictures")))

  (define MzScheme
    (italic (link "../reference/index.html" "PLT Scheme Reference Manual")))

  (define HtDP
    (italic (link "http://www.htdp.org" "How to Design Programs")))

  (define (tool name . desc)
    (apply item (bold name) ", " desc))

  (define (refdetails* tag what . s)
    (apply margin-note
           (decode-content (append (list "For " what " on ")
                                   s
                                   (list ", see "
                                         (refsecref tag)
                                         ".")))))

  (define (refdetails tag . s)
    (apply refdetails* tag "more" s))

  (define (refdetails/gory tag . s)
    (apply refdetails* tag "gory details" s))

  (define (refsecref s)
    (make-element #f (list (secref s) " in " MzScheme))))
