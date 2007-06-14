(module guide-utils mzscheme
  (require (lib "manual.ss" "scribble")
           (lib "struct.ss" "scribble")
           (lib "decode.ss" "scribble")
           (lib "kw.ss")
           (lib "eval.ss" "scribble"))

  (interaction-eval (require (lib "new-lambda.ss" "scribblings")))

  
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

  (define/kw (refdetails* tag what #:body s)
    (apply margin-note
           (decode-content (append (list "For " what " on ")
                                   s
                                   (list ", see "
                                         (refsecref tag)
                                         ".")))))

  (define/kw (refdetails tag #:body s)
    (apply refdetails* tag "more" s))

  (define/kw (refdetails/gory tag #:body s)
    (apply refdetails* tag "gory details" s))

  (define (refsecref s)
    (make-element #f (list (secref s) " in " MzScheme))))
