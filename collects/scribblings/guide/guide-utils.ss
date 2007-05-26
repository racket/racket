(module guide-utils mzscheme
  (require (lib "manual.ss" "scribble")
           (lib "struct.ss" "scribble")
           (lib "decode.ss" "scribble")
           (lib "kw.ss"))
  
  (provide Quick MzScheme HtDP
           tool
           refdetails
           refsecref)

  (define Quick
    (italic (link "../quick/index.html" "An Introduction to PLT Scheme with Pictures")))

  (define MzScheme
    (italic (link "../reference/index.html" "PLT Scheme Reference Manual")))

  (define HtDP
    (italic (link "http://www.htdp.org" "How to Design Programs")))

  (define (tool name . desc)
    (apply item (bold name) ", " desc))

  (define/kw (refdetails tag #:body s)
    (let ([c (decode-content (append (list "For more on ")
                                     s
                                     (list ", see "
                                           (refsecref tag)
                                           ".")))])
      (make-styled-paragraph (list (make-element "refcontent"
                                                 c))
                             "refpara")))

  (define (refsecref s)
    (make-element #f (list (secref s) " in " MzScheme))))




