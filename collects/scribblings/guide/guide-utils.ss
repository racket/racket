(module guide-utils mzscheme
  (require (lib "manual.ss" "scribble"))
  
  (provide Quick MzScheme HtDP
           tool)

  (define Quick
    (link "../quick/index.html" "An Introduction to PLT Scheme with Pictures"))

  (define MzScheme
    (link "../reference/index.html" "PLT Scheme Reference Manual"))

  (define HtDP
    (italic (link "http://www.htdp.org" "How to Design Programs")))

  (define (tool name . desc)
    (apply item (bold name) ", " desc)))



