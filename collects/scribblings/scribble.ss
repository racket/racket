
(module scribble mzscheme
  (require "to-html.ss"
           (prefix scribble: "scribble/scribble.scrbl"))

  (provide build)
  
  (define (build)
    (to-html #t #f
             (list scribble:doc )
             (list "scribble"))))
