
(module quick mzscheme
  (require "to-html.ss"
           (prefix quick: "quick/quick.scrbl"))

  (provide build)
  
  (define (build)
    (to-html #f #f
             (list quick:doc)
             (list "quick"))))
