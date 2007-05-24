
(module core mzscheme
  (require "to-html.ss"
           (prefix ref: "reference/reference.scrbl")
           (prefix guide: "guide/guide.scrbl"))

  (provide build)
  
  (define (build)
    (to-html #t #t 
             (list ref:doc 
                   guide:doc)
             (list "reference"
                   "guide"))))


