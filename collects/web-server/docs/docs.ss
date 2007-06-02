(module docs mzscheme
  (require (lib "to-html.ss" "scribblings")
           (prefix ref: "reference/reference.scrbl")
           (prefix guide: "guide/guide.scrbl"))
  (provide build)
  
  (define (build)
    (to-html #t #t 
             (list ref:doc 
                   guide:doc)
             (list "web-server-reference"
                   "web-server-guide")))
  
  (build))