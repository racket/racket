
(module gui mzscheme
  (require "to-html.ss"
           (prefix gui: "gui/gui.scrbl"))

  (provide build)
  
  (define (build)
    (to-html #t #f
             (list gui:doc)
             (list "gui"))))
