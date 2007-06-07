(module doc-installer mzscheme
  (require (lib "dirs.ss" "setup")
           (lib "to-html.ss" "scribblings")
           (prefix ref: "reference/reference.scrbl")
           (prefix guide: "guide/guide.scrbl"))
  
  (provide post-installer)
  
  (define post-installer
    (lambda (path)
      (let ([doc (find-doc-dir)])
        (when doc
          (build)))))
  
  (define (build)
    (to-html #t #t 
             (list ref:doc 
                   guide:doc)
             (list "web-server-reference"
                   "web-server-guide")))
  
  (build))