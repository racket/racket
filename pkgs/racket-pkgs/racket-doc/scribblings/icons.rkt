(module icons scheme/base
  (require scribble/manual
           scribble/struct
           setup/main-collects
           (only-in scribble/core make-style)
           scribble/html-properties
           scribble/latex-properties)

  (provide magnify
           finger)

  (define (mk name)
    (make-element (make-style "imageleft"
                              (list (make-css-addition
                                     (path->main-collects-relative
                                      (collection-file-path "icons.css" "scribblings")))
                                    (make-tex-addition
                                     (path->main-collects-relative
                                      (collection-file-path "icons.tex" "scribblings")))))
                  (list
                   (make-element (make-image-file
                                  (path->main-collects-relative
                                   (collection-file-path name "scribblings"))
                                  1.0)
                                 (list "+")))))
  (define magnify (mk "magnify.png"))
  (define finger (mk "finger.png")))
