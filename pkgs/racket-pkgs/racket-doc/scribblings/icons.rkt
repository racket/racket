(module icons scheme/base
  (require scribble/manual
           scribble/struct)

  (provide magnify
           finger)

  (define (mk name)
    (make-element "imageleft"
                  (list
                   (make-element (make-image-file (collection-file-path name "scribblings")
                                                  1.0)
                                 (list "+")))))
  (define magnify (mk "magnify.png"))
  (define finger (mk "finger.png")))
