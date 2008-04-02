(module icons scheme/base
  (require scribble/manual
           scribble/struct)

  (provide magnify
           finger)

  (define (mk name)
    (make-element "imageleft"
                  (list
                   (make-element (make-image-file (build-path (collection-path "scribblings")
                                                              name)
                                                  1.0)
                                 (list "+")))))
  (define magnify (mk "magnify.png"))
  (define finger (mk "finger.png")))
