(module icons (lib "lang.ss" "big")
  (require (lib "manual.ss" "scribble")
           (lib "struct.ss" "scribble"))

  (provide magnify
           finger)

  (define (mk name)
    (make-element "imageleft"
                  (list
                   (make-element (make-image-file (build-path (collection-path "scribblings")
                                                              name))
                                 (list "+")))))
  (define magnify (mk "magnify.png"))
  (define finger (mk "finger.png")))
