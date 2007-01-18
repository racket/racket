
(module image mzscheme
  (define-struct image (filename data w h dx dy))
  (provide (struct image (filename data w h dx dy))))
