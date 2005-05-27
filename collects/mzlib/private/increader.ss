
(module increader mzscheme
  (define-struct reader (val))
  (provide reader? make-reader reader-val))

