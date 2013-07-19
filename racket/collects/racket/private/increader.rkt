
(module increader racket/base
  (define-struct reader (val))
  (provide reader? make-reader reader-val))

