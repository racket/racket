#lang at-exp s-exp "shared.rkt"

(provide blog)
(define blog
  (page #:file "blog/"
        ;; #:part-of community  <-- TODO: is doing this a good idea
    "This is a stub page to get the header for the blog."))
