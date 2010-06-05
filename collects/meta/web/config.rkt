#lang racket/base

(provide (struct-out site) sites)
(struct site (dir url))
(define sites (list (site "www"        "http://racket-lang.org/")
                    (site "download"   "http://download.racket-lang.org/")
                    (site "stubs/git"  "http://git.racket-lang.org/")
                    (site "stubs/blog" "http://blog.racket-lang.org/")))
