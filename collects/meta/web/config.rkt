#lang racket/base

(provide sites)
(define sites
  '(("www"        "http://racket-lang.org/")
    ("download"   "http://download.racket-lang.org/")
    ("stubs/pre"  "http://pre.racket-lang.org/")
    ("stubs/git"  "http://git.racket-lang.org/")
    ("stubs/blog" "http://blog.racket-lang.org/")))

(provide distributions)
(define distributions
  ;; Each is a "hostname:dest-path", and then a list of directories to
  ;; put in that path.  (Warning: "dest" should not be a top-level
  ;; directory that already exists.)
  '(("champlain:/www/new" "www" "download")))
