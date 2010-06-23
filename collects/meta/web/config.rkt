#lang racket/base

(provide sites)
(define sites
  '(("www"           "http://racket-lang.org/")
    ("download"      "http://download.racket-lang.org/")
    ("lists"         "http://lists.racket-lang.org/")
    ("bugs"          "http://bugs.racket-lang.org/")
    ;; stubs usually use absolute paths for resources, since they're
    ;; templates that often get used in sub-dir pages too
    ("stubs/planet"  "http://planet.racket-lang.org/"   abs)
    ("stubs/pre"     "http://pre.racket-lang.org/"      abs)
    ("stubs/git"     "http://git.racket-lang.org/"      abs)
    ("stubs/blog"    "http://blog.racket-lang.org/"     abs)
    ("stubs/mailman" "http://lists.racket-lang.org/"    abs)
    ("stubs/dirlist" "http://download.racket-lang.org/" abs)))

(provide distributions)
(define distributions
  ;; Each is a "hostname:dest-path", and then a list of directories to
  ;; put in that path.  (Warning: "dest" should not be a top-level
  ;; directory that already exists.)
  '(["champlain:/www" "www" "download" "lists" "stubs"]))
