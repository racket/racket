#lang racket/base

(provide sites)
(define sites
  '(("www"           "http://racket-lang.org/")
    ("download"      "http://download.racket-lang.org/")
    ("lists"         "http://lists.racket-lang.org/")
    ("stubs/planet"  "http://planet.racket-lang.org/")
    ("stubs/pre"     "http://pre.racket-lang.org/")
    ("stubs/git"     "http://git.racket-lang.org/")
    ("stubs/blog"    "http://blog.racket-lang.org/")
    ("stubs/mailman" "http://lists.racket-lang.org/")))

(provide distributions)
(define distributions
  ;; Each is a "hostname:dest-path", and then a list of directories to
  ;; put in that path.  (Warning: "dest" should not be a top-level
  ;; directory that already exists.)
  '(["champlain:/www" "www" "download" "lists" "stubs/mailman"]
    ["winooski:~wwwplanet/html-template/" "stubs/planet/"])) ; note "/"s
