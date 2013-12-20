#lang racket/base

(provide sites)
(define sites
  '(("www"           "http://racket-lang.org/")
    ("download"      "http://download.racket-lang.org/")
    ("bugs"          "http://bugs.racket-lang.org/")
    ("lists"         "http://lists.racket-lang.org/")
    ("drracket"      "http://drracket.org/")
    ;; stubs usually use absolute paths for resources, since they're
    ;; templates that often get used in sub-dir pages too
    ("stubs/planet"  "http://planet.racket-lang.org/"   abs)
    ("stubs/packages""http://pkgs.racket-lang.org/"     abs)
    ("stubs/pre"     "http://pre.racket-lang.org/"      abs)
    ("stubs/git"     "http://git.racket-lang.org/"      abs)
    ("stubs/blog"    "http://blog.racket-lang.org/"     abs)
    ("stubs/mailman" "http://lists.racket-lang.org/"    abs)
    ("stubs/dirlist" "http://download.racket-lang.org/" abs)
    ("stubs/docs"    "http://docs.racket-lang.org/"     abs)
    ("stubs/wiki"    "http://wiki.racket-lang.org/"     abs)))

(provide distributions)
(define distributions
  (make-parameter
   ;; Each is a "hostname:dest-path", and then a list of directories to put in
   ;; that path.  Warning: distributed directories are replicated from the
   ;; source, including removing material that is not distributed.  A directory
   ;; can also have "*" parts which will be expanded recursively -- useful to
   ;; avoid deletions in case a target directory has additional materials.
   '(["winooski:/www" "www" "download" "bugs" "lists" "drracket" "stubs"])))
