#lang info

(define scribblings
  '(("root.scrbl" (main-doc-root no-depend-on) (omit))
    ("start.scrbl"
     (depends-all-main no-depend-on) (omit))
    ("search.scrbl"         (depends-all-main no-depend-on) (omit) "search" 1 10)
    ("local-redirect.scrbl" (depends-all-main no-depend-on) (omit) "local-redirect" 1 10)
    ("license.scrbl" () (omit))
    ("acks.scrbl"    () (omit))
    ("release.scrbl" (depends-all-main no-depend-on) (omit))))
