#lang info

(define scribblings
  '(("start.scrbl"   (main-doc-root depends-all-main no-depend-on) (omit))
    ("search.scrbl"  (depends-all-main no-depend-on every-main-layer) (omit) "search" 1 10)
    ("local-redirect.scrbl" (depends-all-main no-depend-on every-main-layer) (omit) "local-redirect" 1 10)
    ("license.scrbl" () (omit))
    ("acks.scrbl"    () (omit))
    ("release.scrbl" (depends-all-main no-depend-on) (omit))))
