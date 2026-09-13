#lang info

(define scribblings
  '(("start.scrbl"   (main-doc-root depends-all-main no-depend-on) (omit))
    ("search.scrbl"  (depends-all-main no-depend-on every-main-layer) (omit 0 ()) "search" 1 10)
    ("local-redirect.scrbl" (depends-all-main no-depend-on every-main-layer) (omit) "local-redirect" 1 10)
    ("family.scrbl"  (depends-all-main no-depend-on every-main-layer depends-family) (omit 0 ()))
    ("license.scrbl" (depends-all-main) (omit-start 0 ()))
    ("acks.scrbl"    (depends-all-main) (omit-start 0 ()))
    ("release.scrbl" (depends-all-main no-depend-on every-main-layer) (omit 0 ()))))
