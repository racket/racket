#lang setup/infotab

(define scribblings
  '(("start.scrbl"
     (main-doc-root depends-all-main no-depend-on) (omit))
    ("search.scrbl"         (depends-all-main no-depend-on) (omit))
    ("local-redirect.scrbl" (depends-all-main no-depend-on) (omit))
    ("license.scrbl" () (omit))
    ("acks.scrbl"    () (omit))
    ("release.scrbl" () (omit))))
