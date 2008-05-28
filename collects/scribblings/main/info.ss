#lang setup/infotab

(define scribblings
  '(("start.scrbl"
     (main-doc-root always-run depends-all-main no-depend-on) (omit))
    ("user/start.scrbl"
     (user-doc-root always-run depends-all no-depend-on) (omit))
    ("search.scrbl"       (depends-all-main no-depend-on) (omit))
    ("master-index.scrbl" (depends-all-main no-depend-on) (omit))
    ("user/search.scrbl"       (user-doc depends-all no-depend-on) (omit))
    ("user/master-index.scrbl" (user-doc depends-all no-depend-on) (omit))
    ("license.scrbl" () (omit))
    ("acks.scrbl"    () (omit))
    ("release.scrbl" () (omit))))
