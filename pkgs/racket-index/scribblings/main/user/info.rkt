#lang info

(define scribblings
  '(("start.scrbl" (user-doc-root depends-all no-depend-on) (omit))
    ("search.scrbl" (user-doc depends-all-user no-depend-on) (omit 0 ()))
    ("local-redirect.scrbl" (user-doc depends-all-user no-depend-on) (omit))
    ("family.scrbl"  (user-doc depends-all no-depend-on depends-family) (omit 0 ()))
    ("release.scrbl" (user-doc depends-all no-depend-on) (omit 0 ()))))
