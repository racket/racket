#lang info

(define scribblings
  '(("root.scrbl" (user-doc-root no-depend-on) (omit))
    ("user-start.scrbl" (depends-all no-depend-on) (omit))
    ("search.scrbl" (user-doc depends-all-user no-depend-on) (omit))
    ("local-redirect.scrbl" (user-doc depends-all-user no-depend-on) (omit))
    ("release.scrbl" (user-doc depends-all no-depend-on) (omit))))
