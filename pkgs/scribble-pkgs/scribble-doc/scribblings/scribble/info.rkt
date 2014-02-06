#lang info

(define scribblings '(("scribble.scrbl" (multi-page) (racket-core -24))
                      ("scribble-pp.scrbl" (multi-page) (tool))

                      ("demo-s1.scrbl" (keep-style no-search) (omit-start))
                      ("demo-m1.scrbl" (multi-page keep-style no-search) (omit-start))
                      ("demo-s2.scrbl" (keep-style) (omit-start))
                      ("demo-m2.scrbl" (multi-page keep-style) (omit-start))

                      ("demo-manual-s1.scrbl" (keep-style no-search) (omit-start))
                      ("demo-manual-m1.scrbl" (multi-page keep-style no-search) (omit-start))
                      ("demo-manual-s2.scrbl" (keep-style) (omit-start))
                      ("demo-manual-m2.scrbl" (multi-page keep-style) (omit-start))))
