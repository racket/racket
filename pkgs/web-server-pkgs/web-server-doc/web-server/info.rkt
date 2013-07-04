#lang info

(define scribblings
  '(("scribblings/web-server.scrbl" (multi-page) (tool 100))
    ("scribblings/web-server-internal.scrbl" (multi-page) (tool))
    ("scribblings/tutorial/continue.scrbl" () (tutorial 5))))

(define compile-omit-paths '("scribblings/tutorial/examples"))
