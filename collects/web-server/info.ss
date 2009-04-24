#lang setup/infotab

(define scribblings
  '(("scribblings/web-server.scrbl" (multi-page) (tool))
    ("scribblings/web-server-internal.scrbl" (multi-page) (tool))
    ("scribblings/tutorial/continue.scrbl" () (getting-started 5))))

(define mzscheme-launcher-libraries '("main.ss"))
(define mzscheme-launcher-names     '("PLT Web Server"))

(define compile-omit-paths '("default-web-root"
                             "scribblings/tutorial/examples"))
