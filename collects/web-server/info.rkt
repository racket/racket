#lang setup/infotab

(define scribblings
  '(("scribblings/web-server.scrbl" (multi-page) (tool 100))
    ("scribblings/web-server-internal.scrbl" (multi-page) (tool))
    ("scribblings/tutorial/continue.scrbl" () (tutorial 5))))

(define mzscheme-launcher-libraries '("main.rkt"))
(define mzscheme-launcher-names     '("PLT Web Server"))

(define compile-omit-paths '("default-web-root"
                             "scribblings/tutorial/examples"))
