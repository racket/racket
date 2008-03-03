#lang setup/infotab

(define scribblings '(("scribblings/web-server.scrbl" (multi-page))))
(define doc-categories '(tool))

(define mzscheme-launcher-libraries '("main.ss"))
(define mzscheme-launcher-names     '("PLT Web Server"))

(define compile-omit-paths '("default-web-root"))
