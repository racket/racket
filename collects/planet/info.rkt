#lang setup/infotab

(define name "PLaneT")
(define mzscheme-launcher-names '("planet"))
(define mzscheme-launcher-libraries '("planet.rkt"))
(define scribblings '(("planet.scrbl" (multi-page) (tool))))

(define raco-commands '(("planet" planet/raco "manage Planet package installations" 80)))
