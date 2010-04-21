#lang setup/infotab

(define name "PLaneT")
(define mzscheme-launcher-names '("planet"))
(define mzscheme-launcher-libraries '("planet.ss"))
(define scribblings '(("planet.scrbl" (multi-page) (tool))))

(define racket-tools '(("planet" planet/planet "manage Planet package installations" 80)))
