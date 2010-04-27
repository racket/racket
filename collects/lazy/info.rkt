#lang setup/infotab

(require string-constants)
(define name "Lazy Scheme")
(define scribblings '(("lazy.scrbl" () (experimental))))
(define drscheme-language-modules '(("lazy.ss" "lazy")))
(define drscheme-language-positions
  `((,(string-constant experimental-languages) "Lazy Scheme")))
(define drscheme-language-numbers '((1000 -500)))
(define drscheme-language-one-line-summaries '("Lazy Scheme"))
