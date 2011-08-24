#lang setup/infotab

(define scribblings '(("lazy.scrbl" () (experimental 50))))

;; STC 2010-06-01
;; Changing lazy to be a tools-based language so I can use 
;; stepper-language-interface to add a stepper button. This change is temporary
;; until the stepper works with #lang languages.

;(require string-constants)
;(define name "Lazy Scheme")
;(define drscheme-language-modules '(("lazy.rkt" "lazy")))
;(define drscheme-language-positions
;  `((,(string-constant experimental-languages) "Lazy Racket")))
;(define drscheme-language-numbers '((1000 -500)))
;(define drscheme-language-one-line-summaries '("Lazy Racket"))

(define drracket-tools '(("lazy-tool.rkt")))
(define drracket-tool-names '("Lazy Racket"))
