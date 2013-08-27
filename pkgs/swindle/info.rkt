;;; Written by Eli Barzilay: Maze is Life!  (eli@barzilay.org)
#lang info

(define collection "swindle")

(define mzscheme-launcher-names '("swindle"))
(define mzscheme-launcher-flags '(("-li" "swindle")))

(define scribblings '(("swindle.scrbl" ())))

;; This simple interface is not enough, use tool.rkt instead
;; (define drscheme-language-modules
;;   '(("swindle.rkt" "swindle")
;;     ("turbo.rkt" "swindle")
;;     ("html.rkt" "swindle")))
;; (define drscheme-language-positions
;;   '(("Swindle" "Full Swindle")
;;     ("Swindle" "Swindle without CLOS")
;;     ("Swindle" "HTML Swindle")))
;; (define drscheme-language-numbers
;;   '((-900 0) (-900 1) (-900 2)))
;; (define drscheme-language-one-line-summaries
;;   '("Scheme with Full Swindle extensions"
;;     "Scheme with Swindle without the object system"
;;     "Scheme with the HTML and Swindle extensions"))
;; (define drscheme-language-urls
;;   '("http://www.barzilay.org/Swindle/"
;;     "http://www.barzilay.org/Swindle/"
;;     "http://www.barzilay.org/Swindle/"))

(define tools      '(("tool.rkt")))
(define tool-names '("Swindle"))
(define tool-icons '(("swindle-icon.png" "swindle")))
(define tool-urls  '("http://www.barzilay.org/Swindle/"))
(define deps '("scheme-lib"
               "base"
               "compatibility-lib"
               "drracket-plugin-lib"
               "gui-lib"
	       "net-lib"
               "string-constants-lib"))
(define build-deps '("compatibility-doc"
                     "racket-doc"
                     "scribble-lib"))

(define pkg-desc "The implementation of the Swindle language")

(define pkg-authors '(eli))
