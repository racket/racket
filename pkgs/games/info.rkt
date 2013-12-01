#lang info

(define collection "games")

(define scribblings '(("scribblings/games.scrbl" (multi-page) (gui-library))))

(define gracket-launcher-libraries (list "main.rkt"))
(define gracket-launcher-names (list "PLT Games"))
(define deps '("base"
               "draw-lib"
               "drracket"
               "gui-lib"
	       "net-lib"
               "htdp-lib"
               "math-lib"
               "scribble-lib"
               "racket-index"
               "sgl"
               "srfi-lib"
               "string-constants-lib"))
(define build-deps '("draw-doc"
                     "gui-doc"
                     "racket-doc"
                     "pict-lib"
                     "rackunit-lib"
                     "htdp-doc"))

(define pkg-desc "Games")

(define pkg-authors '(mflatt robby))
