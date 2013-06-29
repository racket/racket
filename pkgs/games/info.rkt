#lang setup/infotab

(define collection "games")

(define scribblings '(("scribblings/games.scrbl" (multi-page) (gui-library))))

(define gracket-launcher-libraries (list "main.rkt"))
(define gracket-launcher-names (list "PLT Games"))
(define deps '("base"
               "draw-lib"
               "drracket"
               "gui-lib"
               "htdp"
               "math"
               "scribble-lib"
               "sgl"
               "srfi-lib"
               "string-constants-lib"))
(define build-deps '("pict-lib"
                     "rackunit-lib"))
