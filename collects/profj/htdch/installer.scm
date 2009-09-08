#lang mzscheme

(require (prefix geo:   "geometry/installer.ss")
         (prefix color: "colors/installer.ss")
         (prefix draw:  "draw/installer.ss")
         (prefix idraw: "idraw/installer.ss")
         (prefix graph: "graphics/installer.ss"))

(provide installer)
(define (installer plthome)
  (geo:installer   plthome)
  (color:installer plthome)
  (draw:installer  plthome)
  (idraw:installer plthome)
  #;(graph:installer plthome))
