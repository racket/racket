#lang scheme

(require scribble/core)

(define (port old new)
  (make-table 
   (make-style 'boxed '())
   (list           
    (list (make-paragraph plain "World Style") (make-paragraph plain "Universe Style"))
    (list old new))))

(provide port)
