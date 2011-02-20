#lang racket

; things to be shared among all sections of the style guide

(require (for-label racket)
         scribble/manual
         scribble/struct
         (only-in scribble/core table-columns style)
         scribble/html-properties
         racket/list)

(provide (for-label (all-from-out racket))
         (all-from-out scribble/manual))

(provide compare)

;; compare: two elements,

(define (compare stuff1 stuff2)
  (define stuff (list (list stuff1) (list stuff2)))
  (define space (style #f (list (attributes '((width . "500") (valign . "top"))))))
  (table
   (style #f
          (list
           (attributes '((border . "1") (cellpadding . "10")))
           (table-columns (make-list (length stuff) space))))
   (apply map (compose make-flow list) stuff)))
