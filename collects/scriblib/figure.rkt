#lang scheme/base

(require scribble/manual
         scribble/core
         scribble/decode
         scribble/html-properties
         scribble/latex-properties
         setup/main-collects
         "private/counter.rkt")

(provide figure
         figure*
         figure**
         figure-here
         Figure-target 
         Figure-ref
         figure-ref)

(define figure-style-extras
  (let ([abs (lambda (s)
               (path->main-collects-relative
                (collection-file-path s "scriblib")))])
    (list (make-css-addition (abs "figure.css"))
          (make-tex-addition (abs "figure.tex")))))

(define centerfigure-style (make-style "Centerfigure" figure-style-extras))
(define herefigure-style  (make-style "Herefigure" figure-style-extras))
(define figureinside-style (make-style "FigureInside" figure-style-extras))
(define legend-style (make-style "Legend" figure-style-extras))
(define centerfiguremulti-style (make-style "CenterfigureMulti" figure-style-extras))
(define centerfiguremultiwide-style (make-style "CenterfigureMultiWide" figure-style-extras))

(define (figure tag caption . content)
  (apply figure-helper centerfigure-style tag caption content))
(define (figure-here tag caption . content)
  (apply figure-helper herefigure-style tag caption content))
(define (figure-helper style tag caption . content)
  (make-nested-flow
   style
   (list
    (make-nested-flow
     figureinside-style
     (append
      (decode-flow content)
      (list
       (make-paragraph
        plain
        (list
         (make-element legend-style
                       (list (Figure-target tag) ": " 
                             caption))))))))))

(define (*figure style tag caption content)
  (make-nested-flow
   style
   (list
    (make-nested-flow
     figureinside-style
     (append
      (decode-flow content)
      (list
       (make-paragraph
        plain
        (list
         (make-element legend-style
                       (list (Figure-target tag) ": " 
                             caption))))))))))

(define (figure* tag caption . content)
  (*figure centerfiguremulti-style tag caption content))
(define (figure** tag caption . content)
  (*figure centerfiguremultiwide-style tag caption content))

(define figures (new-counter "figure"))
(define (Figure-target tag)
  (counter-target figures tag "Figure"))
(define (Figure-ref tag)
  (make-element #f (list (counter-ref figures tag "Figure"))))
(define (figure-ref tag)
  (make-element #f (list (counter-ref figures tag "figure"))))
