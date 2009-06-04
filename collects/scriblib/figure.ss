#lang scheme/base

(require scribble/manual
         scribble/struct
         scribble/decode
         "private/counter.ss")

(provide figure
         figure*
         figure**
         Figure-target 
         Figure-ref
         figure-style-extras)

(define (figure-style-extras)
  (let ([abs (lambda (s)
               (build-path (collection-path "scriblib") s))])
    `((css ,(abs "figure.css")) (tex ,(abs "figure.tex")))))

(define (figure tag caption . content)
  (make-blockquote
   "Centerfigure"
   (list
    (make-blockquote
     "FigureInside"
     (append
      (flow-paragraphs
       (decode-flow content))
      (list
       (make-paragraph
        (list
         (make-element "Legend"
                       (list* (Figure-target tag) ": " 
                              (decode-content (list caption))))))))))))

(define (*figure style tag caption content)
  (make-blockquote
   style
   (list
    (make-blockquote
     "FigureInside"
     (append
      (flow-paragraphs
       (decode-flow content))
      (list
       (make-paragraph
        (list
         (make-element "Legend"
                       (list* (Figure-target tag) ": " 
                              (decode-content (list caption))))))))))))

(define (figure* tag caption . content)
  (*figure "CenterfigureMulti" tag caption content))
(define (figure** tag caption . content)
  (*figure "CenterfigureMultiWide" tag caption content))

(define figures (new-counter "figure"))
(define (Figure-target tag)
  (counter-target figures tag "Figure"))
(define (Figure-ref tag)
  (make-element #f (list (counter-ref figures tag "Figure"))))
