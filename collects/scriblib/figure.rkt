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
         figure-ref
         (rename-out [leftfigure-style left]
                     [leftfiguremulti-style leftfiguremulti]
                     [leftfiguremultiwide-style leftfiguremultiwide]))

(define figure-style-extras
  (let ([abs (lambda (s)
               (path->main-collects-relative
                (collection-file-path s "scriblib")))])
    (list (make-css-addition (abs "figure.css"))
          (make-tex-addition (abs "figure.tex")))))

(define herefigure-style  (make-style "Herefigure" figure-style-extras))
(define figureinside-style (make-style "FigureInside" figure-style-extras))
(define legend-style (make-style "Legend" figure-style-extras))
(define figure-target-style (make-style "FigureTarget" figure-style-extras))

(define centertext-style (make-style "Centertext" figure-style-extras))
(define figure-style (make-style "Figure" figure-style-extras))

(define centerfigure-style (make-style "Centerfigure" figure-style-extras))
(define centerfiguremulti-style (make-style "CenterfigureMulti" figure-style-extras))
(define centerfiguremultiwide-style (make-style "CenterfigureMultiWide" figure-style-extras))

(define leftfigure-style (make-style "Leftfigure" figure-style-extras))
(define leftfiguremulti-style (make-style "LeftfigureMulti" figure-style-extras))
(define leftfiguremultiwide-style (make-style "LeftfigureMultiWide" figure-style-extras))

(define (figure tag caption #:style [style centerfigure-style] . content)
  (apply figure-helper figure-style style tag caption content))

(define (figure-here tag caption . content)
  (apply figure-helper herefigure-style centerfigure-style tag caption content))

(define (figure-helper figure-style content-style tag caption . content)
  (make-nested-flow 
   figure-style 
   (list
     (make-nested-flow content-style (list (make-nested-flow figureinside-style (decode-flow content))))
     (make-paragraph centertext-style (list (make-element legend-style (list (make-element figure-target-style (list (Figure-target tag) ": ")) caption)))))))

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
        (list (make-element legend-style (list (make-element figure-target-style (list (Figure-target tag) ": ")) caption))))))))))

(define (figure* tag caption . content)
  (*figure centerfiguremulti-style tag caption content))
(define (figure** tag caption . content)
  (*figure centerfiguremultiwide-style tag caption content))

(define figures (new-counter "figure"))
(define (Figure-target tag)
  (counter-target figures tag "Figure"))

(define (ref-proc initial)
  (case-lambda 
   [(tag)
    (make-element #f (list (counter-ref figures tag (string-append initial "igure"))))]
   [(tag1 tag2)
    (make-element #f (list (counter-ref figures tag1 (string-append initial "igures"))
                           " and "
                           (counter-ref figures tag2 #f)))]
   [(tag . tags)
    (make-element #f (cons (counter-ref figures tag (string-append initial "igures"))
                           (let loop ([tags tags])
                             (cond
                              [(null? (cdr tags))
                               (list ", and "
                                     (counter-ref figures (car tags) #f))]
                              [else
                               (list* ", "
                                      (counter-ref figures (car tags) #f)
                                      (loop (cdr tags)))]))))]))

(define Figure-ref (ref-proc "F"))
(define figure-ref (ref-proc "f"))
