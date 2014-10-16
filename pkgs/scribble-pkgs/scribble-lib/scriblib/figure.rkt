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
         left-figure-style
         center-figure-style
         right-figure-style
         suppress-floats
         (rename-out [left-figure-style left]))

(define figure-style-extras
  (let ([abs (lambda (s)
               (path->main-collects-relative
                (collection-file-path s "scriblib")))])
    (list (make-css-addition (abs "figure.css"))
          (make-tex-addition (abs "figure.tex")))))

;; outer layer:
(define herefigure-style  (make-style "Herefigure" figure-style-extras))
(define figure-style (make-style "Figure" figure-style-extras))
(define figuremulti-style (make-style "FigureMulti" figure-style-extras))
(define figuremultiwide-style (make-style "FigureMultiWide" figure-style-extras))

;; middle layer:
(define center-figure-style (make-style "Centerfigure" figure-style-extras))
(define left-figure-style (make-style "Leftfigure" figure-style-extras))
(define right-figure-style (make-style "Rightfigure" figure-style-extras))

;; inner layer:
(define figureinside-style (make-style "FigureInside" figure-style-extras))

(define legend-style (make-style "Legend" figure-style-extras))
(define legend-continued-style (make-style "LegendContinued" figure-style-extras))

(define centertext-style (make-style "Centertext" figure-style-extras))

;; See "figure.js":
(define figure-target-style
  (make-style #f
              (list
               (make-attributes '((x-target-lift . "Figure")))
               (make-js-addition
                (path->main-collects-relative
                 (collection-file-path "figure.js" "scriblib"))))))

(define (make-figure-ref c s)
  (element (style "FigureRef" (list* (command-extras (list s))
                                     figure-style-extras))
    c))
(define (make-figure-target c s)
  (element (style "FigureTarget" (cons (command-extras (list s))
                                       figure-style-extras))
    c))

(define (figure tag caption 
                #:style [style center-figure-style]
                #:continue? [continue? #f]
                . content)
  (figure-helper figure-style style tag caption content continue?))

(define (figure-here tag caption 
                     #:style [style center-figure-style] 
                     #:continue? [continue? #f]
                     . content)
  (figure-helper herefigure-style style tag caption content continue?))

(define (figure* tag caption 
                 #:style [style center-figure-style]
                 #:continue? [continue? #f]
                 . content)
  (figure-helper figuremulti-style style tag caption content continue?))
(define (figure** tag caption 
                  #:style [style center-figure-style] 
                  #:continue? [continue? #f]
                  . content)
  (figure-helper figuremultiwide-style style tag caption content continue?))

(define (figure-helper figure-style content-style tag caption content continue?)
  (make-nested-flow 
   figure-style 
   (list
    (make-nested-flow
     content-style
     (list (make-nested-flow figureinside-style (decode-flow content))))
    (make-paragraph
     centertext-style
     (list (make-element (if continue?
                             legend-continued-style
                             legend-style)
                         (list (Figure-target tag #:continue? continue?) caption)))))))

(define figures (new-counter "figure" 
                             #:target-wrap make-figure-target
                             #:ref-wrap make-figure-ref))
(define (Figure-target tag #:continue? [continue? #f])
  (counter-target figures tag 
                  "Figure" 
                  (if continue? " (continued): " ": ")
                  #:target-style figure-target-style
                  #:continue? continue?))

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

(define (suppress-floats)
  (make-element "suppressfloats" null))
