#lang racket/base
(require slideshow/base slideshow/pict
         racket/contract/base racket/list racket/match
         (for-syntax racket/base)
         "pict.rkt")
(provide (all-from-out "pict.rkt"))

(define-syntax-rule (define-with-parameter name parameter)
  (define-syntax-rule (name value body (... ...))
    (parameterize ([parameter value]) body (... ...))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Font Controls
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-with-parameter with-size current-font-size)
(define-syntax-rule (with-scale scale expr)
  (with-size (inexact->exact (ceiling (* scale (current-font-size)))) expr))
(define-syntax-rule (define-scale name scale)
  (define-syntax-rule (name expr) (with-scale scale expr)))
(define-scale big 3/2)
(define-scale small 2/3)

(define-with-parameter with-font current-main-font)
(define-syntax-rule (with-style style expr)
  (with-font (cons style (current-main-font)) expr))
(define-syntax-rule (define-style name style)
  (define-syntax-rule (name expr) (with-style style expr)))
(define-style bold 'bold)
(define-style italic 'italic)
(define-style subscript 'subscript)
(define-style superscript 'superscript)
(define-style caps 'caps)

(provide with-size
         with-scale
         big
         small

         with-font
         with-style
         bold
         italic
         subscript
         superscript
         caps)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Slide / Paragraph Manipulation
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-with-parameter column current-para-width)

(define (columns . picts)
  (apply hc-append gap-size (map baseless picts)))

(define (column-size n [r (/ n)])
  (* r (- (current-para-width) (* (sub1 n) gap-size))))

(define-syntax-rule (two-columns a b)
  (columns (column (column-size 2) a)
           (column (column-size 2) b)))

(define (mini-slide . picts)
  (apply vc-append gap-size picts))

(provide column
         columns
         column-size
         two-columns
         mini-slide)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Simple Tables
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define =!
  (case-lambda
    [(n) n]
    [(n . ns)
     (if (apply = n ns)
         n
         (error '=! "not all equal: ~a" (cons n ns)))]))

(define (elem->pict elem)
  (if (string? elem) (t elem) elem))

(define (tabular #:gap [gap gap-size]
                 #:vgap [vgap gap]
                 #:hgap [hgap gap]
                 #:align [align lbl-superimpose]
                 #:halign [halign align]
                 #:valign [valign align]
                 . cells)
  (let* ([rows (length cells)]
         [cols (apply =! (map length cells))]
         [picts (map elem->pict (append* cells))]
         [haligns (for/list ([i (in-range 0 cols)]) halign)]
         [valigns (for/list ([i (in-range 0 rows)]) valign)]
         [hseps (for/list ([i (in-range 1 cols)]) hgap)]
         [vseps (for/list ([i (in-range 1 rows)]) vgap)])
    (table cols picts haligns valigns hseps vseps)))

(define (matrixof c)
  (and/c (listof (listof c))
         (flat-named-contract "matrix"
           (match-lambda
             [(list) #t]
             [(list _) #t]
             [(list xs ...) (apply = (map length xs))]))))

(provide/contract
 [tabular (->* []
               [#:gap natural-number/c
                #:hgap natural-number/c
                #:vgap natural-number/c
                #:align (->* [] [] #:rest (listof pict?) pict?)
                #:halign (->* [] [] #:rest (listof pict?) pict?)
                #:valign (->* [] [] #:rest (listof pict?) pict?)]
               #:rest (matrixof (or/c string? pict?))
               pict?)])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Slide Staging
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-syntax-rule (slide/staged [name ...] body ...)
  (staged [name ...] (slide body ...)))

(provide slide/staged)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Misc
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (blank-line)
  (blank 0 (current-font-size)))

(provide/contract
 [blank-line (-> pict?)])
