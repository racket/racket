#lang s-exp racket

;; ---------------------------------------------------------------------------------------------------
; things to be shared among all sections of the style guide

(require (for-label racket)
         scribble/base
         scribble/manual
         scribble/struct
         (only-in scribble/core table-columns style plain)
         scribble/html-properties
         racket/list)

(provide (for-label (all-from-out racket))
         (all-from-out scribble/manual))

(provide
  LINEWIDTH
  eli
  codebox
  compare ;; create a comparison box for two code snippets
  ;; good    ;; label a code fragment 'good' [doesn't work]
  ;; bad     ;; label a code fragment 'bad' [doesn't work]
  column-table
  row-table
  rkt rkt/base rkt/gui xml)

(define eli "eli@barzilay.org")

(define (LINEWIDTH) "102")

;; ---------------------------------------------------------------------------------------------------

(define (rkt) (racketmodname racket))
(define (rkt/base) (racketmodname racket/base))
(define (rkt/gui) (racketmodname racket/gui))
(define (xml) (racketmodname xml))


;; compare: two code snippets, in two columns: left is good, right is bad
(define (compare stuff1 stuff2)
  (define stuff (list (list stuff1) (list stuff2)))
  (table (sty 2 500) (apply map (compose make-flow list) stuff)))

;; good: a code snippet in a box
(define (codebox stuff1)
  (define stuff (list (list stuff1)))
  (table (sty 1 700) (apply map (compose make-flow list) stuff)))

(define-syntax (column-table stx)
  (syntax-case stx (col)
    [(_ (col x ...) ...)
     #`(begin
	 (define stuff (list (list (paragraph plain (format "~a" 'x)) ...) ...))
	 (table (sty (length stuff) 200)
	        (apply map (compose make-flow list) stuff)))]))

(define-syntax (row-table stx)
  (syntax-case stx (row)
    [(_ (row x ...) ...)
     #`(begin
	 (define stuff (list (list (paragraph plain (format "~a" 'x)) ...) ...))
	 (table (sty (length (first stuff)) 200)
	        (map make-flow stuff)))]))

(define (sty columns width)
  (define space
    (style #f `(,(attributes `((width . ,(format "~a" width)) (align . "left") (valign . "top"))))))
  ;; -- in --
  (style #f
    (list
      (attributes '((border . "1") (cellpadding . "1")))
      (table-columns (make-list columns space)))))

;; ===================================================================================================
;; the following doesn't work

;; label a piece of syntax good or bad
(define-syntax-rule
  (good form code ...)
(racketmod #:file
(tt "good")
racket
form
code
...))

(define-syntax-rule
  (bad form code ...)
  (labeled-code
   "bad"
   form
   code
   ...))

(define-syntax-rule
  (labeled-code lbl:string form code ...)
  ;; ===>
  (racketmod
   #:file
   (tt lbl:string)
   racket
   form
   code
   ...))
