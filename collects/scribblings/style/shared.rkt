#lang s-exp racket

; things to be shared among all sections of the style guide

(require (for-label racket)
         scribble/base
         scribble/manual
         scribble/struct
         (only-in scribble/core table-columns style)
         scribble/html-properties
         racket/list)

(provide (for-label (all-from-out racket))
         (all-from-out scribble/manual))

(provide
  compare ;; create a comparison box for two code snippets
  ;; good    ;; label a code fragment 'good' [doesn't work]
  ;; bad     ;; label a code fragment 'bad' [doesn't work]
  rkt rkt/base rkt/gui)

(define (rkt) (racketmodname racket))
(define (rkt/base) (racketmodname racket/base))
(define (rkt/gui) (racketmodname racket/gui))

;; compare: two code snippets, in two columns: left is good, right is bad
(define (compare stuff1 stuff2)
  (define stuff (list (list stuff1) (list stuff2)))
  (define space (style #f (list (attributes '((width . "500") (valign . "top"))))))
  (table
   (style #f
          (list
           (attributes '((border . "1") (cellpadding . "10")))
           (table-columns (make-list (length stuff) space))))
   (apply map (compose make-flow list) stuff)))

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