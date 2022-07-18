#lang s-exp racket

;; ---------------------------------------------------------------------------------------------------
; things to be shared among all sections of the style guide

(provide (for-label (all-from-out racket))
         (all-from-out scribble/manual))

(provide
  1/2-line
  LINEWIDTH
  eli
  codebox
  compare
  codebox0
  compare0
  column-table
  row-table
  rkt rkt/base rkt/gui xml)

(require (for-label racket)
         scribble/base
         scribble/manual
         scribble/struct
         (only-in scribble/core table-columns table-cells style plain
                  color-property nested-flow)
         scribble/html-properties
         racket/list)

(define eli "eli@barzilay.org")

(define (LINEWIDTH) "102")

;; ---------------------------------------------------------------------------------------------------

(define (1/2-line (n 11)) (t (string-join (map string (make-list n #\-)))))

(define (rkt) (racketmodname racket))
(define (rkt/base) (racketmodname racket/base))
(define (rkt/gui) (racketmodname racket/gui))
(define (xml) (racketmodname xml))


(define stretching-style
  (style #f (list (attributes '([style . "margin-left: 0; margin-right: 0"])))))

(define (stretch d)
  (match d
    [(nested-flow _ content) (nested-flow stretching-style content)]
    [_ d]))

;; compare: two code snippets, in two columns: left is good, right is bad
;; The styling is slightly broken.
;; Consider using compare0 instead;
;; compare is provided only for backward compatibility
(define (compare stuff1 stuff2)
  (define stuff (list (list stuff1) (list stuff2)))
  (table (sty 2 500) (apply map (compose make-flow list) stuff)))

;; compare0: two code snippets, in two columns: left is good, right is bad
(define (compare0 #:left [left "good"] #:right [right "bad"]
                  stuff1 stuff2)
  (define stuff (list (list (stretch (filebox (tt left) stuff1)))
                      (list (stretch (filebox (tt right) stuff2)))))
  (table (sty 2 500) (apply map (compose make-flow list) stuff)))

;; codebox: a code snippet in a box. The styling is slightly broken.
;; Consider using codebox0 instead;
;; codebox is provided only for backward compatibility
(define (codebox stuff1)
  (define stuff (list (list stuff1)))
  (table (sty 1 700) (apply map (compose make-flow list) stuff)))

;; codebox0: a code snippet in a box.
(define (codebox0 stuff1 #:label [label "good"])
  (define stuff (list (list (stretch (filebox (tt label) stuff1)))))
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
    [(row-table (row titles ...) (row char kind example) ...)
     #`(row-table/proc
        (list
         (list (paragraph plain (format "~a" 'titles)) ...)
         (list (paragraph plain (litchar (~a 'char)))
               (paragraph plain (format "~a" 'kind))
               (paragraph plain (litchar (~a 'example)))) ...))]))

(define (row-table/proc stuff)
  (table (sty (length (car stuff)) 200 #:valign? #f)
         stuff))

(define (sty columns width #:valign? [valign? #t])
  (define space
    (style #f `(,(attributes `((width . ,(format "~a" width)) (align . "left")
                                                              ,@(if valign?
                                                                    (list '(valign . "top"))
                                                                    (list)))))))
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
