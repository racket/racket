#lang scheme/base
(require scheme/contract
         "../decode.ss"
         "../struct.ss"
         "../basic.ss"
         "on-demand.ss"
         (only-in "../core.ss" make-style)
         "manual-sprop.ss"
         "manual-utils.ss"
         "manual-style.ss")

(define-struct a-bib-entry (key val))

(provide/contract
 [cite ((string?) () #:rest (listof string?) . ->* . element?)]
 [bib-entry ((#:key string? #:title (or/c false/c pre-content?))
             (#:is-book? boolean? #:author (or/c false/c pre-content?) 
                         #:location (or/c false/c pre-content?) 
                         #:date (or/c false/c pre-content?) 
                         #:url (or/c false/c pre-content?))
             . ->* .
             a-bib-entry?)]
 [rename a-bib-entry? bib-entry? (any/c . -> . boolean?)]
 [bibliography (() (#:tag string?) #:rest (listof a-bib-entry?) . ->* . part?)]) 

(define (cite key . keys)
  (make-element
   #f
   (list "["
         (let loop ([keys (cons key keys)])
           (if (null? (cdr keys))
             (make-link-element
              #f
              (list (car keys))
              `(cite ,(car keys)))
             (make-element
              #f
              (list (loop (list (car keys)))
                    ", "
                    (loop (cdr keys))))))
         "]")))

(define (bib-entry #:key key
                   #:title title
                   #:is-book? [is-book? #f]
                   #:author [author #f]
                   #:location [location #f]
                   #:date [date #f]
                   #:url [url #f])
  (make-a-bib-entry
   key
   (make-element
    "bibentry"
    (append
     (if author `(,@(decode-content (list author)) ", ") null)
     (if is-book? null '(ldquo))
     (if is-book?
       (list (italic title))
       (decode-content (list title)))
     (if location '(",") '("."))
     (if is-book? null '(rdquo))
     (if location
       `(" " ,@(decode-content (list location)) ,(if date "," "."))
       null)
     (if date `(" " ,@(decode-content (list date)) ".") null)
     (if url `(" " ,(link url (tt url))) null)))))

(define-on-demand bib-style (make-style "RBibliography" scheme-properties))

(define (bibliography #:tag [tag "doc-bibliography"] . citations)
  (make-unnumbered-part
   #f
   `((part ,tag))
   '("Bibliography")
   '()
   null
   (make-flow
    (list
     (make-table
      bib-style
      (map (lambda (c)
             (let ([key (a-bib-entry-key c)]
                   [val (a-bib-entry-val c)])
               (list
                (to-flow (make-target-element #f `("[" ,key "]") `(cite ,key)))
                flow-spacer
                (to-flow val))))
           citations))))
   null))
