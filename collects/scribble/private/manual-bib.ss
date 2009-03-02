#lang scheme/base
(require "../decode.ss"
         "../struct.ss"
         "../basic.ss"
         "manual-utils.ss"
         "manual-style.ss")

(provide cite
         bib-entry
         (rename-out [a-bib-entry? bib-entry?])
         bibliography)

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

(define-struct a-bib-entry (key val))

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
      "SBibliography"
      (map (lambda (c)
             (let ([key (a-bib-entry-key c)]
                   [val (a-bib-entry-val c)])
               (list
                (to-flow (make-target-element #f `("[" ,key "]") `(cite ,key)))
                flow-spacer
                (to-flow val))))
           citations))))
   null))
