#lang at-exp racket/base

(require scribble/base 
         scribble/core
         scriblib/autobib)
(provide exercise exref 
         generate-bibliography ~cite citet
         amb1 amb2 Hanford)

(define i 0)
(define (exercise [id #f])
  (set! i (+ i 1))
  (when id (hash-set! ex-ids id i))
  (element (style 'bold '()) 
           (format "Exercise ~a" i)))
(define ex-ids (make-hash))
(define (exref id) (format "~a" (hash-ref ex-ids id)))

(define (book-chapter-location
          #:title title
          #:author [author #f]
          #:edition [edition #f]
          #:publisher [publisher #f])
   (let* ([s @italic{@(string-titlecase title)}]
          [s (if edition
                 @elem{@|s| @(string-titlecase edition) edition}
                 s)]
          [s (if author
                 @elem{@|s| by @|author|}
                 s)]
          [s (if publisher
                 (if s
                     @elem{@|s|. @|publisher|}
                     publisher)
                 s)])
     (unless s
       (error 'book-chapter-location "no arguments"))
     @elem{In @|s|}))

(define-cite ~cite citet generate-bibliography)
(define amb1
   (make-bib #:title "A Basis for a Mathematical Theory of Computation"
             #:author "John McCarthy"
             #:location
             (book-chapter-location #:title "Computer Programming and Formal Systems"
                                    #:author (editor (authors "P. Braffort" "D. Hirschberg")))
             #:date 1963))
(define amb2
   (make-bib #:title "Non-deterministic Lisp with dependency-directed backtracking"
             #:author (authors "Ramin Zabih"
                               "David McAllester"
                               "David Chapman")
             #:location (proceedings-location "Proceedings of the Sixth National Conference on Artificial Intelligence"
                                              #:pages '(59 64))
             #:date 1987))

(define ibm-sys "IBM Systems Journal")
(define Hanford
  (make-bib
   #:author (authors "Kenneth V. Hanford")
   #:title "Automatic generation of test cases"
   #:location (journal-location ibm-sys
                                #:volume 9
                                #:number "4"
                                #:pages '(244 257))
   #:date "1970"))
