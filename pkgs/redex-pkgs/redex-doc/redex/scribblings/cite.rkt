#lang at-exp racket/base
(require scriblib/autobib
         scribble/base
         scribble/core)
(provide generate-bibliography ~cite citet
         amb1 amb2 Hanford
         
         racket-virtual-machine
         palka-workshop
         list-machine
         delim-cont-cont)

(define jar "Journal of Automated Reasoning")
(define hosc "Higher-Order and Symbolic Computation")
(define esop "European Symposium on Programming")

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
            #:date 1963
            #:url "http://www-formal.stanford.edu/jmc/basis.html"))
(define amb2
  (make-bib #:title "Non-deterministic Lisp with dependency-directed backtracking"
            #:author (authors "Ramin Zabih"
                              "David McAllester"
                              "David Chapman")
            #:location (proceedings-location
                        "Proceedings of the Sixth National Conference on Artificial Intelligence"
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
   #:date "1970"
   #:url "http://dl.acm.org/citation.cfm?id=1663480"))

(define racket-virtual-machine
  (make-bib
   #:author (authors "Casey Klein" "Robert Bruce Findler" "Matthew Flatt")
   #:title "The Racket virtual machine and randomized testing"
   #:location (journal-location hosc)
   #:date 2013
   #:url "http://plt.eecs.northwestern.edu/racket-machine/"))

(define palka-workshop
  (make-bib
   #:author (authors "Michał H. Pałka" "Koen Claessen"
                     "Alejandro Russo" "John Hughes")
   #:title "Testing an Optimising Compiler by Generating Random Lambda Terms"
   #:location (proceedings-location "International Workshop on Automation of Software Test")
   #:date 2011
   #:url "http://dl.acm.org/citation.cfm?id=1982615"))

(define list-machine
  (make-bib
   #:author (authors "Andrew W. Appel" "Robert Dockins" "Xavier Leroy")
   #:title "A list-machine benchmark for mechanized metatheory"
   #:location (journal-location jar
                       #:volume 49
                       #:number 3
                       #:pages '(453 491))
   #:date 2012
   #:url "http://www.cs.princeton.edu/~appel/listmachine/"))

(define delim-cont-cont
  (make-bib
   #:author (authors "Asumu Takikawa" "T. Stephen Strickland" "Sam Tobin-Hochstadt")
   #:title "Constraining Delimited Control with Contracts"
   #:location (proceedings-location esop
                                    #:pages '(229 248))
   #:date 2013
   #:url "http://dl.acm.org/citation.cfm?id=2450287"))
