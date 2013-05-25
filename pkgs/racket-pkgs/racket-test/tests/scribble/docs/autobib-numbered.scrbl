#lang scribble/manual
@(require scriblib/autobib)

@(define-cite ~cite citet generate-bibliography
              #:style number-style)

@(define a1
   (make-bib
    #:title    "One"
    #:author   "A"
    #:date     "2012"
    #:location "There"))

@(define a2
   (make-bib
    #:title    "Two"
    #:author   "A"
    #:date     "2012"
    #:location "Here"))

@(define a2x
   (make-bib
    #:title    "Twoish"
    #:author   "A"
    #:date     "2012"
    #:location "HereX"))

@(define a3
   (make-bib
    #:title    "Three"
    #:author   "A"
    #:date     "2013"
    #:location "Where?"))

@(define b1
   (make-bib
    #:title    "Uno"
    #:author   "B"
    #:date     "2012"
    #:location "Ici"))

A1@~cite[a1 a2 b1].

A1@~cite[a1 a2 a3].

@citet[a1 a2 a3].

In A2@~cite[(in-bib a2 " p. 17")]

In A2 and A3@~cite[(in-bib a2 " p. 17") a3]

In A1 and more@~cite[a1 (in-bib a2 " p. 17") a3]

B&B@~cite[b1 a1].


@generate-bibliography[]
