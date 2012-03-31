#lang scribble/base
@(require scriblib/autobib)

@(define-cite cite citet gen)

@(define redex
  (make-bib
    #:author (authors "Matthias Felleisen" "Robert Bruce Findler" "Matthew Flatt")
    #:title "Semantics Engineering with PLT Redex"
    #:location (book-location #:publisher "MIT Press")
    #:is-book? #t
    #:date "2010"))

@(define schelog ; tests that urls work
  (make-bib #:title "Programming in Schelog"
            #:author "Dorai Sitaram"
            #:url "http://www.ccs.neu.edu/~dorai/schelog/schelog.html"
            #:date "1993"))


@cite[redex]
@cite[(in-bib redex ", part I")]
@cite[(in-bib redex ", part II")]
@cite[redex]

@cite[schelog]

@cite[(make-bib #:title "Look ma, no authors")]

@cite[(make-bib #:title "Look ma, no authors" #:author "And no date")]

@(gen)
