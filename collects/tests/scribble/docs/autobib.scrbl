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

@cite[redex]
@cite[(in-bib redex ", part I")]
@cite[(in-bib redex ", part II")]
@cite[redex]

@(gen)
