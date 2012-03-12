#lang scribble/base
@(require scriblib/autobib)
@(define-cite cite citet gen-bib)

@(define a (make-bib #:title "Diss 1"
                     #:is-book? #t
                     #:location (dissertation-location #:institution "NEU")
                     #:author (authors "Little" "Bo" "Peep")
                     #:date "2012"))
@(define b (make-bib #:title "Diss 2"
                     #:is-book? #t
                     #:location (dissertation-location #:institution "NEU")
                     #:author (authors "Little" "Bo" "Peep")
                     #:date "2012"))
According to the following morons,
@cite[a]
@cite[b]
@citet[a]
@citet[b]
@cite[a b]
@cite[b a]
@citet[a b]
@citet[b a]

@gen-bib[]