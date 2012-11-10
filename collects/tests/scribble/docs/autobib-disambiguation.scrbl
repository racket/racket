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
@(define c (make-bib #:title "Diss 3"
                     #:is-book? #t
                     #:location (dissertation-location #:institution "NEU")
                     #:author (authors "Little" "Bo" "Peep" "Peep2")
                     #:date "2012"))
According to the following people,
@cite[a]
@cite[b]
@cite[c]
@citet[a]
@citet[b]
@citet[c]
@cite[a b]
@cite[b a]
@citet[a b]
@citet[b a]

@gen-bib[]
