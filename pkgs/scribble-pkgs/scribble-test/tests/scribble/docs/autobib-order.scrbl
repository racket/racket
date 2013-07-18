#lang scribble/base
@(require scriblib/autobib)
@(define-cite cite citet gen)

@(define a (make-bib #:title "Diss 1"
                     #:is-book? #t
                     #:location (dissertation-location #:institution "NEU")
                     #:author (authors (author-name "Little Bo" "Peep") (author-name "Samwise" "Gamgee"))
                     #:date "2012"))
@(define b (make-bib #:title "Diss 2"
                     #:is-book? #t
                     #:location (dissertation-location #:institution "NEU")
                     #:author (authors (author-name "Ayo" "Shucks"))
                     #:date "2012"))
Order matters. Must sort by last names.
@cite[b]
@cite[a]

@(gen)
