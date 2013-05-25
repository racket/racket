#lang scribble/base
@(require scriblib/autobib)
@(define-cite cite citet gen-bib)

@(define a (make-bib #:title "Diss A"
                     #:is-book? #t
                     #:location (dissertation-location #:institution "NEU")
                     #:author (authors "Little" "Bo" "Peep")
                     #:date "2012"))
@(define b (make-bib #:title "Diss B"
                     #:is-book? #t
                     #:location (dissertation-location #:institution "NEU")
                     #:author (authors "Little" "Bo" "Peep")
                     #:date "2012"))
@;{Citing ambiguous entries without other single citations}
@;{should not cause a warning}
@cite[a b]

@gen-bib[]
