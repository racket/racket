#lang scribble/manual
@(require scribble/eval "guide-utils.rkt")

@title{@bold{The Racket Guide}}

@author["Matthew Flatt" "Robert Bruce Findler" "PLT"]

This guide is intended for programmers who are new to Racket or new to
some part of Racket. It assumes programming experience, so if you are
new to programming, consider instead reading @|HtDP|. If you want an
especially quick introduction to Racket, start with @|Quick|.

@seclink["to-scheme"]{Chapter 2} provides a brief introduction to
Racket. From @seclink["datatypes"]{Chapter 3} on, this guide dives
into details---covering much of the Racket toolbox, but leaving
precise details to @|Racket| and other reference manuals.

@table-of-contents[]

@include-section["welcome.scrbl"]

@include-section["to-scheme.scrbl"]

@include-section["data.scrbl"]

@include-section["forms.scrbl"]

@include-section["define-struct.scrbl"]

@include-section["modules.scrbl"]

@include-section["contracts.scrbl"]

@include-section["io.scrbl"]

@include-section["regexp.scrbl"]

@include-section["control.scrbl"]

@include-section["for.scrbl"]

@include-section["match.scrbl"]

@include-section["class.scrbl"]

@include-section["unit.scrbl"]

@include-section["namespaces.scrbl"]

@include-section["macros.scrbl"]

@include-section["languages.scrbl"]

@include-section["performance.scrbl"]

@include-section["running.scrbl"]

@include-section["other.scrbl"]

@include-section["dialects.scrbl"]

@include-section["other-editors.scrbl"]

@; ----------------------------------------------------------------------

@(bibliography
 
  (bib-entry #:key "Goldberg04"
             #:author "David Goldberg, Robert Bruce Findler, and Matthew Flatt"
             #:title "Super and Inner---Together at Last!"
             #:location "Object-Oriented Programming, Languages, Systems, and Applications"
             #:date "2004"
             #:url "http://www.cs.utah.edu/plt/publications/oopsla04-gff.pdf")

  (bib-entry #:key "Flatt02"
             #:author "Matthew Flatt"
             #:title "Composable and Compilable Macros: You Want it When?"
             #:location "International Conference on Functional Programming"
             #:date "2002")
 
  (bib-entry #:key "Flatt06"
             #:author "Matthew Flatt, Robert Bruce Findler, and Matthias Felleisen"
             #:title "Scheme with Classes, Mixins, and Traits (invited tutorial)"
             #:location "Asian Symposium on Programming Languages and Systems"
             #:url "http://www.cs.utah.edu/plt/publications/aplas06-fff.pdf"
             #:date "2006")
 
 (bib-entry #:key "Mitchell02"
            #:author "Richard Mitchell and Jim McKim"
            #:title "Design by Contract, by Example"
            #:is-book? #t
            #:date "2002")

 (bib-entry #:key "Sitaram05"
            #:author "Dorai Sitaram"
            #:title "pregexp: Portable Regular Expressions for Scheme and Common Lisp"
            #:url "http://www.ccs.neu.edu/home/dorai/pregexp/pregexp.html"
            #:date "2002")

)

@index-section[]
