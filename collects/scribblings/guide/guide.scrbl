#lang scribble/doc
@(require scribble/manual
          scribble/eval
          "guide-utils.ss")

@title{@bold{Guide}: PLT Scheme}

This guide is intended for programmers who are new to Scheme, new to PLT
Scheme, or new to some part of PLT Scheme. It assumes
programming experience, so if you are new to programming, consider
instead reading @|HtDP|. If you want a brief introduction to PLT
Scheme, start with @|Quick|.

@seclink["to-scheme"]{Chapter 2} provides a brief introduction to
Scheme. From @seclink["datatypes"]{Chapter 3} on, this guide dives
into details---covering much of the PLT Scheme toolbox, but leaving
precise details to @|MzScheme| and other reference manuals.

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

@include-section["performance.scrbl"]

@include-section["running.scrbl"]

@include-section["compile.scrbl"]

@; ----------------------------------------------------------------------
@section{More Libraries}

@other-manual['(lib "scribblings/gui/gui.scrbl")] describes the PLT
Scheme graphics toolbox, whose core is implemented by the @exec{mred}
executable.

@other-manual['(lib "scribblings/foreign/foreign.scrbl")] describes
tools for using Scheme to access libraries that are normally used by C
programs.

@other-manual['(lib "web-server/scribblings/web-server.scrbl")]
describes the PLT Scheme web server, which supports servlets
implemented in Scheme.

@link["../index.html"]{PLT Scheme Documentation} lists documentation
for many other installed libraries. Run @exec{plt-help} to find
documentation for libraries that are installed on your system and
specific to your user account.

@link["http://planet.plt-scheme.org/"]{@|PLaneT|} offers even more
downloadable packages contributed by PLT Scheme users.

@; ----------------------------------------------------------------------

@(bibliography
 
  (bib-entry #:key "Goldberg04"
             #:author "David Goldberg, Robert Bruce Findler, and Matthew Flatt"
             #:title "Super and Inner---Together at Last!"
             #:location "Object-Oriented Programming, Languages, Systems, and Applications"
             #:date "2004"
             #:url "http://www.cs.utah.edu/plt/publications/oopsla04-gff.pdf")

  (bib-entry #:key "Flatt06"
             #:author "Matthew Flatt, Robert Bruce Findler, and Matthias Felleisen"
             #:title "Scheme with Classes, Mixins, and Traits (invited tutorial)"
             #:location "Asian Symposium on Programming Languages and Systems"
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
