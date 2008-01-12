#lang scribble/doc
@require[scribble/manual]
@require[scribble/eval]
@require["guide-utils.ss"]

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

@; ----------------------------------------------------------------------
@section[#:tag "control"]{Exceptions and Control}

@; ----------------------------------------------------------------------
@include-section["for.scrbl"]


@; ----------------------------------------------------------------------
@include-section["match.scrbl"]

@; ----------------------------------------------------------------------
@include-section["class.scrbl"]

@; ----------------------------------------------------------------------
@section[#:tag "units"]{Units@aux-elem{ (Components)}}


@; ----------------------------------------------------------------------
@section[#:tag "threads"]{Threads}

@subsection[#:tag "parameters"]{Parameters}

A @deftech{parameter} holds a kind of global option. For example,
there is a parameter that determines the default destination for
printed output.

@; ----------------------------------------------------------------------
@include-section["namespaces.scrbl"]


@; ----------------------------------------------------------------------
@include-section["macros.scrbl"]

@; ----------------------------------------------------------------------
@section[#:tag "reader"]{Reader Extension}

@; ----------------------------------------------------------------------
@section[#:tag "security"]{Security}


@; ----------------------------------------------------------------------
@section[#:tag "memory-management"]{Memory Management}

@subsection[#:tag "weakboxes"]{Weak Boxes}

@subsection[#:tag "ephemerons"]{Ephemerons}

@; ----------------------------------------------------------------------
@include-section["performance.scrbl"]

@; ----------------------------------------------------------------------
@section[#:tag "scripts"]{Scripts}

@; ----------------------------------------------------------------------
@section{Configuration and Compilation}

@itemize{

 @tool["setup-plt"]{a command-line tool for installation tasks}

 @tool["planet"]{a command-line tool for managing packages that are
 normally downloaded automatically, on demand}

 @tool["mzc"]{a command-line tool for miscellaneous tasks, such as
 compiling Scheme source, compiling C-implemented extensions to the
 run-time system, generating executables, and building distribution
 packages}

}

@; ----------------------------------------------------------------------
@section{More Libraries}

@other-manual['(lib "scribblings/gui/gui.scrbl")] describes the PLT
Scheme graphics toolbox, whose core is implemented by the MrEd
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


)

@index-section[]
