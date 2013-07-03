#lang scribble/doc

@(require scribble/manual 
          (for-label [except-in racket require]
                     htdp/color-structs
                     [only-in lang/htdp-beginner require]))

@title[#:style 'toc #:tag "htdp"]{Implementing HtDP Teachpacks, Libraries, and Customized Teaching Languages}

DrRacket has two different mechanisms for making available additional functions
and functionality to students using the teaching languages. 


@itemize[
@item{HtDP Teachpacks are added to a student's program by clicking on the
``Language'' menu and selecting ``add Teachpack''. Students can then install a
new Teachpack by clicking ``Add Teachpack to List'' and choosing the Teachpack
file from the filesystem.}

@item{HtDP Libraries are brought into the student's program using a
@racket[require] statement.}]

Under the hood, HtDP Teachpacks and HtDP Libraries are implemented the same way,
using normal Racket @secref[#:doc '(lib "scribblings/guide/guide.scrbl") "modules"]. 

When implementing such an extension for students, pay a special attention
to two aspects: 
@itemlist[#:style 'ordered

@item{@bold{choice of construct}: The teaching languages limit the
expressive power in comparison to plain Racket. One goal is to teach
``design subject to constraints,'' and the other one is to help restrict
the set of explanations for student errors. With regard to the first, we
consider it imperative that new teachpacks and libraries avoid features
intended for upper-level students or professionals.}

@item{@bold{error messages}: The error messages from the teaching languages
go to great length to never confront students messages that uses vocabulary
or phrases outside of the scope of the chosen level. While teachpacks and
libraries can be used at all levels, they should ideally restrict the
vocabulary in error message to the lowest level language in which they are
to be used.}

]


This manual describes library support for authors of HtDP Teachpacks, libraries,
and customized teaching languages. Use the HtDP
@seclink["error-reporting"]{error reporting functions} to create error messages
that integrate smoothly with those of the teaching languages. Before composing
new error messages, we recommend you read the @seclink["error-guidelines"]{error
message composition guidelines} that informed the design of the error messages
of DrRacket's teaching languages.

@local-table-of-contents[#:style 'immediate-only]

@include-section["error-composition.scrbl"]

@include-section["error-reporting.scrbl"]
@include-section["testing.scrbl"]
@include-section["htdp-lib.scrbl"]

@section{Color and Alpha Color Structs}

@defmodule[htdp/color-structs]

@defstruct[color ([red any/c] [green any/c] [blue any/c])]{
  This is the color sturct that is also exported by @racketmodname[htdp/image],
  but here it is exported via @racket[(provide (struct-out color))].
}
@defstruct[alpha-color ([alpha any/c] [red any/c] [green any/c] [blue any/c])]{
  This is the color sturct that is also exported by @racketmodname[htdp/image],
  but here it is exported via @racket[(provide (struct-out alpha-color))].
}
