#lang scribble/doc

@(require scribble/manual 
          (for-label [except-in racket require]
                     [only-in lang/htdp-beginner require]))

@title[#:style 'toc #:tag "htdp"]{Implementing HtDP Teachpacks, Libraries, and Customized Teaching Languages}

DrRacket has two different mechanisms for making available additional functions
and functionality to students using the teaching languages. 


@itemize[
@item{HTdP Teachpacks are added to a student's program by clicking on the
``Language'' menu and selecting ``add Teachpack''. Students can then install a
new Teachpack by clicking ``Add Teachpack to List'' and choosing the Teachpack
file from the filesystem.}

@item{HTdP Libraries are brought into the student's program using a
@racket[require] statement.}]

Under the hood, HTdP Teachpacks and HTdP Libraries are implemented the same way,
using normal Racket @secref[#:doc '(lib "scribblings/guide/module.scrbl") "modules"]. 

When implementing an extension intended for students, pay a special attention to
the error messages. The error messages of DrRacket's teaching languages go to
great length to ensure that students are never confronted with messages that
uses vocabulary or phrases the students has not learned yet.  The teaching languages
also ensure that students cannot stumble by accident onto challenging or
confusing features intended for professional or for higher-level students.

This manual describes library support for authors of HTdP Teachpacks, libraries,
and customized teaching languages. Use the HTdP
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
