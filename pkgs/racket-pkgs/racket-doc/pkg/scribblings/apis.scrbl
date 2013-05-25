#lang scribble/manual
@(require (for-label (except-in racket/base
                                remove)
                     racket/contract/base
                     pkg
                     pkg/lib))

@title[#:tag "apis" #:style 'toc]{Package APIs}

The @racketmodname[pkg] provides a programmatic interface to the
@exec{raco pkg} commands, but additional libraries provide smaller
building blocks and local-database support.

@local-table-of-contents[]

@include-section["lib.scrbl"]
@include-section["db.scrbl"]
