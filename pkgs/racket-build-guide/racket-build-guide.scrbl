#lang scribble/manual
@(require "common.rkt")

@title{Building, Distributing, and Contributing to Racket}

The main Racket source code repository is

@centerline{@url[git-repo]} @; `git-repo` is defined in "common.rkt"

This guide explains how to build those sources, how to create Racket
distributions like the ones at @url{https://download.racket-lang.org}, and
how to contribute to Racket development.

@para[#:style (html-hidden-attribute)]{If you're reading this document in Markdown form, you may find the
@hyperlink["https://docs.racket-lang.org/racket-build-guide/index.html"]{online
HTML version} more readable. There's no guarantee that the online
version is still available or matches the Racket sources you're using,
however.}

@table-of-contents[]

@include-section["build.scrbl"]
@include-section["distribute.scrbl"]
@include-section["contribute.scrbl"]
@include-section["zuo.scrbl"]
@include-section["bootstrap.scrbl"]
