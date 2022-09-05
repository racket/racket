#lang scribble/manual
@(require "common.rkt")

@title{Building, Distributing, and Contributing to Racket}

The main Racket source code repository is

@centerline{@url[git-repo]} @; `git-repo` is defined in "common.rkt"

This guide explains how to build those sources, how to create Racket
distributions like the ones at @url{https://download.racket-lang.org}, and
how to contribute to Racket development.

@table-of-contents[]

@include-section["build.scrbl"]
@include-section["distribute.scrbl"]
@include-section["contribute.scrbl"]
@include-section["zuo.scrbl"]
@include-section["bootstrap.scrbl"]
