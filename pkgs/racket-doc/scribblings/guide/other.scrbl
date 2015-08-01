#lang scribble/doc
@(require scribble/manual "guide-utils.rkt")

@title{More Libraries}

This guide covers only the Racket language and libraries that are
documented in @|Racket|. The Racket distribution includes many
additional libraries.

@include-section["graphics.scrbl"]

@section{The Web Server}

@other-manual['(lib "web-server/scribblings/web-server.scrbl")]
describes the Racket web server, which supports servlets implemented
in Racket.

@section{Using Foreign Libraries}

@other-manual['(lib "scribblings/foreign/foreign.scrbl")] describes
tools for using Racket to access libraries that are normally used by C
programs.

@section{And More}

@link["../index.html"]{Racket Documentation} lists documentation for
many other installed libraries. Run @exec{raco docs} to find
documentation for libraries that are installed on your system and
specific to your user account.

@link["http://pkgs.racket-lang.org/"]{The Racket package repository}
offer even more downloadable packages that are contributed by
Racketeers.

The legacy @link["http://planet.racket-lang.org/"]{@|PLaneT|} site
offers additional packages, although maintained packages have
generally migrated to the newer package repository.