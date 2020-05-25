#lang scribble/doc
@(require scribble/manual "guide-utils.rkt" scribblings/private/docname)

@title{More Libraries}

This guide covers only the Racket language and libraries that are
documented in @|Racket|. The Racket distribution includes many
additional libraries.

@include-section["graphics.scrbl"]

@section{The Web Server}

@Web[] describes the Racket web server, which supports servlets implemented
in Racket.

@section{Using Foreign Libraries}

@other-manual['(lib "scribblings/foreign/foreign.scrbl")] describes
tools for using Racket to access libraries that are normally used by C
programs.

@section{And More}

@link["../index.html"]{Racket Documentation} lists documentation for
other libraries, including libraries that are installed as packages.
Run @exec{raco docs} to find documentation for libraries that are
installed on your system and specific to your user account.

@link["http://pkgs.racket-lang.org/"]{Racket Package Catalog} at
@url{https://pkgs.racket-lang.org} offers even more downloadable
packages contributed by Racketeers. The
@link["https://docs.racket-lang.org"]{online Racket documentation}
includes documentation for packages in that catalog, updated daily.
For more information about packages, see see @other-manual['(lib
"pkg/scribblings/pkg.scrbl")].

@link["https://planet.racket-lang.org/"]{@|PLaneT|} serves packages that
were developed using an older package system. Racket packages should
use the newer system, instead.
