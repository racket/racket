#lang scribble/doc
@(require scribble/manual
          "guide-utils.ss")

@title{More Libraries}

@other-manual['(lib "scribblings/gui/gui.scrbl")] describes the Racket
graphics toolbox, whose core is implemented by the @exec{gracket}
executable.

@other-manual['(lib "scribblings/foreign/foreign.scrbl")] describes
tools for using Racket to access libraries that are normally used by C
programs.

@other-manual['(lib "web-server/scribblings/web-server.scrbl")]
describes the Racket web server, which supports servlets implemented
in Racket.

@link["../index.html"]{Racket Documentation} lists documentation for
many other installed libraries. Run @exec{raco docs} to find
documentation for libraries that are installed on your system and
specific to your user account.

@link["http://planet.plt-racket.org/"]{@|PLaneT|} offers even more
downloadable packages contributed by Racketeers.
