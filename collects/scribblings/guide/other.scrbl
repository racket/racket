#lang scribble/doc
@(require scribble/manual
          "guide-utils.ss")

@title{More Libraries}

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
