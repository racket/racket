#lang scribble/doc
@(require "common.rkt")

@(define draw-doc '(lib "scribblings/draw/draw.scrbl"))

@title[#:tag "libs"]{Platform Dependencies}

See @secref[#:doc draw-doc "libs"] in @other-manual[draw-doc] for
information on platform library dependencies for
@racketmodname[racket/draw]. On Unix, the following additional
system libraries must be installed for @racketmodname[racket/gui/base]:

@itemlist[
 @item{@filepath{libgdk-x11-2.0[.0]}}
 @item{@filepath{libgdk_pixbuf-2.0[.0]}}
 @item{@filepath{libgtk-x11-2.0[.0]}}
 @item{@filepath{libGL} --- optional, for OpenGL support}
 @item{@filepath{libunique-1.0[.0]} --- optional, for single-instance support}
]
