#lang scribble/doc
@(require "common.rkt")

@title[#:tag "libs"]{Platform Dependencies}

On Windows and Mac OS X, the Racket distribution includes all
necessary libraries that are not part of a stock installation of the
operating system, and the libraries are included in any distribution
created with @exec{raco distribute} (see @secref[#:doc '(lib
"scribblings/raco/raco.scrbl") "exe-dist"]).

On Unix, the following system libraries must be installed. Numbers
in square brackets indicate a version that is tried first, and if that
fails, the name without the version is tried.

@itemlist[
 @item{@filepath{libglib-2.0[.0]}}
 @item{@filepath{libgmodule-2.0[.0]}}
 @item{@filepath{libgobject-2.0[.0]}}
 @item{@filepath{libpango-1.0[.0]}}
 @item{@filepath{libpangocairo-1.0[.0]}}
 @item{@filepath{libcairo[.2]}}
 @item{@filepath{libjpeg[.62]}}
 @item{@filepath{libpng12[.0]} or @filepath{libpng}}
]
