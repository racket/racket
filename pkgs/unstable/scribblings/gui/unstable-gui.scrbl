#lang scribble/doc
@(require scribble/base
          scribble/manual
          (for-label scribble/base))

@title[#:style '(toc) #:tag "unstable-gui"]{Unstable GUI: May Change Without Warning}

This manual documents GUI libraries available in the
@racketidfont{unstable} collection. See @other-doc['(lib
"unstable/scribblings/unstable.scrbl")] for more information about
unstable libraries.

@local-table-of-contents[#:style 'immediate-only]

@include-section["notify.scrbl"]
@include-section["prefs.scrbl"]
@include-section["pict.scrbl"]
@include-section["slideshow.scrbl"]
@include-section["pslide.scrbl"]
@include-section["snip.scrbl"]
@include-section["scribble.scrbl"]
