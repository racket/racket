#lang scribble/doc
@(require scribble/manual "common.rkt")

@title[#:tag "docs"]{@exec{raco docs}: Documentation Search}

The @exec{raco docs} command searches the documentation for
the given identifiers or search terms.

Command-line flags:

@itemlist[
  @item{@Flag{h} or @DFlag{help} --- show help information for this command}
  @item{@DFlag{} --- do not treat remaining arguments as switches}
]
