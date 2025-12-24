#lang scribble/doc
@(require scribble/manual
          scribble/bnf
          "common.rkt")

@title[#:tag "docs"]{@exec{raco docs}: Documentation Search}

The @exec{raco docs} command searches the documentation for
the given identifiers or search terms.

Command-line flags:

@itemlist[
  @item{@Flag{f} @nonterm{name} or @DFlag{family}  @nonterm{name} --- Navigate documentation
        as language family @nonterm{name}, which may affect the starting point if
        no search terms are given, may affect the ordering of results for a
        search, and may affect ``top'' and ``up'' navigation.}
  @item{@Flag{h} or @DFlag{help} --- Show help information for this command.}
  @item{@DFlag{} --- Do not treat remaining arguments as switches.}
]
