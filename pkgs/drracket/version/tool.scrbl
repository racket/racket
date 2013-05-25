#lang scribble/manual
@(require scribble/bnf
          (for-label version/tool))

@title{DrRacket Version Tool}

@defmodule[version/tool]

The @racket[version/tool] library implements a DrRacket tool that

@itemize[

@item{makes the patchlevel display as a version @tt{p}@nonterm{N}
  suffix in DrRacket (though the base verion reported by
  @racket[(version)] is not changed);}

@item{if enabled by the user, periodically checks whether a
  new Racket distribution is available for download.}

]
