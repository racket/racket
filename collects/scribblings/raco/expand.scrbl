#lang scribble/doc
@(require scribble/manual "common.rkt"
          (for-label racket/base))

@title[#:tag "expand"]{@exec{raco expand}: Macro Expansion}

The @exec{raco expand} command macro expands the contents of
the given source files. Also see @racket[expand].

Command-line flags:

@itemlist[
  @item{@Flag{h} or @DFlag{help} --- show help information for this command}
  @item{@DFlag{} --- do not treat remaining arguments as switches}
]