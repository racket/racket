#lang scribble/doc
@(require scribble/manual
          scribble/bnf
          "common.rkt"
          (for-label racket/base))

@title[#:tag "read"]{@exec{raco read}: Reading and Pretty-Printing}

@; to associate `history` to the right package:
@(declare-exporting compiler/commands/read)

The @exec{raco read} command @racket[read]s and pretty-prints the
contents of the given files. This command is useful for showing
how a @tt{#reader} or @hash-lang[]-based reader extension converts
input to an S-expression. It is also useful for pretty-printing a term
that is already in S-expression form.

Command-line flags:

@itemlist[
  @item{@Flag{n} @nonterm{n} or @DFlag{columns} @nonterm{n}  --- format output for a display with @nonterm{n} columns}
  @item{@Flag{h} or @DFlag{help} --- show help information for this command}
  @item{@DFlag{} --- do not treat remaining arguments as switches}
]

@history[#:added "1.3"]