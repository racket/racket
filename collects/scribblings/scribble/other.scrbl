#lang scribble/manual
@(require "utils.ss"
          (for-label scribble/sigplan))

@title{More Document Styles}

In addition to @schememodname[scribble/base] and
@schememodname[scribble/manual], Scribble provides a few extra
document styles as a convenience.

@section{SIGPLAN Paper Format}

@defmodulelang[scribble/sigplan]{The @schememodname[scribble/sigplan]
language is like @schememodname[scribble/manual], but configured with
Latex style defaults to use the @filepath{sigplanconf.cls} class
file that is included with Scribble.}

@defidform[preprint]{

Enables the @tt{preprint} option. Use @scheme[preprint] only on the
same line as @hash-lang[], with only whitespace between
@schememodname[scribble/sigplan] and @scheme[preprint]:

@verbatim[#:indent 2]|{
  #lang scribble/sigplan @preprint
}|}
