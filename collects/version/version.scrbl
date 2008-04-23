#lang scribble/doc
@(require scribble/manual
	  scribble/bnf
          (for-label version/patchlevel
                     version/check
                     version/tool
                     scheme/base
                     scheme/contract))

@(define (selflink s) (link s (tt s)))

@title{@bold{Version}: PLT Version Checking}

The version collection contains several version-related pieces that
are used by PLT Scheme. See also @scheme[version] from
@schememodname[scheme/base].

@; ----------------------------------------------------------------------

@section{Installed Patch Level}

@defmodule[version/patchlevel]

@defthing[patchlevel exact-nonnegative-integer?]{

Indicates the current installed patch level, which is normally zero,
but may be updated by patches to DrScheme.}

@; ----------------------------------------

@section{Checking Available Versions}

@defmodule[version/check]

@defproc[(check-version) (or/c symbol? list?)]{

Checks the currently available version on the PLT website
(@selflink["http://download.plt-scheme.org"]) and returns a value that
indicates the current state of the curent installation:

  @itemize{

    @item{@scheme[`ok] You're fine.}

    @item{@scheme[`(ok-but ,_version)] You have a fine stable version,
      but note that there is a newer alpha version available numbered
      @scheme[_version].}

    @item{@scheme[`(newer ,_version)] You have an old version. Please
      upgrade to @scheme[_version].}

    @item{@scheme[`(newer ,_version ,_alpha)] You have an
      old-but-stable version, please upgrade to @scheme[_version]; you
      may consider also the newer alpha version numbered
      @scheme[_alpha].}

    @item{@scheme[`(error ,_message)] An error occurred, and
      @scheme[_message] is a string that indicates the error.}

    @item{@scheme[`(error ,_message ,_additional-info)] An error
       occurred; @scheme[_message] is a string that indicates the
       error, and @scheme[_additional-info] is a string containing a
       system error. The @scheme[_additional-info] content is always
       parenthesizes, so @scheme[message] is a short error and
       @scheme[(string-append message " " additional-info)] is a
       verbose one.}

  }

}

@; ----------------------------------------------------------------------

@section{DrScheme Version Tool}

@defmodule[version/tool]

The @scheme[version/tool] library implements a DrScheme tool that

@itemize{

  @item{makes the patchlevel appears as a version @tt{p}@nonterm{N}
  suffix in DrScheme, though the base verion reported by
  @scheme[(version)] is not changed);}

  @item{if enabled by the user, periodically checks whether a
  new PLT Scheme distribution is available for download.}

}
