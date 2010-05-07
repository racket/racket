#lang scribble/doc
@(require scribble/manual
          scribble/bnf
          (for-label version/patchlevel
                     version/check
                     version/tool
                     version/utils
                     scheme/base
                     scheme/contract))

@(define (selflink s) (link s (tt s)))

@title{@bold{Version}: Racket Version Checking}

The version collection contains several version-related pieces that
are used by Racket. See also @scheme[version] from
@schememodname[scheme/base].

@; ----------------------------------------------------------------------

@section{Installed Patch Level}

@defmodule[version/patchlevel]

@defthing[patchlevel exact-nonnegative-integer?]{

Indicates the current installed patch level, which is normally zero,
but may be updated by patches to DrRacket.}

@; ----------------------------------------

@section{Checking Available Versions}

@defmodule[version/check]

@defproc[(check-version) (or/c symbol? list?)]{

Checks the currently available version on the PLT website
(@selflink["http://download.plt-scheme.org"]) and returns a value that
indicates the current state of the curent installation:

  @itemize[

    @item{@scheme[`ok] --- You're fine.}

    @item{@scheme[`(ok-but ,_version)] --- You have a fine stable
      version, but note that there is a newer alpha version available
      numbered @scheme[_version].}

    @item{@scheme[`(newer ,_version)] --- You have an old
      version.  Please upgrade to @scheme[_version].}

    @item{@scheme[`(newer ,_version ,_alpha)] --- You have an
      old-but-stable version, please upgrade to @scheme[_version]; you
      may consider also the newer alpha version numbered
      @scheme[_alpha].}

    @item{@scheme[`(error ,_message)] --- An error occurred, and
      @scheme[_message] is a string that indicates the error.}

    @item{@scheme[`(error ,_message ,_additional-info)] --- An error
       occurred; @scheme[_message] is a string that indicates the
       error, and @scheme[_additional-info] is a string containing a
       system error.  The @scheme[_additional-info] content is always
       parenthesizes, so @scheme[message] is a short error and
       @scheme[(string-append message " " additional-info)] is a
       verbose one.}

  ]

}

@; ----------------------------------------------------------------------

@section{DrRacket Version Tool}

@defmodule[version/tool]

The @scheme[version/tool] library implements a DrRacket tool that

@itemize[

  @item{makes the patchlevel display as a version @tt{p}@nonterm{N}
  suffix in DrRacket (though the base verion reported by
  @scheme[(version)] is not changed);}

  @item{if enabled by the user, periodically checks whether a
  new Racket distribution is available for download.}

]

@; ----------------------------------------------------------------------

@section{Version Utilities}

@defmodule[version/utils]{

The @schememodname[version/utils] library provides a few of convenient
utilities for dealing with version strings.  Unless explicitly noted,
these functions do not handle legacy versions of Racket.}

@defproc[(valid-version? [str string?]) boolean?]{

Returns @scheme[#t] if @scheme[str] is a valid Racket version
string, @scheme[#f] otherwise.}

@defproc[(version->list [str valid-version?])
         (list integer? integer? integer? integer?)]{

Returns a list of four numbers that the given version string
represent.  @scheme[str] is assumed to be a valid version.}

@defproc[(version<? [str1 valid-version?] [str2 valid-version?]) boolean?]{

Returns @scheme[#t] if @scheme[str1] represents a version that is
strictly smaller than @scheme[str2], @scheme[#f] otherwise.
@scheme[str1] and @scheme[str2] are assumed to be valid versions.}

@defproc[(version<=? [str1 valid-version?] [str2 valid-version?]) boolean?]{

Returns @scheme[#t] if @scheme[str1] represents a version that is
smaller than or equal to @scheme[str2], @scheme[#f] otherwise.
@scheme[str1] and @scheme[str2] are assumed to be valid versions.}

@defproc[(alpha-version? [str valid-version?]) boolean?]{

Returns @scheme[#t] if the version that @scheme[str] represents is an
alpha version.  @scheme[str] is assumed to be a valid version.}

@defproc[(version->integer [str string?]) (or/c integer? false/c)]{

Converts the version string into an integer.  For version
@scheme["X.YY.ZZZ.WWW"], the result will be @schemevalfont{XYYZZZWWW}.
This function works also for legacy Racket versions, by
translating @scheme["XYY.ZZZ"] to @schemevalfont{XYYZZZ000}.  The
resulting integer can thefore be used to conveniently compare any two
(valid) version strings.  If the version string is invalid the
resulting value is @scheme[#f].

Note that this is the only function that deals with legacy version
strings.}
