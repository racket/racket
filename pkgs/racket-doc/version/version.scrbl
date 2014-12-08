#lang scribble/doc
@(require scribble/manual
          scribble/bnf
          (for-label version/patchlevel
                     version/check
                     version/utils
                     racket/base
                     racket/contract))

@(define (selflink s) (link s (tt s)))

@title{Version: Racket Version Checking}

The @filepath{version} collection contains several version-related
pieces that are used by Racket.  See also @racket[version] from
@racketmodname[racket/base].

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
(@selflink["http://download.racket-lang.org"]) and returns a value that
indicates the current state of the curent installation:

@itemize[

@item{@racket[`ok] --- You're fine.}

@item{@racket[`(ok-but ,_version)] --- You have a fine stable
  version, but note that there is a newer alpha version available
  numbered @racket[_version].}

@item{@racket[`(newer ,_version)] --- You have an old
  version.  Please upgrade to @racket[_version].}

@item{@racket[`(newer ,_version ,_alpha)] --- You have an
  old-but-stable version, please upgrade to @racket[_version]; you
  may consider also the newer alpha version numbered
  @racket[_alpha].}

@item{@racket[`(error ,_message)] --- An error occurred, and
  @racket[_message] is a string that indicates the error.}

@item{@racket[`(error ,_message ,_additional-info)] --- An error
   occurred; @racket[_message] is a string that indicates the
   error, and @racket[_additional-info] is a string containing a
   system error.  The @racket[_additional-info] content is always
   parenthesizes, so @racket[message] is a short error and
   @racket[(string-append message " " additional-info)] is a
   verbose one.}

]}

@; ----------------------------------------------------------------------

@section{Version Utilities}

@defmodule[version/utils]{
  The @racketmodname[version/utils] library provides a few of convenient
  utilities for dealing with version strings.  Unless explicitly noted,
  these functions do not handle legacy versions of Racket.}

@defproc[(valid-version? [str string?]) boolean?]{
  Returns @racket[#t] if @racket[str] is a valid Racket version
  string, @racket[#f] otherwise.}

@defproc[(version->list [str valid-version?])
         (list integer? integer? integer? integer?)]{
  Returns a list of four numbers that the given version string
  represent.  @racket[str] is assumed to be a valid version.}

@defproc[(version<? [str1 valid-version?] [str2 valid-version?]) boolean?]{
  Returns @racket[#t] if @racket[str1] represents a version that is
  strictly smaller than @racket[str2], @racket[#f] otherwise.
  @racket[str1] and @racket[str2] are assumed to be valid versions.}

@defproc[(version<=? [str1 valid-version?] [str2 valid-version?]) boolean?]{
  Returns @racket[#t] if @racket[str1] represents a version that is
  smaller than or equal to @racket[str2], @racket[#f] otherwise.
  @racket[str1] and @racket[str2] are assumed to be valid versions.}

@defproc[(alpha-version? [str valid-version?]) boolean?]{
  Returns @racket[#t] if the version that @racket[str] represents is an
  alpha version.  @racket[str] is assumed to be a valid version.}

@defproc[(version->integer [str string?]) (or/c integer? false/c)]{
  Converts the version string into an integer.  For version
  @racket["X.YY.ZZZ.WWW"], the result will be @racketvalfont{XYYZZZWWW}.
  This function works also for legacy Racket versions, by
  translating @racket["XYY.ZZZ"] to @racketvalfont{XYYZZZ000}.  The
  resulting integer can thefore be used to conveniently compare any two
  (valid) version strings.  If the version string is invalid the
  resulting value is @racket[#f].

  Note that this is the only function that deals with legacy version
  strings.}
