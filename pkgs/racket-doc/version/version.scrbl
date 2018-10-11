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
indicates the current state of the current installation:

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
  utilities for dealing with version strings.}

@defproc[(valid-version? [v any/c]) boolean?]{
  Returns @racket[#t] if @racket[v] is a valid Racket version
  string, @racket[#f] otherwise.

  A valid version has one of the following forms:

  @itemlist[
    @item{@nonterm{maj}@litchar{.}@nonterm{min}}
    @item{@nonterm{maj}@litchar{.}@nonterm{min}@litchar{.}@nonterm{sub}}
    @item{@nonterm{maj}@litchar{.}@nonterm{min}@litchar{.}@nonterm{sub}@litchar{.}@nonterm{rel}}
  ]

  subject to the following constraints:

  @itemlist[

     @item{@nonterm{maj}, @nonterm{min}, @nonterm{sub}, and
           @nonterm{rel} are all canonical decimal representations of
           natural numbers (i.e., decimal digits with no leading
           @litchar{0} unless the number is exactly @litchar{0})}

    @item{@nonterm{rel} is not @litchar{0}}

    @item{@nonterm{sub} is not @litchar{0} unless @nonterm{rel} is included}

    @item{@nonterm{min} has no more than two digits}

    @item{@nonterm{sub} and @nonterm{rel} have no more than three digits}

  ]

  The constraints force version numbers to be in a canonical form. For
  example, a would-be version string @racket["4.3.0"] must be written
  instead as @racket["4.3"], @racket["4.3.1.0"] must be written
  instead as @racket["4.3.1"], and @racket["4"] must be written as
  @racket["4.0"].}
  
@defproc[(version->list [str valid-version?])
         (list/c integer? integer? integer? integer?)]{
  Returns a list of four numbers that the given version string
  represent.}

@defproc[(version<? [str1 valid-version?] [str2 valid-version?]) boolean?]{
  Returns @racket[#t] if @racket[str1] represents a version that is
  strictly smaller than @racket[str2], @racket[#f] otherwise.}

@defproc[(version<=? [str1 valid-version?] [str2 valid-version?]) boolean?]{
  Returns @racket[#t] if @racket[str1] represents a version that is
  smaller than or equal to @racket[str2], @racket[#f] otherwise.}

@defproc[(alpha-version? [str valid-version?]) boolean?]{
  Returns @racket[#t] if the version that @racket[str] represents is an
  alpha version.

  A version number of the form @nonterm{maj}@litchar{.}@nonterm{min},
  @nonterm{maj}@litchar{.}@nonterm{min}@litchar{.}@nonterm{sub},
  or @nonterm{maj}@litchar{.}@nonterm{min}@litchar{.}@nonterm{sub}@litchar{.}@nonterm{rel}
  is an alpha version if @nonterm{min} is @litchar{90} or more,
  @nonterm{sub} is @litchar{900} or more, or @nonterm{rel} is
  @litchar{900} or more.}

@defproc[(version->integer [str string?]) (or/c integer? #f)]{
  Converts the version string into an integer.  For version
  @racket["X.YY.ZZZ.WWW"], the result will be @racketvalfont{XYYZZZWWW}.
  This function works also for legacy Racket versions by
  translating @racket["XYY.ZZZ"] to @racketvalfont{XYYZZZ000}.  The
  resulting integer can thefore be used to conveniently compare any two
  (valid) version strings. If the version string is invalid as either a
  regular version string or a legacy version string, the resulting
  value is @racket[#f].

  Note that this is the only function that deals with legacy version
  strings.}
