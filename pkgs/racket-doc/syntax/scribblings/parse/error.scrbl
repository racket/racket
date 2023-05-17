#lang scribble/doc
@(require scribble/manual
          scribble/struct
          scribble/decode
          scribble/eval
          "../common.rkt"
          "parse-common.rkt"
          (for-label syntax/parse/report-config))

@(define the-eval (make-sp-eval))

@title[#:tag "error"]{Configuring Error Reporting}

@defmodule[syntax/parse/report-config]

@history[#:added "8.9.0.5"]

@defparam[current-report-configuration config report-configuration?]{

 A parameter that determines parts error messages that are generated
 by @racket[syntax-parse] for failed matches. When
 @racket[syntax-parse] needs to report that a particular datum or
 literal identifier was expected, it consults the configuration in
 this parameter.

 A configuration is a hash table with the following keys:

 @itemlist[

  @item{@racket['datum-to-what] --- a procedure of one argument used
  to get a noun describing an expected datum, which appears in a
  pattern either with @racket[~datum], as ``self-quoting,'' or so on.
  The procedure's argument is the datum value. The result must be
  either a string or a list containing two strings; if two strings are
  provided, the first is used when a singular noun is needed, and the
  second is used as a plural noun.

  The default configuration returns @racket['("literal symbol"
  "literal symbols")] for a symbol and @racket['("literal"
  "literals")] for any other datum value.}

  @item{@racket['datum-to-string] --- a procedure of one argument, used
  to convert the datum value to a string that is included in the error
  message. The procedure's argument is the datum value, and the result
  must be a string.

  The default configuration formats a symbol value using
  @racket[(format "`~s'" v)] any other datum value using
  @racket[(format "~s" v)].}

  @item{@racket['literal-to-what] --- a procedure of one argument used
  to get a noun describing an expected literal identifier, which
  appears in a pattern with @racket[~literal], as declared with
  @racket[#:literals], or so on. The procedure's argument is an
  identifier when available, or a symbol when only simplified
  information has been preserved. The result must be either a string
  or a list containing two strings, like the result for a
  @racket['datum-to-what] procedure.

  The default configuration returns @racket['("identifier"
  "identifiers")].}

  @item{@racket['literal-to-string] --- a procedure of one argument,
  used to convert a literal identifier or symbol to a string that is
  included in the error message.

  The default configuration formats a symbol value using
  @racket[(format "`~s'" v)], and it formats an identifier the same
  after extracting its symbol with @racket[syntax-e].}

 ]

}


@defproc[(report-configuration? [v any/c]) boolean?]{

 Checks whether @racket[v] is an immutable hash table that maps each
 of the keys @racket['datum-to-what],
 @racket['datum-to-string] @racket['identifier-to-what] and
 @racket['identifier-to-string] to a procedure that accepts one argument.

}
