#lang scribble/doc
@(require "common.rkt"
          (for-label mzlib/string
                     scheme/contract
                     (only-in scheme/base
                              regexp-try-match)))

@mzlib[#:mode title string]

@deprecated[@racketmodname[racket/base]]{
Also see @racketmodname[racket/string]
}

The @racketmodname[mzlib/string] library re-exports several functions
from @racketmodname[scheme/base]:

@racketblock[
real->decimal-string
regexp-quote
regexp-replace-quote
regexp-match*
regexp-match-positions*
regexp-match-peek-positions*
regexp-split
regexp-match-exact?
]

It also re-exports @racket[regexp-try-match] as
@racket[regexp-match/fail-without-reading].


@defproc[(glob->regexp [str (or/c string bytes?)?]
                       [hide-dots? any/c #t]
                       [case-sensitive? any/c (eq? (system-path-convention-type)'unix)]
                       [simple? any/c #f])
         (or/c regexp? byte-regexp?)]{

Produces a regexp for a an input ``glob pattern'' @racket[str].  A
glob pattern is one that matches @litchar{*} with any string,
@litchar{?} with a single character, and character ranges are the same
as in regexps (unless @racket[simple?] is true). In addition, the
resulting regexp does not match strings that begin with @litchar{.},
unless @racket[str] begins with @litchar{.} or @racket[hide-dots?] is
@racket[#f].  The resulting regexp can be used with string file names
to check the glob pattern.  If the glob pattern is provided as a byte
string, the result is a byte regexp.

The @racket[case-sensitive?] argument determines whether the resulting
regexp is case-sensitive.

If @racket[simple?] is true, then ranges with
@litchar{[}...@litchar{]} in @racket[str] are treated as literal
character sequences.}


@defproc[(string-lowercase! [str (and/c string? (not/c immutable?))]) void?]{

Destructively changes @racket[str] to contain only lowercase
characters.}


@defproc[(string-uppercase! [str (and/c string? (not/c immutable?))]) void?]{

Destructively changes @racket[str] to contain only uppercase
characters.}



@defproc[(eval-string [str (or/c string? bytes?)]
                      [err-handler (or/c false/c
                                         (any/c . -> . any/c)
                                         (-> any/c))
                                   #f])
         list?]{

Reads and evaluates S-expressions from @racket[str], returning results
for all of the expressions in the string. If any expression produces
multiple results, the results are spliced into the resulting list.  If
@racket[str] contains only whitespace and comments, an empty list is
returned, and if @racket[str] contains multiple expressions, the
result will be contain multiple values from all subexpressions.

The @racket[err-handler] argument can be:
@itemize[
@item{@racket[#f] (the default) which means that errors are not
      caught;}
@item{a one-argument procedure, which will be used with an exception
      (when an error occurs) and its result will be returned}
@item{a thunk, which will be used to produce a result.}
]}


@defproc[(expr->string [expr any/c]) string?]{

Prints @racket[expr] into a string and returns the string.}


@defproc[(read-from-string [str (or/c string? bytes?)]
                           [err-handler (or/c false/c
                                              (any/c . -> . any/c)
                                              (-> any/c))
                                        #f])
          any/c]{

Reads the first S-expression from @racket[str] and returns it.  The
@racket[err-handler] is as in @racket[eval-string].}


@defproc[(read-from-string-all [str (or/c string? bytes?)]
                               [err-handler (or/c false/c
                                                  (any/c . -> . any/c)
                                                  (-> any/c))
                                            #f])
          list?]{

Reads all S-expressions from the string (or byte string) @racket[str]
and returns them in a list.  The @racket[err-handler] is as in
@racket[eval-string].}
