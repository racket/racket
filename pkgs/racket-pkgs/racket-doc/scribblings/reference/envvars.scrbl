#lang scribble/doc
@(require "mz.rkt")

@title[#:tag "envvars"]{Environment Variables}

An @deftech{environment variable set} encapsulates a partial mapping
from byte strings to bytes strings. A Racket process's initial
@tech{environment variable set} is connected to the operating system's
environment variables: accesses or changes to the set read or change
operating-system environment variables for the Racket process.

Since Windows environment variables are case-insensitive,
@tech{environment variable set}'s key byte strings on Windows are
case-folded. More precisely, key byte strings are coerced to a UTF-8
encoding of characters that are converted to lowercase via
@racket[string-locale-downcase].

The current @tech{environment variable set}, which is determined by
the @racket[current-environment-variables] parameter, is propagated to
a @tech{subprocess} when the @tech{subprocess} is created.


@defproc[(environment-variables? [v any/c]) boolean?]{

Returns @racket[#t] if @racket[v] is an @tech{environment variable
set}, @racket[#f] otherwise.}


@defparam[current-environment-variables env environment-variables?]{

A @tech{parameter} that determines the @tech{environment variable set}
that is propagated to a @tech{subprocess} and that is used as the
default set for @racket[getenv] and @racket[putenv].}


@defproc[(bytes-environment-variable-name? [v any/c]) boolean?]{

Returns @racket[#t] if @racket[v] is a byte string and if it is valid
for an environment variable name. An environment variable name must
contain no bytes with the value @racket[0] or @racket[61], where
@racket[61] is @racket[(char->integer #\=)]. On Windows, an
environment variable name also must have a non-zero length.}


@defproc[(make-environment-variables [name bytes-environment-variable-name?]
                                     [val bytes-no-nuls?]
                                     ... ...)
         environment-variables?]{

Creates a fresh @tech{environment variable set} that is initialized
with the given @racket[name] to @racket[val] mappings.}


@defproc[(environment-variables-ref [env environment-variables?]
                                    [name bytes-environment-variable-name?])
         (or/c #f (and/c bytes-no-nuls? immutable?))]{

Returns the mapping for @racket[name] in @racket[env], returning
@racket[#f] if @racket[name] has no mapping.

Normally, @racket[name] should be a byte-string encoding of a string
using the default encoding of the current @tech{locale}. On Windows,
@racket[name] is coerced to a UTF-8 encoding and case-normalized.}


@defproc[(environment-variables-set! [env environment-variables?]
                                     [name bytes-environment-variable-name?]
                                     [maybe-bstr (or/c bytes-no-nuls? #f)]
                                     [fail (-> any)
                                           (lambda ()
                                             (raise (make-exn:fail ....)))])
         any]{

Changes the mapping for @racket[name] in @racket[env] to
@racket[maybe-bstr].  If @racket[maybe-bstr] is @racket[#f] and
@racket[env] is the initial @tech{environment variable set} of the
Racket process, then the operating system environment-variable mapping
for @racket[name] is removed.

Normally, @racket[name] and @racket[maybe-bstr] should be a
byte-string encoding of a string using the default encoding of the
current @tech{locale}. On Windows, @racket[name] is 
coerced to a UTF-8 encoding and case-normalized, and
@racket[maybe-bstr] is coerced to a UTF-8 encoding if @racket[env] is
the initial @tech{environment variable set} of the Racket process.

On success, the result of @racket[environment-variables-set!] is
@|void-const|. If @racket[env] is the initial @tech{environment
variable set} of the Racket process, then attempting to adjust the
operating system environment-variable mapping might fail for some reason,
in which case @racket[fail] is called in tail position with respect to the
@racket[environment-variables-set!]. The default @racket[fail] raises
an exception.}


@defproc[(environment-variables-names [env environment-variables?])
         (listof (and/c bytes-environment-variable-name? immutable?))]{

Returns a list of byte strings that corresponds to names mapped by
@racket[env].}


@defproc[(environment-variables-copy [env environment-variables?])
         environment-variables?]{

Returns an @tech{environment variable set} that is initialized with
the same mappings as @racket[env].}


@deftogether[(
@defproc[(getenv [name string-environment-variable-name?]) 
                 (or/c string-no-nuls? #f)]
@defproc[(putenv [name string-environment-variable-name?]
                 [value string-no-nuls?]) boolean?]
)]{

Convenience wrappers for @racket[environment-variables-ref] and
@racket[environment-variables-set!] that convert between strings and
byte strings using the current @tech{locale}'s default encoding (using
@racket[#\?] as the replacement character for encoding errors) and
always using the current @tech{environment variable set} from
@racket[current-environment-variables]. The @racket[putenv] function
returns @racket[#t] for success and @racket[#f] for failure.}


@defproc[(string-environment-variable-name? [v any/c]) boolean?]{

Returns @racket[#t] if @racket[v] is a string and if its encoding
using the current @tech{locale}'s encoding is valid for an environment
variable name according to @racket[bytes-environment-variable-name?].}

