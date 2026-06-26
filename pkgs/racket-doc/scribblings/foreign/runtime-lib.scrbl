#lang scribble/doc
@(require "utils.rkt"
          (for-label ffi/unsafe/runtime-lib))

@title[#:tag "runtime-lib"]{Declaring Foreign Libraries Needed at Run Time}

@defmodule[ffi/unsafe/runtime-lib]{The
@racketmodname[ffi/unsafe/runtime-lib] library is similar to
@racketmodname[racket/runtime-path] (and builds on it), but provides more
support for platform-specific choices of libraries.}

@history[#:added "9.2.0.6"]

@defform[#:literals(else so or and)
         (define-runtime-lib id
            maybe-ffi-lib-args
            [platform-spec lib-spec ...]
            ...
            [else else-body ...+])
         #:grammar ([platform-spec os-id
                                   os*-id
                                   arch-id
                                   platform-string
                                   32
                                   64
                                   (or platform-spec ...)
                                   (and platform-spec ...)]
                    [lib-spec (so lib-string)
                              (so lib-string (vers ...))]
                    [vers string
                          #f]
                    [maybe-ffi-lib-args code:blank
                                        (code:line #:ffi-lib-args (arg ...))])]{

 Similar to @racket[define-runtime-path], but binds @racket[id] to a
 result from @racket[ffi-lib] for a platform that matches one of the
 @racket[platform-spec]s (which are tried in order), or to the result
 of the @racket[else-body] sequence otherwise.

 A @racket[platform-spec] implies a set of libraries that are loaded
 in order as enumerated by the accompanying @racket[lib-spec]s, where
 the @racket[lib-string] within a @racket[lib-spec] becomes the first
 argument to @racket[ffi-lib], and the @racket[vers] sequence (if
 present) is quoted as the second argument. The defined @racket[id] is
 bound to a @racket[ffi-lib] result for the last @racket[lib-spec], or
 to @racket[#f] if no @racket[lib-spec]s are present in the matching
 clause. Besides loading the library for each @racket[lib-spec] at run
 time, the libraries are declared at compile time for use by tools
 such as @exec{raco exe} and @exec{raco dist}. Cross compilation is
 handled automatically, so that the target platforms libraries are
 listed at compile time, while run time loads libraries suitable to
 the host platform.

 If no @racket[platform-spec] matches, then @racket[id] is bound to
 the result of the @racket[else-body] sequence---which does not have
 to be a result from @racket[ffi-lib], but typically it is. The
 @racket[else-body] sequence is evaluated only at run-time, and no
 libraries are declared at compile time.

 Each @racket[platform-spec] is is compared to the result of
 @racket[(system-type 'os)], @racket[(system-type 'os*)],
 @racket[(system-type 'arch)], @racket[(system-type 'platform)],
 and/or @racket[(system-type 'word)]. In an @racket[and] form, all
 @racket[platform-spec]s much match, while only one matching
 @racket[platform-spec] is needed ro an @racket[or] form.

 Extra keyword arguments can be supplied the run-time @racket[ffi-lib]
 call for a @racket[platform-spec] match through an optional
 @racket[#:ffi-lib-args] declaration. There's a single
 @racket[#:ffi-lib-args] declaration, because it needs to be
 independent of the @racket[platform-spec] that turns out to match.
 The run-time call receives either an absolute path based on resolved
 @racket[lib-spec], or it receives just the initial
 @racket[lib-string] within a @racket[lib-spec] (due to limitations of
 @racket[define-runtime-path]), so @racket[#:fail] is the most likely
 useful extra argument to @racket[ffi-lib].

}
