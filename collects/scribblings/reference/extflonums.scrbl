#lang scribble/doc
@(require "mz.rkt" (for-label racket/extflonum
                              racket/flonum))

@title[#:tag "extflonums"]{Extflonums}

@defmodule[racket/extflonum]

An @deftech{extflonum} is an extended-precision (80-bit)
floating-point number. extflonum arithmetic is supported on
platforms with extended-precision hardware and where the
extflonum implementation does not conflict with normal
double-precision arithmetic (i.e., on x86 and x86_64 platforms when
Racket is compiled to use SSE instructions for floating-point
operations, and on Windows when @as-index{@filepath{longdouble.dll}}
is available).

A extflonum is @bold{not} a @tech{number} in the sense of
@racket[number?]. Only extflonum-specific operations such as 
@racket[extfl+] perform extflonum arithmetic.

A literal extflonum is written like an @tech{inexact number},
but using an explicit @litchar{t} or @litchar{T} exponent marker (see
@secref["parse-extflonum"]). For example, @racket[3.5t0] is an
extflonum. The extflonum infinities and non-a-number values are
@as-index{@racket[+inf.t]}, @as-index{@racket[-inf.t]}, 
and @as-index{@racket[+nan.t]}.

If @racket[(extflonum-available?)] produces @racket[#f], then all
operations exported by @racketmodname[racket/extflonum] raise
@racket[exn:fail:unsupported], except for @racket[extflonum?],
@racket[extflonum-available?], and @racket[extflvector?] (which always
work). The reader (see @secref["reader"]) always accepts extflonum
input; when extflonum operations are not supported, printing an
extflonum from the reader uses its source notation (as opposed to
normalizing the format).

Two extflonums are @racket[equal?] if @racket[extfl=]
produces @racket[#t] for the extflonums. If extflonums
are not supported in a platform, extflonums are @racket[equal?]
only if they are @racket[eq?].

@defproc[(extflonum? [v any/c]) boolean?]{

Returns @racket[#t] if @racket[v] is an extflonum, @racket[#f]
otherwise.}

@defproc[(extflonum-available?) boolean?]{

Returns @racket[#t] if @tech{extflonum} operations are supported on the
current platform, @racket[#f] otherwise.}

@; ------------------------------------------------------------------------

@section{Extflonum Arithmetic}

@deftogether[(
@defproc[(extfl+ [a extflonum?] [b extflonum?]) extflonum?]
@defproc[(extfl- [a extflonum?] [b extflonum?]) extflonum?]
@defproc[(extfl* [a extflonum?] [b extflonum?]) extflonum?]
@defproc[(extfl/ [a extflonum?] [b extflonum?]) extflonum?]
@defproc[(extflabs [a extflonum?]) extflonum?]
)]{

Like @racket[fl+], @racket[fl-], @racket[fl*], @racket[fl/], and @racket[flabs],
but for @tech{extflonums}.}

@deftogether[(
@defproc[(extfl=   [a extflonum?] [b extflonum?]) boolean?]
@defproc[(extfl<   [a extflonum?] [b extflonum?]) boolean?]
@defproc[(extfl>   [a extflonum?] [b extflonum?]) boolean?]
@defproc[(extfl<=  [a extflonum?] [b extflonum?]) boolean?]
@defproc[(extfl>=  [a extflonum?] [b extflonum?]) boolean?]
@defproc[(extflmin [a extflonum?] [b extflonum?]) extflonum?]
@defproc[(extflmax [a extflonum?] [b extflonum?]) extflonum?]
)]{

Like @racket[fl=], @racket[fl<], @racket[fl>], @racket[fl<=], @racket[fl>=],
@racket[flmin], and @racket[flmax], but for @tech{extflonums}.}

@deftogether[(
@defproc[(extflround    [a extflonum?]) extflonum?]
@defproc[(extflfloor    [a extflonum?]) extflonum?]
@defproc[(extflceiling  [a extflonum?]) extflonum?]
@defproc[(extfltruncate [a extflonum?]) extflonum?]
)]{

Like @racket[flround], @racket[flfloor], @racket[flceiling], and
@racket[fltruncate], but for @tech{extflonums}.}

@deftogether[(
@defproc[(extflsin  [a extflonum?]) extflonum?]
@defproc[(extflcos  [a extflonum?]) extflonum?]
@defproc[(extfltan  [a extflonum?]) extflonum?]
@defproc[(extflasin [a extflonum?]) extflonum?]
@defproc[(extflacos [a extflonum?]) extflonum?]
@defproc[(extflatan [a extflonum?]) extflonum?]
@defproc[(extfllog  [a extflonum?]) extflonum?]
@defproc[(extflexp  [a extflonum?]) extflonum?]
@defproc[(extflsqrt [a extflonum?]) extflonum?]
@defproc[(extflexpt  [a extflonum?] [b extflonum?]) extflonum?]
)]{

Like @racket[flsin], @racket[flcos], @racket[fltan], @racket[flasin],
@racket[flacos], @racket[flatan], @racket[fllog], @racket[flexp], and
@racket[flsqrt], and @racket[flexpt], but for @tech{extflonums}.}

@deftogether[(
@defproc[(->extfl [a exact-integer?]) extflonum?]
@defproc[(extfl->exact-integer [a extflonum?]) exact-integer?]
@defproc[(real->extfl [a real?]) extflonum?]
@defproc[(extfl->exact [a real?]) (and/c real? exact?)]
@defproc[(extfl->inexact [a real?]) flonum?]
)]{

The first four are like @racket[->fl], @racket[fl->exact],
@racket[fl->real], @racket[inexact->exact], but for @tech{extflonums}.
The @racket[extfl->inexact] function converts a @tech{extflonum} to
its closest @racket{flonum} approximation.}

@; ------------------------------------------------------------------------

@section[#:tag "extflvectors"]{Extflonum Vectors}

An @deftech{extflvector} is like an @tech{flvector}, but it holds only
@tech{extflonums}. See also @secref["unsafeextfl"].

Two @tech{extflvectors} are @racket[equal?] if they have the same length,
and if the values in corresponding slots of the @tech{extflvectors} are
@racket[equal?].

@deftogether[(
@defproc[(extflvector? [v any/c]) boolean?]
@defproc[(extflvector [x extflonum?] ...) extflvector?]
@defproc[(make-extflvector [size exact-nonnegative-integer?]
                           [x extflonum? 0.0l0])
         extflvector?]
@defproc[(extflvector-length [vec extflvector?]) exact-nonnegative-integer?]
@defproc[(extflvector-ref [vec extflvector?] [pos exact-nonnegative-integer?])
         extflonum?]
@defproc[(extflvector-set! [vec extflvector?] [pos exact-nonnegative-integer?]
                           [x extflonum?])
         extflonum?]
@defproc[(extflvector-copy [vec extflvector?]
                           [start exact-nonnegative-integer? 0]
                           [end exact-nonnegative-integer? (vector-length v)]) 
         extflvector?]
)]{

Like @racket[flvector?], @racket[flvector], @racket[make-flvector],
@racket[flvector-length], @racket[flvector-ref], @racket[flvector-set],
and @racket[flvector-copy], but for @tech{extflvectors}.}

@deftogether[(
@defproc[(in-extflvector [vec extflvector?]
                         [start exact-nonnegative-integer? 0]
                         [stop (or/c exact-integer? #f) #f]
                         [step (and/c exact-integer? (not/c zero?)) 1])
         sequence?]
@defform[(for/extflvector maybe-length (for-clause ...) body ...)]
@defform/subs[(for*/extflvector maybe-length (for-clause ...) body ...)
              ([maybe-length (code:line)
                             (code:line #:length length-expr)
                             (code:line #:length length-expr #:fill fill-expr)])
              #:contracts ([length-expr exact-nonnegative-integer?]
                           [fill-expr extflonum?])]
)]{

Like @racket[in-flvector], @racket[for/flvector], and @racket[for*/flvector],
but for @tech{extflvectors}.}

@defproc[(make-shared-extflvector [size exact-nonnegative-integer?]
                                  [x extflonum? 0.0l0]) 
         extflvector?]{

Like @racket[make-shared-flvector], but for @tech{extflvectors}.}

@; ------------------------------------------------------------

@section[#:tag "extflutils"]{Extflonum Byte Strings}

@defproc[(floating-point-bytes->extfl [bstr bytes?]
                                      [big-endian? any/c (system-big-endian?)]
                                      [start exact-nonnegative-integer? 0]
                                      [end exact-nonnegative-integer? (bytes-length bstr)])
         extflonum?]{

Like @racket[floating-point-bytes->real], but  for @tech{extflonums}:
Converts the extended-precision floating-point number encoded in
@racket[bstr] from position @racket[start] (inclusive) to @racket[end]
(exclusive) to an @tech{extflonum}. The difference between
@racket[start] an @racket[end] must be 10 bytes.}


@defproc[(extfl->floating-point-bytes [x extflonum?]
                                      [big-endian? any/c (system-big-endian?)]
                                      [dest-bstr (and/c bytes? (not/c immutable?))
                                                 (make-bytes 10)]
                                      [start exact-nonnegative-integer? 0])
          bytes?]{

Like @racket[real->floating-point-bytes], but  for @tech{extflonums}:
Converts @racket[x] to its representation in a byte
string of length 10.}
