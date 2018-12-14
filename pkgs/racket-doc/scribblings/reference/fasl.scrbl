#lang scribble/doc
@(require "mz.rkt" (for-label racket/fasl))

@(define fasl-eval (make-base-eval))
@examples[#:hidden #:eval fasl-eval (require racket/fasl)]

@title[#:tag "fasl"]{Fast-Load Serialization}

@note-lib-only[racket/fasl]

@deftogether[(
@defproc[(s-exp->fasl [v any/c]
                      [out (or/c output-port? #f) #f]
                      [#:keep-mutable? keep-mutable? any/c #f])
         (or/c (void) bytes?)]
@defproc[(fasl->s-exp [in (or/c input-port? bytes?)]
                      [#:datum-intern? datum-intern? any/c #t])
         any/c]
)]{

The @racket[s-exp->fasl] function serializes @racket[v] to a byte
string, printing it directly to @racket[out] if @racket[out] is an
output port or returning the byte string otherwise. The
@racket[fasl->s-exp] function decodes a value from a byte string
(supplied either directly or as an input port) that was encoded with
@racket[s-exp->fasl].

The @racket[v] argument must be a value that could be @racket[quote]d
as a literal---that is, a value without syntax objects for which
@racket[(compile `(quote ,v))]
would work and be @racket[read]able after @racket[write]. The
byte string produced by @racket[s-exp->fasl] does not use the same
format as compiled code, however.

Like @racket[(compile `(quote ,v))], @racket[s-exp->fasl] does not
preserve graph structure, support cycles, or handle non-@tech{prefab}
structures. Compose @racket[s-exp->fasl] with @racket[serialize] to
preserve graph structure, handle cyclic data, and encode serializable
structures. The @racket[s-exp->fasl] and @racket[fasl->s-exp]
functions consult @racket[current-write-relative-directory] and
@racket[current-load-relative-directory], respectively, in the same
way as bytecode saving and loading to store paths in relative form,
and they similarly allow and convert constrained @racket[srcloc]
values (see @secref["print-compiled"]).

Unless @racket[keep-mutable?] is provided as true to
@racket[s-exp->fasl], then mutable values in @racket[v] are replaced
by immutable values when the result is decoded by
@racket[fasl->s-exp]. Unless @racket[datum-intern?] is provided as
@racket[#f], then any immutable value produced by @racket[fasl->s-exp]
is filtered by @racket[datum-intern-literal]. The defaults make the
composition of @racket[s-exp->fasl] and @racket[fasl->s-exp] behave
like the composition of @racket[write] and @racket[read].

The byte-string encoding produced by @racket[s-exp->fasl] is
independent of the Racket version, except as future Racket versions
introduce extensions that are not currently recognized. In particular,
the result of @racket[s-exp->fasl] will be valid as input to any
future version of @racket[fasl->s-exp].

@mz-examples[
#:eval fasl-eval
(define fasl (s-exp->fasl (list #("speed") 'racer #\!)))
fasl
(fasl->s-exp fasl)
]

@history[#:changed "6.90.0.21" @elem{Made @racket[s-exp->fasl] format version-independent
                                     and added the @racket[#:keep-mutable?]
                                     and @racket[#:datum-intern?] arguments.}]}

@; ----------------------------------------------------------------------

@close-eval[fasl-eval]
