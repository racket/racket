#lang scribble/doc
@(require "mz.rkt" (for-label racket/fasl))

@(define fasl-eval (make-base-eval))
@(interaction-eval #:eval fasl-eval (require racket/fasl))

@title[#:tag "fasl"]{Fast-Load Serialization}

@note-lib-only[racket/fasl]

@deftogether[(
@defproc[(s-exp->fasl [v any/c] [out (or/c output-port? #f) #f]) (or/c (void) bytes?)]
@defproc[(fasl->s-exp [in (or/c input-port? bytes?)]) any/c]
)]{

The @racket[s-exp->fasl] function serializes @racket[v] to a byte
string, printing it directly to @racket[out] if @racket[out] is an
output port or return the byte string otherwise. The
@racket[fasl->s-exp] function decodes a value from a byte string
(supplied either directly or as an input port) that was encoded with
@racket[s-exp->fasl].

The @racket[v] argument must be a value that could be @racket[quote]d
as a literal, because @racket[s-exp->fasl] essentially uses
@racket[(compile `(quote ,v))] to encode the value using Racket's
built-in fast-load format for bytecode.

The byte-string encoding produced by @racket[s-exp->fasl] is specific
to a version of Racket. That is, the resulting byte string can be
decoded back to @racket[v] only using the same version with which it
was encoded.

@mz-examples[
#:eval fasl-eval
(define fasl (s-exp->fasl (list #("speed") 'racer #\!)))
fasl
(fasl->s-exp fasl)
]}

@; ----------------------------------------------------------------------

@close-eval[fasl-eval]
