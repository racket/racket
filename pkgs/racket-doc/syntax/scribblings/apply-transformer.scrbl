#lang scribble/doc
@(require "common.rkt" (for-label syntax/apply-transformer))

@title[#:tag "syntax/apply-transformer"]{Applying Macro Transformers}

@defmodule[syntax/apply-transformer]

@defproc[(local-apply-transformer [transformer (or/c (-> syntax? syntax?) set!-transformer?)]
                                  [stx syntax?]
                                  [context (or/c 'expression 'top-level 'module 'module-begin list?)]
                                  [intdef-ctxs (listof internal-definition-context?) '()])
         syntax?]{

Applies @racket[transformer] as a @tech[#:doc refman]{syntax transformer} to @racket[stx] in the
current expansion context. The result is similar to expanding a use of an
@tech[#:doc refman]{identifier} bound as a @tech[#:doc refman]{syntax transformer} bound to
@racket[transformer] with @racket[local-expand], except that expansion is guaranteed to stop after
applying a single macro transformation (assuming @racket[transformer] does not explicitly force
further recursive expansion).

Unlike simply applying @racket[transformer] to @racket[stx] directly, using
@racket[local-apply-transformer] introduces the appropriate @tech[#:doc refman]{use-site scope} and
@tech[#:doc refman]{macro-introduction scope} that would be added by the expander.

The @racket[context] and @racket[intdef-ctxs] arguments are treated the same way as the corresponding
arguments to @racket[local-expand].

@history[#:added "6.90.0.29"]}
