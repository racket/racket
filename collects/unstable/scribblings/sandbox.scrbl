#lang scribble/manual
@(require scribble/eval "utils.rkt"
          (for-label racket/base racket/contract unstable/sandbox scribble/eval))

@title[#:tag "sandbox"]{Sandbox}
@unstable[@author+email["Ryan Culpepper" "ryanc@racket-lang.org"]]

@defmodule[unstable/sandbox]

@defproc[(make-log-based-eval [log-file path-string?]
                              [mode (or/c 'record 'replay)])
         (-> any/c any)]{

Creates an evaluator (like @racket[make-base-eval]) that uses a log
file to either record or replay evaluations.

If @racket[mode] is @racket['record], the evaluator records every
interaction to @racket[log-file], replacing @racket[log-file] if it
already exists. The result of each interaction must be
@seclink["serialization" #:doc '(lib
"scribblings/reference/reference.scrbl")]{serializable}.

If @racket[mode] is @racket['replay], the evaluator uses the contents
of @racket[log-file] instead of actually performing evaluatings. For
each interaction, it compares the term to evaluate against the next
interaction recorded in @racket[log-file]. If the term matches, the
stored result is returned; if not, the evaluator raises an error
indicating that it is out of sync with @racket[log-file].

Use @racket[make-log-based-eval] to document libraries when the
embedded examples rely on external features that may not be present or
appropriately configured on all machines.
}
