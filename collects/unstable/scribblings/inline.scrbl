#lang scribble/doc
@(require scribble/base
          scribble/manual
          "utils.rkt"
          (for-label unstable/inline
                     racket/contract
                     racket/base
		     racket/performance-hint))

@title[#:tag "inline"]{Inlining}
@unstable-header[]

@defmodule[unstable/inline]

@addition{Vincent St-Amour}

@defform*/subs[[(define-inline id expr)
                (define-inline (head args) body ...+)]
                ([head id
                       (head args)]
                 [args (code:line arg ...)
                       (code:line arg ... @#,racketparenfont{.} rest-id)]
                 [arg arg-id
                      [arg-id default-expr]
                      (code:line keyword arg-id)
                      (code:line keyword [arg-id default-expr])])]{
Like @racket[define], but ensures that the definition will be inlined at its
call sites. Recursive calls are not inlined, to avoid infinite inlining.
Higher-order uses are supported, but also not inlined.

@racket[define-inline] may interfere with the Racket compiler's own inlining
heuristics, and should only be used when other inlining attempts (such as
@racket[begin-encourage-inline]) fail.
}
