#lang scribble/manual
@(require "utils.rkt"
          (for-label racket/base
                     racket/future
                     unstable/future))

@title[#:tag "future"]{Futures}
@unstable[@author+email["Ryan Culpepper" "ryanc@racket-lang.org"]]

@defmodule[unstable/future]

@deftogether[[
@defform[(for/async (for-clause ...) body ...+)]
@defform[(for*/async (for-clause ...) body ...+)]]]{

Like @racket[for] and @racket[for*], but each iteration of the
@racket[body] is executed in a separate @racket[future], and
the futures may be @racket[touch]ed in any order.
}
