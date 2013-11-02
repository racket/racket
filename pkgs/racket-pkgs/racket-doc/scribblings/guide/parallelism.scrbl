#lang scribble/doc
@(require scribble/manual "guide-utils.rkt"
          (for-label racket/flonum
                     racket/unsafe/ops
                     racket/performance-hint))

@title[#:tag "parallelism"]{Parallelism}

Racket provides two forms of @deftech{parallelism}: @tech{futures} and
@tech{places}. On a platform that provides multiple processors,
parallelism can improve the run-time performance of a program.

See also @secref["performance"] for information on sequential
performance in Racket. Racket also provides threads for
@tech{concurrency}, but threads do not provide parallelism; see
@secref["concurrency"] for more information.

@include-section["futures.scrbl"]
@include-section["places.scrbl"]
@include-section["distributed.scrbl"]
