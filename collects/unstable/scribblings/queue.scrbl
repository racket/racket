#lang scribble/manual
@(require scribble/eval "utils.rkt" (for-label racket unstable/queue))

@title{Imperative Queues}

@defmodule[unstable/queue]

@unstable[@author+email["Carl Eastlund" "cce@racket-lang.org"]]

This module provides a mutable queue representation.

@defproc[(make-queue) queue/c]{
Produces an empty queue.
}

@defproc[(enqueue! [q queue/c] [v any/c]) void?]{
Adds an element to the back of a queue.
}

@defproc[(dequeue! [q nonempty-queue/c]) any/c]{
Removes an element from the front of a nonempty queue, and returns that element.

@defexamples[
#:eval (eval/require 'unstable/queue)
(define q (make-queue))
(enqueue! q 1)
(dequeue! q)
(enqueue! q 2)
(enqueue! q 3)
(dequeue! q)
(dequeue! q)
]
}

@defproc[(queue-empty? [q queue/c]) boolean?]{
Recognizes whether a queue is empty or not.

@defexamples[
#:eval (eval/require 'unstable/queue)
(define q (make-queue))
(queue-empty? q)
(enqueue! q 1)
(queue-empty? q)
(dequeue! q)
(queue-empty? q)
]
}

@defproc[(queue? [v any/c]) boolean?]{
This predicate recognizes queues.

@defexamples[
#:eval (eval/require 'unstable/queue)
(queue? (make-queue))
(queue? 'not-a-queue)
]
}

@deftogether[(
@defthing[queue/c flat-contract?]
@defthing[nonempty-queue/c flat-contract?]
)]{
These contracts recognize queues; the latter requires the queue to contain at
least one value.
}
