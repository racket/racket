#lang scribble/doc
@(require scribble/manual
          scribble/eval
          "../scribble.ss"
          "eval.ss")
@(require (for-label scheme unstable/cce/queue))

@title[#:style 'quiet #:tag "cce-queue"]{Imperative Queues}

@defmodule[unstable/cce/queue]

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
#:eval (evaluator 'unstable/cce/queue)
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
#:eval (evaluator 'unstable/cce/queue)
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
#:eval (evaluator 'unstable/cce/queue)
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
