#lang scribble/manual
@(require scribble/eval (for-label racket data/queue))
@(define qeval (make-base-eval))
@(qeval '(require data/queue))

@title{Imperative Queues}

@defmodule[data/queue]

@author[@author+email["Carl Eastlund" "cce@racket-lang.org"]]

This module provides a simple mutable queue representation,
first-in/first-out only.  Operations on queues mutate it in a
thread-unsafe way.

@defproc[(make-queue) queue/c]{
  Produces an empty queue.
}

@defproc[(enqueue! [q queue/c] [v any/c]) void?]{
  Adds an element to the back of a queue.
}

@defproc[(dequeue! [q nonempty-queue/c]) any/c]{
  Removes an element from the front of a nonempty queue, and returns that
  element.

  @defexamples[#:eval qeval
    (define q (make-queue))
    (enqueue! q 1)
    (dequeue! q)
    (enqueue! q 2)
    (enqueue! q 3)
    (dequeue! q)
    (dequeue! q)]
}

@defproc[(queue->list [queue queue/c]) (listof any/c)]{
  Returns an immutable list containing the elements of the queue
  in the order the elements were added.

  @defexamples[#:eval qeval
    (define queue (make-queue))
    (enqueue! queue 8)
    (enqueue! queue 9)
    (enqueue! queue 0)
    (queue->list queue)]
}

@defproc[(queue-length [queue queue/c]) integer?]{
  Returns the number of elements in the queue.

  @defexamples[#:eval qeval
    (define queue (make-queue))
    (queue-length queue)
    (enqueue! queue 5)
    (enqueue! queue 12)
    (queue-length queue)
    (dequeue! queue)
    (queue-length queue)]
}

@defproc[(queue-empty? [q queue/c]) boolean?]{
  Recognizes whether a queue is empty or not.

  @defexamples[#:eval qeval
    (define q (make-queue))
    (queue-empty? q)
    (enqueue! q 1)
    (queue-empty? q)
    (dequeue! q)
    (queue-empty? q)]
}

@defproc[(queue? [v any/c]) boolean?]{
  This predicate recognizes queues.

  @defexamples[#:eval qeval
    (queue? (make-queue))
    (queue? 'not-a-queue)]
}

@defproc[(in-queue [queue queue?])
         sequence?]{

Returns a sequence whose elements are the elements of
@racket[queue].
}

@deftogether[(
  @defthing[queue/c flat-contract?]
  @defthing[nonempty-queue/c flat-contract?]
)]{
  These contracts recognize queues; the latter requires the queue to
  contain at least one value.
}
