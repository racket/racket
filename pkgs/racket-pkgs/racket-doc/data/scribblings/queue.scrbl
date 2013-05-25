#lang scribble/manual
@(require scribble/eval (for-label racket data/queue))
@(define qeval (make-base-eval))
@(qeval '(require data/queue))

@title{Imperative Queues}

@defmodule[data/queue]

@author[@author+email["Carl Eastlund" "cce@racket-lang.org"]]

This module provides a simple mutable queue representation,
providing first-in/first-out semantics. 

Operations on queues mutate it in a thread-unsafe way.

@defproc[(make-queue) queue?]{
  Produces an empty queue.
}

@defproc[(enqueue! [q queue?] [v any/c]) void?]{
  Adds an element to the back of a queue.
  
  This takes constant time, independent of the number
  of elements in @racket[q].
}

@defproc[(enqueue-front! [q queue?] [v any/c]) void?]{
  Adds an element to the front of a queue.

  This takes constant time, independent of the number
  of elements in @racket[q].
}

@defproc[(dequeue! [q non-empty-queue?]) any/c]{
  Removes an element from the front of a non-empty queue, and returns that
  element.

  This takes constant time, independent of the number
  of elements in @racket[q].

@defexamples[#:eval qeval
    (define q (make-queue))
    (enqueue! q 1)
    (dequeue! q)
    (enqueue! q 2)
    (enqueue! q 3)
    (dequeue! q)
    (dequeue! q)
    (enqueue! q 2)
    (enqueue! q 1)
    (enqueue-front! q 3)
    (enqueue-front! q 4)
    (queue->list q)]
}

@defproc[(queue-filter! [q queue?] [pred? (-> any/c any/c)]) void?]{
  Applies @racket[pred?] to each element of the queue,
  removing any where @racket[pred?] returns @racket[#f]. 
  
  This takes time proportional to the number of elements in @racket[q]
  (assuming that @racket[pred?] takes constant time, independent
   of the number of elements in @racket[q]). It does not allocate and
   it calls @racket[pred?] exactly once for each element of @racket[q].
  
    @defexamples[#:eval qeval
    (define q (make-queue))
    (enqueue! q 1)
    (enqueue! q 2)
    (enqueue! q 3)
    (enqueue! q 4)
    (queue-filter! q even?)
    (queue->list q)]
}

@defproc[(queue->list [queue queue?]) (listof any/c)]{
  Returns an immutable list containing the elements of the queue
  in the order the elements were added.

  This takes time proportional to the number of elements in @racket[q].
  
  @defexamples[#:eval qeval
    (define q (make-queue))
    (enqueue! q 8)
    (enqueue! q 9)
    (enqueue! q 0)
    (queue->list q)]
}

@defproc[(queue-length [queue queue?]) exact-nonnegative-integer?]{
  Returns the number of elements in the queue.

  This takes constant time, independent of the number
  of elements in @racket[q].
  
  @defexamples[#:eval qeval
    (define queue (make-queue))
    (queue-length q)
    (enqueue! q 5)
    (enqueue! q 12)
    (queue-length q)
    (dequeue! q)
    (queue-length q)]
}

@defproc[(queue-empty? [q queue?]) boolean?]{
  Recognizes whether a queue is empty or not.
  
  This takes constant time, independent of the number
  of elements in @racket[q].

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
  
  This takes constant time, independent of the 
  size of the argument @racket[v].

  @defexamples[#:eval qeval
    (queue? (make-queue))
    (queue? 'not-a-queue)]
}

@defproc[(non-empty-queue? [v any/c]) boolean?]{
  This predicate recognizes non-empty queues.
  
  This takes constant time, independent of the 
  size of the argument @racket[v].

  @defexamples[#:eval qeval
    (non-empty-queue? (let ([q (make-queue)])
                        (enqueue! q 1)
                        q))
    (non-empty-queue? (make-queue))
    (non-empty-queue? 'not-a-queue)]
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
 These are provided for backwards compatibility. They are
 identical to @racket[queue?] and @racket[non-empty-queue?],
 respectively.
}

@close-eval[qeval]
