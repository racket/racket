#lang scribble/manual
@(require scribble/eval
          (for-label data/heap
                     racket/contract
                     racket/base))

@title{Binary Heaps}

@(define the-eval (make-base-eval))
@(the-eval '(require data/heap))

@defmodule[data/heap]

@author[@author+email["Ryan Culpepper" "ryanc@racket-lang.org"]]

Binary heaps are a simple implementation of priority queues.

@defproc[(make-heap [<=? (-> any/c any/c any/c)])
         heap?]{

Makes a new empty heap using @racket[<=?] to order elements.
}

@defproc[(heap? [x any/c]) boolean?]{

Returns @racket[#t] if @racket[x] is a heap, @racket[#f] otherwise.
}

@defproc[(heap-count [h heap?]) exact-nonnegative-integer?]{

Returns the number of elements in the heap.
}

@defproc[(heap-add! [h heap?] [v any/c] ...) void?]{

Adds each @racket[v] to the heap.
}

@defproc[(heap-add-all! [h heap?] [v (or/c list? vector? heap?)]) void?]{

Adds each element contained in @racket[v] to the heap, leaving
@racket[v] unchanged.
}

@defproc[(heap-min [h heap?]) any/c]{

Returns the least element in the heap @racket[h], according to the
heap's ordering. If the heap is empty, an exception is raised.
}

@defproc[(heap-remove-min! [h heap?]) void?]{

Removes the least element in the heap @racket[h]. If the heap is
empty, an exception is raised.
}

@defproc[(vector->heap [<=? (-> any/c any/c any/c)] [items vector?]) heap?]{

Builds a heap with the elements from @racket[items]. The vector is not
modified.
}

@defproc[(heap->vector [h heap?]) vector?]{

Returns a vector containing the elements of heap @racket[h] in the
heap's order. The heap is not modified.
}

@defproc[(heap-copy [h heap?]) heap?]{

Makes a copy of heap @racket[h].
}


@;{--------}

@defproc[(heap-sort! [<=? (-> any/c any/c any/c)] [v vector?]) void?]{

Sorts vector @racket[v] using the comparison function @racket[<=?].
}



@defproc[(in-heap/consume! [heap heap?]) sequence?]{
Returns a sequence equivalent to @racket[heap], maintaining the heap's ordering. 
The heap is consumed in the process. Equivalent to repeated calling 
@racket[heap-min], then @racket[heap-remove-min!].

  @examples[#:eval the-eval
            (define h (make-heap <=))
            (heap-add-all! h '(50 40 10 20 30))
            
            (for ([x (in-heap/consume! h)])
              (displayln x))
                            
            (heap-count h)]
}
@defproc[(in-heap [heap heap?]) sequence?]{
Returns a sequence equivalent to @racket[heap], maintaining the heap's ordering.
Equivalent to @racket[in-heap/consume!] except the heap is copied first.

  @examples[#:eval the-eval
            (define h (make-heap <=))
            (heap-add-all! h '(50 40 10 20 30))
            
            (for ([x (in-heap h)])
              (displayln x))
                            
            (heap-count h)]
}

@close-eval[the-eval]