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

Operations on binary heaps are not thread-safe.

@defproc[(make-heap [<=? (-> any/c any/c any/c)])
         heap?]{

Makes a new empty heap using @racket[<=?] to order elements.

@examples[#:eval the-eval
  (define a-heap-of-strings (make-heap string<=?))
  a-heap-of-strings
  @code:comment{With structs:}
  (struct node (name val))
  (define (node<=? x y)
    (<= (node-val x) (node-val y)))
  (define a-heap-of-nodes (make-heap node<=?))
  a-heap-of-nodes]
}

@defproc[(heap? [x any/c]) boolean?]{

Returns @racket[#t] if @racket[x] is a heap, @racket[#f] otherwise.

@examples[#:eval the-eval
  (heap? (make-heap <=))
  (heap? "I am not a heap")]
}

@defproc[(heap-count [h heap?]) exact-nonnegative-integer?]{

Returns the number of elements in the heap.
@examples[#:eval the-eval
  (define a-heap (make-heap <=))
  (heap-add-all! a-heap '(7 3 9 1 13 21 15 31))
  (heap-count a-heap)
]
}

@defproc[(heap-add! [h heap?] [v any/c] ...) void?]{

Adds each @racket[v] to the heap.

@examples[#:eval the-eval
  (define a-heap (make-heap <=))
  (heap-add! a-heap 2009 1009)]
}


@defproc[(heap-add-all! [h heap?] [v (or/c list? vector? heap?)]) void?]{

Adds each element contained in @racket[v] to the heap, leaving
@racket[v] unchanged.

@examples[#:eval the-eval
  (define heap-1 (make-heap <=))
  (define heap-2 (make-heap <=))
  (define heap-12 (make-heap <=))
  (heap-add-all! heap-1 '(3 1 4 1 5 9 2 6))
  (heap-add-all! heap-2 #(2 7 1 8 2 8 1 8))
  (heap-add-all! heap-12 heap-1)
  (heap-add-all! heap-12 heap-2)
  (heap-count heap-12)]
}

@defproc[(heap-min [h heap?]) any/c]{

Returns the least element in the heap @racket[h], according to the
heap's ordering. If the heap is empty, an exception is raised.

@examples[#:eval the-eval
  (define a-heap (make-heap string<=?))
  (heap-add! a-heap "sneezy" "sleepy" "dopey" "doc"
             "happy" "bashful" "grumpy")
  (heap-min a-heap)

  @code:comment{Taking the min of the empty heap is an error:}
  (heap-min (make-heap <=))
]
}

@defproc[(heap-remove-min! [h heap?]) void?]{

Removes the least element in the heap @racket[h]. If the heap is
empty, an exception is raised.

@examples[#:eval the-eval
  (define a-heap (make-heap string<=?))
  (heap-add! a-heap "fili" "fili" "oin" "gloin" "thorin"
                    "dwalin" "balin" "bifur" "bofur"
                    "bombur" "dori" "nori" "ori")
  (heap-min a-heap)
  (heap-remove-min! a-heap)
  (heap-min a-heap)]
}

@defproc[(heap-remove! [h heap?] [v any/c] [#:same? same? (-> any/c any/c any/c) equal?]) void?]{
Removes @racket[v] from the heap @racket[h] if it exists. 
@examples[#:eval the-eval
  (define a-heap (make-heap string<=? string=?))
  (heap-add! a-heap "a" "b" "c")
  (heap-remove! a-heap "b")
  (for/list ([a (in-heap a-heap)]) a)]
}

@defproc[(vector->heap [<=? (-> any/c any/c any/c)] [items vector?]) heap?]{

Builds a heap with the elements from @racket[items]. The vector is not
modified.
@examples[#:eval the-eval
  (struct item (val frequency))
  (define (item<=? x y)
    (<= (item-frequency x) (item-frequency y)))
  (define some-sample-items 
    (vector (item #\a 17) (item #\b 12) (item #\c 19)))
  (define a-heap (vector->heap item<=? some-sample-items))
]
}

@defproc[(heap->vector [h heap?]) vector?]{

Returns a vector containing the elements of heap @racket[h] in the
heap's order. The heap is not modified.

@examples[#:eval the-eval
  (define word-heap (make-heap string<=?))
  (heap-add! word-heap "pile" "mound" "agglomerate" "cumulation")
  (heap->vector word-heap)
]
}

@defproc[(heap-copy [h heap?]) heap?]{

Makes a copy of heap @racket[h].
@examples[#:eval the-eval
  (define word-heap (make-heap string<=?))
  (heap-add! word-heap "pile" "mound" "agglomerate" "cumulation")
  (define a-copy (heap-copy word-heap))
  (heap-remove-min! a-copy)
  (heap-count word-heap)
  (heap-count a-copy)
]
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

@;{--------}

@defproc[(heap-sort! [v (and/c vector? (not/c immutable?))] [<=? (-> any/c any/c any/c)]) void?]{

Sorts vector @racket[v] using the comparison function @racket[<=?].

@examples[#:eval the-eval
  (define terms (vector "batch" "deal" "flock" "good deal" "hatful" "lot"))
  (heap-sort! terms string<=?)
  terms
]
}

@close-eval[the-eval]
