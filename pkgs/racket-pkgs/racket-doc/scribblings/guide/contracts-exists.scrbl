#lang scribble/doc
@(require scribble/manual scribble/eval "guide-utils.rkt" "contracts-utils.rkt"
          (for-label racket/contract))

@title[#:tag "contracts-exists"]{Abstract Contracts using @racket[#:exists] and @racket[#:∃]}

The contract system provides existential contracts that can
protect abstractions, ensuring that clients of your module
cannot depend on the precise representation choices you make
for your data structures.

@; @ctc-section{Getting Started, with a Queue Example}

@margin-note{
  You can type @racket[#:exists] instead of @racket[#:∃] if you 
cannot easily type unicode characters; in DrRacket, typing
@litchar{\exists} followed by either alt-\ or control-\ (depending
on your platform) will produce @racket[∃].}
The @racket[contract-out] form allows you to write
@racketblock[#:∃ _name-of-a-new-contract] as one of its clauses. This declaration
introduces the variable @racket[_name-of-a-new-contract], binding it to a new
contract that hides information about the values it protects.

As an example, consider this (simple) implementation of a queue datastructure:
@racketmod[racket
           (define empty '())
           (define (enq top queue) (append queue (list top)))
           (define (next queue) (car queue))
           (define (deq queue) (cdr queue))
           (define (empty? queue) (null? queue))
           
           (provide
            (contract-out
             [empty (listof integer?)]
             [enq (-> integer? (listof integer?) (listof integer?))]
             [next (-> (listof integer?) integer?)]
             [deq (-> (listof integer?) (listof integer?))]
             [empty? (-> (listof integer?) boolean?)]))]
This code implements a queue purely in terms of lists, meaning that clients
of this data structure might use @racket[car] and @racket[cdr] directly on the
data structure (perhaps accidentally) and thus any change in the representation
(say to a more efficient representation that supports amortized constant time
enqueue and dequeue operations) might break client code.

To ensure that the queue representation is abstract, we can use @racket[#:∃] in the
@racket[contract-out] expression, like this:
@racketblock[(provide
              (contract-out
               #:∃ queue
               [empty queue]
               [enq (-> integer? queue queue)]
               [next (-> queue integer?)]
               [deq (-> queue queue)]
               [empty? (-> queue boolean?)]))]

Now, if clients of the data structure try to use @racket[car] and @racket[cdr], they
receive an error, rather than mucking about with the internals of the queues.

See also @ctc-link["exists-gotcha"].
