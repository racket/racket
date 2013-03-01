#lang scribble/doc
@(require scribble/manual scribble/eval "guide-utils.rkt" "contracts-utils.rkt"
          (for-label racket/contract))

@title[#:tag "contracts-struct"]{Contracts on Structures}

Modules deal with structures in two ways. First they export
@racket[struct] definitions, i.e., the ability to create
structs of a certain kind, to access their fields, to modify
them, and to distinguish structs of this kind against every
other kind of value in the world. Second, on occasion a
module exports a specific struct and wishes to promise that
its fields contain values of a certain kind. This section
explains how to protect structs with contracts for both
uses.

@; ----------------------------------------------------------------------
@ctc-section[#:tag "single-struct"]{Guarantees for a Specific Value}

If your module defines a variable to be a structure, then you can
specify the structure's shape using @racket[struct/c]:

@racketmod[
racket
(require lang/posn)
  
(define origin (make-posn 0 0))

(provide (contract-out
          [origin (struct/c posn zero? zero?)]))
]

In this example, the module imports a library for representing positions, which
exports a @racket[posn] structure. One of the @racket[posn]s it creates
and exports stands for the origin, i.e., @tt{(0,0)}, of the grid. 

@margin-note{See also @racket[vector/c] and similar contract
combinators for (flat) compound data.}

@; ----------------------------------------------------------------------
@ctc-section[#:tag "define-struct"]{Guarantees for All Values}

The book @|HtDP| teaches that @racket[posn]s should contain only
numbers in their two fields. With contracts we would enforce this
informal data definition as follows:

@racketmod[
racket
(struct posn (x y))
  
(provide (contract-out
          [struct posn ((x number?) (y number?))]
          [p-okay posn?]
          [p-sick posn?]))

(define p-okay (posn 10 20))
(define p-sick (posn 'a 'b))
]

This module exports the entire structure definition: @racket[posn],
@racket[posn?], @racket[posn-x], @racket[posn-y],
@racket[set-posn-x!], and @racket[set-posn-y!]. Each function enforces
or promises that the two fields of a @racket[posn] structure are
numbers --- when the values flow across the module boundary.  Thus, if
a client calls @racket[posn] on @racket[10] and @racket['a], the
contract system signals a contract violation.

The creation of @racket[p-sick] inside of the @racket[posn] module,
however, does not violate the contracts. The function @racket[posn] is
used internally, so @racket['a] and @racket['b] don't cross the module
boundary. Similarly, when @racket[p-sick] crosses the boundary of
@racket[posn], the contract promises a @racket[posn?] and nothing
else. In particular, this check does @italic{not} require that the
fields of @racket[p-sick] are numbers.

The association of contract checking with module boundaries implies that
@racket[p-okay] and @racket[p-sick] look alike from a client's
perspective until the client extracts the pieces: 

@racketmod[
racket
(require lang/posn)
  
... (posn-x p-sick) ...
]

Using @racket[posn-x] is the only way the client can find out what
a @racket[posn] contains in the @racket[x] field. The application of
@racket[posn-x] sends @racket[p-sick] back into the
@racket[posn] module and the result value -- @racket['a] here -- back to
the client, again across the module boundary. At this very point, the contract
system discovers that a promise is broken. Specifically, @racket[posn-x]
doesn't return a number but a symbol and is therefore blamed. 

This specific example shows that the explanation for a contract violation
doesn't always pinpoint the source of the error. The good news is that the
error is located in the @racket[posn] module. The bad news is that the
explanation is misleading. Although it is true that @racket[posn-x]
produced a symbol instead of a number, it is the fault of the programmer who
created a @racket[posn] from symbols, i.e., the programmer who added

@racketblock[
(define p-sick (posn 'a 'b))
]

 to the module. So, when you are looking for bugs based on contract
 violations, keep this example in mind.

If we want to fix the contract for @racket[p-sick] so that the error
is caught when @racket[sick] is exported, a single change suffices:

@racketblock[
(provide
 (contract-out
  ...
  [p-sick (struct/c posn number? number?)]))
]

That is, instead of exporting @racket[p-sick] as a plain
@racket[posn?], we use a @racket[struct/c] contract to enforce
constraints on its components.

@; ----------------------------------------------------------------------
@ctc-section[#:tag "lazy-contracts"]{Checking Properties of Data Structures}

Contracts written using @racket[struct/c] immediately
check the fields of the data structure, but sometimes this
can have disastrous effects on the performance of a program
that does not, itself, inspect the entire data structure.

As an example, consider the binary search tree
search algorithm. A binary search tree is like a binary
tree, except that the numbers are organized in the tree to
make searching the tree fast. In particular, for each
interior node in the tree, all of the numbers in the left
subtree are smaller than the number in the node, and all of
the numbers in the right subtree are larger than the number
in the node.

We can implement a search function @racket[in?] that takes
advantage of the structure of the binary search tree.
@racketmod[
racket

(struct node (val left right))
  
(code:comment "determines if `n' is in the binary search tree `b',")
(code:comment "exploiting the binary search tree invariant")
(define (in? n b)
  (cond
    [(null? b) #f]
    [else (cond
            [(= n (node-val b))
             #t]
            [(< n (node-val b))
             (in? n (node-left b))]
            [(> n (node-val b))
             (in? n (node-right b))])]))

(code:comment "a predicate that identifies binary search trees")
(define (bst-between? b low high)
  (or (null? b)
      (and (<= low (node-val b) high)
           (bst-between? (node-left b) low (node-val b))
           (bst-between? (node-right b) (node-val b) high))))

(define (bst? b) (bst-between? b -inf.0 +inf.0))
  
(provide (struct-out node))
(provide (contract-out
          [bst? (any/c . -> . boolean?)]
          [in? (number? bst? . -> . boolean?)]))
]

In a full binary search tree, this means that
the @racket[in?] function only has to explore a
logarithmic number of nodes.

The contract on @racket[in?] guarantees that its input
is a binary search tree. But a little careful thought
reveals that this contract defeats the purpose of the binary
search tree algorithm. In particular, consider the
inner @racket[cond] in the @racket[in?]
function. This is where the @racket[in?] function gets
its speed: it avoids searching an entire subtree at each
recursive call. Now compare that to the @racket[bst-between?]
function. In the case that it returns @racket[#t], it
traverses the entire tree, meaning that the speedup
of @racket[in?] is lost.

In order to fix that, we can employ a new strategy for
checking the binary search tree contract. In particular, if
we only checked the contract on the nodes
that @racket[in?] looks at, we can still guarantee that
the tree is at least partially well-formed, but without
changing the complexity.

To do that, we need to use @racket[struct/dc] to define
@racket[bst-between?]. Like @racket[struct/c], @racket[struct/dc] defines a
contract for a structure. Unlike
@racket[struct/c], it allows fields to be marked as lazy, so that
the contracts are only checked when the matching selector is called.
Also, it does not allow mutable fields to be marked as lazy.

The @racket[struct/dc] form accepts a contract for each
field of the struct and returns a contract on the
struct. More interestingly, @racket[struct/dc] allows us to write dependent
contracts, i.e., contracts where some of the contracts on
the fields depend on the values of other fields. We can use
this to define the binary search tree contract:

@racketmod[
racket

(struct node (val left right))

(code:comment "determines if `n' is in the binary search tree `b'")
(define (in? n b) ... as before ...)

(code:comment "bst-between : number number -> contract")
(code:comment "builds a contract for binary search trees")
(code:comment "whose values are between low and high")
(define (bst-between/c low high)
  (or/c null?
        (struct/dc node [val (between/c low high)]
                        [left (val) #:lazy (bst-between/c low val)]
                        [right (val) #:lazy (bst-between/c val high)])))

(define bst/c (bst-between/c -inf.0 +inf.0))

(provide (struct-out node))
(provide (contract-out
          [bst/c contract?]
          [in? (number? bst/c . -> . boolean?)]))
]

In general, each use of @racket[struct/dc] must name the
fields and then specify contracts for each field. In the
above, the @racket[val] field is a contract that accepts
values between @racket[low] and @racket[high].
The @racket[left] and @racket[right] fields are
dependent on the value of the @racket[val] field,
indicated by their second sub-expressions. They are
also marked with the @racket[#:lazy] keyword to indicate
that they should be checked only when the appropriate
accessor is called on the struct instance. Their contracts
are built by recursive calls to
the @racket[bst-between/c] function. Taken together,
this contract ensures the same thing that
the @racket[bst-between?] function checked in the
original example, but here the checking only happens
as @racket[in?] explores the tree.

Although this contract improves the performance
of @racket[in?], restoring it to the logarithmic
behavior that the contract-less version had, it is still
imposes a fairly large constant overhead. So, the contract
library also provides @racket[define-opt/c] that brings
down that constant factor by optimizing its body. Its shape
is just like the @racket[define] above. It expects its
body to be a contract and then optimizes that contract.

@racketblock[
(define-opt/c (bst-between/c low high)
  (or/c null?
        (struct/dc node [val (between/c low high)]
                        [left (val) #:lazy (bst-between/c low val)]
                        [right (val) #:lazy (bst-between/c val high)])))
]

