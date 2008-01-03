#lang scribble/doc
@require[scribble/manual]
@require[scribble/eval]
@require["guide-utils.ss"]
@require["contracts-utils.ss"]
@(require (for-label scheme/contract))

<section title="Contracts on Structures" tag="structs" />

<p>Modules deal with structures in two ways. First they export
@scheme[struct] definitions, i.e., the ability to create structs of a
certain kind, to access their fields, to modify them, and to distinguish structs
of this kind against every other kind of value in the world. Second, on occasion
a module exports a specific struct and wishes to promise that its fields contain
values of a certain kind. This section explains how to protect structs with
contracts for both uses. 

<question title="Can a module promise something about a specific struct?" tag="single-struct"> 

<p>Yes. If your module defines a variable to be a structure, then on export you
can specify the structures shape: 
<scheme>
(module geometry mzscheme 
  (require (lib "contract.ss"))
  (require posn)
  
  (define origin (make-posn 0 0))
  ... 

  (provide/contract 
   [origin (struct/c posn zero? zero?)]
   ...)
  ... )
</scheme>

 In this example, the module imports a library for representing positions, which
 exports a @scheme[posn] structure. One of the @scheme[posn]s it creates
 and exports stands for the origin, i.e., (0,), of the grid. 

</question>

<question title="Can a module promise something about a specific vector?" tag="single-vector"> 

<p>Yes, again. See the help desk for information on @scheme[vector/c] and
similar contract combinators for (flat) compound data. 

</question>

<question title="Can a contract enforce that all structs are well-formed?" tag="define-struct"> 

<p>"How to Design Programs" teaches that @scheme[posn]s should contain only
numbers in their two fields. With contracts we would enforce this informal data
definition as follows: 

<scheme>
(module posn mzscheme 
  (require (lib "contract.ss"))

  (define-struct posn (x y))
  
  (provide/contract 
   [struct posn ((x number?) (y number?))]
   [p-okay posn?]
   [p-sick posn?])

  (define p-okay (make-posn 10 20))
  (define p-sick (make-posn 'a 'b)))
</scheme>

This module exports the entire structure definition: @scheme[make-posn],
@scheme[posn?], @scheme[posn-x], @scheme[posn-y],
@scheme[set-posn-x!], and @scheme[set-posn-y!]. Each function enforces
or promises that the two fields of a @scheme[posn] structure are
numbers---when the values flow across the module boundary. 

<p>Thus, if a client calls @scheme[make-posn] on @scheme[10] and
@scheme['a], the contract system will signal a contract
violation. Similarly, if @scheme[(set-posn-x! (make-posn 10 10) 'a)] causes
an error. 

<p>The creation of @scheme[p-sick] inside of the @scheme[posn] module,
however, does not violate the contracts. The function @scheme[make-posn] is
internal so @scheme['a] and @scheme['b] don't cross the module
boundary. Similarly, when @scheme[p-sick] crosses the boundary of
@scheme[posn], the contract promises a @scheme[posn?] and nothing
else. In particular, this check does <em>not</em> enforce that the fields of
@scheme[p-sick] are numbers. 

<p>The association of contract checking with module boundaries implies that
@scheme[p-okay] and @scheme[p-sick] look alike from a client's
perspective until the client extracts the pieces: 

<scheme>
(module client mzscheme 
  (require posn)
  
  ... (posn-x p-sick) ...)
</scheme>

 Using @scheme[posn-x] is the only way @scheme[client] can find out what
 a @scheme[posn] contains in the @scheme[x] field. The application of
 @scheme[posn-x] sends @scheme[p-sick] back into the
 @scheme[posn]module and the result value -- @scheme['a] here -- back to
 the client, again across the module boundary. At this very point, the contract
 system discovers that a promise is broken. Specifically, @scheme[posn-x]
 doesn't return a number but a symbol and is therefore blamed. 

<p>This specific example shows that the explanation for a contract violation
 doesn't always pinpoint the source of the error. The good news is that the
 error is located in the @scheme[posn] module. The bad news is that the
 explanation is misleading. Although it is true that @scheme[posn-x]
 produced a symbol instead of a number, it is the fault of the programmer who
 created a @scheme[posn] from symbols, i.e., the programmer who added

<scheme>
  (define p-sick (make-posn 'a 'b))
</scheme>
 to the module. So, when you are looking for bugs based on contract violations,
 keep this example in mind. 

<p><strong>Exercise 2:</strong> Use your knowledge from the <a href="#single-struct">
section on exporting specific structs</a> and change the contract for
@scheme[p-sick] so that the error is caught when clients refer to the
structure. <a href="#exercise2">Solution</a>

</question>

<question title="What about contracts that check properties of data structures?" tag="lazy-contracts">

<p>
Contracts written using @scheme[struct/c] immediately
check the fields of the data structure, but sometimes this
can have disastrous effects on the performance of a program
that does not, itself, inspect the entire data structure.
</p>

<p> As an example, consider the the binary search tree
search algorithm. A binary search tree is like a binary
tree, except that the numbers are organized in the tree to
make searching the tree fast. In particular, for each
interior node in the tree, all of the numbers in the left
subtree are smaller than the number in the node, and all of
the numbers in the right subtree are larger than the number
in the node. </p>

<p>
We can implement implement a search
function @scheme[in?] that takes advantage of the
structure of the binary search tree.
<scheme>
(module bst mzscheme
  (require (lib "contract.ss"))
  (define-struct node (val left right))
  
  ;; determines if `n' is in the binary search tree `b',
  ;; exploiting the binary search tree invariant
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
  
  ;; a predicate that identifies binary search trees
  (define (bst-between? b low high)
    (or (null? b)
        (and (<= low (node-val b) high)
             (bst-between? (node-left b) low (node-val b))
             (bst-between? (node-right b) (node-val b) high))))
  
  (define (bst? b) (bst-between? b -inf.0 +inf.0))
  
  (provide (struct node (val left right)))
  (provide/contract
   [bst? (any/c . -> . boolean?)]
   [in? (number? bst? . -> . boolean?)]))
</scheme>
In a full binary search tree, this means that
the @scheme[in?] function only has to explore a
logarithmic number of nodes.
</p>
<p>
The contract on @scheme[in?] guarantees that its input
is a binary search tree. But a little careful thought
reveals that this contract defeats the purpose of the binary
search tree algorithm. In particular, consider the
inner @scheme[cond] in the @scheme[in?]
function. This is where the @scheme[in?] function gets
its speed: it avoids searching an entire subtree at each
recursive call. Now compare that to the @scheme[bst-between?]
function. In the case that it returns @scheme[#t], it
traverses the entire tree, meaning that the speedup
of @scheme[in?] is lost.
</p>

<p>
In order to fix that, we can employ a new strategy for
checking the binary search tree contract. In particular, if
we only checked the contract on the nodes
that @scheme[in?] looks at, we can still guarantee that
the tree is at least partially well-formed, but without
the performance loss.
</p>

<p>
To do that, we need to
use @scheme[define-contract-struct] in place
of @scheme[define-struct]. Like @scheme[define-struct], 
@scheme[define-contract-struct] defines a maker,
predicate, and selectors for a new
structure. Unlike @scheme[define-struct], it also
defines contract combinators, in this
case @scheme[node/c] and @scheme[node/dc]. Also unlike 
@scheme[define-struct], it does not define mutators, making 
its structs immutable. 
</p>

<p>
The @scheme[node/c] function accepts a contract for each
field of the struct and returns a contract on the
struct. More interestingly, the syntactic
form @scheme[node/dc] allows us to write dependent
contracts, i.e., contracts where some of the contracts on
the fields depend on the values of other fields. We can use
this to define the binary search tree contract:
<scheme>
(module bst mzscheme (require (lib "contract.ss"))
  (define-contract-struct node (val left right))
  
  ;; determines if `n' is in the binary search tree `b'
  (define (in? n b) ... as before ...)
  
  ;; bst-between : number number -> contract
  ;; builds a contract for binary search trees 
  ;; whose values are betweeen low and high
  (define (bst-between/c low high)
    (or/c null?
          (node/dc [val (between/c low high)]
                   [left (val) (bst-between/c low val)]
                   [right (val) (bst-between/c val high)])))
  
  (define bst/c (bst-between/c -inf.0 +inf.0))
  
  (provide make-node node-left node-right node-val node?)
  (provide/contract
   [bst/c contract?]
   [in? (number? bst/c . -> . boolean?)]))
</scheme>
In general, each use of @scheme[node/dc] must name the
fields and then specify contracts for each fields. In the
above, the @scheme[val] field is a contract that accepts
values between @scheme[low] and @scheme[high].
The @scheme[left] and @scheme[right] fields are
dependent on the value of the @scheme[val] field,
indicated by their second sub-expressions. Their contracts
are built by recursive calls to
the @scheme[bst-between/c] function. Taken together,
this contract ensures the same thing that
the @scheme[bst-between?] function checked in the
original example, but here the checking only happens
as @scheme[in?] explores the tree.
</p>

<p>
Although this contract improves the performance
of @scheme[in?], restoring it to the logarithmic
behavior that the contract-less version had, it is still
imposes a fairly large constant overhead. So, the contract
library also provides @scheme[define-opt/c] that brings
down that constant factor by optimizing its body. Its shape
is just like the @scheme[define] above. It expects its
body to be a contract and then optimizes that contract.
<scheme>
(define-opt/c (bst-between/c low high)
  (or/c null?
        (node/dc [val (between/c low high)]
                 [left (val) (bst-between/c low val)]
                 [right (val) (bst-between/c val high)])))
</scheme>
</p>
</question>

<question title="Solution to Exercise 2" tag="exercise2">

<p>A single change suffices: 

<scheme>
(module posn mzscheme 
  (require (lib "contract.ss"))

  (define I (make-inspector))
  (provide I)

  (define-struct posn (x y) I)
  
  (provide/contract 
   [struct posn ((x number?) (y number?))]
   [p-okay posn?]
   [p-sick <font color="red">(struct/c posn number? number?)</font>])

  (define p-okay (make-posn 10 20))
  (define p-sick (make-posn 'a 'b)))
</scheme>

Instead of exporting @scheme[p-sick] as a plain @scheme[posn?], we use a
@scheme[struct/c] contract to enforce constraints on its components. 

</question>

