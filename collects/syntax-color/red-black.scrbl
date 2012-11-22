#lang scribble/doc
@(require scribble/manual
          scribble/eval
          (for-label syntax-color/private/red-black
                     racket/base
                     racket/string))

@(define my-eval (make-base-eval))
@(my-eval '(require syntax-color/private/red-black racket/string))

@title{Ordered Red-Black Trees}
@author+email["Danny Yoo" "dyoo@hashcollision.org"]


@defmodule[syntax-color/private/red-black]

This is an implementation of an augmented red-black tree with extra information
to support position-based queries.

The intended usage case of this structure is to maintain an ordered sequence of
items, where each item has an internal length.  Given such a sequence, we want
to support quick lookup by position and in-place insertions and deletions.
We also want to support the catenation and splitting of sequences.

For example:

@interaction[#:eval my-eval
(define a-tree (new-tree))
(for ([w (in-list '("This" " " "is" " " "a" " " "test"))])
  (insert-last/data! a-tree w (string-length w)))
(node-data (search a-tree 0))
(node-data (search a-tree 10))
(define at-test-node (search a-tree 10))
(insert-before/data! a-tree at-test-node "small" 5)
(tree-items a-tree)
@code:comment{Split at the node holding "small":}
(define at-small-node (search a-tree 10))
(define-values (left-side right-side) (split! a-tree at-small-node))
(tree-items left-side)
(tree-items right-side)
(define joined-tree (join! left-side right-side))
(tree-items joined-tree)
]


This implementation follows the basic outline for order-statistic red-black
trees described in @cite{clrs2009} and incorporates a few extensions suggsted
in @cite{wein2005}.  As a red-black tree, the structure ensures that the tree's
height is never greater than @math{2*lg(#-of-nodes + 1)}, guaranteeing good
worst-case behavior for its operations.

The main types of values used in the library are @emph{trees} and @emph{nodes}.
A tree has a @emph{root} node, and each node has holds arbitrary @emph{data}
and a natural @emph{self-width}, along with a reference to the elements smaller
(@racket[node-left]) and larger (@racket[node-right]).  Each node also
remembers the entire width of its subtree, which can be accessed with
@racket[node-subtree-width].  The tree holds first and last pointers into the
structure to allow for fast access to the beginning and end of the sequence.  A
distinguished @racket[nil] node lies at the leaves of the tree.



@section{API}
@declare-exporting[syntax-color/private/red-black]


@subsection{Data types}

@defproc[(new-tree) tree?]{
Constructs a new tree.  The tree's root is initially @racket[nil].
@interaction[#:eval my-eval
(define a-tree (new-tree))
a-tree
(nil-node? (tree-root a-tree))
]
}



@defproc[(tree? [x any/c]) boolean?]{
Returns @racket[#t] if @racket[x] is a tree.

@interaction[#:eval my-eval
(define a-tree (new-tree))
(tree? a-tree)
(tree? "not a tree")
(tree? (new-node '(not a tree either) 0))
]}



@defproc[(tree-root [t tree?]) node?]{
Returns the root node of the tree @racket[t].
If the tree is empty, returns the distinguished @racket[nil] node.

@interaction[#:eval my-eval
(define a-tree (new-tree))
(nil-node? (tree-root (new-tree)))
(define a-node (new-node "first node!" 11))
(insert-first! a-tree a-node)
(eq? a-node (tree-root a-tree))]
}



@defproc[(tree-first [t tree?]) node?]{
Returns the first node in the tree.

@interaction[#:eval my-eval
(define a-tree (new-tree))
(nil-node? (tree-first (new-tree)))
(define a-node (new-node "first node!" 11))
(define another-node (new-node "last node!" 11))
(insert-first! a-tree a-node)
(insert-last! a-tree another-node)
(eq? a-node (tree-first a-tree))]
}


@defproc[(tree-last [t tree?]) node?]{
Returns the last node in the tree.

@interaction[#:eval my-eval
(define a-tree (new-tree))
(nil-node? (tree-first (new-tree)))
(define a-node (new-node "first node!" 11))
(define another-node (new-node "last node!" 11))
(insert-first! a-tree a-node)
(insert-last! a-tree another-node)
(eq? another-node (tree-last a-tree))]
}


@defproc[(new-node [data any/c] [width natural-number/c]) singleton-node?]{
Constructs a new singleton node.  This node can be inserted into a tree with
@racket[insert-first!], @racket[insert-last!], @racket[insert-before!], or
@racket[insert-after!].

@interaction[#:eval my-eval
(new-node #("a" "node") 7)]
}


@defproc[(node? [x any/c]) boolean?]{
Returns @racket[#t] if @racket[x] is a node.
@interaction[#:eval my-eval
(node? (new-node #("a" "node") 7))
@code:comment{Trees are not nodes: they _have_ nodes.}
(node? (new-tree))
(node? (tree-root (new-tree)))
]
}


@defproc[(singleton-node? [x any/c]) boolean?]{
Returns @racket[#t] if @racket[x] is a @emph{singleton node}.  A singleton node
is unattached to any tree, and is not the @racket[nil] node.
@interaction[#:eval my-eval
(singleton-node? (new-node #("a" "node") 7))
(singleton-node? nil)

@code:comment{Create a fresh node:}
(define a-node (new-node "about to attach" 0))
(singleton-node? a-node)
@code:comment{After attachment, it is no longer singleton:}
(define a-tree (new-tree))
(insert-first! a-tree a-node)
(singleton-node? a-node)
@code:comment{Operations such as delete! or split! will break}
@code:comment{off nodes as singletons again:}
(delete! a-tree a-node)
(singleton-node? a-node)
]
}


@defthing[nil node?]{

The distinguished @racket[nil] node.  By definition, @racket[nil] is colored
black, and its @racket[node-parent], @racket[node-left], and
@racket[node-right] are pointed to itself.}


@defproc[(non-nil-node? [x any/c]) boolean?]{
Returns @racket[#t] if @racket[x] is a non-nil node.
@interaction[#:eval my-eval
(non-nil-node? nil)
(non-nil-node? (new-node "I am not a number" 1))
]
}


@defproc[(nil-node? [x any/c]) boolean?]{
Returns @racket[#t] if @racket[x] is the nil node.
@interaction[#:eval my-eval
(nil-node? nil)
(nil-node? (new-node "I am not a number" 1))
]
}


@defproc[(node-data [n node?]) any/c]{
Returns the data associated to node @racket[n].  Note that the
@racket[node-data] and @racket[node-self-width] are entirely independent.

@interaction[#:eval my-eval
(define a-node (new-node "utah" 4))
(node-data a-node)
]
}

@defproc[(set-node-data! [n node?] [v any/c]) void?]{
Assigns the data associated to node @racket[n].  Note that the
@racket[node-data] and @racket[node-self-width] are entirely independent.

@interaction[#:eval my-eval
(define a-node (new-node "utah" 4))
(set-node-data! a-node "rhode island")
(node-data a-node)
]
}



@defproc[(node-self-width [n node?]) any/c]{
Returns the self-width associated to node @racket[n].  Note that the
@racket[node-data] and @racket[node-self-width] are entirely independent.

@interaction[#:eval my-eval
(define a-node (new-node "utah" 4))
(node-self-width a-node)
]
}


@defproc[(update-node-self-width! [n node?] [w natural-number/c]) any/c]{
Updates the self-width associated to node @racket[n].  When attached to a tree,
also propagates the width's change to the widths of subtrees, upward through
its parents to the root.  Note that the @racket[node-data] and
@racket[node-self-width] are entirely independent.


@interaction[#:eval my-eval
(define a-tree (new-tree))
(insert-last/data! a-tree "hello" 5)
(insert-last/data! a-tree "world" 1)
@code:comment{The tree as a whole has width 6:}
(node-subtree-width (tree-root a-tree))
@code:comment{Updates will propagate to the root:}
(update-node-self-width! (tree-last a-tree) 5)
(node-self-width (tree-last a-tree))
(node-subtree-width (tree-root a-tree))
]
}


@defproc[(node-subtree-width [n node?]) any/c]{
Returns the width of the entire subtree at node @racket[n].  This sums the
width of the left and right child subtrees, as well as its self-width.

@interaction[#:eval my-eval
(define a-tree (new-tree))
(insert-last/data! a-tree "berkeley" 1)
(insert-last/data! a-tree "stanford" 1)
(insert-last/data! a-tree "wpi" 1)
(insert-last/data! a-tree "brown" 1)
(insert-last/data! a-tree "utah" 1)
@code:comment{The entire tree should sum to five, since each element contributes one.}
(node-subtree-width (tree-root a-tree))
(node-subtree-width (node-left (tree-root a-tree)))
(node-subtree-width (node-right (tree-root a-tree)))
]
}



@defproc[(node-parent [n node?]) node?]{
Returns the parent of the node @racket[n].
@interaction[#:eval my-eval
(define a-tree (new-tree))
(insert-last/data! a-tree "bill and ted's excellent adventure" 1)
(insert-last/data! a-tree "the matrix" 1)
(insert-last/data! a-tree "speed" 1)
(define p (node-parent (tree-last a-tree)))
(node-data p)]
}


@defproc[(node-left [n node?]) node?]{
Returns the left child of the node @racket[n].
@interaction[#:eval my-eval
(define a-tree (new-tree))
(insert-last/data! a-tree "bill and ted's excellent adventure" 1)
(insert-last/data! a-tree "the matrix" 1)
(insert-last/data! a-tree "speed" 1)
(define p (node-left (tree-root a-tree)))
(node-data p)]
}


@defproc[(node-right [n node?]) node?]{
Returns the right child of the node @racket[n].
@interaction[#:eval my-eval
(define a-tree (new-tree))
(insert-last/data! a-tree "bill and ted's excellent adventure" 1)
(insert-last/data! a-tree "the matrix" 1)
(insert-last/data! a-tree "speed" 1)
(define p (node-right (tree-root a-tree)))
(node-data p)]
}



@defproc[(node-color [n node?]) (or/c 'red 'black)]{
Returns the color of the node @racket[n].  The red-black tree structure uses
this value to maintain balance.
@interaction[#:eval my-eval
(define a-tree (new-tree))
(insert-last/data! a-tree "the color purple" 1)
(insert-last/data! a-tree "pretty in pink" 1)
(insert-last/data! a-tree "the thin red line" 1)
(insert-last/data! a-tree "clockwork orange" 1)
(insert-last/data! a-tree "fried green tomatoes" 1)
(node-color (tree-root a-tree))
(tree-fold-inorder a-tree 
                   (lambda (n acc) 
                     (cons (list (node-data n) (node-color n))
                           acc))
                   '())]
}


@defproc[(red? [n node?]) boolean?]{
Returns @racket[#t] if node @racket[n] is red.
@interaction[#:eval my-eval
(define a-tree (new-tree))
(insert-last/data! a-tree "the hobbit" 1)
(insert-last/data! a-tree "the fellowship of the ring" 1)
(red? (tree-root a-tree))
(red? (node-right (tree-root a-tree)))
]
}


@defproc[(black? [n node?]) boolean?]{
Returns @racket[#t] if node @racket[n] is black.
@interaction[#:eval my-eval
(define a-tree (new-tree))
(insert-last/data! a-tree "the fellowship of the ring" 1)
(insert-last/data! a-tree "the two towers" 1)
(insert-last/data! a-tree "return of the king" 1)
@code:comment{The root is always black.}
(black? (tree-root a-tree))
@code:comment{The tree should have towers as the root, with}
@code:comment{the fellowship and king as left and right respectively.}
(map node-data
     (list (tree-root a-tree)
           (node-left (tree-root a-tree))
           (node-right (tree-root a-tree))))
(black? (tree-root a-tree))
(black? (node-left (tree-root a-tree)))
(black? (node-right (tree-root a-tree)))
]
}


@subsection{Operations}

@defproc[(insert-first! [t tree?] [n singleton-node?]) void?]{
Adds node @racket[n] as the first element in tree @racket[t].
@interaction[#:eval my-eval
(define a-tree (new-tree))
(define a-node (new-node "pear" 1))
(insert-first! a-tree a-node)
(eq? (tree-root a-tree) a-node)
]

Note that attempting to add an attached, non-singleton node to a tree will
raise a contract error.
@interaction[#:eval my-eval
(define a-tree (new-tree))
(define a-node (new-node "persimmon" 1))
(insert-first! a-tree a-node)
(insert-first! a-tree a-node)
]
}

@defproc[(insert-last! [t tree?] [n singleton-node?]) void?]{
Adds node @racket[n] as the last element in tree @racket[t].
@interaction[#:eval my-eval
(define a-tree (new-tree))
(define a-node (new-node "apple" 1))
(insert-last! a-tree a-node)
(eq? (tree-root a-tree) a-node)
]

Note that attempting to add an attached, non-singleton node to a tree will
raise a contract error.
@interaction[#:eval my-eval
(define a-tree (new-tree))
(define a-node (new-node "orange" 1))
(insert-last! a-tree a-node)
(insert-last! a-tree a-node)
]
}



@defproc[(insert-before! [t tree?] [n1 node?] [n2 node?]) void?]{
Adds node @racket[n2] before node @racket[n1] in tree @racket[t].  This effectively
makes @racket[n2] the @racket[predecessor] of @racket[n1].
@interaction[#:eval my-eval
(define a-tree (new-tree))
(define a-node (new-node "banana" 1))
(define b-node (new-node "mango" 1))
(insert-first! a-tree a-node)
(insert-before! a-tree a-node b-node)
(eq? (predecessor a-node) b-node)
(eq? (successor b-node) a-node)
]

Note that attempting to add an attached, non-singleton node to a tree will
raise a contract error.
@interaction[#:eval my-eval
(define a-tree (new-tree))
(define a-node (new-node "peach" 1))
(insert-first! a-tree a-node)
(insert-before! a-tree a-node a-node)
]
}



@defproc[(insert-after! [t tree?] [n1 node?] [n2 node?]) void?]{
Adds node @racket[n2] after node @racket[n1] in tree @racket[t].  This effectively
makes @racket[n2] the @racket[successor] of @racket[n1].
@interaction[#:eval my-eval
(define a-tree (new-tree))
(define a-node (new-node "cherry" 1))
(define b-node (new-node "pawpaw" 1))
(insert-first! a-tree a-node)
(insert-after! a-tree a-node b-node)
(eq? (successor a-node) b-node)
(eq? (predecessor b-node) a-node)
]

Note that attempting to add an attached, non-singleton node to a tree will
raise a contract error.
@interaction[#:eval my-eval
(define a-tree (new-tree))
(define a-node (new-node "grapefruit" 1))
(insert-first! a-tree a-node)
(insert-after! a-tree a-node a-node)
]
}



@deftogether[
(
@defproc[(insert-first/data! [t tree?] [data any/c] [width natural-number/c]) void?]{}
@defproc[(insert-last/data! [t tree?] [data any/c] [width natural-number/c]) void?]{}
@defproc[(insert-before/data! [t tree?] [n node?] [data any/c] [width natural-number/c]) void?]{}
@defproc[(insert-after/data! [t tree?] [n node?] [data any/c] [width natural-number/c]) void?]{})
]{

For user convenience, the functions @racket[insert-first/data!],
@racket[insert-last/data!], @racket[insert-before/data!], and
@racket[insert-after/data!] have been provided.  These create nodes and insert
into the tree structure the same way as @racket[insert-first!],
@racket[insert-last!], @racket[insert-before!], and @racket[insert-after!].

@interaction[#:eval my-eval
(define t (new-tree))
(insert-first/data! t "message in a bottle" 1)
(insert-last/data! t "don't stand so close to me" 1)
(insert-before/data! t (tree-first t) "everything she does is magic" 1)
(insert-after/data! t (tree-last t) "king of pain" 1)
(tree-items t)
]
}


@defproc[(delete! [t tree?] [n non-nil-node?]) void?]{
Deletes node @racket[n] from the tree @racket[t].  After deletion, @racket[n]
will become a singleton node.
@interaction[#:eval my-eval
(define t (new-tree))
(define n1 (new-node "George, George, George of the Jungle," 1))
(define n2 (new-node "strong as he can be..." 1))
(define n3 (new-node "aaaaaaaaaaah!" 1))
(define n4 (new-node "watch out for that..." 1))
(define n5 (new-node "<thump!>" 1))
(define n6 (new-node "treeeeeeeeee!, " 1))
(for ([n (in-list (list n1 n2 n3 n4 n5 n6))])
  (insert-last! t n))
(delete! t n5)
(tree-items t)
]

Note that @racket[n] must be attached to tree @racket[t] or else an
error will be raised:
@interaction[#:eval my-eval
(define t1 (new-tree))
(define t2 (new-tree))
(insert-first/data! t1 "tricky" 1)
(insert-first/data! t2 "tricky" 1)
@code:comment{This should raise an error:}
(delete! t1 (tree-root t2))
]}


@defproc[(join! [t1 tree?] [t2 tree?]) tree?]{
Destructively joins trees @racket[t1] and @racket[t2], returning a tree that
has the contents of both.  Every element in @racket[t1] is treated less than
the elements in @racket[t2].

@interaction[#:eval my-eval
(define t1 (new-tree))
(for ([name (in-list '(goku gohan krillin piccolo vegeta))])
  (insert-last/data! t1 name 1))
@code:comment{Tier two characters:}
(define t2 (new-tree))
(for ([name (in-list '(yamcha tien chiaotzu bulma chi-chi
                       oolong puar master-roshi))])
  (insert-last/data! t2 name 1))
(define tree-of-mighty-z-warriors (join! t1 t2))
(tree-items tree-of-mighty-z-warriors)
]
}


@defproc[(concat! [t1 tree?] [n singleton-node?] [t2 tree?]) tree?]{
Destructively joins tree @racket[t1], singleton node @racket[n], and tree
@racket[t2], returning a tree that has the contents of both.  Every element in
@racket[t1] is treated less than @racket[x], and @racket[x] is treated smaller than all
the elements in @racket[t2].

@interaction[#:eval my-eval
(define t1 (new-tree))
(define t2 (new-tree))
(insert-last/data! t1 "inigo" 50)
(define x (new-node "vizzini" 1))
(insert-last/data! t2 "fezzik" 100)
(define poor-lost-circus-performers (concat! t1 x t2))
(tree-items poor-lost-circus-performers)
]
}


@defproc[(split! [t tree?] [n non-nil-node?]) (values tree? tree?)]{
Destructively splits tree @racket[t] into two trees, the first containing the
elements smaller than node @racket[n], and the second containing those larger.
Afterwards, @racket[n] becomes a singleton node.

@interaction[#:eval my-eval
(define t (new-tree))
(for ([name '(melchior caspar bob balthazar)])
  (insert-last/data! t name 1))
(define bob-node (search t 2))
(singleton-node? bob-node)
(define-values (l r) (split! t bob-node))
@code:comment{We tree kings of orient are:}
(append (tree-items l) (tree-items r))
(singleton-node? bob-node)
]

Note that @racket[n] must be attached to tree @racket[t] or else
an error will be raised.
@interaction[#:eval my-eval
(define t (new-tree))
(for ([name '(melchior caspar bob balthazar)])
  (insert-last/data! t name 1))
@code:comment{This should raise an error:}
(define t2 (new-tree))
(insert-last! t2 (new-node "bob" 1))
(split! t (tree-root t2))
]}


@defproc[(search [t tree?] [p natural-number/c]) node?]{
Searches for the node at or within the given position @racket[p] of the tree.
If the position is out of bounds, returns @racket[nil].

@interaction[#:eval my-eval
(define t (new-tree))
(for ([word '("alpha" "beta" "gamma" "delta" "epsilon" "zeta")])
  (insert-last/data! t word (string-length word)))
(node-data (search t 0))
(node-data (search t 5))
(node-data (search t 6))
(node-data (search t 7))
(node-data (search t 8))
(node-data (search t 9))
(nil-node? (search t 100))
]

Note: nodes with a self-width of zero are effectively invisible to
@racket[search], and will be skipped over.
}


@defproc[(search/residual [t tree?] [p natural-number/c]) (values node? natural-number/c)]{
Searches for the node at or within the given position @racket[p] of the tree.
This is an extension of @racket[search] that returns a second value: the offset
into the element where the search has terminated.  If the position is out of
bounds of any element, the first component of the returned value is
@racket[nil].

@interaction[#:eval my-eval
(define t (new-tree))
(for ([word '("alpha" "beta" "gamma" "delta" "epsilon" "zeta")])
  (insert-last/data! t word (string-length word)))
(search/residual t 5)
(search/residual t 6)
(search/residual t 7)
(define-values (a-node residual)
  (search/residual t 100))
(nil-node? a-node)
residual
(+ residual (node-subtree-width (tree-root t)))
]
}


@defproc[(minimum [n node?]) node?]{
Given a node @racket[n], returns the minimum element of the subtree rooted at
@racket[n].
@interaction[#:eval my-eval
(define t (new-tree))
(for ([x (in-list '("ftl" "xcom" "civ"))])
  (insert-first/data! t x (string-length x)))
(node-data (minimum (tree-root t)))
]
Note: to get the minimum of the whole tree, it's faster to use
@racket[tree-first].
}


@defproc[(maximum [n node?]) node?]{
Given a node @racket[n], returns the maximum element of the subtree rooted at
@racket[n].
@interaction[#:eval my-eval
(define t (new-tree))
(for ([x (in-list '("ftl" "xcom" "civ"))])
  (insert-first/data! t x (string-length x)))
(node-data (maximum (tree-root t)))
]
Note: to get the maximum of the whole tree, it's faster to use
@racket[tree-last].
}


@defproc[(successor [n node?]) node?]{
Given a node @racket[n] contained in some tree, returns the immediate
successor of @racket[n] in an inorder traversal of that tree.

@interaction[#:eval my-eval
(define partial-alien-tree (new-tree))
(for ([name '("sectoid" "floater" "thin man" "chryssalid" 
              "muton" "cyberdisk")])
  (insert-last/data! partial-alien-tree name 1))
(define first-alien (tree-first partial-alien-tree))
(node-data (successor first-alien))
(node-data (successor (successor first-alien)))
]
}



@defproc[(predecessor [n node?]) node?]{
Given a node @racket[n] contained in some tree, returns the immediate
predecessor of @racket[n] in an inorder traversal of that tree.

@interaction[#:eval my-eval
(define partial-alien-tree (new-tree))
(for ([name '("sectoid" "floater" "thin man" "chryssalid" 
              "muton" "cyberdisk")])
  (insert-last/data! partial-alien-tree name 1))
(define last-alien (tree-last partial-alien-tree))
(node-data (predecessor last-alien))
(node-data (predecessor (predecessor last-alien)))
]
}



@defproc[(position [n node?]) natural-number/c]{
Given a node @racket[n] contained in some tree, returns the immediate
position of @racket[n] in that tree.

@interaction[#:eval my-eval
(define story-tree (new-tree))
(for ([word (string-split "if you give a mouse a cookie")])
  (insert-last/data! story-tree word (string-length word)))
(define a-pos (position (tree-last story-tree)))
a-pos
(node-data (search story-tree a-pos))
]
}



@defproc[(tree-items [t tree?]) (listof/c (list/c any/c natural-number/c))]{
Given a tree, returns a list of its data and width pairs.

@interaction[#:eval my-eval
(define t (new-tree))
(insert-last/data! t "rock" 4)
(insert-last/data! t "paper" 5)
(insert-last/data! t "scissors" 8)
(tree-items t)
]
}


@deftogether[
(@defproc[(tree-fold-inorder [t tree?] [f (node? any/c . -> . any)] [acc any/c]) any]{}
 @defproc[(tree-fold-preorder [t tree?] [f (node? any/c . -> . any)] [acc any/c]) any]{}
 @defproc[(tree-fold-postorder [t tree?] [f (node? any/c . -> . any)] [acc any/c]) any]{})]{

Iterates a function @racket[f] across the nodes of the tree, in inorder, preorder,
and postorder respectively.

@interaction[#:eval my-eval
(define t (new-tree))
(insert-last/data! t "three" 1)
(insert-last/data! t "blind" 1)
(insert-last/data! t "mice" 1)
@code:comment{"blind" should be the root, with}
@code:comment{"three" and "mice" as left and right.}
(define (f n acc) (cons (node-data n) acc))
(reverse (tree-fold-inorder t f '()))
(reverse (tree-fold-preorder t f '()))
(reverse (tree-fold-postorder t f '()))
]

}


@section{Uncontracted library}

This library uses contracts extensively to prevent the user from messing up;
however, the contract checking may be prohibitively
expensive for certain applications.

The uncontracted bindings of this library can be accessed through:

@racketblock[(require (submod syntax-color/private/red-black uncontracted))]

This provides the same bindings as the regular API, but with no contract
checks.  Use this with extreme care: Improper use of the uncontracted form of
this library may lead to breaking the red-black invariants, or (even worse)
introducing cycles in the structure.  If you don't know whether you should be
using the uncontracted forms or not, you probably should not.


@section{Bibliography}

@bibliography[
@bib-entry[#:key "clrs2009"
           #:title "Introduction to Algorithms, Third Edition"
           #:is-book? #t
           #:author "Thomas H. Cormen, Charles E. Leiserson, Ronald L. Rivest, Clifford Stein"
           #:date "2009"
           #:url "http://mitpress.mit.edu/books/introduction-algorithms"]

@bib-entry[#:key "wein2005"
           #:title "Efficient implementation of red-black trees with split and catenate operations"
           #:author "Ron Wein"
           #:date "2005"
           #:url "http://citeseerx.ist.psu.edu/viewdoc/summary?doi=10.1.1.109.4875"]
]




@close-eval[my-eval]