#lang scribble/doc
@(require scribble/manual
          scribble/eval
          (for-label syntax-color/private/augmented-red-black
                     racket/base
                     racket/string))

@(define my-eval (make-base-eval))
@(my-eval '(require syntax-color/private/augmented-red-black racket/string))

@title{Augmented Red-Black Trees}
@author+email["Danny Yoo" "dyoo@hashcollision.org"]


@defmodule[syntax-color/private/augmented-red-black]

This is an implementation of an augmented red-black tree that extends the nodes
of a basic red-black tree with attached metadata at every node.  The metadata
at a node should be a function of the data of the current node and the left and
right children.

One intended usage case of this structure is to maintain an ordered sequence of
items, where each item has an internal length.  Given such a sequence, we want
to support quick lookup by position and in-place insertions and deletions.  We
also want to support the catenation and splitting of sequences.

For example:

@interaction[#:eval my-eval
@code:comment{Here, the metadata represents the length of the contents}
@code:comment{of the entire subtree:}
(define (size-of-data data)
  (string-length data))
(define (new-catenated-string-tree)
  (new-tree #:metadata-f (lambda (data left right)
                           (+ (size-of-data data)
                              (or (node-metadata left) 0)
                              (or (node-metadata right) 0)))))
(define a-tree (new-catenated-string-tree))
(for ([w (in-list '("This" " " "is" " " "a" " " "test"))])
  (insert-last/data! a-tree w))

@code:comment{Assuming the metadata is correct at every node, we can search}
@code:comment{for a node by its "position" by using the metadata:}
(define (search a-tree offset)
  (let loop ([offset offset] [a-node (tree-root a-tree)])
    (cond
     [(nil-node? a-node) nil]
     [else
      (define left (node-left a-node))
      (define left-subtree-width (or (node-metadata left) 0))
      (cond [(< offset left-subtree-width)
             (loop offset left)]
            [else 
             (define residual-offset (- offset left-subtree-width))
             (define len (size-of-data (node-data a-node)))
             (cond
              [(< residual-offset len)
               a-node]
              [else
               (loop (- residual-offset len)
                     (node-right a-node))])])])))
@code:comment{Now we can search:}
(node-data (search a-tree 0))
(node-data (search a-tree 10))
(define at-test-node (search a-tree 10))
@code:comment{We can also insert within the tree,}
(insert-before/data! a-tree at-test-node "small")
(tree-items a-tree)
@code:comment{and split at the node holding "small".}
(define at-small-node (search a-tree 10))
(define-values (left-side right-side) (split! a-tree at-small-node))
(tree-items left-side)
(tree-items right-side)
(define joined-tree (join! left-side right-side))
(tree-items joined-tree)
]


The interpretation of the metadata is up to clients.  Another approprate
metadata may hold subtree @emph{size} rather than string length, in which case
the tree acts as an container where items can be found through their index:

@interaction[#:eval my-eval
@code:comment{The definitions above depend on the value of}
@code:comment{size-of-data.  Let's mutate it to be evil.}
@code:comment{(Note: don't do this in production code.)}
(set! size-of-data (lambda (data) 1))
@code:comment{And now we get a different kind of search altogether:}
(define t (new-catenated-string-tree))
(insert-last/data! t "rock")
(insert-last/data! t "scissors")
(insert-after/data! t (tree-first t) "paper")
(node-data (search t 0))
(node-data (search t 1))
(node-data (search t 2))
]


This augmented red-black tree implementation follows the basic outline in
@cite{clrs2009} and incorporates a few extensions suggsted in @cite{wein2005}.
As a red-black tree, the structure ensures that the tree's height is never
greater than @math{2*lg(#-of-nodes + 1)}, guaranteeing good worst-case behavior
for its operations.

The main types of values used in the library are @emph{trees} and @emph{nodes}.
A tree has a @emph{root} node (@racket[tree-root]), and each node has holds
arbitrary @emph{data} (@racket[node-data]) and @emph{metadata}
(@racket[node-metadata]), along with a reference to the elements smaller
(@racket[node-left]) and larger (@racket[node-right]).  The tree holds first
and last pointers into the structure to allow for fast access to the beginning
and end of the sequence.  A distinguished @racket[nil] node lies at the leaves
of the tree.



@section{API}
@declare-exporting[syntax-color/private/augmented-red-black]


@subsection{Data types}

@defproc[(new-tree [#:metadata-f metadata-f #f (or/c #f (any/c node? node? . -> . any))]) tree?]{
Constructs a new tree.  The tree's root is initially @racket[nil].
@interaction[#:eval my-eval
(define a-tree (new-tree))
a-tree
(nil-node? (tree-root a-tree))
]


When provided a @racket[#:metadata-f], each node in the tree will
have an associated @racket[node-metadata] that is computed through its
@racket[node-data], @racket[node-left] and @racket[node-right].

The @racket[#:metadata-f] must not mutate the tree as a side effect; contracts
currently do not enforce this requirement, but may in the future.}



@defproc[(tree? [x any/c]) boolean?]{
Returns @racket[#t] if @racket[x] is a tree.

@interaction[#:eval my-eval
(define a-tree (new-tree))
(tree? a-tree)
(tree? "not a tree")
(tree? (new-node '(not a tree either)))
]}



@defproc[(tree-root [t tree?]) node?]{
Returns the root node of the tree @racket[t].
If the tree is empty, returns the distinguished @racket[nil] node.

@interaction[#:eval my-eval
(nil-node? (tree-root (new-tree)))
(define a-tree (new-tree))
(define a-node (new-node "first node!"))
(insert-first! a-tree a-node)
(eq? a-node (tree-root a-tree))]
}


@defproc[(tree-metadata-f [t tree?]) (or/c #f (any/c node? node? . -> . any))]{
Returns the metadata-computing function for the tree @racket[t].

@interaction[#:eval my-eval
(define a-tree (new-tree))
(tree-metadata-f a-tree)
(define (indexed-metadata-f data left right)
  (+ 1 (or (node-metadata left) 0) (or (node-metadata right) 0)))
(define another-tree (new-tree #:metadata-f indexed-metadata-f))
(tree-metadata-f another-tree)
]

}



@defproc[(tree-first [t tree?]) node?]{
Returns the first node in the tree.

@interaction[#:eval my-eval
(define a-tree (new-tree))
(nil-node? (tree-first (new-tree)))
(define a-node (new-node "first node!"))
(define another-node (new-node "last node!"))
(insert-first! a-tree a-node)
(insert-last! a-tree another-node)
(eq? a-node (tree-first a-tree))]
}


@defproc[(tree-last [t tree?]) node?]{
Returns the last node in the tree.

@interaction[#:eval my-eval
(define a-tree (new-tree))
(nil-node? (tree-first (new-tree)))
(define a-node (new-node "first node!"))
(define another-node (new-node "last node!"))
(insert-first! a-tree a-node)
(insert-last! a-tree another-node)
(eq? another-node (tree-last a-tree))]
}


@defproc[(new-node [data any/c]) singleton-node?]{
Constructs a new singleton node.  This node can be inserted into a tree with
@racket[insert-first!], @racket[insert-last!], @racket[insert-before!], or
@racket[insert-after!], and spliced with @racket[concat!].

@interaction[#:eval my-eval
(new-node #("a" "node"))]
}


@defproc[(node? [x any/c]) boolean?]{
Returns @racket[#t] if @racket[x] is a node.
@interaction[#:eval my-eval
(node? (new-node #("a" "node")))
@code:comment{Trees are not nodes: they _have_ nodes.}
(node? (new-tree))
(node? (tree-root (new-tree)))
]
}


@defproc[(singleton-node? [x any/c]) boolean?]{
Returns @racket[#t] if @racket[x] is a @emph{singleton node}.  A singleton node
is unattached to any tree, and is not the @racket[nil] node.
@interaction[#:eval my-eval
(singleton-node? (new-node #("a" "node")))
(singleton-node? nil)

@code:comment{Create a fresh node:}
(define a-node (new-node "about to attach"))
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
@racket['black], its @racket[node-metadata] is @racket[#f], and its
@racket[node-parent], @racket[node-left], and @racket[node-right] are pointed
to itself.}


@defproc[(non-nil-node? [x any/c]) boolean?]{
Returns @racket[#t] if @racket[x] is a non-nil node.
@interaction[#:eval my-eval
(non-nil-node? nil)
(non-nil-node? (new-node "I am not a number"))
]
}


@defproc[(nil-node? [x any/c]) boolean?]{
Returns @racket[#t] if @racket[x] is the nil node.
@interaction[#:eval my-eval
(nil-node? nil)
(nil-node? (new-node "I am not a number"))
]
}


@defproc[(node-data [n node?]) any/c]{
Returns the data associated to node @racket[n].
@interaction[#:eval my-eval
(define a-node (new-node "utah"))
(node-data a-node)
]
}

@defproc[(update-node-data! [t tree?] [n node?] [v any/c]) void?]{

Assigns the data associated to node @racket[n].  Note that this also may update
the metadata of the tree if the tree has been constructed with a
@racket[#:metadata-f].

@interaction[#:eval my-eval
(define a-tree (new-tree))
(define a-node (new-node "utah"))
(insert-first! a-tree a-node)
(update-node-data! a-tree a-node "rhode island")
(node-data a-node)
]
}



@defproc[(node-metadata [n node?]) any/c]{
Returns the width of the entire subtree at node @racket[n].  This sums the
width of the left and right child subtrees, as well as its self-width.

@interaction[#:eval my-eval
(define (size-metadata str left right)
   (+ 1
      (or (node-metadata left) 0)
      (or (node-metadata right) 0)))
(define a-tree (new-tree #:metadata-f size-metadata))
(insert-last/data! a-tree "berkeley")
(insert-last/data! a-tree "stanford")
(insert-last/data! a-tree "wpi")
(insert-last/data! a-tree "brown")
(insert-last/data! a-tree "utah")
@code:comment{The entire tree should have a metadata of five, the size of the tree.}
(node-metadata (tree-root a-tree))
(node-metadata (node-left (tree-root a-tree)))
(node-metadata (node-right (tree-root a-tree)))
]
}



@defproc[(node-parent [n node?]) node?]{
Returns the parent of the node @racket[n].
@interaction[#:eval my-eval
(define a-tree (new-tree))
(insert-last/data! a-tree "bill and ted's excellent adventure")
(insert-last/data! a-tree "the matrix")
(insert-last/data! a-tree "speed")
(define p (node-parent (tree-last a-tree)))
(node-data p)]
}


@defproc[(node-left [n node?]) node?]{
Returns the left child of the node @racket[n].
@interaction[#:eval my-eval
(define a-tree (new-tree))
(insert-last/data! a-tree "bill and ted's excellent adventure")
(insert-last/data! a-tree "the matrix")
(insert-last/data! a-tree "speed")
(define p (node-left (tree-root a-tree)))
(node-data p)]
}


@defproc[(node-right [n node?]) node?]{
Returns the right child of the node @racket[n].
@interaction[#:eval my-eval
(define a-tree (new-tree))
(insert-last/data! a-tree "bill and ted's excellent adventure")
(insert-last/data! a-tree "the matrix")
(insert-last/data! a-tree "speed")
(define p (node-right (tree-root a-tree)))
(node-data p)]
}



@defproc[(node-color [n node?]) (or/c 'red 'black)]{

Returns the color of the node @racket[n].  The red-black tree structure uses
this value internally to maintain binary tree balance; most users will not need
to inspect this value.

@interaction[#:eval my-eval
(define a-tree (new-tree))
(insert-last/data! a-tree "the color purple")
(insert-last/data! a-tree "pretty in pink")
(insert-last/data! a-tree "the thin red line")
(insert-last/data! a-tree "clockwork orange")
(insert-last/data! a-tree "fried green tomatoes")
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
(insert-last/data! a-tree "the hobbit")
(insert-last/data! a-tree "the fellowship of the ring")
(red? (tree-root a-tree))
(red? (node-right (tree-root a-tree)))
]
}


@defproc[(black? [n node?]) boolean?]{
Returns @racket[#t] if node @racket[n] is black.
@interaction[#:eval my-eval
(define a-tree (new-tree))
(insert-last/data! a-tree "the fellowship of the ring")
(insert-last/data! a-tree "the two towers")
(insert-last/data! a-tree "return of the king")
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
(define a-node (new-node "pear"))
(insert-first! a-tree a-node)
(eq? (tree-root a-tree) a-node)
]

Note that attempting to add an attached, non-singleton node to a tree will
raise a contract error.
@interaction[#:eval my-eval
(define a-tree (new-tree))
(define a-node (new-node "persimmon"))
(insert-first! a-tree a-node)
(insert-first! a-tree a-node)
]
}

@defproc[(insert-last! [t tree?] [n singleton-node?]) void?]{
Adds node @racket[n] as the last element in tree @racket[t].
@interaction[#:eval my-eval
(define a-tree (new-tree))
(define a-node (new-node "apple"))
(insert-last! a-tree a-node)
(eq? (tree-root a-tree) a-node)
]

Note that attempting to add an attached, non-singleton node to a tree will
raise a contract error.
@interaction[#:eval my-eval
(define a-tree (new-tree))
(define a-node (new-node "orange"))
(insert-last! a-tree a-node)
(insert-last! a-tree a-node)
]
}



@defproc[(insert-before! [t tree?] [n1 node?] [n2 node?]) void?]{
Adds node @racket[n2] before node @racket[n1] in tree @racket[t].  This effectively
makes @racket[n2] the @racket[predecessor] of @racket[n1].
@interaction[#:eval my-eval
(define a-tree (new-tree))
(define a-node (new-node "banana"))
(define b-node (new-node "mango"))
(insert-first! a-tree a-node)
(insert-before! a-tree a-node b-node)
(eq? (predecessor a-node) b-node)
(eq? (successor b-node) a-node)
]

Note that attempting to add an attached, non-singleton node to a tree will
raise a contract error.
@interaction[#:eval my-eval
(define a-tree (new-tree))
(define a-node (new-node "peach"))
(insert-first! a-tree a-node)
(insert-before! a-tree a-node a-node)
]
}



@defproc[(insert-after! [t tree?] [n1 node?] [n2 node?]) void?]{
Adds node @racket[n2] after node @racket[n1] in tree @racket[t].  This effectively
makes @racket[n2] the @racket[successor] of @racket[n1].
@interaction[#:eval my-eval
(define a-tree (new-tree))
(define a-node (new-node "cherry"))
(define b-node (new-node "pawpaw"))
(insert-first! a-tree a-node)
(insert-after! a-tree a-node b-node)
(eq? (successor a-node) b-node)
(eq? (predecessor b-node) a-node)
]

Note that attempting to add an attached, non-singleton node to a tree will
raise a contract error.
@interaction[#:eval my-eval
(define a-tree (new-tree))
(define a-node (new-node "grapefruit"))
(insert-first! a-tree a-node)
(insert-after! a-tree a-node a-node)
]
}



@deftogether[
(
@defproc[(insert-first/data! [t tree?] [data any/c]) void?]{}
@defproc[(insert-last/data! [t tree?] [data any/c]) void?]{}
@defproc[(insert-before/data! [t tree?] [n node?] [data any/c]) void?]{}
@defproc[(insert-after/data! [t tree?] [n node?] [data any/c]) void?]{})
]{

For user convenience, the functions @racket[insert-first/data!],
@racket[insert-last/data!], @racket[insert-before/data!], and
@racket[insert-after/data!] have been provided.  These create nodes and insert
into the tree structure the same way as @racket[insert-first!],
@racket[insert-last!], @racket[insert-before!], and @racket[insert-after!].

@interaction[#:eval my-eval
(define t (new-tree))
(insert-first/data! t "message in a bottle")
(insert-last/data! t "don't stand so close to me")
(insert-before/data! t (tree-first t) "everything she does is magic")
(insert-after/data! t (tree-last t) "king of pain")
(tree-items t)
]
}


@defproc[(delete! [t tree?] [n non-nil-node?]) void?]{
Deletes node @racket[n] from the tree @racket[t].  After deletion, @racket[n]
will become a singleton node.
@interaction[#:eval my-eval
(define t (new-tree))
(define n1 (new-node "George, George, George of the Jungle,"))
(define n2 (new-node "strong as he can be..."))
(define n3 (new-node "aaaaaaaaaaah!"))
(define n4 (new-node "watch out for that..."))
(define n5 (new-node "<thump!>"))
(define n6 (new-node "treeeeeeeeee!, "))
(for ([n (in-list (list n1 n2 n3 n4 n5 n6))])
  (insert-last! t n))
(delete! t n5)
(tree-items t)
]

Note that @racket[n] must be attached to tree @racket[t] or else will raise
a contract error:
@interaction[#:eval my-eval
(define t1 (new-tree))
(insert-first/data! t1 "tricky")
(define n (new-node "tricky"))
@code:comment{This should raise an error:}
(delete! t1 n)
]}


@defproc[(join! [t1 tree?] [t2 tree?]) tree?]{
Destructively joins trees @racket[t1] and @racket[t2], returning a tree that
has the contents of both.  Every element in @racket[t1] is treated less than
the elements in @racket[t2].

@interaction[#:eval my-eval
(define t1 (new-tree))
(for ([name (in-list '(goku gohan krillin piccolo vegeta))])
  (insert-last/data! t1 name))
@code:comment{Tier two characters:}
(define t2 (new-tree))
(for ([name (in-list '(yamcha tien chiaotzu bulma chi-chi
                       roshi))])
  (insert-last/data! t2 name))
(define tree-of-mighty-z-warriors (join! t1 t2))
(tree-items tree-of-mighty-z-warriors)
]

Note that @racket[t1] and @racket[t2] should share the same
@racket[tree-metadata-f] and neither tree should be @racket[eq?] to the other.
Violations of either condition will raise a contract error.

@interaction[#:eval my-eval
(define t1 (new-tree))
(join! t1 t1)
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
(insert-last/data! t1 "inigo")
(define x (new-node "vizzini"))
(insert-last/data! t2 "fezzik")
(define poor-lost-circus-performers (concat! t1 x t2))
(tree-items poor-lost-circus-performers)
]


Note that @racket[t1] and @racket[t2] should share the same
@racket[tree-metadata-f] and neither tree should be @racket[eq?] to the other.
Violations of either condition will raise a contract error.

@interaction[#:eval my-eval
(define (f1 data left right) 1)
(define (f2 data left right) 1)
@code:comment{f1 and f2 are distinct function values: they won't compare the same.}
(define t1 (new-tree #:metadata-f f1))
(define t2 (new-tree #:metadata-f f2))
(define n (new-node "a-node"))
(concat! t1 n t2)
]
}


@defproc[(split! [t tree?] [n non-nil-node?]) (values tree? tree?)]{
Destructively splits tree @racket[t] into two trees, the first containing the
elements smaller than node @racket[n], and the second containing those larger.
Afterwards, @racket[n] becomes a singleton node.

@interaction[#:eval my-eval
(define t (new-tree))
(for ([name '(melchior caspar bob balthazar)])
  (insert-last/data! t name))
(define bob-node (predecessor (tree-last t)))
(singleton-node? bob-node)
(define-values (l r) (split! t bob-node))
@code:comment{We tree kings of orient are:}
(append (tree-items l) (tree-items r))
(singleton-node? bob-node)
]

Note that @racket[n] must be attached to tree @racket[t] or else raise
a contract error.
@interaction[#:eval my-eval
(define t (new-tree))
(for ([name '(melchior caspar bob balthazar)])
  (insert-last/data! t name))
@code:comment{This should raise an error:}
(define t2 (new-tree))
(insert-last! t2 (new-node "bob"))
(split! t (tree-root t2))
]}


@defproc[(reset! [t tree?]) void?]{
Resets the contents of the tree to the empty state.
@interaction[#:eval my-eval
(define t (new-tree))
(insert-last/data! t "house")
(insert-last/data! t "cleaning")
(tree-items t)
(reset! t)
(tree-items t)]
}



@defproc[(minimum [n node?]) node?]{
Given a node @racket[n], returns the minimum element of the subtree rooted at
@racket[n].
@interaction[#:eval my-eval
(define t (new-tree))
(for ([x (in-list '("ftl" "xcom" "civ"))])
  (insert-first/data! t x))
(node-data (minimum (tree-root t)))
]
Note: to get the minimum of a whole tree, it's faster to use
@racket[tree-first].
}


@defproc[(maximum [n node?]) node?]{
Given a node @racket[n], returns the maximum element of the subtree rooted at
@racket[n].
@interaction[#:eval my-eval
(define t (new-tree))
(for ([x (in-list '("ftl" "xcom" "civ"))])
  (insert-first/data! t x))
(node-data (maximum (tree-root t)))
]
Note: to get the maximum of a whole tree, it's faster to use
@racket[tree-last].
}


@defproc[(successor [n node?]) node?]{
Given a node @racket[n] contained in some tree, returns the immediate
successor of @racket[n] in an inorder traversal of that tree.

@interaction[#:eval my-eval
(define partial-alien-tree (new-tree))
(for ([name '("sectoid" "floater" "thin man" "chryssalid" 
              "muton" "cyberdisk")])
  (insert-last/data! partial-alien-tree name))
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
  (insert-last/data! partial-alien-tree name))
(define last-alien (tree-last partial-alien-tree))
(node-data (predecessor last-alien))
(node-data (predecessor (predecessor last-alien)))
]
}



@defproc[(tree-items [t tree?]) (listof/c (list/c any/c natural-number/c))]{
Given a tree, returns a list of its data and width pairs.

@interaction[#:eval my-eval
(define t (new-tree))
(insert-last/data! t "rock")
(insert-last/data! t "paper")
(insert-last/data! t "scissors")
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
(insert-last/data! t "three")
(insert-last/data! t "blind")
(insert-last/data! t "mice")
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