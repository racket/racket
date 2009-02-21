#reader "literate-reader.ss"

@(require (for-label scheme/math) ;; for 'pi' below
          scheme/math)

@;{
The command to build this:

mzc chat-noir-doc.ss && rm -rf chat-noir-doc && scribble ++xref-in setup/xref load-collections-xref --htmls chat-noir-doc.ss

}

@title{Chat Noir}

@author[(link "http://www.eecs.northwestern.edu/~robby" "Robby Findler")
        (link "http://www.barzilay.org/" "Eli Barzilay")
        (link "http://www.cs.utah.edu/~mflatt/" "Matthew Flatt")]

The goal of Chat Noir is to stop the cat from escaping the board. Each
turn you click on a circle, which prevents the cat from stepping on
that space, and the cat responds by taking a step. If the cat is
completely boxed in and thus unable reach the border, you win. If the
cat does reach the border, you lose.

To get some insight into the cat's behavior, hold down the ``h''
key. It will show you the cells that are on the cat's shortest path to
the edge, assuming that the cell underneath the mouse has been
blocked, so you can experiment to see how the shortest paths change
by moving your mouse around.

The game was inspired by this one the one at
@link["http://www.gamedesign.jp/flash/chatnoir/chatnoir.html"]{Game
Design} and has essentially the same rules. It also inspired the final
project for the introductory programming course at the University of
Chicago in the fall of 2008.

The remainder of this document explains the implementation of 
the Chat Noir game in a 
@link["http://www.literateprogramming.com/"]{Literate Programming} style.

@section{Overview}

Chat Noir is implemented using @link["http://www.htdp.org/"]{HtDP}'s universe
library:  @schememodname[teachpack/2htdp/universe] 
(although it only uses the ``world'' portions of that library). 
The program is divided up into
six parts: the world data definition, an implementation of breadth-first search,
code that handles drawing of the world, code that handles user input,
and some code that builds an initial world and starts the game.
 
@chunk[<main>
       (require scheme/local scheme/list scheme/bool scheme/math
                lang/private/imageeq ;; don't like this require, but need it for image?
                (for-syntax scheme/base))
       (require htdp/world lang/posn scheme/contract)
       <world>
       <breadth-first-search>
       <board->graph>
       <cats-path>
       <drawing-the-cat>
       <drawing>
       <input>
       <tests>
       <go>]

Each section also comes with a series of test cases that are collected into the 
@chunkref[<tests>] chunk at the end of the program.

@chunk[<tests>
       <test-infrastructure>
       <world-tests>
       <board->graph-tests>
       <breadth-first-search-tests>
       <cats-path-tests>
       <drawing-tests>
       <input-tests>]

Each test case uses either @scheme[test], a simple form that accepts two
arguments and compares them with @scheme[equal?], or @scheme[test/set]
which accepts two lists and compares them as if they were sets.

In general, most of the test cases are left to the end of the document, organized
in a series of chunks that match the functions being tested. Some of the test cases,
however, provide nice illustrations of the behavior of the function and so are
included in the function's description.

@section{The World}

The main data structure for Chat Noir is @tt{world}. It comes with a few functions that 
construct empty worlds and test cases for them.

@chunk[<world> 
       <cell-struct> <world-struct> <empty-world> <empty-board> <blocked-cells>]

@chunk[<world-tests>
       <empty-world-test> <empty-board-test> <blocked-cells-tests>]

The main structure definition is the @scheme[world] struct.

@chunk[<world-struct>
(define-struct/contract world ([board (listof cell?)]
                               [cat posn?]
                               [state (or/c 'playing 
                                            'cat-won
                                            'cat-lost)]
                               [size (and/c natural-number/c
                                            odd?
                                            (>=/c 3))]
                               [mouse-posn (or/c #f posn?)]
                               [h-down? boolean?])
  #:transparent)
]

It consists of a structure with six fields:
@itemize[

@item{@tt{board}: representing the state of the board as a list of
  @tt{cell}s, one for each circle on the game. 
  }

@item{@tt{cat}: a @scheme[posn] indicating the position of the cat
  (interpreting the @scheme[posn] in the way that they are interpreted
  for the @tt{board} field),}

@item{@tt{state}: the state of the game, which can be one of
  @itemize[
  @item{@scheme['playing], indicating that the game is still going; this is the
    initial state.}
  @item{@scheme['cat-won], indicating that the game is over and the
    cat won, or}
  @item{@scheme['cat-lost], indicating that the game is over and the
    cat lost.}]}

@item{@tt{size}: an odd natural number indicating the size of the board}

@item{@tt{mouse-posn}: a @scheme[posn] for the location of the
  mouse (or @scheme[#f] if the mouse is not in the window), and}

@item{@tt{h-down?}: a boolean indicating if the @tt{h} key is being
  pushed down.}
]

A @scheme[cell] is a structure with two fields:

@chunk[<cell-struct>
       (define-struct/contract cell ([p posn?]
                                     [blocked? boolean?])
         #:transparent)]
       
The coordinates of
the @scheme[posn] in the first field
indicate a position on the hexagonal grid. 
This program reprsents the hexagon grid as a series of rows that
are offset from each other by 1/2 the size of the each cell.
The @tt{y} field
of the @scheme[posn] refers to the row of the cell, and the @tt{x}
coordinate the position in the row.  This means that, for example,
@scheme[(make-posn 1 0)] is centered above @scheme[(make-posn 1 0)]
and @scheme[(make-posn 1 1)]. 

The boolean in the @tt{blocked?} field indicates if the cell has been
clicked on, thus blocking the cat from stepping there.

The @scheme[empty-board] function builds a list of @scheme[cell]s
that correspond to an empty board. For example, here's what an empty
7x7 board looks like, as a list of cells.

@image["7x7-empty-board.png"]

It contains 7 rows and, with the exception of the first and last rows,
each row contains 7 cells. Notice how the even and odd rows are offset
from each other by 1/2 of the size of the cell.
The first and last row are missing their left-most cells
because those cells are useless, from the perspective of the gameplay, 
Specifically, all of the neighbors of the missing cells
are also on the boundary and thus
the cat would win if it ever steps on one of those neighboring cells,
ending the game.

The 3x3 board also has the same property that it consists of three
rows, each with three cells, but where the first and last row are missing
their left-most cells.

@image["3x3-empty-board.png"]

And here is how that board looks as a list of cells.

@chunk[<empty-board-test>
       
       (test (empty-board 3)
             (list
              (make-cell (make-posn 0 1) false)
              (make-cell (make-posn 1 0) false)
              (make-cell (make-posn 1 1) false)
              (make-cell (make-posn 1 2) false)
              (make-cell (make-posn 2 0) false)
              (make-cell (make-posn 2 1) false)
              (make-cell (make-posn 2 2) false)))]

The @scheme[empty-board] function consists
of two (nested) calls to @scheme[build-list] 
that build a list of lists of cells, one for
each pair of coordinates between @scheme[0]
and @scheme[board-size]. Then, @scheme[append]
flattens the nested lists and the
@scheme[filter] expression removes the corners.

@chunk[<empty-board>
       (define/contract (empty-board board-size)
         (-> (and/c natural-number/c odd? (>=/c 3))
             (listof cell?))
         (filter
          (not-corner? board-size)
          (apply
           append
           (build-list
            board-size
            (lambda (i)
              (build-list
               board-size
               (lambda (j)
                 (make-cell (make-posn i j)
                            false))))))))
       
       (define/contract ((not-corner? board-size) c)
         (-> (and/c natural-number/c odd? (>=/c 3))
             (-> cell?
                 boolean?))
         (not (and (= 0 (posn-x (cell-p c)))
                   (or (= 0 (posn-y (cell-p c)))
                       (= (- board-size 1)
                          (posn-y (cell-p c)))))))]

Building an empty world is simply 
a matter of building an empty board, finding 
the initial position of the cat and filling
in all of the fields of the @scheme[world] struct.
For example, this is the empty world of size @scheme[3].
It puts the cat at @scheme[(make-posn 1 1)],
sets the state to @scheme['playing], records the
size @scheme[3], and sets the current mouse position
to @scheme[false] and the state of the ``h'' key to
@scheme[false].

@chunk[<empty-world-test>
       
       (test (empty-world 3)
             (make-world (empty-board 3)
                         (make-posn 1 1)
                         'playing
                         3
                         false
                         false))]


The @scheme[empty-world] function
generalizes the exmaple by computing the
cats initial position as the center spot on the board.

@chunk[<empty-world>
       
       (define/contract (empty-world board-size)
         (-> (and/c natural-number/c odd? (>=/c 3))
             world?)
         (make-world (empty-board board-size)
                     (make-posn (quotient board-size 2)
                                (quotient board-size 2))
                     'playing
                     board-size
                     false
                     false))]

@chunk[<blocked-cells>
       
       ;; add-n-random-blocked-cells : number (listof cell) number -> (listof cell)
       (define (add-n-random-blocked-cells n all-cells board-size)
         (cond
           [(zero? n) all-cells]
           [else
            (local [(define unblocked-cells
                      (filter (lambda (x)
                                (let ([cat-cell? (and (= (posn-x (cell-p x))
                                                         (quotient board-size 2))
                                                      (= (posn-y (cell-p x))
                                                         (quotient board-size 2)))])
                                  
                                  (and (not (cell-blocked? x))
                                       (not cat-cell?))))
                              all-cells))
                    (define to-block (list-ref unblocked-cells
                                               (random (length unblocked-cells))))]
              (add-n-random-blocked-cells
               (sub1 n)
               (block-cell (cell-p to-block) all-cells)
               board-size))]))
       
       ;; block-cell : posn board -> board
       (define (block-cell to-block board)
         (map (lambda (c) (if (equal? to-block (cell-p c))
                              (make-cell to-block true)
                              c))
              board))]

@chunk[<blocked-cells-tests>
       (test (block-cell (make-posn 1 1)
                         (list (make-cell (make-posn 0 0) false)
                               (make-cell (make-posn 1 1) false)
                               (make-cell (make-posn 2 2) false)))
             (list (make-cell (make-posn 0 0) false)
                   (make-cell (make-posn 1 1) true)
                   (make-cell (make-posn 2 2) false)))
       
       (test (add-n-random-blocked-cells 0 (list (make-cell (make-posn 0 0)
                                                            true))
                                         10)
             (list (make-cell (make-posn 0 0) true)))
       (test (add-n-random-blocked-cells 1 (list (make-cell (make-posn 0 0)
                                                            false))
                                         10)
             (list (make-cell (make-posn 0 0) true)))]

@section{Breadth-first Search}

The cat's move decision is based on a breadth-first search of a graph.
The graph's nodes are the cells on the board plus a special
node called @scheme['boundary] that is adjacent to every cell 
on the boundary of the graph. In addition to the boundary edges,
there are edges
between each pair of adjacent cells, unless one of the cells is
blocked, in which case it has no edges at all (even to the boundary).

This section describes the implementation of the breadth-first search, leaving
details of how the graph connectivity is computed from the board to the next section.

@chunk[<breadth-first-search>
       <dist-cell-data-definition>
       <lookup-in-table>
       <build-bfs-table>]
       
@chunk[<breadth-first-search-tests>
       <lookup-in-table-tests>
       <build-bfs-table-tests>]

The breadth-first function constructs a @scheme[distance-map],
which is a list of @scheme[dist-cell] structs:

@chunk[<dist-cell-data-definition>
       (define-struct/contract dist-cell ([p (or/c 'boundary posn?)]
                                          [n natural-number/c])
         #:transparent)]

Each @tt{p} field in the @scheme[dist-cell] is a position on the board
and the @tt{n} field is a natural number, indicating
the distance of the shortest path from the node to some fixed point on
the board. 

The function @scheme[lookup-in-table] returns the distance from the fixed
point to the given posn, returning @scheme['∞] if the posn is not in the
table.

@chunk[<lookup-in-table>
       (define/contract (lookup-in-table t p)
         (-> (listof dist-cell?) posn?
             (or/c '∞ natural-number/c))
         (cond
           [(empty? t) '∞]
           [else (cond
                   [(equal? p (dist-cell-p (first t)))
                    (dist-cell-n (first t))]
                   [else
                    (lookup-in-table (rest t) p)])]))]

The @scheme[build-bfs-table] accepts a world and a cell 
(indicating the fixed point) 
and returns a distance map encoding the distance to that cell.
For example, here is the distance map for the distance to the boundary.

@chunk[<build-bfs-table-tests>
       (test/set (build-bfs-table (empty-world 3)
                                  'boundary)
                 (list
                  (make-dist-cell 'boundary 0)
                  
                  (make-dist-cell (make-posn 1 0) 1)
                  (make-dist-cell (make-posn 2 0) 1)
                  
                  (make-dist-cell (make-posn 0 1) 1)
                  (make-dist-cell (make-posn 1 1) 2)
                  (make-dist-cell (make-posn 2 1) 1)
                  
                  (make-dist-cell (make-posn 1 2) 1)
                  (make-dist-cell (make-posn 2 2) 1)))]

The boundary is zero steps away; each of the cells that are on the boundary
are one step away and the center is two steps away.

The core of the breadth-first search is this function,
@scheme[bst]. It accepts a queue of the pending nodes to visit
and a @scheme[dist-table] that records the same information as a
@scheme[distance-map], but in an immutable hash-table. The
@scheme[dist-map] is an accumulator, recording the distances
to all of the nodes that have already been visited in the graph,
and is used here to speed up the compuation. The queue is
represented as a list of vectors of length two. Each element
in the queue contains a @scheme[posn], or the symbol @scheme['boundary]
and that @scheme[posn]'s  distance.

@chunk[<bfs>
       
       (define/contract (bfs queue dist-table)
         (-> (listof (vector/c (or/c 'boundary posn?) natural-number/c))
             hash?
             hash?)
         #:freevar neighbors/w (-> (or/c 'boundary posn?)
                                   (listof (or/c 'boundary posn?)))
         (cond
           [(empty? queue) dist-table]
           [else
            (let* ([p (vector-ref (first queue) 0)]
                   [dist (vector-ref (first queue) 1)])
              (cond
                [(hash-ref dist-table p #f)
                 (bfs (rest queue) dist-table)]
                [else
                 (bfs
                  (append (rest queue)
                          (map (λ (p) (vector p (+ dist 1)))
                               (neighbors/w p)))
                  (hash-set dist-table p dist))]))]))]

If the @scheme[queue] is empty, then the accumulator contains
bindings for all of the (reachable) nodes in the graph, so
we just return it. If it isn't empty, then we extract
the first element from the queue and name its consituents
@scheme[p] and @scheme[dist].
Next we check to see if the node at the head of the queue
is in @scheme[dist-table]. If it is, we just move on to the
next element in the queue. If that node is not in the @scheme[dist-table],
then we add all of the neighbors to the queue, in the @scheme[append]
expression, and update the @scheme[dist-table] with the distance to 
this node. Because we always add the new children to the end of the queue
and always look at the front of the queue, we are guaranteed that
the first time we see a node, it will be with the shortest distance.

The @scheme[build-bfs-table] function packages up @scheme[bfs]
function. It accepts a @scheme[world] and an initial position
and returns a @scheme[distance-table].

@chunk[<build-bfs-table>
       
       (define/contract (build-bfs-table world init-point)
         (-> world? (or/c 'boundary posn?)
             (listof dist-cell?))
         (define neighbors/w (neighbors world))
         <bfs>
         
         (hash-map
          (bfs (list (vector init-point 0))
               (make-immutable-hash '()))
          make-dist-cell))]

As you can see, the first thing it does is bind the free variable in @scheme[bfs]
to the result of calling the @scheme[neighbors] function (defined in the chunk
@chunkref[<neighbors>]) and then it has the @chunkref[<bfs>] chunk. In the body
it calls the @scheme[bfs] function 
and then transforms the result, using
@scheme[hash-map], into a list of @scheme[cell]s.

@section{Board to Graph}

As far as the @scheme[build-bfs-table] function goes,
all of the information specific to Chat Noir is
encoded in the neighbors function. 
It accepts a world and returns a function
that computes the neighbors of the boundary
and of nodes. This section describes how
it is implemented.

@chunk[<board->graph>
       <neighbors>
       <neighbors-blocked/boundary>
       <adjacent>
       <in-bounds?>
       <on-boundary?>]

@chunk[<board->graph-tests>
       <on-boundary?-tests>
       <in-bounds?-tests>
       <adjacent-tests>
       <neighbors-tests>]

The neighbors functions accepts a @scheme[world] and then
returns a function that computes the neighbors of a @scheme[posn]
and of the @scheme['boundary].

For example, @scheme[(make-posn 1 0)] has four
neighbors:

@chunk[<neighbors-tests>
       (test ((neighbors (empty-world 7)) (make-posn 1 0))
             (list 'boundary
                   (make-posn 2 0)
                   (make-posn 0 1)
                   (make-posn 1 1)))]

and @scheme[(make-posn 0 1)] has four neighbors:

@chunk[<neighbors-tests>
       (test ((neighbors (empty-world 7)) (make-posn 0 1))
             (list 'boundary
                   (make-posn 1 0)
                   (make-posn 1 1)
                   (make-posn 0 2)
                   (make-posn 1 2)))]

as you can see in the earlier pictures of the 7x7 empty board.
Also, there are 6 neighbors of the boundary in the 3x3 board:

@chunk[<neighbors-tests>
       (test ((neighbors (empty-world 3)) 'boundary)
             (list (make-posn 0 1)
                   (make-posn 1 0)
                   (make-posn 1 2)
                   (make-posn 2 0)
                   (make-posn 2 1)
                   (make-posn 2 2)))]

This is the neighbors function. After it accepts the @scheme[world],
it builds a list of the blocked cells in the world and a
list of the cells that are on the boundary (and not blocked). Then it
returns a function that is specialized to those values.

@chunk[<neighbors>
(define/contract (neighbors w)
  (-> world?
      (-> (or/c 'boundary posn?)
          (listof (or/c 'boundary posn?))))
  (define blocked
    (map cell-p
         (filter (lambda (c)
                   (or (cell-blocked? c)
                       (equal? (cell-p c) (world-mouse-posn w))))
                 (world-board w))))
  (define boundary-cells
    (filter (lambda (p)
              (and (not (member p blocked))
                   (on-boundary? p (world-size w))))
            (map cell-p (world-board w))))
  (λ (p)
    (neighbors-blocked/boundary blocked
                                boundary-cells
                                (world-size w)
                                p)))]

The @scheme[neighbors-blocked/boundary] function is given next.
If @scheme[p] is blocked, it returns the empty list. If it is
on the boundary, the function simply returns @scheme[boundary-cells].
Otherwise, @scheme[neighbors-blocked/boundary] calls
@scheme[adjacent] to compute the posns that are adjacent to @scheme[p],
filtering out the blocked @scheme[posn]s and binds that to @scheme[adjacent-posns].
It then filters out the @scheme[posn]s that would be outside of the board.
If those two lists are the same, then @scheme[p] is not on the boundary,
so we just return @scheme[in-bounds]. If the lists are different, then
we know that @scheme[p] must have been on the boundary, so we add
@scheme['boundary] to the result list.

@chunk[<neighbors-blocked/boundary>
(define/contract (neighbors-blocked/boundary blocked 
                                             boundary-cells
                                             size
                                             p)
  (-> (listof posn?)
      (listof posn?)
      natural-number/c
      (or/c 'boundary posn?)
      (listof (or/c 'boundary posn?)))

  (cond
    [(member p blocked)
     '()]
    [(equal? p 'boundary)
     boundary-cells]
    [else
     (let* ([x (posn-x p)]
            [adjacent-posns 
             (filter (λ (x) (not (member x blocked)))
                     (adjacent p))]
            [in-bounds
             (filter (λ (x) (in-bounds? x size))
                     adjacent-posns)])
       (cond
         [(equal? in-bounds adjacent-posns)
          in-bounds]
         [else
          (cons 'boundary in-bounds)]))]))]


There are the three functions that build the basic graph structure
from a board as used by @scheme[neighbors].

The first function is @scheme[adjacent]. It consumes a
@scheme[posn] and returns six @scheme[posn]s that
indicate what the neighbors are, without consideration
of the size of the board (or the missing corner pieces).

For example, these are the @scheme[posn]s that are adjacent
to @scheme[(make-posn 0 1)]; note that the first and the third
are not on the board and do not show up in 
@scheme[neighbors] function example above.

@chunk[<adjacent-tests>
       (test (adjacent (make-posn 0 1))
             (list (make-posn 0 0)
                   (make-posn 1 0)
                   (make-posn -1 1)
                   (make-posn 1 1)
                   (make-posn 0 2)
                   (make-posn 1 2)))]

The adjacent function has two main cases; first when the
@scheme[y] coordinate of the @scheme[posn] is even and 
second when it is odd. In each case, it is just a matter
of looking at the board and calculating coordinate offsets.

@chunk[<adjacent>
       (define/contract (adjacent p)
         (-> posn? 
             (and/c (listof posn?)
                    (lambda (l) (= 6 (length l)))))
         (local [(define x (posn-x p))
                 (define y (posn-y p))]
           (cond
             [(even? y)
              (list (make-posn (- x 1) (- y 1))
                    (make-posn x (- y 1))
                    (make-posn (- x 1) y)
                    (make-posn (+ x 1) y)
                    (make-posn (- x 1) (+ y 1))
                    (make-posn x (+ y 1)))]
             [else
              (list (make-posn x (- y 1))
                    (make-posn (+ x 1) (- y 1))
                    (make-posn (- x 1) y)
                    (make-posn (+ x 1) y)
                    (make-posn x (+ y 1))
                    (make-posn (+ x 1) (+ y 1)))])))]

The @scheme[on-boundary?] function returns @scheme[true] when 
the posn would be on the boundary of a board of size
@scheme[board-size]. Note that this function does not
have to special case the missing @scheme[posn]s from the corners.

@chunk[<on-boundary?>
       (define/contract (on-boundary? p board-size)
         (-> posn? natural-number/c 
             boolean?)
         (or (= (posn-x p) 0)
             (= (posn-y p) 0)
             (= (posn-x p) (- board-size 1))
             (= (posn-y p) (- board-size 1))))]

The @scheme[in-bounds?] function returns @scheme[true]
when the @scheme[posn] is actually on the board, meaning
that the coordinates of the @scheme[posn] are within the
board's size, and that the @scheme[posn] is not one
of the two corners that have been removed.

@chunk[<in-bounds?>
       (define/contract (in-bounds? p board-size)
         (-> posn? natural-number/c
             boolean?)
         (and (<= 0 (posn-x p) (- board-size 1))
              (<= 0 (posn-y p) (- board-size 1))
              (not (equal? p (make-posn 0 0)))
              (not (equal? p (make-posn 0 (- board-size 1))))))]

@section{The Cat's Path}

Once we have a breadth-first search all sorted out, we can use it to build a function that 
determines where the shortest paths from the cat's current position to the boundary are.

@chunk[<cats-path>
       <on-cats-path?>
       <+/f>]

@chunk[<cats-path-tests>
       <on-cats-path?-tests>
       <+/f-tests>]

The function @scheme[on-cats-path?] accepts a world and returns a predicate
on the @scheme[posn]s in the world. The predicate indicates if the given
@scheme[posn] is on the shortest path. 

For example, in a world of size @scheme[7] with the cat at
@scheme[(make-posn 2 2)], the circles with white centers
are on the shortest path to the boundary:

@image["cat-distance-example.png"]

So we can formulate two test cases using this world, one
in the white circles and one not:

@chunk[<on-cats-path?-tests>
       (let ([on-the-path?
              (on-cats-path? (make-world (empty-board 7)
                                         (make-posn 2 2)
                                         'playing
                                         7
                                         false
                                         true))])
         (test (on-the-path? (make-posn 1 0))
               true)
         (test (on-the-path? (make-posn 4 4))
               false))]

The computation of the shortest path to the boundary proceeds by computing
two distance maps; the distance map to the boundary and the distance map
to the cat. Then, a node is on one of the shortest paths if the distance
to the cat plus the distance to the boundary is equal to the distance from
the cat to the boundary. 

The code is essentially that, plus two other special cases. Specifically if the
``h'' key is not pressed down, then we just consider no cells to be on that shortest
path. And if the distance to the cat is @scheme['∞], then again no nodes are on the
path. The second situation happens when the cat is completely boxed in and has
lost the game.

@chunk[<on-cats-path?>
       (define/contract (on-cats-path? w)
         (-> world? (-> posn? boolean?))
         (cond
           [(world-h-down? w)
            (let ()
              (define edge-distance-map (build-bfs-table w 'boundary))
              (define cat-distance-map (build-bfs-table w (world-cat w)))
              (define cat-distance
                (lookup-in-table edge-distance-map (world-cat w)))
              (cond
                [(equal? cat-distance '∞)
                 (lambda (p) false)]
                [else
                 (lambda (p)
                   (equal? (+/f (lookup-in-table cat-distance-map p)
                                (lookup-in-table edge-distance-map p))
                           cat-distance))]))]
           [else
            (lambda (p) false)]))]

Finally, the helper function @scheme[+/f] is just like @scheme[+], except that
it returns @scheme['∞] if either argument is @scheme['∞].

@chunk[<+/f>
       (define (+/f x y)
         (cond
           [(or (equal? x '∞) (equal? y '∞))
            '∞]
           [else
            (+ x y)]))]

@section{Drawing the Cat}

This code is three large, similar constants,
bundled up into the @scheme[cat] function. 
The @scheme[thinking-cat] is the one that 
is visible when the game is being played. It
differs from the others in that it does not
have a mouth. The @scheme[mad-cat] is the one
that you see when the cat loses. It differs
from the others in that its pinks turn pink.
Finally, the @scheme[happy-cat] shows up when
the cat wins and it is just like the @scheme[thinking-cat]
except it has a smile.

@chunk[<drawing-the-cat>
       (define/contract (cat mode)
         (-> (or/c 'mad 'happy 'thinking) image?)
         (local [(define face-width 36)
                 (define face-height 22)
                 
                 (define face-color
                   (cond
                     [(symbol=? mode 'mad) 'pink]
                     [else 'lightgray]))
                 
                 (define left-ear 
                   (regular-polygon 3 8 'solid 'black (/ pi -3)))
                 (define right-ear
                   (regular-polygon 3 8 'solid 'black 0))
                 (define ear-x-offset 14)
                 (define ear-y-offset 9)
                 
                 (define eye (overlay (ellipse 12 8 'solid 'black)
                                      (ellipse 6 4 'solid 'limegreen)))
                 (define eye-x-offset 8)
                 (define eye-y-offset 3)
                 
                 (define nose
                   (regular-polygon 3 5 'solid 'black (/ pi 2)))
                 
                 (define mouth-happy
                   (overlay (ellipse 8 8 'solid face-color)
                            (ellipse 8 8 'outline 'black)
                            (move-pinhole
                             (rectangle 10 5 'solid face-color)
                             0
                             4)))
                 (define mouth-no-expression
                   (overlay (ellipse 8 8 'solid face-color)
                            (ellipse 8 8 'outline face-color)
                            (rectangle 10 5 'solid face-color)))
                 
                 (define mouth
                   (cond
                     [(symbol=? mode 'happy) mouth-happy]
                     [else mouth-no-expression]))
                 (define mouth-x-offset 4)
                 (define mouth-y-offset -5)
                 
                 (define (whiskers img)
                   (add-line
                    (add-line
                     (add-line
                      (add-line
                       (add-line
                        (add-line
                         img
                         6 4 30 12 'black)
                        6 4 30 4 'black)
                       6 4 30 -4 'black)
                      -6 4 -30 12 'black)
                     -6 4 -30 4 'black)
                    -6 4 -30 -4 'black))]
           (whiskers
            (overlay
             (move-pinhole left-ear (- ear-x-offset) ear-y-offset)
             (move-pinhole right-ear (- ear-x-offset 1) ear-y-offset)
             (ellipse (+ face-width 4) (+ face-height 4) 'solid 'black)
             (ellipse face-width face-height 'solid face-color)
             (move-pinhole mouth (- mouth-x-offset) mouth-y-offset)
             (move-pinhole mouth mouth-x-offset mouth-y-offset)
             (move-pinhole eye (- eye-x-offset) eye-y-offset)
             (move-pinhole eye eye-x-offset eye-y-offset)
             (move-pinhole nose -1 -4)))))
       
       (define thinking-cat (cat 'thinking))
       (define happy-cat (cat 'happy))
       (define mad-cat (cat 'mad))]


@section{Drawing the World}

@chunk[<drawing>
       <constants>
       <render-world>
       <chop-whiskers>
       <render-board>
       <render-cell>
       <world-width>
       <world-height>
       <cell-center-x>
       <cell-center-y>]

@chunk[<drawing-tests>
       <cell-center-x-tests>
       <cell-center-y-tests>
       <world-size-tests>
       <render-cell-tests>
       <render-board-tests>
       <chop-whiskers-tests>
       <render-world-tests>]

There are a number of constants
that are given names to make the code
more readable. 

These first two constants give the radius
of the circles that are drawn on the board,
plus the radius of an invisible circle
that, if they were drawn on top of
the circles, would touch
each other. Accordingly, @scheme[circle-spacing]
is used when computing the positions of the circles,
but the circles are drawn using @scheme[circle-radius].

@chunk[<constants>
       (define circle-radius 20)
       (define circle-spacing 22)]

The other four constants specify the colors of the circles.

@chunk[<constants>
       (define normal-color 'lightskyblue)
       (define on-shortest-path-color 'white)
       (define blocked-color 'black)
       (define under-mouse-color 'black)]

The main function for drawing a world is @scheme[render-world].
It is a fairly straightforward composition of helper functions.
First, it builds the image of a board, and then puts the cat on it.
Lastly, since the whiskers of the cat might now hang off of the edge
of the board (if the cat is on a leftmost or rightmost cell),
it trims them. This ensures that the image is always the same size
and that the pinhole is always in the upper-left corner of the window.

@chunk[<render-world>
       (define/contract (render-world w)
         (-> world? image?)
         (chop-whiskers
          (overlay/xy (render-board (world-board w)
                                    (world-size w)
                                    (on-cats-path? w)
                                    (world-mouse-posn w))
                      (cell-center-x (world-cat w))
                      (cell-center-y (world-cat w))
                      (cond
                        [(equal? (world-state w) 'cat-won) happy-cat]
                        [(equal? (world-state w) 'cat-lost) mad-cat]
                        [else thinking-cat]))))]

Trimming the cat's whiskers amounts to removing any extra
space in the image that appears to the left or above the pinhole.
For example, the @scheme[rectangle] function returns
an image with a pinhole in the middle. So trimming 5x5
rectangle results in a 3x3 rectangle with the pinhole
at (0,0).

@chunk[<chop-whiskers-tests>
       (test (chop-whiskers (rectangle 5 5 'solid 'black))
             (put-pinhole (rectangle 3 3 'solid 'black) 0 0))]

The function uses @scheme[shrink] to remove all of the material above
and to the left of the pinhole.

@chunk[<chop-whiskers>
(define/contract (chop-whiskers img)
  (-> image? image?)
  (shrink img
          0
          0
          (- (image-width img) (pinhole-x img) 1)
          (- (image-height img) (pinhole-y img) 1)))]

The @scheme[render-board] function uses @scheme[for/fold] to iterate
over all of the @scheme[cell]s in @scheme[cs]. It starts with
an empty rectangle and, one by one, puts the cells on @scheme[image].

@chunk[<render-board>
       ;; render-board : board number (posn -> boolean) posn-or-false -> image
       (define/contract (render-board cs world-size on-cat-path? mouse)
         (-> (listof cell?) 
             natural-number/c
             (-> posn? boolean?)
             (or/c #f posn?)
             image?)
         (for/fold ([image (nw:rectangle (world-width world-size)
                                         (world-height world-size)
                                         'solid
                                         'white)])
                   ([c cs])
           (overlay image
                    (render-cell c
                                 (on-cat-path? (cell-p c))
                                 (and (posn? mouse)
                                      (equal? mouse (cell-p c)))))))]

The @scheme[render-cell] function accepts a @scheme[cell], 
a boolean indicating if the cell is on the shortest path between
the cat and the boundary, and a second boolean indicating
if the cell is underneath the mouse. It returns an image 
of the cell, with the pinhole placed in such a way that overlaying
the image on an empty image with pinhole in the upper-left corner
results in the cell being placed in the right place.

@chunk[<render-cell>
       (define/contract (render-cell c on-short-path? under-mouse?)
         (-> cell? boolean? boolean? image?)
         (local [(define x (cell-center-x (cell-p c)))
                 (define y (cell-center-y (cell-p c)))
                 (define main-circle
                   (cond
                     [(cell-blocked? c)
                      (circle circle-radius 'solid blocked-color)]
                     [else
                      (circle circle-radius 'solid normal-color)]))]
           (move-pinhole
            (cond
              [under-mouse?
               (overlay main-circle
                        (circle (quotient circle-radius 2) 'solid under-mouse-color))]
              [on-short-path?
               (overlay main-circle
                        (circle (quotient circle-radius 2) 'solid
                                on-shortest-path-color))]
              [else
               main-circle])
            (- x)
            (- y))))]

The @scheme[world-width] function computes the width of the rendered world,
given the world's size by finding the center of the rightmost posn,
and then adding an additional radius.

@chunk[<world-width>
       (define/contract (world-width board-size)
         (-> natural-number/c number?)
         (local [(define rightmost-posn
                   (make-posn (- board-size 1) (- board-size 2)))]
           (+ (cell-center-x rightmost-posn) circle-radius)))]

Similarly, the @scheme[world-height] function computest the 
height of the rendered world, given the world's size.

@chunk[<world-height>
       (define/contract (world-height board-size)
         (-> natural-number/c number?)
         (local [(define bottommost-posn
                   (make-posn (- board-size 1) (- board-size 1)))]
           (+ (cell-center-y bottommost-posn) circle-radius)))]

The @scheme[cell-center-x] function returns the
@tt{x} coordinate of the center of the cell specified
by @scheme[p].

For example, the first cell in
the third row (counting from @scheme[0]) is
flush with the edge of the screen, so its
center is just the radius of the circle that
is drawn.

@chunk[<cell-center-x-tests>
       (test (cell-center-x (make-posn 0 2))
             circle-radius)]


The first cell in the second row, in contrast
is offset from the third row by
@scheme[circle-spacing].

@chunk[<cell-center-x-tests>
       (test (cell-center-x (make-posn 0 1))
             (+ circle-spacing circle-radius))]


The definition of @scheme[cell-center-x] 
multiplies the @scheme[x] coordinate of 
@scheme[p] by twice @scheme[circle-spacing]
and then adds @scheme[circle-radius] to move
over for the first circle. In addition
if the @scheme[y] coordinate is odd, then
it adds @scheme[circle-spacing], shifting 
the entire line over.

@chunk[<cell-center-x>
       (define/contract (cell-center-x p)
         (-> posn? number?)
         (let ([x (posn-x p)]
               [y (posn-y p)])
           (+ circle-radius
              (* x circle-spacing 2)
              (if (odd? y)
                  circle-spacing
                  0))))]

The @scheme[cell-center-y] function computes the
@scheme[y] coordinate of a cell's location on
the screen. For example, the @scheme[y] 
coordinate of the first row is 
the radius of a circle, ensuring that
the first row is flush against the top of
the screen.

@chunk[<cell-center-y-tests>
       (test (cell-center-y (make-posn 1 0))
             circle-radius)]

Because the grid is hexagonal, the @scheme[y] coordinates
of the rows do not have the same spacing as the @scheme[x]
coordinates. In particular, they are off by
@tt{sin(pi/3)}. We approximate that by @scheme[866/1000]
in order to keep the computations and test cases simple
and using exact numbers.
A more precise approximation would be
@(scheme #,(sin (/ pi 3))), but it is not necessary at
the screen resolution.

@chunk[<cell-center-y>
       (define/contract (cell-center-y p)
         (-> posn? number?)
         (+ circle-radius
            (* (posn-y p) 
               circle-spacing 2
               866/1000)))]
       
@section{Handling Input}

@chunk[<input>
       <clack>
       <update-world-posn>
       <move-cat>
       <find-best-positions>
       <lt/f>
       <circle-at-point>
       <point-in-this-circle?>
       <change>]

@chunk[<input-tests>
       <change-tests>
       <point-in-this-circle?-tests>
       <circle-at-point-tests>
       <lt/f-tests>
       <find-best-positions-tests>
       <move-cat-tests>
       <update-world-posn-tests>
       <clack-tests>]

@chunk[<clack>
       (define (clack world x y evt)
         (let ([new-mouse-posn
                (and (not (eq? evt 'leave))
                     (make-posn x y))])
           (update-world-posn
            (cond
              [(and (equal? evt 'button-up)
                    (equal? 'playing (world-state world))
                    (circle-at-point (world-board world) x y))
               =>
               (λ (circle)
                 (move-cat
                  (make-world (block-cell circle (world-board world))
                              (world-cat world)
                              (world-state world)
                              (world-size world)
                              (world-mouse-posn world)
                              (world-h-down? world))))]
              [else world])
            new-mouse-posn)))]

@chunk[<clack-tests>
       (test (clack (make-world '() (make-posn 0 0) 'playing 3 false false)
                    1 1 'button-down)
             (make-world '() (make-posn 0 0) 'playing 3 #f false))
       (test (clack (make-world '() (make-posn 0 0) 'playing 3 false false)
                    1 1 'drag)
             (make-world '() (make-posn 0 0) 'playing 3 false false))
       (test (clack (make-world (list (make-cell (make-posn 0 0) false))
                                (make-posn 0 1)
                                'playing
                                3
                                false
                                false)
                    (cell-center-x (make-posn 0 0))
                    (cell-center-y (make-posn 0 0))
                    'move)
             (make-world (list (make-cell (make-posn 0 0) false))
                         (make-posn 0 1)
                         'playing
                         3
                         (make-posn 0 0)
                         false))
       (test (clack (make-world (list (make-cell (make-posn 0 0) false))
                                (make-posn 0 1)
                                'playing
                                3
                                false
                                false)
                    (cell-center-x (make-posn 0 0))
                    (cell-center-y (make-posn 0 0))
                    'enter)
             (make-world (list (make-cell (make-posn 0 0) false))
                         (make-posn 0 1)
                         'playing
                         3
                         (make-posn 0 0)
                         false))
       (test (clack (make-world '() (make-posn 0 0)
                                'playing 3 (make-posn 0 0) false)
                    1 1 'leave)
             (make-world '() (make-posn 0 0) 'playing 3 false false))
       
       (test (clack (make-world '() (make-posn 0 0)
                                'playing 3 (make-posn 0 0) false)
                    10
                    10
                    'button-down)
             (make-world '() (make-posn 0 0) 'playing 3 false false))
       
       (test (clack (make-world (list (make-cell (make-posn 0 0) false)
                                      (make-cell (make-posn 1 1) false))
                                (make-posn 1 1)
                                'playing
                                3
                                (make-posn 0 0)
                                false)
                    (cell-center-x (make-posn 0 0))
                    (cell-center-y (make-posn 0 0))
                    'button-up)
             (make-world (list (make-cell (make-posn 0 0) true)
                               (make-cell (make-posn 1 1) false))
                         (make-posn 1 1)
                         'cat-lost
                         3
                         (make-posn 0 0)
                         false))
       
       
       (test (clack (make-world '() (make-posn 0 0)
                                'cat-lost 3 (make-posn 0 0) false)
                    10
                    10
                    'button-up)
             (make-world '() (make-posn 0 0)
                         'cat-lost 3 (make-posn 0 0) false))
       (test (clack
              (make-world
               (list (make-cell (make-posn 1 0) false)
                     (make-cell (make-posn 2 0) true)
                     (make-cell (make-posn 0 1) true)
                     (make-cell (make-posn 1 1) false)
                     (make-cell (make-posn 2 1) true)
                     (make-cell (make-posn 1 2) true)
                     (make-cell (make-posn 2 2) true))
               (make-posn 1 1)
               'playing
               3
               false
               false)
              (cell-center-x (make-posn 1 0))
              (cell-center-y (make-posn 1 0))
              'button-up)
             (make-world
              (list (make-cell (make-posn 1 0) true)
                    (make-cell (make-posn 2 0) true)
                    (make-cell (make-posn 0 1) true)
                    (make-cell (make-posn 1 1) false)
                    (make-cell (make-posn 2 1) true)
                    (make-cell (make-posn 1 2) true)
                    (make-cell (make-posn 2 2) true))
              (make-posn 1 1)
              'cat-lost
              3
              false
              false))
       
       (test (clack
              (make-world
               (list (make-cell (make-posn 1 0) false)
                     (make-cell (make-posn 2 0) false)
                     (make-cell (make-posn 0 1) true)
                     (make-cell (make-posn 1 1) false)
                     (make-cell (make-posn 2 1) true)
                     (make-cell (make-posn 1 2) true)
                     (make-cell (make-posn 2 2) true))
               (make-posn 1 1)
               'playing
               3
               false
               false)
              (cell-center-x (make-posn 1 0))
              (cell-center-y (make-posn 1 0))
              'button-up)
             (make-world
              (list (make-cell (make-posn 1 0) true)
                    (make-cell (make-posn 2 0) false)
                    (make-cell (make-posn 0 1) true)
                    (make-cell (make-posn 1 1) false)
                    (make-cell (make-posn 2 1) true)
                    (make-cell (make-posn 1 2) true)
                    (make-cell (make-posn 2 2) true))
              (make-posn 2 0)
              'cat-won
              3
              false
              false))]

@chunk[<update-world-posn>
       ;; update-world-posn/playing : world posn-or-false -> world
       (define (update-world-posn w p)
         (cond
           [(equal? (world-state w) 'playing)
            (cond
              [(posn? p)
               (let ([mouse-spot
                      (circle-at-point (world-board w)
                                       (posn-x p)
                                       (posn-y p))])
                 (make-world (world-board w)
                             (world-cat w)
                             (world-state w)
                             (world-size w)
                             (cond
                               [(equal? mouse-spot (world-cat w))
                                false]
                               [else
                                mouse-spot])
                             (world-h-down? w)))]
              [else
               (make-world (world-board w)
                           (world-cat w)
                           (world-state w)
                           (world-size w)
                           false
                           (world-h-down? w))])]
           [else w]))]

@chunk[<update-world-posn-tests>

       (test (update-world-posn
              (make-world (list (make-cell (make-posn 0 0) false))
                          (make-posn 0 1) 'playing 3 false false)
              (make-posn (cell-center-x (make-posn 0 0))
                         (cell-center-y (make-posn 0 0))))
             (make-world (list (make-cell (make-posn 0 0) false))
                         (make-posn 0 1) 'playing 3 (make-posn 0 0) false))
       
       (test (update-world-posn
              (make-world (list (make-cell (make-posn 0 0) false))
                          (make-posn 0 0) 'playing 3 false false)
              (make-posn (cell-center-x (make-posn 0 0))
                         (cell-center-y (make-posn 0 0))))
             (make-world (list (make-cell (make-posn 0 0) false))
                         (make-posn 0 0) 'playing 3 false false))
       
       (test (update-world-posn
              (make-world (list (make-cell (make-posn 0 0) false))
                          (make-posn 0 1) 'playing 3 (make-posn 0 0) false)
              (make-posn 0 0))
             (make-world (list (make-cell (make-posn 0 0) false))
                         (make-posn 0 1) 'playing 3 false false))
       (test (update-world-posn
              (make-world (list (make-cell (make-posn 0 0) false))
                          (make-posn 0 1) 'cat-won 3 false false)
              (make-posn (cell-center-x (make-posn 0 0))
                         (cell-center-y (make-posn 0 0))))
             (make-world (list (make-cell (make-posn 0 0) false))
                         (make-posn 0 1) 'cat-won 3 false false))
       (test (update-world-posn
              (make-world (list (make-cell (make-posn 0 0) false))
                          (make-posn 0 1) 'cat-lost 3 false false)
              (make-posn (cell-center-x (make-posn 0 0))
                         (cell-center-y (make-posn 0 0))))
             (make-world (list (make-cell (make-posn 0 0) false))
                         (make-posn 0 1) 'cat-lost 3 false false))]

@chunk[<move-cat>
       ;; move-cat : world -> world
       (define (move-cat world)
         (local [(define cat-position (world-cat world))
                 (define table (build-bfs-table world 'boundary))
                 (define neighbors (adjacent cat-position))
                 (define next-cat-positions
                   (find-best-positions neighbors
                                        (map (lambda (p) (lookup-in-table table p))
                                             neighbors)))
                 (define next-cat-position
                   (cond
                     [(boolean? next-cat-positions) false]
                     [else
                      (list-ref next-cat-positions
                                (random (length next-cat-positions)))]))]
           (make-world (world-board world)
                       (cond
                         [(boolean? next-cat-position)
                          cat-position]
                         [else next-cat-position])
                       (cond
                         [(boolean? next-cat-position)
                          'cat-lost]
                         [(on-boundary? next-cat-position (world-size world))
                          'cat-won]
                         [else 'playing])
                       (world-size world)
                       (world-mouse-posn world)
                       (world-h-down? world))))]

@chunk[<move-cat-tests>
       (test
        (move-cat
         (make-world (list (make-cell (make-posn 1 0) false)
                           (make-cell (make-posn 2 0) false)
                           (make-cell (make-posn 3 0) false)
                           (make-cell (make-posn 4 0) false)
                           
                           (make-cell (make-posn 0 1) false)
                           (make-cell (make-posn 1 1) true)
                           (make-cell (make-posn 2 1) true)
                           (make-cell (make-posn 3 1) false)
                           (make-cell (make-posn 4 1) false)
                           
                           (make-cell (make-posn 0 2) false)
                           (make-cell (make-posn 1 2) true)
                           (make-cell (make-posn 2 2) false)
                           (make-cell (make-posn 3 2) true)
                           (make-cell (make-posn 4 2) false)
                           
                           (make-cell (make-posn 0 3) false)
                           (make-cell (make-posn 1 3) true)
                           (make-cell (make-posn 2 3) false)
                           (make-cell (make-posn 3 3) false)
                           (make-cell (make-posn 4 3) false)
                           
                           (make-cell (make-posn 1 4) false)
                           (make-cell (make-posn 2 4) false)
                           (make-cell (make-posn 3 4) false)
                           (make-cell (make-posn 4 4) false))
                     (make-posn 2 2)
                     'playing
                     5
                     (make-posn 0 0)
                     false))
        (make-world (list (make-cell (make-posn 1 0) false)
                          (make-cell (make-posn 2 0) false)
                          (make-cell (make-posn 3 0) false)
                          (make-cell (make-posn 4 0) false)
                          
                          (make-cell (make-posn 0 1) false)
                          (make-cell (make-posn 1 1) true)
                          (make-cell (make-posn 2 1) true)
                          (make-cell (make-posn 3 1) false)
                          (make-cell (make-posn 4 1) false)
                          
                          (make-cell (make-posn 0 2) false)
                          (make-cell (make-posn 1 2) true)
                          (make-cell (make-posn 2 2) false)
                          (make-cell (make-posn 3 2) true)
                          (make-cell (make-posn 4 2) false)
                          
                          (make-cell (make-posn 0 3) false)
                          (make-cell (make-posn 1 3) true)
                          (make-cell (make-posn 2 3) false)
                          (make-cell (make-posn 3 3) false)
                          (make-cell (make-posn 4 3) false)
                          
                          (make-cell (make-posn 1 4) false)
                          (make-cell (make-posn 2 4) false)
                          (make-cell (make-posn 3 4) false)
                          (make-cell (make-posn 4 4) false))
                    (make-posn 2 3)
                    'playing
                    5
                    (make-posn 0 0)
                    false))]

@chunk[<find-best-positions>
       ;; find-best-positions : (nelistof posn) (nelistof number or '∞)
       ;;                       -> (nelistof posn) or false
       (define (find-best-positions posns scores)
         (local [(define best-score (foldl (lambda (x sofar)
                                             (if (<=/f x sofar)
                                                 x
                                                 sofar))
                                           (first scores)
                                           (rest scores)))]
           (cond
             [(symbol? best-score) false]
             [else
              (map
               second
               (filter (lambda (x) (equal? (first x) best-score))
                       (map list scores posns)))])))]

@chunk[<find-best-positions-tests>
       (test (find-best-positions (list (make-posn 0 0)) (list 1))
             (list (make-posn 0 0)))
       (test (find-best-positions (list (make-posn 0 0)) (list '∞))
             false)
       (test (find-best-positions (list (make-posn 0 0)
                                        (make-posn 1 1))
                                  (list 1 2))
             (list (make-posn 0 0)))
       (test (find-best-positions (list (make-posn 0 0)
                                        (make-posn 1 1))
                                  (list 1 1))
             (list (make-posn 0 0)
                   (make-posn 1 1)))
       (test (find-best-positions (list (make-posn 0 0)
                                        (make-posn 1 1))
                                  (list '∞ 2))
             (list (make-posn 1 1)))
       (test (find-best-positions (list (make-posn 0 0)
                                        (make-posn 1 1))
                                  (list '∞ '∞))
             false)]

@chunk[<lt/f>
       ;; <=/f : (number or '∞) (number or '∞) -> boolean
       (define (<=/f a b)
         (cond
           [(equal? b '∞) true]
           [(equal? a '∞) false]
           [else (<= a b)]))]

@chunk[<lt/f-tests>
       (test (<=/f 1 2) true)
       (test (<=/f 2 1) false)
       (test (<=/f '∞ 1) false)
       (test (<=/f 1 '∞) true)
       (test (<=/f '∞ '∞) true)]

@chunk[<circle-at-point>
       ;; circle-at-point : board number number -> posn-or-false
       ;; returns the posn corresponding to cell where the x,y coordinates are
       (define (circle-at-point board x y)
         (cond
           [(empty? board) false]
           [else
            (cond
              [(point-in-this-circle? (cell-p (first board)) x y)
               (cell-p (first board))]
              [else
               (circle-at-point (rest board) x y)])]))]

@chunk[<circle-at-point-tests>
       (test (circle-at-point empty 0 0) false)
       (test (circle-at-point (list (make-cell (make-posn 0 0) false))
                              (cell-center-x (make-posn 0 0))
                              (cell-center-y (make-posn 0 0)))
             (make-posn 0 0))
       (test (circle-at-point (list (make-cell (make-posn 0 0) false))
                              0 0)
             false)]

@chunk[<point-in-this-circle?>
       ;; point-in-this-circle? : posn number number -> boolean
       (define (point-in-this-circle? p x y)
         (let ([center (+ (cell-center-x p)
                          (* (sqrt -1)
                             (cell-center-y p)))]
               [p2 (+ x (* (sqrt -1) y))])
           (<= (magnitude (- center p2)) 
               circle-radius)))]

@chunk[<point-in-this-circle?-tests>
       (test (point-in-this-circle? (make-posn 0 0)
                                    (cell-center-x (make-posn 0 0))
                                    (cell-center-y (make-posn 0 0)))
             true)
       (test (point-in-this-circle? (make-posn 0 0) 0 0)
             false)]

@chunk[<change>
       ;; change : world key-event -> world
       (define (change w ke)
         (make-world (world-board w)
                     (world-cat w)
                     (world-state w)
                     (world-size w)
                     (world-mouse-posn w)
                     (key=? ke #\h)))]

@chunk[<change-tests>
       (test (change (make-world '() (make-posn 1 1)
                                 'playing 3 (make-posn 0 0) false)
                     #\h)
             (make-world '() (make-posn 1 1)
                         'playing 3 (make-posn 0 0) true))
       (test (change (make-world '() (make-posn 1 1)
                                 'playing 3 (make-posn 0 0) true)
                     'release)
             (make-world '() (make-posn 1 1) 'playing 3 (make-posn 0 0) false))]


]

@section{Tests}

This section consists of some infrastructure for
maintaining tests, plus a pile of additional tests
for the other functions in this document

@chunk[<test-infrastructure>

(define-syntax (test stx)
  (syntax-case stx ()
    [(_ actual expected)
     (with-syntax ([line (syntax-line stx)]
                   [pos (syntax-position stx)])
       #'(test/proc (λ () actual)
                    (λ () expected)
                    equal?
                    line
                    'actual))]))

(define-syntax (test/set stx)
  (syntax-case stx ()
    [(_ actual expected)
     (with-syntax ([line (syntax-line stx)]
                   [pos (syntax-position stx)])
       #'(test/proc (λ () actual)
                    (λ () expected)
                    (λ (x y) (same-sets? x y))
                    line
                    'actual))]))

(define test-count 0)

(define (test/proc actual-thunk expected-thunk cmp line sexp)
  (set! test-count (+ test-count 1))
  (let ([actual (actual-thunk)]
        [expected (expected-thunk)])
    (unless (cmp actual expected)
      (error 'check-expect "test #~a~a\n  ~s\n  ~s\n"
             test-count
             (if line
                 (format " on line ~a failed:" line)
                 (format " failed: ~s" sexp))
             actual
             expected))))


(define (same-sets? l1 l2)
  (and (andmap (lambda (e1) (member e1 l2)) l1)
       (andmap (lambda (e2) (member e2 l1)) l2)
       #t))

(test (same-sets? (list) (list)) true)
(test (same-sets? (list) (list 1)) false)
(test (same-sets? (list 1) (list)) false)
(test (same-sets? (list 1 2) (list 2 1)) true)]

@chunk[<lookup-in-table-tests>
       (test (lookup-in-table empty (make-posn 1 2)) '∞)
       (test (lookup-in-table (list (make-dist-cell (make-posn 1 2) 3))
                              (make-posn 1 2))
             3)
       (test (lookup-in-table (list (make-dist-cell (make-posn 2 1) 3))
                              (make-posn 1 2))
             '∞)]

@chunk[<build-bfs-table-tests>
       (test/set (build-bfs-table
                  (make-world (empty-board 3) (make-posn 1 1)
                              'playing 3 (make-posn 0 0) false)
                  (make-posn 1 1))
                 (list
                  (make-dist-cell 'boundary 2)
                  
                  (make-dist-cell (make-posn 1 0) 1)
                  (make-dist-cell (make-posn 2 0) 1)
                  
                  (make-dist-cell (make-posn 0 1) 1)
                  (make-dist-cell (make-posn 1 1) 0)
                  (make-dist-cell (make-posn 2 1) 1)
                  
                  (make-dist-cell (make-posn 1 2) 1)
                  (make-dist-cell (make-posn 2 2) 1)))
       
       (test/set (build-bfs-table
                  (make-world
                   (list
                    (make-cell (make-posn 0 1) true)
                    (make-cell (make-posn 1 0) true)
                    (make-cell (make-posn 1 1) false)
                    (make-cell (make-posn 1 2) true)
                    (make-cell (make-posn 2 0) true)
                    (make-cell (make-posn 2 1) true)
                    (make-cell (make-posn 2 2) true))
                   (make-posn 1 1)
                   'playing
                   3
                   (make-posn 0 0)
                   false)
                  'boundary)
                 (list
                  (make-dist-cell 'boundary 0)))

       (test/set (build-bfs-table 
                  (make-world (empty-board 5)
                              (make-posn 2 2)
                              'playing
                              5
                              (make-posn 0 0)
                              false)
                  'boundary)
                 (list
                  (make-dist-cell 'boundary 0)
                  
                  (make-dist-cell (make-posn 1 0) 1)
                  (make-dist-cell (make-posn 2 0) 1)
                  (make-dist-cell (make-posn 3 0) 1)
                  (make-dist-cell (make-posn 4 0) 1)
                  
                  (make-dist-cell (make-posn 0 1) 1)
                  (make-dist-cell (make-posn 1 1) 2)
                  (make-dist-cell (make-posn 2 1) 2)
                  (make-dist-cell (make-posn 3 1) 2)
                  (make-dist-cell (make-posn 4 1) 1)
                  
                  (make-dist-cell (make-posn 0 2) 1)
                  (make-dist-cell (make-posn 1 2) 2)
                  (make-dist-cell (make-posn 2 2) 3)
                  (make-dist-cell (make-posn 3 2) 2)
                  (make-dist-cell (make-posn 4 2) 1)
                  
                  (make-dist-cell (make-posn 0 3) 1)
                  (make-dist-cell (make-posn 1 3) 2)
                  (make-dist-cell (make-posn 2 3) 2)
                  (make-dist-cell (make-posn 3 3) 2)
                  (make-dist-cell (make-posn 4 3) 1)
                  
                  (make-dist-cell (make-posn 1 4) 1)
                  (make-dist-cell (make-posn 2 4) 1)
                  (make-dist-cell (make-posn 3 4) 1)
                  (make-dist-cell (make-posn 4 4) 1)))

       (test/set (build-bfs-table 
                  (make-world (block-cell
                               (make-posn 4 2)
                               (empty-board 5))
                              (make-posn 2 2)
                              'playing
                              5
                              (make-posn 0 0)
                              false)
                  'boundary)
                 (list
                  (make-dist-cell 'boundary 0)
                  
                  (make-dist-cell (make-posn 1 0) 1)
                  (make-dist-cell (make-posn 2 0) 1)
                  (make-dist-cell (make-posn 3 0) 1)
                  (make-dist-cell (make-posn 4 0) 1)
                  
                  (make-dist-cell (make-posn 0 1) 1)
                  (make-dist-cell (make-posn 1 1) 2)
                  (make-dist-cell (make-posn 2 1) 2)
                  (make-dist-cell (make-posn 3 1) 2)
                  (make-dist-cell (make-posn 4 1) 1)
                  
                  (make-dist-cell (make-posn 0 2) 1)
                  (make-dist-cell (make-posn 1 2) 2)
                  (make-dist-cell (make-posn 2 2) 3)
                  (make-dist-cell (make-posn 3 2) 3)
                  
                  (make-dist-cell (make-posn 0 3) 1)
                  (make-dist-cell (make-posn 1 3) 2)
                  (make-dist-cell (make-posn 2 3) 2)
                  (make-dist-cell (make-posn 3 3) 2)
                  (make-dist-cell (make-posn 4 3) 1)
                  
                  (make-dist-cell (make-posn 1 4) 1)
                  (make-dist-cell (make-posn 2 4) 1)
                  (make-dist-cell (make-posn 3 4) 1)
                  (make-dist-cell (make-posn 4 4) 1)))
       
       (test/set (build-bfs-table
                  (make-world (empty-board 5)
                              (make-posn 2 2)
                              'playing
                              5
                              (make-posn 0 0)
                              false)
                  (make-posn 2 2))
                 (list
                  (make-dist-cell 'boundary 3)
                  
                  (make-dist-cell (make-posn 1 0) 2)
                  (make-dist-cell (make-posn 2 0) 2)
                  (make-dist-cell (make-posn 3 0) 2)
                  (make-dist-cell (make-posn 4 0) 3)
                  
                  (make-dist-cell (make-posn 0 1) 2)
                  (make-dist-cell (make-posn 1 1) 1)
                  (make-dist-cell (make-posn 2 1) 1)
                  (make-dist-cell (make-posn 3 1) 2)
                  (make-dist-cell (make-posn 4 1) 3)
                  
                  (make-dist-cell (make-posn 0 2) 2)
                  (make-dist-cell (make-posn 1 2) 1)
                  (make-dist-cell (make-posn 2 2) 0)
                  (make-dist-cell (make-posn 3 2) 1)
                  (make-dist-cell (make-posn 4 2) 2)
                  
                  (make-dist-cell (make-posn 0 3) 2)
                  (make-dist-cell (make-posn 1 3) 1)
                  (make-dist-cell (make-posn 2 3) 1)
                  (make-dist-cell (make-posn 3 3) 2)
                  (make-dist-cell (make-posn 4 3) 3)
                  
                  (make-dist-cell (make-posn 1 4) 2)
                  (make-dist-cell (make-posn 2 4) 2)
                  (make-dist-cell (make-posn 3 4) 2)
                  (make-dist-cell (make-posn 4 4) 3)))

       (test (lookup-in-table
              (build-bfs-table (make-world (empty-board 5)
                                           (make-posn 2 2)
                                           'playing
                                           5
                                           (make-posn 0 0)
                                           false)
                               (make-posn 2 2))
              (make-posn 1 4))
             2)]

@chunk[<neighbors-tests>
       (test ((neighbors (empty-world 11))  (make-posn 1 1))
             (adjacent (make-posn 1 1)))
       (test ((neighbors (empty-world 11)) (make-posn 2 2))
             (adjacent (make-posn 2 2)))
       (test ((neighbors (empty-world 3)) 'boundary)
             (list (make-posn 0 1)
                   (make-posn 1 0)
                   (make-posn 1 2)
                   (make-posn 2 0)
                   (make-posn 2 1)
                   (make-posn 2 2)))
       (test ((neighbors (make-world (list
                                      (make-cell (make-posn 0 1) false)
                                      (make-cell (make-posn 1 0) false)
                                      (make-cell (make-posn 1 1) true)
                                      (make-cell (make-posn 1 2) false)
                                      (make-cell (make-posn 2 0) false)
                                      (make-cell (make-posn 2 1) false)
                                      (make-cell (make-posn 2 2) false))
                                     (make-posn 1 1)
                                     'playing
                                     3
                                     (make-posn 0 0)
                                     false))
              (make-posn 1 1))
             '())
       (test ((neighbors (make-world (list
                                      (make-cell (make-posn 0 1) false)
                                      (make-cell (make-posn 1 0) false)
                                      (make-cell (make-posn 1 1) true)
                                      (make-cell (make-posn 1 2) false)
                                      (make-cell (make-posn 2 0) false)
                                      (make-cell (make-posn 2 1) false)
                                      (make-cell (make-posn 2 2) false))
                                     (make-posn 1 1)
                                     'playing
                                     3
                                     (make-posn 0 0)
                                     false))
              (make-posn 1 0))
             (list 'boundary (make-posn 2 0) (make-posn 0 1)))]

@chunk[<adjacent-tests>
       (test (adjacent (make-posn 1 1))
             (list (make-posn 1 0)
                   (make-posn 2 0)
                   (make-posn 0 1)
                   (make-posn 2 1)
                   (make-posn 1 2)
                   (make-posn 2 2)))
       (test (adjacent (make-posn 2 2))
             (list (make-posn 1 1)
                   (make-posn 2 1)
                   (make-posn 1 2)
                   (make-posn 3 2)
                   (make-posn 1 3)
                   (make-posn 2 3)))]


@chunk[<on-boundary?-tests>
       (test (on-boundary? (make-posn 0 1) 13) true)
       (test (on-boundary? (make-posn 1 0) 13) true)
       (test (on-boundary? (make-posn 12 1) 13) true)
       (test (on-boundary? (make-posn 1 12) 13) true)
       (test (on-boundary? (make-posn 1 1) 13) false)
       (test (on-boundary? (make-posn 10 10) 13) false)]


@chunk[<in-bounds?-tests>
       (test (in-bounds? (make-posn 0 0) 11) false)
       (test (in-bounds? (make-posn 0 1) 11) true)
       (test (in-bounds? (make-posn 1 0) 11) true)
       (test (in-bounds? (make-posn 10 10) 11) true)
       (test (in-bounds? (make-posn 0 -1) 11) false)
       (test (in-bounds? (make-posn -1 0) 11) false)
       (test (in-bounds? (make-posn 0 11) 11) false)
       (test (in-bounds? (make-posn 11 0) 11) false)
       (test (in-bounds? (make-posn 10 0) 11) true)
       (test (in-bounds? (make-posn 0 10) 11) false)]

@chunk[<on-cats-path?-tests>
       (test ((on-cats-path? (make-world (empty-board 5)
                                         (make-posn 1 1)
                                         'playing
                                         5
                                         (make-posn 0 0)
                                         true))
              (make-posn 1 0))
             true)
       (test ((on-cats-path? (make-world (empty-board 5)
                                         (make-posn 1 1)
                                         'playing
                                         5 
                                         (make-posn 0 0)
                                         false))
              (make-posn 1 0))
             false)

       (test ((on-cats-path? (make-world (empty-board 5) (make-posn 1 1)
                                         'playing 5 (make-posn 0 0) true))
              (make-posn 2 1))
             false)
       (test ((on-cats-path?
               (make-world (list
                            (make-cell (make-posn 0 1) true)
                            (make-cell (make-posn 1 0) true)
                            (make-cell (make-posn 1 1) false)
                            (make-cell (make-posn 1 2) true)
                            (make-cell (make-posn 2 0) true)
                            (make-cell (make-posn 2 1) true)
                            (make-cell (make-posn 2 2) true))
                           (make-posn 1 1)
                           'cat-lost
                           3
                           (make-posn 0 0)
                           true))
              (make-posn 0 1))
             false)]


@chunk[<+/f-tests>       
       (test (+/f '∞ '∞) '∞)
       (test (+/f '∞ 1) '∞)
       (test (+/f 1 '∞) '∞)
       (test (+/f 1 2) 3)]

@chunk[<render-world-tests>

       (test
        (render-world
         (make-world (list (make-cell (make-posn 0 1) false))
                     (make-posn 0 1)
                     'playing
                     3
                     (make-posn 0 0)
                     false))
        (overlay
         (render-board (list (make-cell (make-posn 0 1) false))
                       3
                       (lambda (x) true)
                       false)
         (move-pinhole thinking-cat
                       (- (cell-center-x (make-posn 0 1)))
                       (- (cell-center-y (make-posn 0 1))))))
       
       (test
        (render-world
         (make-world (list (make-cell (make-posn 0 1) false))
                     (make-posn 0 1)
                     'cat-won
                     3
                     false
                     false))
        (overlay
         (render-board (list (make-cell (make-posn 0 1) false))
                       3
                       (lambda (x) true)
                       false)
         (move-pinhole happy-cat
                       (- (cell-center-x (make-posn 0 1)))
                       (- (cell-center-y (make-posn 0 1))))))
       
       (test
        (render-world
         (make-world (list (make-cell (make-posn 0 1) false))
                     (make-posn 0 1)
                     'cat-lost
                     3
                     false
                     false))
        (overlay
         (render-board (list (make-cell (make-posn 0 1) false))
                       3
                       (lambda (x) true)
                       false)
         (move-pinhole mad-cat
                       (- (cell-center-x (make-posn 0 1)))
                       (- (cell-center-y (make-posn 0 1))))))
       
       (test
        (render-world
         (make-world (list
                      (make-cell (make-posn 0 1) true)
                      (make-cell (make-posn 1 0) true)
                      (make-cell (make-posn 1 1) false)
                      (make-cell (make-posn 1 2) true)
                      (make-cell (make-posn 2 0) true)
                      (make-cell (make-posn 2 1) true)
                      (make-cell (make-posn 2 2) true))
                     (make-posn 1 1)
                     'cat-lost
                     3
                     false
                     false))
        (overlay
         (render-board (list
                        (make-cell (make-posn 0 1) true)
                        (make-cell (make-posn 1 0) true)
                        (make-cell (make-posn 1 1) false)
                        (make-cell (make-posn 1 2) true)
                        (make-cell (make-posn 2 0) true)
                        (make-cell (make-posn 2 1) true)
                        (make-cell (make-posn 2 2) true))
                       3
                       (lambda (x) false)
                       false)
         (move-pinhole mad-cat
                       (- (cell-center-x (make-posn 1 1)))
                       (- (cell-center-y (make-posn 1 1))))))
       
       (test
        (render-world
         (make-world (list
                      (make-cell (make-posn 0 1) false)
                      (make-cell (make-posn 1 0) false)
                      (make-cell (make-posn 1 1) false)
                      (make-cell (make-posn 1 2) false)
                      (make-cell (make-posn 2 0) false)
                      (make-cell (make-posn 2 1) false)
                      (make-cell (make-posn 2 2) false))
                     (make-posn 1 1)
                     'cat-lost
                     3
                     (make-posn (cell-center-x (make-posn 0 1))
                                (cell-center-y (make-posn 0 1)))
                     true))
        
        (overlay
         (render-board (list
                        (make-cell (make-posn 0 1) false)
                        (make-cell (make-posn 1 0) false)
                        (make-cell (make-posn 1 1) false)
                        (make-cell (make-posn 1 2) false)
                        (make-cell (make-posn 2 0) false)
                        (make-cell (make-posn 2 1) false)
                        (make-cell (make-posn 2 2) false))
                       3
                       (lambda (x) true)
                       (make-posn (cell-center-x (make-posn 0 1))
                                  (cell-center-y (make-posn 0 1))))
         (move-pinhole mad-cat
                       (- (cell-center-x (make-posn 1 1)))
                       (- (cell-center-y (make-posn 1 1))))))]

@chunk[<chop-whiskers-tests>
       (test (chop-whiskers (rectangle 6 6 'solid 'black))
             (put-pinhole (rectangle 3 3 'solid 'black) 0 0))

       (test
        (pinhole-x
         (render-world
          (make-world
           (empty-board 3)
           (make-posn 0 0)
           'playing
           3
           (make-posn 0 0)
           false)))
        0)
       (test
        (pinhole-x
         (render-world
          (make-world
           (empty-board 3)
           (make-posn 0 1)
           'playing
           3
           (make-posn 0 0)
           false)))
        0)]

@chunk[<render-board-tests>
       (test (render-board (list (make-cell (make-posn 0 0) false))
                           3
                           (lambda (x) false)
                           false)
             (overlay
              (nw:rectangle (world-width 3)
                            (world-height 3)
                            'solid
                            'white)
              (render-cell (make-cell (make-posn 0 0) false)
                           false
                           false)))
       
       (test (render-board (list (make-cell (make-posn 0 0) false))
                           3
                           (lambda (x) true)
                           false)
             (overlay
              (nw:rectangle (world-width 3)
                            (world-height 3)
                            'solid
                            'white)
              (render-cell (make-cell (make-posn 0 0) false)
                           true
                           false)))
       
       
       (test (render-board (list (make-cell (make-posn 0 0) false))
                           3
                           (lambda (x) false)
                           false)
             (overlay
              (nw:rectangle (world-width 3)
                            (world-height 3)
                            'solid
                            'white)
              (render-cell (make-cell (make-posn 0 0) false)
                           false
                           false)))
       
       (test (render-board (list (make-cell (make-posn 0 0) false)
                                 (make-cell (make-posn 0 1) false))
                           3
                           (lambda (x) (equal? x (make-posn 0 1)))
                           false)
             (overlay
              (nw:rectangle (world-width 3)
                            (world-height 3)
                            'solid
                            'white)
              (render-cell (make-cell (make-posn 0 0) false)
                           false
                           false)
              (render-cell (make-cell (make-posn 0 1) false)
                           true
                           false)))
       
       (test (render-board (list (make-cell (make-posn 0 0) false)
                                 (make-cell (make-posn 0 1) false))
                           3
                           (lambda (x) (equal? x (make-posn 0 1)))
                           (make-posn 0 0))
             (overlay
              (nw:rectangle (world-width 3)
                            (world-height 3)
                            'solid
                            'white)
              (render-cell (make-cell (make-posn 0 0) false)
                           false
                           true)
              (render-cell (make-cell (make-posn 0 1) false)
                           true
                           false)))]


@chunk[<render-cell-tests>
       (test (render-cell (make-cell (make-posn 0 0) false) false false)
             (move-pinhole (circle circle-radius 'solid normal-color)
                           (- circle-radius)
                           (- circle-radius)))
       (test (render-cell (make-cell (make-posn 0 0) true) false false)
             (move-pinhole (circle circle-radius 'solid 'black)
                           (- circle-radius)
                           (- circle-radius)))
       (test (render-cell (make-cell (make-posn 0 0) false) true false)
             (move-pinhole (overlay (circle circle-radius 'solid normal-color)
                                    (circle (quotient circle-radius 2) 'solid
                                            on-shortest-path-color))
                           (- circle-radius)
                           (- circle-radius)))
       (test (render-cell (make-cell (make-posn 0 0) false) true true)
             (move-pinhole (overlay (circle circle-radius 'solid normal-color)
                                    (circle (quotient circle-radius 2) 'solid
                                            under-mouse-color))
                           (- circle-radius)
                           (- circle-radius)))]


@chunk[<world-size-tests>
       (test (world-width 3) 150)
       (test (world-height 3) #e116.208)]

@chunk[<cell-center-x-tests>
       (test (cell-center-x (make-posn 0 0))
             circle-radius)
       (test (cell-center-x (make-posn 1 0))
             (+ (* 2 circle-spacing) circle-radius))
       (test (cell-center-x (make-posn 1 1))
             (+ (* 3 circle-spacing) circle-radius))]

@chunk[<cell-center-y-tests>
       (test (cell-center-y (make-posn 1 1))
             (+ circle-radius (* 2 circle-spacing 866/1000)))]


@section{Run, program, run}

@chunk[<go>
       (let* ([board-size 11]
              [initial-board
               (add-n-random-blocked-cells
                6
                (empty-board board-size)
                board-size)]
              [initial-world
               (make-world initial-board
                           (make-posn (quotient board-size 2)
                                      (quotient board-size 2))
                           'playing
                           board-size
                           false
                           false)])
         
         (big-bang (world-width board-size)
                   (world-height board-size)
                   1
                   initial-world)
         (on-redraw render-world)
         (on-key-event change)
         (on-mouse-event clack)
         (void))
       
       (printf "passed ~s tests\n" test-count)
       (flush-output)]
