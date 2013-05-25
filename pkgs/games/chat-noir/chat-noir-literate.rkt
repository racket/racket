#lang scribble/lp

@(require (for-label racket/math) ;; for 'pi' below
          racket/math
          games/scribblings/common)

@gametitle*["Chat Noir" "chat-noir" "Puzzle Game" #:style '(toc)]

@author[(link "http://www.eecs.northwestern.edu/~robby" "Robby Findler")
        (link "http://www.barzilay.org/" "Eli Barzilay")
        (link "http://www.cs.utah.edu/~mflatt/" "Matthew Flatt")]

The goal of Chat Noir is to stop the cat from escaping the board. Each
turn you click on a circle, which prevents the cat from stepping on
that space, and the cat responds by taking a step. If the cat is
completely boxed in and thus unable reach the border, you win. If the
cat does reach the border, you lose.

To start a new game, hit the ``n'' key (but only after losing or
winning a game).

@play-margin-note["Chat Noir"]

To get some insight into the cat's behavior, press the ``h''
key. It will show you the cells that are on the cat's shortest path to
the edge, assuming that the cell underneath the mouse has been
blocked, so you can experiment to see how the shortest paths change
by moving your mouse around. Note that this slows down the game
considerably, so you can turn it back off by pressing ``h'' again.

The game was inspired by the one at
@link["http://www.gamedesign.jp/flash/chatnoir/chatnoir.html"]{Game
Design} and has essentially the same rules. It also inspired the final
project for the introductory programming course at the University of
Chicago in the fall of 2008.

The remainder of this document explains the implementation of 
the Chat Noir game in a 
@link["http://www.literateprogramming.com/"]{Literate Programming} style.

@local-table-of-contents[]

@section{Overview}

Chat Noir is implemented using @link["http://www.htdp.org/"]{HtDP}'s universe
library:  @racketmodname[2htdp/universe] 
(although it only uses the ``world'' portions of that library). 
The program is divided up into
six parts: the world data definition, an implementation of breadth-first search,
code that handles drawing of the world, code that handles user input,
and some code that builds an initial world and starts the game.

@chunk[<main>
       (require racket/list racket/math
                (for-syntax racket/base))
       (require 2htdp/universe htdp/image lang/posn racket/contract)
       <world>
       <breadth-first-search>
       <board->graph>
       <cats-path>
       <drawing-the-cat>
       <drawing>
       <input>
       <initial-world>
       <tests>
       <go>]

Each section also comes with a series of test cases that are collected into the 
@racket[<tests>]
chunk at the end of the program.

@chunk[<tests>
       <test-infrastructure>
       <world-tests>
       <board->graph-tests>
       <breadth-first-search-tests>
       <cats-path-tests>
       <drawing-tests>
       <input-tests>]

Each test case uses either @racket[test], a simple form that accepts two
arguments and compares them with @racket[equal?], or @racket[test/set]
which accepts two lists and compares them as if they were sets.

In general, most of the test cases are left to the end of the document, organized
in a series of chunks that match the functions being tested. Some of the test cases,
however, provide nice illustrations of the behavior of the function and so are
included in the function's description.

@section{The World}

The main data structure for Chat Noir is @tt{world}. It comes with a few functions that 
construct empty worlds and test cases for them.

@chunk[<world> 
       <cell-struct> <world-struct> <empty-world> <empty-board> 
       <blocked-cells> <block-cell>]

@chunk[<world-tests>
       <empty-world-test> <empty-board-test> <blocked-cells-tests>]

The main structure definition is the @racket[world] struct.

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
                               [help? boolean?])
  #:transparent)
]

It consists of a structure with six fields:
@itemize[

@item{@tt{board}: representing the state of the board as a list of
  @tt{cell}s, one for each circle on the game. 
  }

@item{@tt{cat}: a @racket[posn] indicating the position of the cat
  (interpreting the @racket[posn] in the way that they are interpreted
  for the @tt{board} field),}

@item{@tt{state}: the state of the game, which can be one of
  @itemize[
  @item{@racket['playing], indicating that the game is still going; this is the
    initial state.}
  @item{@racket['cat-won], indicating that the game is over and the
    cat won, or}
  @item{@racket['cat-lost], indicating that the game is over and the
    cat lost.}]}

@item{@tt{size}: an odd natural number indicating the size of the board}

@item{@tt{mouse-posn}: a @racket[posn] for the location of the
  mouse (or @racket[#f] if the mouse is not in the window), and}

@item{@tt{help?}: a boolean indicating if help should be shown.}
]

A @racket[cell] is a structure with two fields:

@chunk[<cell-struct>
       (define-struct/contract cell ([p posn?]
                                     [blocked? boolean?])
         #:transparent)]

The coordinates of
the @racket[posn] in the first field
indicate a position on the hexagonal grid. 
This program reprsents the hexagon grid as a series of rows that
are offset from each other by 1/2 the size of the each cell.
The @tt{y} field
of the @racket[posn] refers to the row of the cell, and the @tt{x}
coordinate the position in the row.  This means that, for example,
@racket[(make-posn 1 0)] is centered above @racket[(make-posn 1 0)]
and @racket[(make-posn 1 1)]. 

The boolean in the @tt{blocked?} field indicates if the cell has been
clicked on, thus blocking the cat from stepping there.

The @racket[empty-board] function builds a list of @racket[cell]s
that correspond to an empty board. For example, here's what an empty
7x7 board looks like, as a list of cells.

@image["chat-noir/7x7-empty-board.png"]

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

@image["chat-noir/3x3-empty-board.png"]

And here is how that board looks as a list of cells.

@chunk[<empty-board-test>

       (test (empty-board 3)
             (list
              (make-cell (make-posn 0 1) #f)
              (make-cell (make-posn 1 0) #f)
              (make-cell (make-posn 1 1) #f)
              (make-cell (make-posn 1 2) #f)
              (make-cell (make-posn 2 0) #f)
              (make-cell (make-posn 2 1) #f)
              (make-cell (make-posn 2 2) #f)))]

The @racket[empty-board] function consists
of two (nested) calls to @racket[build-list] 
that build a list of lists of cells, one for
each pair of coordinates between @racket[0]
and @racket[board-size]. Then, @racket[append]
flattens the nested lists and the
@racket[filter] expression removes the corners.

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
                            #f))))))))
       
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
in all of the fields of the @racket[world] struct.
For example, this is the empty world of size @racket[3].
It puts the cat at @racket[(make-posn 1 1)],
sets the state to @racket['playing], records the
size @racket[3], and sets the current mouse position
to @racket[#f] and the state of the ``h'' key to
@racket[#f].

@chunk[<empty-world-test>
       
       (test (empty-world 3)
             (make-world (empty-board 3)
                         (make-posn 1 1)
                         'playing
                         3
                         #f
                         #f))]


The @racket[empty-world] function
generalizes the example by computing the
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
                     #f
                     #f))]

The @racket[add-n-random-blocked-cells] function accepts a list of cells
and returns a new list of cells where @racket[n] of the unblocked cells
in @racket[all-cells] are now blocked.

If @racket[n] is zero, of course, no more cells should be blocked,
so the result is just @racket[all-cells]. Otherwise, 
the function computes @racket[unblocked-cells], a list of all
of the unblocked cells (except the cat's initial location),
and then randomly picks a cell from it,
calling @racket[block-cell] to actually block that cell.

@chunk[<blocked-cells>
       (define/contract (add-n-random-blocked-cells n all-cells board-size)
         (-> natural-number/c (listof cell?) (and/c natural-number/c odd? (>=/c 3))
             (listof cell?))
         (cond
           [(zero? n) all-cells]
           [else
            (let* ([unblocked-cells
                    (filter (lambda (x)
                              (let ([cat-cell? (and (= (posn-x (cell-p x))
                                                       (quotient board-size 2))
                                                    (= (posn-y (cell-p x))
                                                       (quotient board-size 2)))])
                                
                                (and (not (cell-blocked? x))
                                     (not cat-cell?))))
                            all-cells)]
                   [to-block (list-ref unblocked-cells
                                       (random (length unblocked-cells)))])
              (add-n-random-blocked-cells
               (sub1 n)
               (block-cell (cell-p to-block) all-cells)
               board-size))]))]


The @racket[block-cell] function accepts a @racket[posn]
and a list of @racket[cell] structs and updates the
relevant cell, setting its @tt{blocked?} field to @racket[#t].

@chunk[<block-cell>
       (define/contract (block-cell to-block board)
         (-> posn? (listof cell?) (listof cell?))
         (map (lambda (c) (if (equal? to-block (cell-p c))
                              (make-cell to-block #t)
                              c))
              board))]

@section{Breadth-first Search}

The cat's move decision is based on a breadth-first search of a graph.
The graph's nodes are the cells on the board plus a special
node called @racket['boundary] that is adjacent to every cell 
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

The breadth-first function constructs a @racket[distance-map],
which is a list of @racket[dist-cell] structs:

@chunk[<dist-cell-data-definition>
       (define-struct/contract dist-cell ([p (or/c 'boundary posn?)]
                                          [n natural-number/c])
         #:transparent)]

Each @tt{p} field in the @racket[dist-cell] is a position on the board
and the @tt{n} field is a natural number, indicating
the distance of the shortest path from the node to some fixed point on
the board. 

The function @racket[lookup-in-table] returns the distance from the fixed
point to the given posn, returning @racket['∞] if the posn is not in the
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

The @racket[build-bfs-table] accepts a world and a cell 
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
@racket[bst]. It accepts a queue of the pending nodes to visit
and a @racket[dist-table] that records the same information as a
@racket[distance-map], but in an immutable hash-table. The
@racket[dist-map] is an accumulator, recording the distances
to all of the nodes that have already been visited in the graph,
and is used here to speed up the computation. The queue is
represented as a list of vectors of length two. Each element
in the queue contains a @racket[posn], or the symbol @racket['boundary]
and that @racket[posn]'s  distance.

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

If the @racket[queue] is empty, then the accumulator contains
bindings for all of the (reachable) nodes in the graph, so
we just return it. If it isn't empty, then we extract
the first element from the queue and name its consituents
@racket[p] and @racket[dist].
Next we check to see if the node at the head of the queue
is in @racket[dist-table]. If it is, we just move on to the
next element in the queue. If that node is not in the @racket[dist-table],
then we add all of the neighbors to the queue, in the @racket[append]
expression, and update the @racket[dist-table] with the distance to 
this node. Because we always add the new children to the end of the queue
and always look at the front of the queue, we are guaranteed that
the first time we see a node, it will be with the shortest distance.

The @racket[build-bfs-table] function packages up @racket[bfs]
function. It accepts a @racket[world] and an initial position
and returns a @racket[distance-table].

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

As you can see, the first thing it does is bind the free variable in @racket[bfs]
to the result of calling the @racket[neighbors] function (defined in the chunk
@racket[<neighbors>]) and then it has the @racket[<bfs>] chunk. In the body
it calls the @racket[bfs] function 
and then transforms the result, using
@racket[hash-map], into a list of @racket[cell]s.

@section{Board to Graph}

As far as the @racket[build-bfs-table] function goes,
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

The neighbors functions accepts a @racket[world] and then
returns a function that computes the neighbors of a @racket[posn]
and of the @racket['boundary].

For example, @racket[(make-posn 1 0)] has four
neighbors:

@chunk[<neighbors-tests>
       (test ((neighbors (empty-world 7)) (make-posn 1 0))
             (list 'boundary
                   (make-posn 2 0)
                   (make-posn 0 1)
                   (make-posn 1 1)))]

and @racket[(make-posn 0 1)] has four neighbors:

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

This is the neighbors function. After it accepts the @racket[world],
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

The @racket[neighbors-blocked/boundary] function is given next.
If @racket[p] is blocked, it returns the empty list. If it is
on the boundary, the function simply returns @racket[boundary-cells].
Otherwise, @racket[neighbors-blocked/boundary] calls
@racket[adjacent] to compute the posns that are adjacent to @racket[p],
filtering out the blocked @racket[posn]s and binds that to @racket[adjacent-posns].
It then filters out the @racket[posn]s that would be outside of the board.
If those two lists are the same, then @racket[p] is not on the boundary,
so we just return @racket[in-bounds]. If the lists are different, then
we know that @racket[p] must have been on the boundary, so we add
@racket['boundary] to the result list.

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
from a board as used by @racket[neighbors].

The first function is @racket[adjacent]. It consumes a
@racket[posn] and returns six @racket[posn]s that
indicate what the neighbors are, without consideration
of the size of the board (or the missing corner pieces).

For example, these are the @racket[posn]s that are adjacent
to @racket[(make-posn 0 1)]; note that the first and the third
are not on the board and do not show up in 
@racket[neighbors] function example above.

@chunk[<adjacent-tests>
       (test (adjacent (make-posn 0 1))
             (list (make-posn 0 0)
                   (make-posn 1 0)
                   (make-posn -1 1)
                   (make-posn 1 1)
                   (make-posn 0 2)
                   (make-posn 1 2)))]

The adjacent function has two main cases; first when the
@racket[y] coordinate of the @racket[posn] is even and 
second when it is odd. In each case, it is just a matter
of looking at the board and calculating coordinate offsets.

@chunk[<adjacent>
       (define/contract (adjacent p)
         (-> posn? 
             (and/c (listof posn?)
                    (lambda (l) (= 6 (length l)))))
         (let ([x (posn-x p)]
               [y (posn-y p)])
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

The @racket[on-boundary?] function returns @racket[#t] when 
the posn would be on the boundary of a board of size
@racket[board-size]. Note that this function does not
have to special case the missing @racket[posn]s from the corners.

@chunk[<on-boundary?>
       (define/contract (on-boundary? p board-size)
         (-> posn? natural-number/c 
             boolean?)
         (or (= (posn-x p) 0)
             (= (posn-y p) 0)
             (= (posn-x p) (- board-size 1))
             (= (posn-y p) (- board-size 1))))]

The @racket[in-bounds?] function returns @racket[#t]
when the @racket[posn] is actually on the board, meaning
that the coordinates of the @racket[posn] are within the
board's size, and that the @racket[posn] is not one
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

The function @racket[on-cats-path?] accepts a world and returns a predicate
on the @racket[posn]s in the world. The predicate indicates if the given
@racket[posn] is on the shortest path. 

For example, in a world of size @racket[7] with the cat at
@racket[(make-posn 2 2)], the circles with white centers
are on the shortest path to the boundary:

@image["chat-noir/cat-distance-example.png"]

So we can formulate two test cases using this world, one
in the white circles and one not:

@chunk[<on-cats-path?-tests>
       (let ([on-the-path?
              (on-cats-path? (make-world (empty-board 7)
                                         (make-posn 2 2)
                                         'playing
                                         7
                                         #f
                                         #t))])
         (test (on-the-path? (make-posn 1 0))
               #t)
         (test (on-the-path? (make-posn 4 4))
               #f))]

The computation of the shortest path to the boundary proceeds by computing
two distance maps; the distance map to the boundary and the distance map
to the cat. Then, a node is on one of the shortest paths if the distance
to the cat plus the distance to the boundary is equal to the distance from
the cat to the boundary. 

The code is essentially that, plus two other special cases. Specifically if the
``h'' key is not pressed down, then we just consider no cells to be on that shortest
path. And if the distance to the cat is @racket['∞], then again no nodes are on the
path. The second situation happens when the cat is completely boxed in and has
lost the game.

@chunk[<on-cats-path?>
       (define/contract (on-cats-path? w)
         (-> world? (-> posn? boolean?))
         (cond
           [(world-help? w)
            (let ()
              (define edge-distance-map (build-bfs-table w 'boundary))
              (define cat-distance-map (build-bfs-table w (world-cat w)))
              (define cat-distance
                (lookup-in-table edge-distance-map (world-cat w)))
              (cond
                [(equal? cat-distance '∞)
                 (lambda (p) #f)]
                [else
                 (lambda (p)
                   (equal? (+/f (lookup-in-table cat-distance-map p)
                                (lookup-in-table edge-distance-map p))
                           cat-distance))]))]
           [else
            (lambda (p) #f)]))]

Finally, the helper function @racket[+/f] is just like @racket[+], except that
it returns @racket['∞] if either argument is @racket['∞].

@chunk[<+/f>
       (define (+/f x y)
         (cond
           [(or (equal? x '∞) (equal? y '∞))
            '∞]
           [else
            (+ x y)]))]

@section{Drawing the Cat}

This code is three large, similar constants,
bundled up into the @racket[cat] function. 
The @racket[thinking-cat] is the one that 
is visible when the game is being played. It
differs from the others in that it does not
have a mouth. The @racket[mad-cat] is the one
that you see when the cat loses. It differs
from the others in that its pinks turn pink.
Finally, the @racket[happy-cat] shows up when
the cat wins and it is just like the @racket[thinking-cat]
except it has a smile.

@chunk[<drawing-the-cat>
       (define/contract (cat mode)
         (-> (or/c 'mad 'happy 'thinking) image?)
         (define face-width 36)
         (define face-height 22)
         
         (define face-color
           (cond
             [(eq? mode 'mad) 'pink]
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
             [(eq? mode 'happy) mouth-happy]
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
            -6 4 -30 -4 'black))
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
           (move-pinhole nose -1 -4))))
       
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
each other. Accordingly, @racket[circle-spacing]
is used when computing the positions of the circles,
but the circles are drawn using @racket[circle-radius].

@chunk[<constants>
       (define circle-radius 20)
       (define circle-spacing 22)]

The other four constants specify the colors of the circles.

@chunk[<constants>
       (define normal-color 'lightskyblue)
       (define on-shortest-path-color 'white)
       (define blocked-color 'black)
       (define under-mouse-color 'black)]

The main function for drawing a world is @racket[render-world].
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
For example, the @racket[rectangle] function returns
an image with a pinhole in the middle. So trimming 5x5
rectangle results in a 3x3 rectangle with the pinhole
at (0,0).

@chunk[<chop-whiskers-tests>
       (test (chop-whiskers (rectangle 5 5 'solid 'black))
             (put-pinhole (rectangle 3 3 'solid 'black) 0 0))]

The function uses @racket[shrink] to remove all of the material above
and to the left of the pinhole.

@chunk[<chop-whiskers>
(define/contract (chop-whiskers img)
  (-> image? image?)
  (shrink img
          0
          0
          (- (image-width img) (pinhole-x img) 1)
          (- (image-height img) (pinhole-y img) 1)))]

The @racket[render-board] function uses @racket[for/fold] to iterate
over all of the @racket[cell]s in @racket[cs]. It starts with
an empty rectangle and, one by one, puts the cells on @racket[image].

@chunk[<render-board>
       ;; render-board : board number (posn -> boolean) posn-or-#f -> image
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

The @racket[render-cell] function accepts a @racket[cell], 
a boolean indicating if the cell is on the shortest path between
the cat and the boundary, and a second boolean indicating
if the cell is underneath the mouse. It returns an image 
of the cell, with the pinhole placed in such a way that overlaying
the image on an empty image with pinhole in the upper-left corner
results in the cell being placed in the right place.

@chunk[<render-cell>
       (define/contract (render-cell c on-short-path? under-mouse?)
         (-> cell? boolean? boolean? image?)
         (let ([x (cell-center-x (cell-p c))]
               [y (cell-center-y (cell-p c))]
               [main-circle
                (cond
                  [(cell-blocked? c)
                   (circle circle-radius 'solid blocked-color)]
                  [else
                   (circle circle-radius 'solid normal-color)])])
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

The @racket[world-width] function computes the width of the rendered world,
given the world's size by finding the center of the rightmost posn,
and then adding an additional radius.

@chunk[<world-width>
       (define/contract (world-width board-size)
         (-> natural-number/c number?)
         (let ([rightmost-posn
                (make-posn (- board-size 1) (- board-size 2))])
           (+ (cell-center-x rightmost-posn) circle-radius)))]

Similarly, the @racket[world-height] function computest the 
height of the rendered world, given the world's size.

@chunk[<world-height>
       (define/contract (world-height board-size)
         (-> natural-number/c number?)
         (let ([bottommost-posn
                (make-posn (- board-size 1) (- board-size 1))])
           (ceiling (+ (cell-center-y bottommost-posn) 
                       circle-radius))))]

The @racket[cell-center-x] function returns the
@tt{x} coordinate of the center of the cell specified
by @racket[p].

For example, the first cell in
the third row (counting from @racket[0]) is
flush with the edge of the screen, so its
center is just the radius of the circle that
is drawn.

@chunk[<cell-center-x-tests>
       (test (cell-center-x (make-posn 0 2))
             circle-radius)]


The first cell in the second row, in contrast
is offset from the third row by
@racket[circle-spacing].

@chunk[<cell-center-x-tests>
       (test (cell-center-x (make-posn 0 1))
             (+ circle-spacing circle-radius))]


The definition of @racket[cell-center-x] 
multiplies the @racket[x] coordinate of 
@racket[p] by twice @racket[circle-spacing]
and then adds @racket[circle-radius] to move
over for the first circle. In addition
if the @racket[y] coordinate is odd, then
it adds @racket[circle-spacing], shifting 
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

The @racket[cell-center-y] function computes the
@racket[y] coordinate of a cell's location on
the screen. For example, the @racket[y] 
coordinate of the first row is 
the radius of a circle, ensuring that
the first row is flush against the top of
the screen.

@chunk[<cell-center-y-tests>
       (test (cell-center-y (make-posn 1 0))
             circle-radius)]

Because the grid is hexagonal, the @racket[y] coordinates
of the rows do not have the same spacing as the @racket[x]
coordinates. In particular, they are off by
@tt{sin(pi/3)}. We approximate that by @racket[866/1000]
in order to keep the computations and test cases simple
and using exact numbers.
A more precise approximation would be
@(racket #,(sin (/ pi 3))), but it is not necessary at
the screen resolution.

@chunk[<cell-center-y>
       (define/contract (cell-center-y p)
         (-> posn? number?)
         (+ circle-radius
            (* (posn-y p) 
               circle-spacing 2
               866/1000)))]
       
@section{Handling Input}

Input handling consists of handling two different kinds of events: key events, and mouse events, 
plus various helper functions.

@chunk[<input>
       <change>
       <clack>
       <update-world-posn>
       <player-moved?>
       <block-cell/world>
       <move-cat>
       <find-best-positions>
       <lt/f>
       <circle-at-point>
       <point-in-this-circle?>]

@chunk[<input-tests>
       <change-tests>
       <point-in-this-circle?-tests>
       <circle-at-point-tests>
       <lt/f-tests>
       <find-best-positions-tests>
       <move-cat-tests>
       <update-world-posn-tests>
       <clack-tests>]

The @racket[change] function handles keyboard input. If the input is @litchar{n} and the
game is over, then restart the game. If the input is @litchar{h} then turn on the help 
and otherwise do nothing.

@chunk[<change>
       ;; change : world key-event -> world
       (define (change w ke)
         (cond
           [(key=? ke "n")
            (if (equal? (world-state w) 'playing)
                w
                (make-initial-world))]
           [(key=? ke "h")
            (make-world (world-board w)
                        (world-cat w)
                        (world-state w)
                        (world-size w)
                        (world-mouse-posn w)
                        (not (world-help? w)))]
           [else w]))]

The @racket[clack] function handles mouse input. It has three tasks and each corresponds 
to a helper function:
@itemize[
@item{block the clicked cell (@racket[block-cell/world]),}
@item{move the cat (@racket[move-cat]), and}
@item{update the black dot as the mouse moves around (@racket[update-world-posn]).}]
The helper functions are combined in the body of @racket[clack],
first checking to see if the mouse event corresponds to a 
player's move (via the @racket[player-moved?] function.

@chunk[<clack>
       (define/contract (clack world x y evt)
         (-> world? integer? integer? any/c 
             world?)
         (let ([moved-world
                (cond
                  [(player-moved? world x y evt)
                   => 
                   (λ (circle)
                     (move-cat
                      (block-cell/world circle world)))]
                  [else world])])
           (update-world-posn
            moved-world
            (and (eq? (world-state moved-world) 'playing)
                 (not (equal? evt "leave"))
                 (make-posn x y)))))]

The @racket[player-moved?] predicate returns
a @racket[posn] indicating where the player chose
to move when the mouse event corresponds to a player move,
and returns @racket[#f]. It first checks to see if the
mouse event is a button up event and that the game
is not over, and then it just calls @racket[circle-at-point].

@chunk[<player-moved?>
       (define/contract (player-moved? world x y evt)
         (-> world? integer? integer? any/c 
             (or/c posn? #f))
         (and (equal? evt "button-up")
              (equal? 'playing (world-state world))
              (circle-at-point (world-board world) x y)))]

The @racket[circle-at-point] function returns a @racket[posn] when
the coordinate (@racket[x],@racket[y]) is inside an unblocked circle
on the given board. Instead of computing the nearest
circle to the coordinates, it simply iterates over the cells on the
board and returns the @racket[posn] of the matching cell.

@chunk[<circle-at-point>
       (define/contract (circle-at-point board x y)
         (-> (listof cell?) real? real? 
             (or/c posn? #f))
         (ormap (λ (cell)
                  (and (point-in-this-circle? (cell-p cell) x y)
                       (not (cell-blocked? cell))
                       (cell-p cell)))
                board))]


The @racket[point-in-this-circle?] function returns @racket[#t]
when the point (@racket[x],@racket[y]) on the screen
falls within the circle located at the @racket[posn] @racket[p].

This is precise about checking the circles. For example, 
a point that is (14,14) away from the center of a circle
is still in the circle:

@chunk[<point-in-this-circle?-tests>
       (test (point-in-this-circle? 
              (make-posn 1 0)
              (+ (cell-center-x (make-posn 1 0)) 14)
              (+ (cell-center-y (make-posn 1 0)) 14))
             #t)]

but one that is (15,15) away is no longer in the circle,
since it crosses the boundary away from a circle of radius
20 at that point.

@chunk[<point-in-this-circle?-tests>
       (test (point-in-this-circle? 
              (make-posn 1 0)
              (+ (cell-center-x (make-posn 1 0)) 15)
              (+ (cell-center-y (make-posn 1 0)) 15))
             #f)]

The implementation of @racket[point-in-this-circle?] uses
complex numbers to represent both points on the screen
and directional vectors. In particular, the 
variable @racket[center] is a complex number whose
real part is the @tt{x} coordinate of the center of
the cell at @racket[p], and its imaginary part is 
@tt{y} coordinate. Similarly, @racket[mp] is bound
to a complex number corresponding to the position of
the mouse, at (@racket[x], @racket[y]). Then, the
function computes the vector between the two points
by subtracting the complex numbers from each
other and extracting the magnitude from that vector.

@chunk[<point-in-this-circle?>
       (define/contract (point-in-this-circle? p x y)
         (-> posn? real? real? boolean?)
         (let ([center (+ (cell-center-x p)
                          (* (sqrt -1)
                             (cell-center-y p)))]
               [mp (+ x (* (sqrt -1) y))])
           (<= (magnitude (- center mp)) 
               circle-radius)))]

In the event that @racket[player-moved?] returns a @racket[posn],
the @racket[clack] function blocks the clicked on cell using
@racket[block-cell/world], which simply calls @racket[block-cell].

@chunk[<block-cell/world>
       (define/contract (block-cell/world to-block w)
         (-> posn? world? world?)
         (make-world (block-cell to-block (world-board w))
                     (world-cat w)
                     (world-state w)
                     (world-size w)
                     (world-mouse-posn w)
                     (world-help? w)))]

The @racket[move-cat] function uses calls @racket[build-bfs-table]
to find the shortest distance from all of the cells to the boundary,
and then uses @racket[find-best-positions] to compute the
list of neighbors of the cat that have the shortest distance
to the boundary. If that list is empty, then @racket[next-cat-position]
is @racket[#f], and otherwise, it is a random element from that list.

@chunk[<move-cat>
       (define/contract (move-cat world)
         (-> world? world?)
         (let* ([cat-position (world-cat world)]
                [table (build-bfs-table world 'boundary)]
                [neighbors (adjacent cat-position)]
                [next-cat-positions
                 (find-best-positions neighbors
                                      (map (lambda (p) (lookup-in-table table p))
                                           neighbors))]
                [next-cat-position
                 (cond
                   [(boolean? next-cat-positions) #f]
                   [else
                    (list-ref next-cat-positions
                              (random (length next-cat-positions)))])])
           
           <moved-cat-world>))]

Once @racket[next-cat-position] has been computed, it is used to update
the @tt{cat} and @tt{state} fields of the world, recording the cat's new
position and whether or not the cat won.

@chunk[<moved-cat-world>
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
                   (world-help? world))]


The @racket[find-best-positions] function accepts
two parallel lists, one of @racket[posn]s, and one
of scores for those @racket[posn]s, and it 
returns either a non-empty list of @racket[posn]s
that have tied for the best score, or it
returns @racket[#f], if the best score is 
@racket['∞].

@chunk[<find-best-positions>
       (define/contract (find-best-positions posns scores)
         (-> (cons/c posn? (listof posn?))
             (cons/c (or/c number? '∞) (listof (or/c number? '∞)))
             (or/c (cons/c posn? (listof posn?)) #f))
         (let ([best-score
                (foldl (lambda (x sofar)
                         (if (<=/f x sofar)
                             x
                             sofar))
                       (first scores)
                       (rest scores))])
           (cond
             [(symbol? best-score) #f]
             [else
              (map
               second
               (filter (lambda (x) (equal? (first x) best-score))
                       (map list scores posns)))])))]

This is a helper function that behaves like
@racket[<=], but is extended to deal properly with
@racket['∞].

@chunk[<lt/f>
       (define/contract (<=/f a b)
         (-> (or/c number? '∞) 
             (or/c number? '∞)
             boolean?)
         (cond
           [(equal? b '∞) #t]
           [(equal? a '∞) #f]
           [else (<= a b)]))]


Finally, to complete the mouse event handling, the @racket[update-world-posn]
function is called from @racket[clack]. It updates the @tt{mouse-down}
field of the @racket[world]. If the @racket[p] argument is a @racket[posn],
it corresponds to the location of the mouse, in graphical coordinates.
So, the function converts it to a cell position on the board and uses that.
Otherwise, when @racket[p] is @racket[#f], the @tt{mouse-down} field
is just updated to @racket[#f].

@chunk[<update-world-posn>
       (define/contract (update-world-posn w p)
         (-> world? (or/c #f posn?)
             world?)
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
                             #f]
                            [else
                             mouse-spot])
                          (world-help? w)))]
           [else
            (make-world (world-board w)
                        (world-cat w)
                        (world-state w)
                        (world-size w)
                        #f
                        (world-help? w))]))]


@section{Tests}

This section consists of some infrastructure for
maintaining tests, plus a pile of additional tests
for the other functions in this document.

The @racket[test] and @racket[test/set] macros
package up their arguments into thunks and then
simply call @racket[test/proc], supplying 
information about the source location of the test
case. The @racket[test/proc] function runs the tests
and reports the results.

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

(test (same-sets? (list) (list)) #t)
(test (same-sets? (list) (list 1)) #f)
(test (same-sets? (list 1) (list)) #f)
(test (same-sets? (list 1 2) (list 2 1)) #t)]

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
                              'playing 3 (make-posn 0 0) #f)
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
                    (make-cell (make-posn 0 1) #t)
                    (make-cell (make-posn 1 0) #t)
                    (make-cell (make-posn 1 1) #f)
                    (make-cell (make-posn 1 2) #t)
                    (make-cell (make-posn 2 0) #t)
                    (make-cell (make-posn 2 1) #t)
                    (make-cell (make-posn 2 2) #t))
                   (make-posn 1 1)
                   'playing
                   3
                   (make-posn 0 0)
                   #f)
                  'boundary)
                 (list
                  (make-dist-cell 'boundary 0)))

       (test/set (build-bfs-table 
                  (make-world (empty-board 5)
                              (make-posn 2 2)
                              'playing
                              5
                              (make-posn 0 0)
                              #f)
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
                              #f)
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
                              #f)
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
                                           #f)
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
                                      (make-cell (make-posn 0 1) #f)
                                      (make-cell (make-posn 1 0) #f)
                                      (make-cell (make-posn 1 1) #t)
                                      (make-cell (make-posn 1 2) #f)
                                      (make-cell (make-posn 2 0) #f)
                                      (make-cell (make-posn 2 1) #f)
                                      (make-cell (make-posn 2 2) #f))
                                     (make-posn 1 1)
                                     'playing
                                     3
                                     (make-posn 0 0)
                                     #f))
              (make-posn 1 1))
             '())
       (test ((neighbors (make-world (list
                                      (make-cell (make-posn 0 1) #f)
                                      (make-cell (make-posn 1 0) #f)
                                      (make-cell (make-posn 1 1) #t)
                                      (make-cell (make-posn 1 2) #f)
                                      (make-cell (make-posn 2 0) #f)
                                      (make-cell (make-posn 2 1) #f)
                                      (make-cell (make-posn 2 2) #f))
                                     (make-posn 1 1)
                                     'playing
                                     3
                                     (make-posn 0 0)
                                     #f))
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
       (test (on-boundary? (make-posn 0 1) 13) #t)
       (test (on-boundary? (make-posn 1 0) 13) #t)
       (test (on-boundary? (make-posn 12 1) 13) #t)
       (test (on-boundary? (make-posn 1 12) 13) #t)
       (test (on-boundary? (make-posn 1 1) 13) #f)
       (test (on-boundary? (make-posn 10 10) 13) #f)]


@chunk[<in-bounds?-tests>
       (test (in-bounds? (make-posn 0 0) 11) #f)
       (test (in-bounds? (make-posn 0 1) 11) #t)
       (test (in-bounds? (make-posn 1 0) 11) #t)
       (test (in-bounds? (make-posn 10 10) 11) #t)
       (test (in-bounds? (make-posn 0 -1) 11) #f)
       (test (in-bounds? (make-posn -1 0) 11) #f)
       (test (in-bounds? (make-posn 0 11) 11) #f)
       (test (in-bounds? (make-posn 11 0) 11) #f)
       (test (in-bounds? (make-posn 10 0) 11) #t)
       (test (in-bounds? (make-posn 0 10) 11) #f)]

@chunk[<on-cats-path?-tests>
       (test ((on-cats-path? (make-world (empty-board 5)
                                         (make-posn 1 1)
                                         'playing
                                         5
                                         (make-posn 0 0)
                                         #t))
              (make-posn 1 0))
             #t)
       (test ((on-cats-path? (make-world (empty-board 5)
                                         (make-posn 1 1)
                                         'playing
                                         5 
                                         (make-posn 0 0)
                                         #f))
              (make-posn 1 0))
             #f)

       (test ((on-cats-path? (make-world (empty-board 5) (make-posn 1 1)
                                         'playing 5 (make-posn 0 0) #t))
              (make-posn 2 1))
             #f)
       (test ((on-cats-path?
               (make-world (list
                            (make-cell (make-posn 0 1) #t)
                            (make-cell (make-posn 1 0) #t)
                            (make-cell (make-posn 1 1) #f)
                            (make-cell (make-posn 1 2) #t)
                            (make-cell (make-posn 2 0) #t)
                            (make-cell (make-posn 2 1) #t)
                            (make-cell (make-posn 2 2) #t))
                           (make-posn 1 1)
                           'cat-lost
                           3
                           (make-posn 0 0)
                           #t))
              (make-posn 0 1))
             #f)]


@chunk[<+/f-tests>       
       (test (+/f '∞ '∞) '∞)
       (test (+/f '∞ 1) '∞)
       (test (+/f 1 '∞) '∞)
       (test (+/f 1 2) 3)]

@chunk[<render-world-tests>

       (test
        (render-world
         (make-world (list (make-cell (make-posn 0 1) #f))
                     (make-posn 0 1)
                     'playing
                     3
                     (make-posn 0 0)
                     #f))
        (overlay
         (render-board (list (make-cell (make-posn 0 1) #f))
                       3
                       (lambda (x) #t)
                       #f)
         (move-pinhole thinking-cat
                       (- (cell-center-x (make-posn 0 1)))
                       (- (cell-center-y (make-posn 0 1))))))
       
       (test
        (render-world
         (make-world (list (make-cell (make-posn 0 1) #f))
                     (make-posn 0 1)
                     'cat-won
                     3
                     #f
                     #f))
        (overlay
         (render-board (list (make-cell (make-posn 0 1) #f))
                       3
                       (lambda (x) #t)
                       #f)
         (move-pinhole happy-cat
                       (- (cell-center-x (make-posn 0 1)))
                       (- (cell-center-y (make-posn 0 1))))))
       
       (test
        (render-world
         (make-world (list (make-cell (make-posn 0 1) #f))
                     (make-posn 0 1)
                     'cat-lost
                     3
                     #f
                     #f))
        (overlay
         (render-board (list (make-cell (make-posn 0 1) #f))
                       3
                       (lambda (x) #t)
                       #f)
         (move-pinhole mad-cat
                       (- (cell-center-x (make-posn 0 1)))
                       (- (cell-center-y (make-posn 0 1))))))
       
       (test
        (render-world
         (make-world (list
                      (make-cell (make-posn 0 1) #t)
                      (make-cell (make-posn 1 0) #t)
                      (make-cell (make-posn 1 1) #f)
                      (make-cell (make-posn 1 2) #t)
                      (make-cell (make-posn 2 0) #t)
                      (make-cell (make-posn 2 1) #t)
                      (make-cell (make-posn 2 2) #t))
                     (make-posn 1 1)
                     'cat-lost
                     3
                     #f
                     #f))
        (overlay
         (render-board (list
                        (make-cell (make-posn 0 1) #t)
                        (make-cell (make-posn 1 0) #t)
                        (make-cell (make-posn 1 1) #f)
                        (make-cell (make-posn 1 2) #t)
                        (make-cell (make-posn 2 0) #t)
                        (make-cell (make-posn 2 1) #t)
                        (make-cell (make-posn 2 2) #t))
                       3
                       (lambda (x) #f)
                       #f)
         (move-pinhole mad-cat
                       (- (cell-center-x (make-posn 1 1)))
                       (- (cell-center-y (make-posn 1 1))))))
       
       (test
        (render-world
         (make-world (list
                      (make-cell (make-posn 0 1) #f)
                      (make-cell (make-posn 1 0) #f)
                      (make-cell (make-posn 1 1) #f)
                      (make-cell (make-posn 1 2) #f)
                      (make-cell (make-posn 2 0) #f)
                      (make-cell (make-posn 2 1) #f)
                      (make-cell (make-posn 2 2) #f))
                     (make-posn 1 1)
                     'cat-lost
                     3
                     (make-posn (cell-center-x (make-posn 0 1))
                                (cell-center-y (make-posn 0 1)))
                     #t))
        
        (overlay
         (render-board (list
                        (make-cell (make-posn 0 1) #f)
                        (make-cell (make-posn 1 0) #f)
                        (make-cell (make-posn 1 1) #f)
                        (make-cell (make-posn 1 2) #f)
                        (make-cell (make-posn 2 0) #f)
                        (make-cell (make-posn 2 1) #f)
                        (make-cell (make-posn 2 2) #f))
                       3
                       (lambda (x) #t)
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
           #f)))
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
           #f)))
        0)]

@chunk[<render-board-tests>
       (test (render-board (list (make-cell (make-posn 0 0) #f))
                           3
                           (lambda (x) #f)
                           #f)
             (overlay
              (nw:rectangle (world-width 3)
                            (world-height 3)
                            'solid
                            'white)
              (render-cell (make-cell (make-posn 0 0) #f)
                           #f
                           #f)))
       
       (test (render-board (list (make-cell (make-posn 0 0) #f))
                           3
                           (lambda (x) #t)
                           #f)
             (overlay
              (nw:rectangle (world-width 3)
                            (world-height 3)
                            'solid
                            'white)
              (render-cell (make-cell (make-posn 0 0) #f)
                           #t
                           #f)))
       
       
       (test (render-board (list (make-cell (make-posn 0 0) #f))
                           3
                           (lambda (x) #f)
                           #f)
             (overlay
              (nw:rectangle (world-width 3)
                            (world-height 3)
                            'solid
                            'white)
              (render-cell (make-cell (make-posn 0 0) #f)
                           #f
                           #f)))
       
       (test (render-board (list (make-cell (make-posn 0 0) #f)
                                 (make-cell (make-posn 0 1) #f))
                           3
                           (lambda (x) (equal? x (make-posn 0 1)))
                           #f)
             (overlay
              (nw:rectangle (world-width 3)
                            (world-height 3)
                            'solid
                            'white)
              (render-cell (make-cell (make-posn 0 0) #f)
                           #f
                           #f)
              (render-cell (make-cell (make-posn 0 1) #f)
                           #t
                           #f)))
       
       (test (render-board (list (make-cell (make-posn 0 0) #f)
                                 (make-cell (make-posn 0 1) #f))
                           3
                           (lambda (x) (equal? x (make-posn 0 1)))
                           (make-posn 0 0))
             (overlay
              (nw:rectangle (world-width 3)
                            (world-height 3)
                            'solid
                            'white)
              (render-cell (make-cell (make-posn 0 0) #f)
                           #f
                           #t)
              (render-cell (make-cell (make-posn 0 1) #f)
                           #t
                           #f)))]


@chunk[<render-cell-tests>
       (test (render-cell (make-cell (make-posn 0 0) #f) #f #f)
             (move-pinhole (circle circle-radius 'solid normal-color)
                           (- circle-radius)
                           (- circle-radius)))
       (test (render-cell (make-cell (make-posn 0 0) #t) #f #f)
             (move-pinhole (circle circle-radius 'solid 'black)
                           (- circle-radius)
                           (- circle-radius)))
       (test (render-cell (make-cell (make-posn 0 0) #f) #t #f)
             (move-pinhole (overlay (circle circle-radius 'solid normal-color)
                                    (circle (quotient circle-radius 2) 'solid
                                            on-shortest-path-color))
                           (- circle-radius)
                           (- circle-radius)))
       (test (render-cell (make-cell (make-posn 0 0) #f) #t #t)
             (move-pinhole (overlay (circle circle-radius 'solid normal-color)
                                    (circle (quotient circle-radius 2) 'solid
                                            under-mouse-color))
                           (- circle-radius)
                           (- circle-radius)))]


@chunk[<world-size-tests>
       (test (world-width 3) 150)
       (test (world-height 3) 117)]

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


@chunk[<clack-tests>
       (test (clack 
              (make-world '() (make-posn 0 0) 'playing 3 #f #f)
              1 1 "button-down")
             (make-world '() (make-posn 0 0) 'playing 3 #f #f))
       (test (clack
              (make-world '() (make-posn 0 0) 'playing 3 #f #f)
              1 1 'drag)
             (make-world '() (make-posn 0 0) 'playing 3 #f #f))
       (test (clack 
              (make-world (list (make-cell (make-posn 0 0) #f))
                          (make-posn 0 1)
                          'playing
                          3
                          #f
                          #f)
              (cell-center-x (make-posn 0 0))
              (cell-center-y (make-posn 0 0))
              'move)
             (make-world
              (list (make-cell (make-posn 0 0) #f))
              (make-posn 0 1)
              'playing
              3
              (make-posn 0 0)
              #f))
       (test (clack 
              (make-world (list (make-cell (make-posn 0 0) #f))
                          (make-posn 0 1)
                          'playing
                          3
                          #f
                          #f)
              (cell-center-x (make-posn 0 0))
              (cell-center-y (make-posn 0 0))
              'enter)
             (make-world
              (list (make-cell (make-posn 0 0) #f))
              (make-posn 0 1)
              'playing
              3
              (make-posn 0 0)
              #f))
       (test (clack 
              (make-world '() (make-posn 0 0)
                          'playing 3 (make-posn 0 0) #f)
                    1 1 'leave)
             (make-world '() (make-posn 0 0) 'playing 3 #f #f))
       
       (test (clack (make-world '() (make-posn 0 0)
                                'playing 3 (make-posn 0 0) #f)
                    10
                    10
                    "button-down")
             (make-world '() (make-posn 0 0) 'playing 3 #f #f))
       
       (test (clack (make-world (list (make-cell (make-posn 0 0) #f)
                                      (make-cell (make-posn 1 1) #f))
                                (make-posn 1 1)
                                'playing
                                3
                                (make-posn 0 0)
                                #f)
                    (cell-center-x (make-posn 0 0))
                    (cell-center-y (make-posn 0 0))
                    "button-up")
             (make-world (list (make-cell (make-posn 0 0) #t)
                               (make-cell (make-posn 1 1) #f))
                         (make-posn 1 1)
                         'cat-lost
                         3
                         #f
                         #f))
       
       
       (test (clack (make-world '() (make-posn 0 0)
                                'cat-lost 3 (make-posn 0 0) #f)
                    10
                    10
                    "button-up")
             (make-world '() (make-posn 0 0)
                         'cat-lost 3 #f #f))
       (test (clack
              (make-world
               (list (make-cell (make-posn 1 0) #f)
                     (make-cell (make-posn 2 0) #t)
                     (make-cell (make-posn 0 1) #t)
                     (make-cell (make-posn 1 1) #f)
                     (make-cell (make-posn 2 1) #t)
                     (make-cell (make-posn 1 2) #t)
                     (make-cell (make-posn 2 2) #t))
               (make-posn 1 1)
               'playing
               3
               #f
               #f)
              (cell-center-x (make-posn 1 0))
              (cell-center-y (make-posn 1 0))
              "button-up")
             (make-world
              (list (make-cell (make-posn 1 0) #t)
                    (make-cell (make-posn 2 0) #t)
                    (make-cell (make-posn 0 1) #t)
                    (make-cell (make-posn 1 1) #f)
                    (make-cell (make-posn 2 1) #t)
                    (make-cell (make-posn 1 2) #t)
                    (make-cell (make-posn 2 2) #t))
              (make-posn 1 1)
              'cat-lost
              3
              #f
              #f))
       
       (test (clack
              (make-world
               (list (make-cell (make-posn 1 0) #f)
                     (make-cell (make-posn 2 0) #f)
                     (make-cell (make-posn 0 1) #t)
                     (make-cell (make-posn 1 1) #f)
                     (make-cell (make-posn 2 1) #t)
                     (make-cell (make-posn 1 2) #t)
                     (make-cell (make-posn 2 2) #t))
               (make-posn 1 1)
               'playing
               3
               #f
               #f)
              (cell-center-x (make-posn 1 0))
              (cell-center-y (make-posn 1 0))
              "button-up")
             (make-world
              (list (make-cell (make-posn 1 0) #t)
                    (make-cell (make-posn 2 0) #f)
                    (make-cell (make-posn 0 1) #t)
                    (make-cell (make-posn 1 1) #f)
                    (make-cell (make-posn 2 1) #t)
                    (make-cell (make-posn 1 2) #t)
                    (make-cell (make-posn 2 2) #t))
              (make-posn 2 0)
              'cat-won
              3
              #f
              #f))]

@chunk[<update-world-posn-tests>

       (test (update-world-posn
              (make-world (list (make-cell (make-posn 0 0) #f))
                          (make-posn 0 1) 'playing 3 #f #f)
              (make-posn (cell-center-x (make-posn 0 0))
                         (cell-center-y (make-posn 0 0))))
             (make-world (list (make-cell (make-posn 0 0) #f))
                         (make-posn 0 1) 'playing 3 (make-posn 0 0) #f))
       
       (test (update-world-posn
              (make-world (list (make-cell (make-posn 0 0) #f))
                          (make-posn 0 0) 'playing 3 #f #f)
              (make-posn (cell-center-x (make-posn 0 0))
                         (cell-center-y (make-posn 0 0))))
             (make-world (list (make-cell (make-posn 0 0) #f))
                         (make-posn 0 0) 'playing 3 #f #f))
       
       (test (update-world-posn
              (make-world (list (make-cell (make-posn 0 0) #f))
                          (make-posn 0 1) 'playing 3 (make-posn 0 0) #f)
              (make-posn 0 0))
             (make-world (list (make-cell (make-posn 0 0) #f))
                         (make-posn 0 1) 'playing 3 #f #f))]

@chunk[<move-cat-tests>
       (test
        (move-cat
         (make-world (list (make-cell (make-posn 1 0) #f)
                           (make-cell (make-posn 2 0) #f)
                           (make-cell (make-posn 3 0) #f)
                           (make-cell (make-posn 4 0) #f)
                           
                           (make-cell (make-posn 0 1) #f)
                           (make-cell (make-posn 1 1) #t)
                           (make-cell (make-posn 2 1) #t)
                           (make-cell (make-posn 3 1) #f)
                           (make-cell (make-posn 4 1) #f)
                           
                           (make-cell (make-posn 0 2) #f)
                           (make-cell (make-posn 1 2) #t)
                           (make-cell (make-posn 2 2) #f)
                           (make-cell (make-posn 3 2) #t)
                           (make-cell (make-posn 4 2) #f)
                           
                           (make-cell (make-posn 0 3) #f)
                           (make-cell (make-posn 1 3) #t)
                           (make-cell (make-posn 2 3) #f)
                           (make-cell (make-posn 3 3) #f)
                           (make-cell (make-posn 4 3) #f)
                           
                           (make-cell (make-posn 1 4) #f)
                           (make-cell (make-posn 2 4) #f)
                           (make-cell (make-posn 3 4) #f)
                           (make-cell (make-posn 4 4) #f))
                     (make-posn 2 2)
                     'playing
                     5
                     (make-posn 0 0)
                     #f))
        (make-world (list (make-cell (make-posn 1 0) #f)
                          (make-cell (make-posn 2 0) #f)
                          (make-cell (make-posn 3 0) #f)
                          (make-cell (make-posn 4 0) #f)
                          
                          (make-cell (make-posn 0 1) #f)
                          (make-cell (make-posn 1 1) #t)
                          (make-cell (make-posn 2 1) #t)
                          (make-cell (make-posn 3 1) #f)
                          (make-cell (make-posn 4 1) #f)
                          
                          (make-cell (make-posn 0 2) #f)
                          (make-cell (make-posn 1 2) #t)
                          (make-cell (make-posn 2 2) #f)
                          (make-cell (make-posn 3 2) #t)
                          (make-cell (make-posn 4 2) #f)
                          
                          (make-cell (make-posn 0 3) #f)
                          (make-cell (make-posn 1 3) #t)
                          (make-cell (make-posn 2 3) #f)
                          (make-cell (make-posn 3 3) #f)
                          (make-cell (make-posn 4 3) #f)
                          
                          (make-cell (make-posn 1 4) #f)
                          (make-cell (make-posn 2 4) #f)
                          (make-cell (make-posn 3 4) #f)
                          (make-cell (make-posn 4 4) #f))
                    (make-posn 2 3)
                    'playing
                    5
                    (make-posn 0 0)
                    #f))]

@chunk[<change-tests>
       (test (change (make-world '() (make-posn 1 1)
                                 'playing 3 (make-posn 0 0) #f)
                     "h")
             (make-world '() (make-posn 1 1)
                         'playing 3 (make-posn 0 0) #t))
       (test (change (make-world '() (make-posn 1 1)
                                 'playing 3 (make-posn 0 0) #t)
                     "h")
             (make-world '() (make-posn 1 1)
                         'playing 3 (make-posn 0 0) #f))
       (test (change (make-world '() (make-posn 1 1)
                                 'playing 3 (make-posn 0 0) #f)
                     "n")
             (make-world '() (make-posn 1 1)
                         'playing 3 (make-posn 0 0) #f))
       (test (world-state (change (make-world '() (make-posn 1 1)
                                              'cat-lost 3 (make-posn 0 0) #f)
                                  "n"))
             'playing)]


@chunk[<point-in-this-circle?-tests>
       (test (point-in-this-circle? (make-posn 0 0)
                                    (cell-center-x (make-posn 0 0))
                                    (cell-center-y (make-posn 0 0)))
             #t)
       (test (point-in-this-circle? (make-posn 0 0) 0 0)
             #f)]

@chunk[<find-best-positions-tests>
       (test (find-best-positions (list (make-posn 0 0)) (list 1))
             (list (make-posn 0 0)))
       (test (find-best-positions (list (make-posn 0 0)) (list '∞))
             #f)
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
             #f)]

@chunk[<lt/f-tests>
       (test (<=/f 1 2) #t)
       (test (<=/f 2 1) #f)
       (test (<=/f '∞ 1) #f)
       (test (<=/f 1 '∞) #t)
       (test (<=/f '∞ '∞) #t)]

@chunk[<circle-at-point-tests>
       (test (circle-at-point empty 0 0) #f)
       (test (circle-at-point (list (make-cell (make-posn 0 0) #f))
                              (cell-center-x (make-posn 0 0))
                              (cell-center-y (make-posn 0 0)))
             (make-posn 0 0))
       (test (circle-at-point (list (make-cell (make-posn 0 0) #f)
                                    (make-cell (make-posn 0 1) #f))
                              (cell-center-x (make-posn 0 1))
                              (cell-center-y (make-posn 0 1)))
             (make-posn 0 1))
       (test (circle-at-point (list (make-cell (make-posn 0 0) #f))
                              0 0)
             #f)]

@chunk[<blocked-cells-tests>
       (test (block-cell (make-posn 1 1)
                         (list (make-cell (make-posn 0 0) #f)
                               (make-cell (make-posn 1 1) #f)
                               (make-cell (make-posn 2 2) #f)))
             (list (make-cell (make-posn 0 0) #f)
                   (make-cell (make-posn 1 1) #t)
                   (make-cell (make-posn 2 2) #f)))
       
       (test (add-n-random-blocked-cells 0 (list (make-cell (make-posn 0 0)
                                                            #t))
                                         3)
             (list (make-cell (make-posn 0 0) #t)))
       (test (add-n-random-blocked-cells 1 (list (make-cell (make-posn 0 0)
                                                            #f))
                                         3)
             (list (make-cell (make-posn 0 0) #t)))]

@section{Run, program, run}

This section contains expressions that start
the Chat Noir game going.

First, here is a function to compute state of the world at the start of a game.

@chunk[<initial-world>
       (define board-size 11)
       (define (make-initial-world)
         (define initial-board
           (add-n-random-blocked-cells
            6
            (empty-board board-size)
            board-size))
         (make-world initial-board
                     (make-posn (quotient board-size 2)
                                (quotient board-size 2))
                     'playing
                     board-size
                     #f
                     #f))]

Finally, we can define and provide a function to start the game
by calling @racket[big-bang] with the appropriate arguments.

@chunk[<go>
       (provide main)
       (define (main)
         (void
          (big-bang (make-initial-world)
                    (on-draw render-world
                             (world-width board-size)
                             (world-height board-size))
                    (on-key change)
                    (on-mouse clack)
                    (name "Chat Noir"))))]
