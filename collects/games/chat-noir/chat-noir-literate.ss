#reader "literate-reader.ss"

@title{Chat Noir}

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
the Chat Noir game.

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
                 (for-syntax scheme/base))
       (require htdp/world lang/posn scheme/contract)
       <world>
       <graph>
       <tests>
       <everything-else>]

Each section also comes with a series of test cases that are collected into the 
@chunkref[<tests>] chunk at the end of the program.

@chunk[<tests>
       <test-infrastructure>
       <graph-tests>
       <world-tests>]

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
       <data-definitions> <empty-world> <empty-board>]

@chunk[<world-tests>
       <empty-world-test> <empty-board-test>]

The main structure definition is the @scheme[world] struct.

@chunk[<data-definitions>
(define-struct world (board cat state size mouse-posn h-down?)
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

@chunk[<data-definitions>
       (define-struct cell (p blocked?) #:transparent)]
       
The first field contains a @scheme[posn] struct.  The coordinates of
the posn indicate a position on the hexagonal grid. 
This program reprsents the hexagon grid as a series of rows that
are offset from each other by 1/2 the size of the each cell.
The @tt{y} field
of the @scheme[posn] refers to the row of the cell, and the @tt{x}
coordinate the position in the row.  This means that, for example,
@scheme[(make-posn 1 0)] is centered above @scheme[(make-posn 1 0)]
and @scheme[(make-posn 1 1)].  (See @scheme[cell-center-x] and
@scheme[cell-center-y] below for the conversion of those positions to
screen coordinates.)

The @tt{blocked?} field is a boolean indicating if the cell has been
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
         (-> natural-number/c (listof cell?))
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
         (-> natural-number/c (-> cell? boolean?))
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
       
       (define (empty-world board-size)
         (make-world (empty-board board-size)
                     (make-posn (quotient board-size 2)
                                (quotient board-size 2))
                     'playing
                     board-size
                     false
                     false))]



@section{Graph}

The cat's move decision is based on a breadth-first search of a graph.
The graph's nodes are the cells on the board plus a special
node called @scheme['boundary] that is adjacent to every cell 
on the boundary of the graph. In addition to the boundary edges,
there are edges
between each pair of adjacent cells, unless one of the cells is
blocked, in which case it has no edges at all (even to the boundary).

The code for the breadth-first search is organized into
X parts ....

@chunk[<graph>
       <dist-cell-data-definition>
       <build-bfs-table>
       <neighbors>
       <neighbors-blocked/boundary>
       
       <lookup-in-table>
       <on-cats-path?>
       <adjacent>
       <in-bounds?>
       <on-boundary?>
       <extended-arithmetic-ops>]

@chunk[<graph-tests>
       <extended-arithmetic-ops-tests>
       <on-boundary?-tests>
       <in-bounds?-tests>
       <adjacent-tests>
       <neighbors-tests>
       <on-cats-path?-tests>
       <lookup-in-table-tests>
       <build-bfs-table-tests>
       
       ]

The breadth-first function constructs a @scheme[distance-map],
which is a list of @scheme[dist-cell] structs:

@chunk[<dist-cell-data-definition>
(define-struct dist-cell (p n) #:transparent)]

Each @tt{p} field in the @scheme[dist-cell] is a position on the board
and the @tt{n} field is a natural number or @scheme['∞], indicating
the distance of the shortest path from the node to some fixed point on
the board. 

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
       
       (define (bfs queue dist-table)
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
this node.

The @scheme[build-bfs-table] function packages up @scheme[bfs]
function. It accepts a @tt{world} and an initial position
and returns a @scheme[distance-table].

@chunk[<build-bfs-table>
       
       (define (build-bfs-table world init-point)
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

As far as the @scheme[build-bfs-table] function goes,
all of the information specific to Chat Noir is
encoded in the neighbors function. 
It accepts a world and returns a function
that computes the neighbors of the boundary
and of nodes.

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

as you can see from the pictures of the 7x7 empty board above.
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
;; neighbors : world -> (or/c 'boundary posn) -> (listof (or/c 'boundary posn))
(define (neighbors w)
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
;; neighbors : world -> (or/c 'boundary posn) -> (listof (or/c 'boundary posn))
(define (neighbors-blocked/boundary blocked boundary-cells size p)
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


@chunk[<lookup-in-table>
;; lookup-in-table : distance-map posn -> number or '∞
;; looks for the distance as recorded in the table t,
;; if not found returns a distance of '∞
(define (lookup-in-table t p)
  (cond
    [(empty? t) '∞]
    [else (cond
            [(equal? p (dist-cell-p (first t)))
             (dist-cell-n (first t))]
            [else
             (lookup-in-table (rest t) p)])]))]

@chunk[<lookup-in-table-tests>

(test (lookup-in-table empty (make-posn 1 2)) '∞)
(test (lookup-in-table (list (make-dist-cell (make-posn 1 2) 3))
                               (make-posn 1 2))
              3)
(test (lookup-in-table (list (make-dist-cell (make-posn 2 1) 3))
                               (make-posn 1 2))
              '∞)]


@chunk[<on-cats-path?>
;; on-cats-path? : world -> posn -> boolean
;; returns true when the posn is on the shortest path
;; from the cat to the edge of the board, in the given world
(define (on-cats-path? w)
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

@chunk[<on-cats-path?-tests>
(test ((on-cats-path? (make-world (empty-board 5) (make-posn 1 1)
                                          'playing 5 (make-posn 0 0) true))
               (make-posn 1 0))
              true)
(test ((on-cats-path? (make-world (empty-board 5) (make-posn 1 1)
                                          'playing 5 (make-posn 0 0) false))
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



@chunk[<adjacent>
;; adjacent : posn -> (listof posn)
;; returns a list of the posns that are adjacent to
;; `p' on an infinite hex grid
(define (adjacent p)
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

@chunk[<on-boundary?>
;; on-boundary? : posn number -> boolean
(define (on-boundary? p board-size)
  (or (= (posn-x p) 0)
      (= (posn-y p) 0)
      (= (posn-x p) (- board-size 1))
      (= (posn-y p) (- board-size 1))))]

@chunk[<on-boundary?-tests>
(test (on-boundary? (make-posn 0 1) 13) true)
(test (on-boundary? (make-posn 1 0) 13) true)
(test (on-boundary? (make-posn 12 1) 13) true)
(test (on-boundary? (make-posn 1 12) 13) true)
(test (on-boundary? (make-posn 1 1) 13) false)
(test (on-boundary? (make-posn 10 10) 13) false)]

@chunk[<in-bounds?>

;; in-bounds? : posn number -> boolean
(define (in-bounds? p board-size)
  (and (<= 0 (posn-x p) (- board-size 1))
       (<= 0 (posn-y p) (- board-size 1))
       (not (equal? p (make-posn 0 0)))
       (not (equal? p (make-posn 0 (- board-size 1))))))]

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

@chunk[<extended-arithmetic-ops>
;; <=/f : (number or '∞) (number or '∞) -> boolean
(define (<=/f a b)
  (cond
    [(equal? b '∞) true]
    [(equal? a '∞) false]
    [else (<= a b)]))

(define (+/f x y)
  (cond
    [(or (equal? x '∞) (equal? y '∞))
     '∞]
    [else
     (+ x y)]))]

@chunk[<extended-arithmetic-ops-tests>
(test (<=/f 1 2) true)
(test (<=/f 2 1) false)
(test (<=/f '∞ 1) false)
(test (<=/f 1 '∞) true)
(test (<=/f '∞ '∞) true)

(test (+/f '∞ '∞) '∞)
(test (+/f '∞ 1) '∞)
(test (+/f 1 '∞) '∞)
(test (+/f 1 2) 3)]

@section{Tests}

@chunk[<test-infrastructure>

(define-syntax (test stx)
  (syntax-case stx ()
    [(_ actual expected)
     (with-syntax ([line (syntax-line stx)])
       #'(test/proc (λ () actual)
                    (λ () expected)
                    equal?
                    line))]))

(define-syntax (test/set stx)
  (syntax-case stx ()
    [(_ actual expected)
     (with-syntax ([line (syntax-line stx)])
       #'(test/proc (λ () actual)
                    (λ () expected)
                    (λ (x y) (same-sets? x y))
                    line))]))

(define test-count 0)
(define test-procs '())

(define (test/proc actual-thunk expected-thunk cmp line)
  (set! test-procs
        (cons
         (λ ()
           (set! test-count (+ test-count 1))
           (let ([actual (actual-thunk)]
                 [expected (expected-thunk)])
             (unless (cmp actual expected)
               (error 'check-expect "test ~a on line ~a failed:\n  ~s\n  ~s\n"
                      test-count
                      line
                      actual
                      expected))))
         test-procs)))


(define (same-sets? l1 l2)
  (and (andmap (lambda (e1) (member e1 l2)) l1)
       (andmap (lambda (e2) (member e2 l1)) l2)
       #t))

(test (same-sets? (list) (list)) true)
(test (same-sets? (list) (list 1)) false)
(test (same-sets? (list 1) (list)) false)
(test (same-sets? (list 1 2) (list 2 1)) true)

(define (run-tests)
  (for-each (λ (t) (t)) (reverse test-procs))
  (printf "passed ~s tests\n" test-count)
  (flush-output))]

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

@section{Everything Else}


@chunk[<everything-else>

#;'()


;; constants
(define circle-radius 20)
(define circle-spacing 22)

(define normal-color 'lightskyblue)
(define on-shortest-path-color 'white)
(define blocked-color 'black)
(define under-mouse-color 'black)


;                                                                          
;                                                                          
;                                                                          
;                                                                          
;          ;;                                     ;;;;                     
;       ;;;;                                      ;;;;;                    
;        ;;;                              ;                                
;    ;;; ;;;    ; ;;;;   ;;;;   ;;;;;;  ;;  ;;;;     ;;;;;;  ;;   ;;;; ;;; 
;   ;;;;;;;;;;;;;; ;;;; ;;;;;;;;;  ;;   ;;   ;   ;;;;;  ;;; ;;;;  ;;;;;;;  
;  ;;;;;;;;;  ;;; ;;;;;;;;;;;;;;; ;;;   ;;   ;;    ;;;  ;;; ;;;;  ;   ;;;; 
;  ;;;   ;;;  ;;; ;;;; ;;;    ;;; ;;;   ;;;  ;;;   ;;;  ;;;; ;;;  ;;   ;;; 
;  ;;   ;;;;  ;;;      ;;    ;;;; ;;;   ;;;; ;;;   ;;;  ;;;  ;;;   ;;;;;;; 
;  ;;;;;;;;   ;;;      ;;;;;;;;;; ;;;   ;; ;;;;;  ;;;;  ;;;  ;;;       ;;; 
;   ;;;;;;;;;;;;;;;;   ;;;;;;;;;;; ;;; ;;;  ;;;  ;;;;;;;;;;  ;;;  ;;;; ;;; 
;                       ;;;;;        ;;;;                   ;;;; ;;;;; ;;; 
;                                                             ;;;;;;; ;;;  
;                                                                 ;;;;;;   
;                                                                          


;; render-world : world -> image
(define (render-world w)
  (chop-whiskers
   (overlay (board->image (world-board w)
                          (world-size w)
                          (on-cats-path? w)
                          (world-mouse-posn w))
            (move-pinhole
             (cond
               [(equal? (world-state w) 'cat-won) happy-cat]
               [(equal? (world-state w) 'cat-lost) sad-cat]
               [else thinking-cat])
             (- (cell-center-x (world-cat w)))
             (- (cell-center-y (world-cat w)))))))

(test
 (render-world
  (make-world (list (make-cell (make-posn 0 1) false))
              (make-posn 0 1)
              'playing
              2
              (make-posn 0 0)
              false))
 (overlay
  (board->image (list (make-cell (make-posn 0 1) false))
                2
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
              2
              false
              false))
 (overlay
  (board->image (list (make-cell (make-posn 0 1) false))
                2
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
              2
              false
              false))
 (overlay
  (board->image (list (make-cell (make-posn 0 1) false))
                2
                (lambda (x) true)
                false)
  (move-pinhole sad-cat
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
  (board->image (list
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
  (move-pinhole sad-cat
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
  (board->image (list
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
  (move-pinhole sad-cat
                (- (cell-center-x (make-posn 1 1)))
                (- (cell-center-y (make-posn 1 1))))))

;; chop-whiskers : image -> image
;; crops the image so that anything above or to the left of the pinhole is gone
(define (chop-whiskers img)
  (shrink img
          0
          0
          (- (image-width img) (pinhole-x img) 1)
          (- (image-height img) (pinhole-y img) 1)))

(test (chop-whiskers (rectangle 5 5 'solid 'black))
              (put-pinhole (rectangle 3 3 'solid 'black) 0 0))
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
 0)


;; board->image : board number (posn -> boolean) posn-or-false -> image
(define (board->image cs world-size on-cat-path? mouse)
  (foldl (lambda (x y) (overlay y x))
         (nw:rectangle (world-width world-size)
                       (world-height world-size)
                       'solid
                       'white)
         (map (lambda (c)
                (cell->image c
                             (on-cat-path? (cell-p c))
                             (and (posn? mouse)
                                  (equal? mouse (cell-p c)))
                             #;
                             (and (posn? mouse)
                                  (point-in-this-circle? (cell-p c)
                                                         (posn-x mouse)
                                                         (posn-y mouse)))))
              cs)))

(test (board->image (list (make-cell (make-posn 0 0) false))
                            3
                            (lambda (x) false)
                            false)
              (overlay
               (nw:rectangle (world-width 3)
                             (world-height 3)
                             'solid
                             'white)
               (cell->image (make-cell (make-posn 0 0) false)
                            false
                            false)))

(test (board->image (list (make-cell (make-posn 0 0) false))
                            3
                            (lambda (x) true)
                            false)
              (overlay
               (nw:rectangle (world-width 3)
                             (world-height 3)
                             'solid
                             'white)
               (cell->image (make-cell (make-posn 0 0) false)
                            true
                            false)))


(test (board->image (list (make-cell (make-posn 0 0) false))
                            3
                            (lambda (x) false)
                            false)
              (overlay
               (nw:rectangle (world-width 3)
                             (world-height 3)
                             'solid
                             'white)
               (cell->image (make-cell (make-posn 0 0) false)
                            false
                            false)))

(test (board->image (list (make-cell (make-posn 0 0) false)
                                  (make-cell (make-posn 0 1) false))
                            3
                            (lambda (x) (equal? x (make-posn 0 1)))
                            false)
              (overlay
               (nw:rectangle (world-width 3)
                             (world-height 3)
                             'solid
                             'white)
               (cell->image (make-cell (make-posn 0 0) false)
                            false
                            false)
               (cell->image (make-cell (make-posn 0 1) false)
                            true
                            false)))

(test (board->image (list (make-cell (make-posn 0 0) false)
                                  (make-cell (make-posn 0 1) false))
                            3
                            (lambda (x) (equal? x (make-posn 0 1)))
                            (make-posn 0 0))
              (overlay
               (nw:rectangle (world-width 3)
                             (world-height 3)
                             'solid
                             'white)
               (cell->image (make-cell (make-posn 0 0) false)
                            false
                            true)
               (cell->image (make-cell (make-posn 0 1) false)
                            true
                            false)))

;; cell->image : cell boolean boolean -> image
(define (cell->image c on-short-path? under-mouse?)
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
     (- y))))

(test (cell->image (make-cell (make-posn 0 0) false) false false)
              (move-pinhole (circle circle-radius 'solid normal-color)
                            (- circle-radius)
                            (- circle-radius)))
(test (cell->image (make-cell (make-posn 0 0) true) false false)
              (move-pinhole (circle circle-radius 'solid 'black)
                            (- circle-radius)
                            (- circle-radius)))
(test (cell->image (make-cell (make-posn 0 0) false) true false)
              (move-pinhole (overlay (circle circle-radius 'solid normal-color)
                                     (circle (quotient circle-radius 2) 'solid
                                             on-shortest-path-color))
                            (- circle-radius)
                            (- circle-radius)))
(test (cell->image (make-cell (make-posn 0 0) false) true true)
              (move-pinhole (overlay (circle circle-radius 'solid normal-color)
                                     (circle (quotient circle-radius 2) 'solid
                                             under-mouse-color))
                            (- circle-radius)
                            (- circle-radius)))

;; world-width : number -> number
;; computes the width of the drawn world in terms of its size
(define (world-width board-size)
  (local [(define rightmost-posn
            (make-posn (- board-size 1) (- board-size 2)))]
    (+ (cell-center-x rightmost-posn) circle-radius)))

(test (world-width 3) 150)

;; world-height : number -> number
;; computes the height of the drawn world in terms of its size
(define (world-height board-size)
  (local [(define bottommost-posn
            (make-posn (- board-size 1) (- board-size 1)))]
    (+ (cell-center-y bottommost-posn) circle-radius)))
(test (world-height 3) 116.208)


;; cell-center-x : posn -> number
(define (cell-center-x p)
  (local [(define x (posn-x p))
          (define y (posn-y p))]
    (+ circle-radius
       (* x circle-spacing 2)
       (if (odd? y)
           circle-spacing
           0))))

(test (cell-center-x (make-posn 0 0))
              circle-radius)
(test (cell-center-x (make-posn 0 1))
              (+ circle-spacing circle-radius))
(test (cell-center-x (make-posn 1 0))
              (+ (* 2 circle-spacing) circle-radius))
(test (cell-center-x (make-posn 1 1))
              (+ (* 3 circle-spacing) circle-radius))

;; cell-center-y : posn -> number
(define (cell-center-y p)
  (local [(define y (posn-y p))]
    (+ circle-radius
       (* y circle-spacing 2
          .866 ;; .866 is an exact approximate to sin(pi/3)
          ))))

(test (cell-center-y (make-posn 1 1))
              (+ circle-radius (* 2 circle-spacing .866)))
(test (cell-center-y (make-posn 1 0))
              circle-radius)


;                                          
;                                          
;                                          
;                                          
;                                          
;          ;;;;;  ;;;;         ;;;;;;      
;            ;;;  ;;;;;          ;;;;      
;            ;;;                 ;;;       
;    ;;;;;;  ;;;     ;   ;;;;;;  ;;;  ;;;; 
;   ;;; ;;;; ;;; ;;;;;  ;;; ;;;; ;;; ;;;;;;
;  ;;; ;;;;; ;;;   ;;; ;;; ;;;;; ;;; ;;;;; 
;  ;;;  ;;;; ;;;   ;;; ;;;  ;;;; ;;;;      
;  ;;;       ;;    ;;; ;;;        ;;;;;;   
;  ;;;    ; ;;;   ;;;; ;;;    ;   ;;    ;; 
;   ;;;  ;  ;;;; ;;;;;;;;;;  ;  ;;;;;  ;;;;
;    ;;;;                ;;;;              
;                                          
;                                          
;                                          


(define (clack world x y evt)
  (cond
    [(equal? evt 'button-up)
     (cond
       [(and (equal? 'playing (world-state world))
             (point-in-a-circle? (world-board world) x y))
        (move-cat
         (update-world-posn
          (make-world (add-obstacle (world-board world) x y)
                      (world-cat world)
                      (world-state world)
                      (world-size world)
                      (world-mouse-posn world)
                      (world-h-down? world))
          (make-posn x y)))]
       [else (update-world-posn world (make-posn x y))])]
    [(equal? evt 'button-down)
     world]
    [(equal? evt 'drag) world]
    [(equal? evt 'move)
     (update-world-posn world (make-posn x y))]
    [(equal? evt 'enter)
     (update-world-posn world (make-posn x y))]
    [(equal? evt 'leave)
     (update-world-posn world false)]))

(test (clack (make-world '() (make-posn 0 0) 'playing 1 false false)
                     1 1 'button-down)
              (make-world '() (make-posn 0 0) 'playing 1 false false))
(test (clack (make-world '() (make-posn 0 0) 'playing 1 false false)
                     1 1 'drag)
              (make-world '() (make-posn 0 0) 'playing 1 false false))
(test (clack (make-world (list (make-cell (make-posn 0 0) false))
                                 (make-posn 0 1)
                                 'playing
                                 1
                                 false
                                 false)
                     (cell-center-x (make-posn 0 0))
                     (cell-center-y (make-posn 0 0))
                     'move)
              (make-world (list (make-cell (make-posn 0 0) false))
                          (make-posn 0 1)
                          'playing
                          1
                          (make-posn 0 0)
                          false))
(test (clack (make-world (list (make-cell (make-posn 0 0) false))
                                 (make-posn 0 1)
                                 'playing
                                 1
                                 false
                                 false)
                     (cell-center-x (make-posn 0 0))
                     (cell-center-y (make-posn 0 0))
                     'enter)
              (make-world (list (make-cell (make-posn 0 0) false))
                          (make-posn 0 1)
                          'playing
                          1
                          (make-posn 0 0)
                          false))
(test (clack (make-world '() (make-posn 0 0)
                                 'playing 1 (make-posn 0 0) false)
                     1 1 'leave)
              (make-world '() (make-posn 0 0) 'playing 1 false false))

(test (clack (make-world '() (make-posn 0 0)
                                 'playing 1 (make-posn 0 0) false)
                     10
                     10
                     'button-down)
              (make-world '() (make-posn 0 0) 'playing 1 (make-posn 0 0) false))

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
                                 'cat-lost 1 (make-posn 0 0) false)
                     10
                     10
                     'button-up)
              (make-world '() (make-posn 0 0)
                          'cat-lost 1 (make-posn 0 0) false))
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
               (make-posn 1 0)
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
               (make-posn 1 0)
               false))

;; update-world-posn/playing : world posn-or-false -> world
(define (update-world-posn w p)
  (cond
    [(equal? (world-state w) 'playing)
     (cond
       [(posn? p)
        (local [(define mouse-spot
                  (circle-at-point (world-board w)
                                   (posn-x p)
                                   (posn-y p)))]
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
    [else w]))

(test (update-world-posn
               (make-world (list (make-cell (make-posn 0 0) false))
                           (make-posn 0 1) 'playing 1 false false)
               (make-posn (cell-center-x (make-posn 0 0))
                          (cell-center-y (make-posn 0 0))))
               (make-world (list (make-cell (make-posn 0 0) false))
                           (make-posn 0 1) 'playing 1 (make-posn 0 0) false))

(test (update-world-posn
               (make-world (list (make-cell (make-posn 0 0) false))
                           (make-posn 0 0) 'playing 1 false false)
               (make-posn (cell-center-x (make-posn 0 0))
                          (cell-center-y (make-posn 0 0))))
               (make-world (list (make-cell (make-posn 0 0) false))
                           (make-posn 0 0) 'playing 1 false false))

(test (update-world-posn
               (make-world (list (make-cell (make-posn 0 0) false))
                           (make-posn 0 1) 'playing 1 (make-posn 0 0) false)
               (make-posn 0 0))
               (make-world (list (make-cell (make-posn 0 0) false))
                           (make-posn 0 1) 'playing 1 false false))
(test (update-world-posn
               (make-world (list (make-cell (make-posn 0 0) false))
                           (make-posn 0 1) 'cat-won 1 false false)
               (make-posn (cell-center-x (make-posn 0 0))
                          (cell-center-y (make-posn 0 0))))
               (make-world (list (make-cell (make-posn 0 0) false))
                           (make-posn 0 1) 'cat-won 1 false false))
(test (update-world-posn
               (make-world (list (make-cell (make-posn 0 0) false))
                           (make-posn 0 1) 'cat-lost 1 false false)
               (make-posn (cell-center-x (make-posn 0 0))
                          (cell-center-y (make-posn 0 0))))
               (make-world (list (make-cell (make-posn 0 0) false))
                           (make-posn 0 1) 'cat-lost 1 false false))

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
                (world-h-down? world))))


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
             false))

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
                (map list scores posns)))])))
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
              false)

;; add-obstacle : board number number -> board
(define (add-obstacle board x y)
  (cond
    [(empty? board) board]
    [else
     (local [(define cell (first board))
             (define cx (cell-center-x (cell-p cell)))
             (define cy (cell-center-y (cell-p cell)))]
       (cond
         [(and (<= (- cx circle-radius) x (+ cx circle-radius))
               (<= (- cy circle-radius) y (+ cy circle-radius)))
          (cons (make-cell (cell-p cell) true)
                (rest board))]
         [else
          (cons cell (add-obstacle (rest board) x y))]))]))

(test (add-obstacle (list (make-cell (make-posn 0 0) false))
                            circle-spacing circle-spacing)
              (list (make-cell (make-posn 0 0) true)))
(test (add-obstacle (list (make-cell (make-posn 0 0) false)) 100 100)
              (list (make-cell (make-posn 0 0) false)))
(test (add-obstacle (list (make-cell (make-posn 0 0) false)
                                  (make-cell (make-posn 0 1) false))
                            circle-spacing circle-spacing)
              (list (make-cell (make-posn 0 0) true)
                    (make-cell (make-posn 0 1) false)))

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
        (circle-at-point (rest board) x y)])]))
(test (circle-at-point empty 0 0) false)
(test (circle-at-point (list (make-cell (make-posn 0 0) false))
                               (cell-center-x (make-posn 0 0))
                               (cell-center-y (make-posn 0 0)))
              (make-posn 0 0))
(test (circle-at-point (list (make-cell (make-posn 0 0) false))
                               0 0)
              false)


;; point-in-a-circle? : board number number -> boolean
(define (point-in-a-circle? board x y)
  (posn? (circle-at-point board x y)))
(test (point-in-a-circle? empty 0 0) false)
(test (point-in-a-circle? (list (make-cell (make-posn 0 0) false))
                                  (cell-center-x (make-posn 0 0))
                                  (cell-center-y (make-posn 0 0)))
              true)
(test (point-in-a-circle? (list (make-cell (make-posn 0 0) false))
                                  0 0)
              false)

;; point-in-this-circle? : posn number number -> boolean
(define (point-in-this-circle? p x y)
  (local [(define center (+ (cell-center-x p)
                            (* (sqrt -1) (cell-center-y p))))
          (define p2 (+ x (* (sqrt -1) y)))]
    (<= (magnitude (- center p2)) circle-radius)))

(test (point-in-this-circle? (make-posn 0 0)
                                     (cell-center-x (make-posn 0 0))
                                     (cell-center-y (make-posn 0 0)))
              true)
(test (point-in-this-circle? (make-posn 0 0) 0 0)
              false)

;; change : world key-event -> world
(define (change w ke)
  (make-world (world-board w)
              (world-cat w)
              (world-state w)
              (world-size w)
              (world-mouse-posn w)
              (key=? ke #\h)))

(test (change (make-world '() (make-posn 1 1)
                                  'playing 1 (make-posn 0 0) false)
                      #\h)
              (make-world '() (make-posn 1 1)
                          'playing 1 (make-posn 0 0) true))
(test (change (make-world '() (make-posn 1 1)
                                  'playing 1 (make-posn 0 0) true)
                      'release)
              (make-world '() (make-posn 1 1) 'playing 1 (make-posn 0 0) false))




;                                
;                                
;                                
;                                
;                                
;                        ;;;;    
;                        ;;;     
;                        ;;;   ; 
;    ;;;;;;   ;;;;   ;;;;;;;;;;; 
;   ;;; ;;;; ;;;;;;;;;   ;;;   ;;
;  ;;; ;;;;;;;;;;;;;;;  ;;;      
;  ;;;  ;;;;;;;    ;;;  ;;; ;;;; 
;  ;;;      ;;    ;;;;  ;;; ;;;;;
;  ;;;    ; ;;;;;;;;;;  ;;;  ;;;;
;   ;;;  ;  ;;;;;;;;;;; ;;;   ;; 
;    ;;;;    ;;;;;       ;;;;;   
;                                
;                                
;                                


;; cat : symbol -> image
(define (cat mode)
  (local [(define face-color
            (cond
              [(symbol=? mode 'sad) 'pink]
              [else 'lightgray]))

          (define left-ear (regular-polygon 3 8 'solid 'black (/ pi -3)))
          (define right-ear (regular-polygon 3 8 'solid 'black 0))
          (define ear-x-offset 14)
          (define ear-y-offset 9)

          (define eye (overlay (ellipse 12 8 'solid 'black)
                               (ellipse 6 4 'solid 'limegreen)))
          (define eye-x-offset 8)
          (define eye-y-offset 3)

          (define nose (regular-polygon 3 5 'solid 'black (/ pi 2)))

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
          (define mouth-y-offset -5)]

    (add-line
     (add-line
      (add-line
       (add-line
        (add-line
         (add-line
          (overlay (move-pinhole left-ear (- ear-x-offset) ear-y-offset)
                   (move-pinhole right-ear (- ear-x-offset 1) ear-y-offset)
                   (ellipse 40 26 'solid 'black)
                   (ellipse 36 22 'solid face-color)
                   (move-pinhole mouth (- mouth-x-offset) mouth-y-offset)
                   (move-pinhole mouth mouth-x-offset mouth-y-offset)
                   (move-pinhole eye (- eye-x-offset) eye-y-offset)
                   (move-pinhole eye eye-x-offset eye-y-offset)
                   (move-pinhole nose -1 -4))
          6 4 30 12 'black)
         6 4 30 4 'black)
        6 4 30 -4 'black)
       -6 4 -30 12 'black)
      -6 4 -30 4 'black)
     -6 4 -30 -4 'black)))

(define happy-cat (cat 'happy))
(define sad-cat (cat 'sad))
(define thinking-cat (cat 'thinking))


;                                                        
;                                                        
;                                                        
;                                                        
;                                                        
;   ;;;;            ;;;;   ;;;;     ;;;;           ;;;;; 
;   ;;;;;           ;;;;;  ;;;      ;;;;;            ;;; 
;                          ;;;   ;                   ;;; 
;      ;;;;;;  ;;      ; ;;;;;;;;;     ;   ;;;;   ;; ;;; 
;  ;;;;;  ;;; ;;;; ;;;;;   ;;;   ;;;;;;;  ;;;;;;;;;  ;;; 
;    ;;;  ;;; ;;;;   ;;;  ;;;        ;;; ;;;;;;;;;;  ;;; 
;    ;;;  ;;;; ;;;   ;;;  ;;; ;;;;   ;;; ;;;    ;;;  ;;; 
;    ;;;  ;;;  ;;;   ;;;  ;;; ;;;;;  ;;; ;;    ;;;;  ;;  
;   ;;;;  ;;;  ;;;  ;;;;  ;;;  ;;;; ;;;; ;;;;;;;;;; ;;;  
;  ;;;;;;;;;;  ;;; ;;;;;;;;;;   ;; ;;;;;;;;;;;;;;;;;;;;; 
;             ;;;;         ;;;;;          ;;;;;          
;               ;;;                                      
;                                                        
;                                                        
;                                                    
;                                                    
;                                                    
;                                                    
;                                                    
;  ;;;;;                                           ;;
;   ;;;;                                        ;;;; 
;   ;;;                                          ;;; 
;   ;;; ;;;    ;;;;;    ;;;;   ;;   ; ;;;;   ;;; ;;; 
;   ;;;;;;;;  ;;;;;;;  ;;;;;;;;;;;;;;; ;;;; ;;;;;;;; 
;   ;;;;;;;; ;;;;;;;; ;;;;;;;;;;  ;;; ;;;;;;;;;;;;;; 
;   ;;;   ;; ;;     ; ;;;    ;;;  ;;; ;;;; ;;;   ;;; 
;   ;;;   ;; ;     ;; ;;    ;;;;  ;;;      ;;   ;;;; 
;   ;;;;;;;  ;;;;;;;; ;;;;;;;;;;  ;;;      ;;;;;;;;  
;  ;;;;;;;    ;;;;;;  ;;;;;;;;;;;;;;;;;;    ;;;;;;;;;
;              ;;;;    ;;;;;                         
;                                                    
;                                                    
;                                                    

;; append-all : (listof (list X)) -> (listof X)
(define (append-all ls)
  (foldr append empty ls))

(test (append-all empty) empty)
(test (append-all (list (list 1 2 3))) (list 1 2 3))
(test (append-all (list (list 1) (list 2) (list 3)))
              (list 1 2 3))

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
       board))
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
              (list (make-cell (make-posn 0 0) true)))

(define dummy
  (local
    [(define board-size 11)
     (define initial-board
       (add-n-random-blocked-cells
        6
        (empty-board board-size)
        board-size))
     (define initial-world
       (make-world initial-board
                   (make-posn (quotient board-size 2)
                              (quotient board-size 2))
                   'playing
                   board-size
                   false
                   false))]

    (and
     (big-bang (world-width board-size)
               (world-height board-size)
               1
               initial-world)
     (on-redraw render-world)
     (on-key-event change)
     (on-mouse-event clack))))

(run-tests)
]
