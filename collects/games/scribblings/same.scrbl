#lang scribble/doc
@(require "common.rkt" racket/class racket/draw (only-in pict dc)
          "../same/same-lib.rkt")

@(define board-width 6)
@(define board-height 6)
@(define scale-factor 24)
@(define (render-board board)
   (define w (ceiling (* board-width scale-factor)))
   (define h (ceiling (* board-height scale-factor)))
   (dc
    (λ (dc dx dy)
      (define-values (ox oy) (send dc get-origin))
      (send dc set-origin (+ ox dx) (+ oy dy))
      (draw-board dc board-width board-height board w h
                  #f #f #f #f)
      (send dc set-origin ox oy))
    w h))

@(define (copy-board board)
   (let loop ([board board])
     (cond
       [(vector? board)
        (list->vector (map loop (vector->list board)))]
       [else board])))



@gametitle["Same" "same" "Dot-Removing Game"]

The object of @game{Same} is to score points by removing blobs from the
board. 

@section{The Rules}

To remove a blob, click on it.  As long the blob is not just
a simple circle, it will disappear.  After the blob disappears,
the remaining pieces of the board shift around, breaking up blobs into
new blobs as pieces of the old blobs fall down to fill in the empty space.
If an entire column is wiped out, all of the blobs from the
right will slide left to take up the empty column's space.

As an example, imagine a board looked like this:
@(define board1 
   (build-vector
    board-width
    (lambda (i)
      (build-vector
       board-height
       (lambda (j)
         (vector
          (cond
            [(and (= i 4) (member j '(3 4 5)))
             3]
            [(and (= i 4) (member j '(1 2)))
             4]
            [(and (member i '(3 5)) (= j 5))
             2]
            [else
             (modulo (+ i j) 2)])
          #f))))))

@(render-board board1)

There are two places where we can click, on the green blob or on the purple one.
Clicking on the green one results in this board:

@(define board2 
   (let ([b (copy-board board1)])
     (make-a-move 4 3 b board-width board-height)
     b))

@(render-board board2)

Notice the new horizontal blue blob that has appeared. That appears because
the blue ball falls down into the vacated space and joins into the two adjacent
blue balls.

Next, if we ignore that new blue blob and click the purple one, we get this board:

@(define board3
   (let ([b (copy-board board2)])
     (make-a-move 4 5 b board-width board-height)
     b))

@(render-board board3)

The blue circle has continued falling, which breaks up our blue blob and no new
one appears because the blue circle is now next to brown circles.

If, however, we had clicked on blue blob before clicking on the purple one, we would get this board:

@(define board4
   (let ([b (copy-board board2)])
     (make-a-move 4 3 b board-width board-height)
     b))

@(render-board board4)

and then clicking the purple one would produce this board:

@(define board5
   (let ([b (copy-board board4)])
     (make-a-move 4 5 b board-width board-height)
     b))
@(render-board board5)

Note that the purple blob was the only blob in its column, so clicking on it shifts
all of the columns to the right over to eliminate the empty space.

@section{Scoring}
Your score increases for each ball removed from the board, in two ways.
First, when you remove a blob, you get as many points as the square of the number
of cells the blob occupied, so removing bigger blobs is better. Second, if there
are fewer than 50 cells occupied on the board, you get a bonus.
Specifically if you have 49 cells left, you
get a 100 point bonus, 48 cells left yields a 200 point bonus, 
47 cells a 200 point bonus etc., and if there
are no cells left, you get a 5000 point bonus.

Click the @onscreen{New Game} button to play again.
