#lang scribble/doc
@(require "common.ss"
          racket/class
          racket/draw
          "../same/same-lib.rkt")

@gametitle["Same" "same" "Dot-Removing Game"]

The object of @game{Same} is to score points by removing blobs from the
board.  To remove a blob, click on it.  As long the blob is not just
a simple circle, it will disappear.  After the blob disappears,
the remaining pieces of the board shift around, breaking up blobs into
new blobs as pieces of the old blobs fall down to fill in the empty space.
If an entire column is wiped out, all of the blobs from the
right will slide left to take up the empty column's space.

As an example, imagine a fragment of the board looked like this:

@(let ()
   (define w 100)
   (define h 100)
   (define bm (make-bitmap w h))
   (define bdc (make-object bitmap-dc% bm))
   (define board-width 6)
   (define board-height 4)
   (define board 
     (build-vector
      board-width
      (lambda (i)
        (build-vector
         board-height
         (lambda (j)
           (vector
            (modulo (+ i j) 3)
            #f))))))
   (draw-board bdc board-width board-height board w h
               #f #f #f #f)
   (send bdc set-bitmap #f)
   bm)

Your score increases for each ball removed from the board, in two ways.
First, when you remove a blob, you get as many points as the square of the number
of cells the blob occupied, so removing bigger blobs is better. Second, if there
are fewer than 20 cells occupied on the board, you get a bonus.
Specifically if you have 19 cells left, you
get a 50 point bonus, 18 cells left yields a 100 point bonus, 
17 cells a 150 point bonus etc., and if there
are no cells left, you get a 1000 point bonus.

Click the @onscreen{New Game} button to play again.
