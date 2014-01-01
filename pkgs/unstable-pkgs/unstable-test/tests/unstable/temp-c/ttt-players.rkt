#lang racket/base
(require "ttt.rkt"
         tests/eli-tester)

(define (print-board b board-ref)
  (for ([r (in-range 3)])
    (for ([c (in-range 3)])
      (define m (board-ref b r c))
      (printf "|~a|" (or m " ")))
    (printf "\n---------\n")))

(define (read-number m l)
  (printf "~a > ~a: " m l)
  (read))

(define (interactive-player mark)
  (λ (b board-ref board-set)
    (print-board b board-ref)
    (let loop ()
      (define row (read-number mark "Row"))
      (define col (read-number mark "Column"))
      (if (board-ref b row col)
          (begin (printf "Don't be a cheater :(\n")
                 (loop))
          (board-set b row col mark)))))

#;(tic-tac-toe (interactive-player 'O)
               (interactive-player 'X))

(define (random-player mark)
  (define (turn b board-ref board-set)
    (define r (random 3))
    (define c (random 3))
    (if (board-ref b r c)
        (turn b board-ref board-set)
        (board-set b r c mark)))
  turn)

(tic-tac-toe (random-player 'O) (random-player 'X))
(tic-tac-toe (random-player 'O) (random-player 'X))

(define (cheater-1 mark)
  (define (turn b board-ref board-set)
    (or
     (for*/or ([r (in-range 3)]
               [c (in-range 3)])
       (and (board-ref b r c)
            (board-set b r c mark)))
     (board-set b 0 0 mark)))
  turn)

(test
 (tic-tac-toe (cheater-1 'O) (random-player 'X))
 =error> "monitor disallowed"
 (tic-tac-toe (random-player 'O) (cheater-1 'X))
 =error> "monitor disallowed")

(define (cheater-2 mark)
  (define (turn b board-ref board-set)
    (board-set
     (board-set
      (board-set b 2 2 mark)
      0 0 mark)
     1 1 mark))
  turn)

(test
 (tic-tac-toe (cheater-2 'O) (random-player 'X))
 =error> "monitor disallowed"
 (tic-tac-toe (random-player 'O) (cheater-2 'X))
 =error> "monitor disallowed")


(module+ test
  (module config info
    (define random? #t)))
