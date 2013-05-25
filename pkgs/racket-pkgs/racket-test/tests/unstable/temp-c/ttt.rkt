#lang racket/base
(require racket/contract
         racket/match
         tests/eli-tester)

; A space is #f, 'X, or 'O
(define space/c 
  (or/c false/c 'X 'O))

; A board is a (hasheq (hasheq space space space) x 3 )
(define posn/c
  (or/c 0 1 2))
(define board/c
  (hash/c posn/c
          (hash/c posn/c
                  space/c
                  #:immutable #t)
          #:immutable #t))

(define empty-board
  (hasheq 0 (hasheq 0 #f 1 #f 2 #f)
          1 (hasheq 0 #f 1 #f 2 #f)
          2 (hasheq 0 #f 1 #f 2 #f)))

(define winning-o-board/col
  (hasheq 0 (hasheq 0 'O 1 #f 2 #f)
          1 (hasheq 0 'O 1 #f 2 #f)
          2 (hasheq 0 'O 1 #f 2 #f)))
(define winning-x-board/row
  (hasheq 0 (hasheq 0 'O 1 #f 2 #f)
          1 (hasheq 0 'X 1 'X 2 'X)
          2 (hasheq 0 'O 1 #f 2 #f)))
(define winning-x-board/left
  (hasheq 0 (hasheq 0 'X 1 #f 2 #f)
          1 (hasheq 0 'O 1 'X 2 'X)
          2 (hasheq 0 'O 1 #f 2 'X)))
(define winning-o-board/right
  (hasheq 0 (hasheq 0 'X 1 #f 2 'O)
          1 (hasheq 0 'O 1 'O 2 'X)
          2 (hasheq 0 'O 1 #f 2 'X)))

(define (board-ref b r c)
  (hash-ref (hash-ref b r) c))

(test
 (board-ref empty-board 0 0) => #f
 (board-ref winning-o-board/right 1 2) => 'X)

(define equal?*
  (match-lambda*
    [(list) #t]
    [(list e) e]
    [(list* e1 e2 es)
     (and (equal? e1 e2)
          (apply equal?* e2 es))]))

(test
 (equal?*)
 (equal?* 1)
 (equal?* 1 1)
 (equal?* 1 1 1)
 (equal?* 1 1 1 2) => #f)

(define (winning-board? b)
  (or
   ; Cols
   (for/or ([c (in-range 3)])
     (equal?*
      (board-ref b 0 c)
      (board-ref b 1 c)
      (board-ref b 2 c)))
   ; Rows
   (for/or ([r (in-range 3)])
     (equal?*
      (board-ref b r 0)
      (board-ref b r 1)
      (board-ref b r 2)))
   ; Left diagonal
   (equal?* (board-ref b 0 0)
            (board-ref b 1 1)
            (board-ref b 2 2))
   ; Right diagonal
   (equal?* (board-ref b 0 2)
            (board-ref b 1 1)
            (board-ref b 2 0))))

(test
 (winning-board? empty-board) => #f
 
 (winning-board? winning-o-board/col) => 'O
 (winning-board? winning-x-board/row) => 'X
 (winning-board? winning-x-board/left) => 'X
 (winning-board? winning-o-board/right) => 'O)

(define (board-set b r c m)
  #;(printf "b[~a][~a] = ~a\n" r c m)
  (hash-update b r (λ (r) (hash-set r c m))))

(test
 (board-set
  (board-set
   (board-set empty-board
              0 0 'O)
   1 0 'O)
  2 0 'O)
 =>
 winning-o-board/col)

(define (full-board? b)
  (for/and ([r (in-range 3)]
            [c (in-range 3)])
    (board-ref b r c)))

(test
 (full-board?
  (for/fold ([b empty-board])
    ([r (in-range 3)]
     [c (in-range 3)])
    (board-set b r c 'X))))

(define (tic-tac-toe o-player x-player)
  (let loop ([board empty-board]
             [os-turn? #t
                       #;(zero? (random 2))])
    (cond
      [(winning-board? board)
       => (λ (winner)
            (printf "~a wins!\n" winner))]
      [(full-board? board)
       (printf "Stalemate!\n")]
      [else
       (loop 
        ((if os-turn? 
             o-player
             x-player)
         board board-ref board-set)
        (not os-turn?))])))

(require unstable/match
         unstable/temp-c/dsl)
(provide
 (rename-out [tic-tac-toe
              tic-tac-toe:raw]))
(provide/contract
 [tic-tac-toe
  (with-monitor
      (label 'game
             (-> (label 'turn
                        (-> board/c 
                            (board/c posn/c posn/c . -> . space/c)
                            (label 'board-set
                                   (board/c posn/c posn/c
                                            (and space/c (not/c false/c))
                                            . -> . board/c))
                            board/c))
                 (label 'turn
                        (-> board/c 
                            (board/c posn/c posn/c . -> . space/c)
                            (label 'board-set
                                   (board/c posn/c posn/c
                                            (and space/c (not/c false/c))
                                            . -> . board/c))
                            board/c))
                 void))
    (complement
     (union
      ; A board set hits something that was hit before
      (seq (star _)
           (call 'game _ _)
           (star _)
           (dseq (call 'board-set _ r c _)
                 (seq (star (not (ret 'game _)))
                      (call 'board-set _ (== r) (== c) _))))
      ; A player takes two turns
      (seq (star _)
           (call 'turn _ _ _)
           (? monitor:proj?)
           (call 'board-set _ _ _ _)
           (ret 'board-set _)
           (call 'board-set _ _ _ _)))))])
