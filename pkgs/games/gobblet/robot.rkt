#lang racket
;; Plays automatic games, often useful when learning is enabled in "explore.rkt"

(require "sig.rkt"
         (only-in "model.rkt" model-unit@)
         (only-in "explore.rkt" explore-unit@)
         (only-in "heuristics.rkt" heuristics-unit@))

(define board-size 3)
(define steps 2)
(define depth 3)

(define timeout 3.0)
(define cannon-size +inf.0)

(define-unit board-config@
  (import)
  (export config^)
  (define BOARD-SIZE board-size))

(define-unit robot-unit@
  (import config^ explore^ model^ heuristics^)
  (export)
  (define init-board 
    empty-board
    #;
    (move empty-board 
          (list-ref red-pieces 2) #f #f 0 0
          (lambda (b)
            (move b
                  (list-ref yellow-pieces 2) #f #f 0 1
                  (lambda (b) b)
                  void))
          void))
  (define init-who 'red)
  ; Only play 50 games to control run time
  (define how-many 50)
  
  ;; Play-a-game test
  (let go ()
    (unless (zero? how-many)
      (set! how-many (sub1 how-many))
      ;(sleep 1)
      (define s (bitwise-and (+ (current-milliseconds) (random 100))
                             (sub1 (expt 2 31))))
      (printf "Random seed: ~s\n" s)
      (random-seed s)
      (let loop ([board init-board]
                 [who init-who]
                 [who-moved "no one"]
                 [history null])
        (cond
          [(winner? board who)
           (printf "----------- ~a wins!-------------\n~a\n" who (board->string 1 board))
           (go)]
          [(winner? board (other who))
           (printf "----------- ~a wins!-------------\n~a\n" (other who) (board->string 1 board))
           (go)]
          [(member board history)
           (printf "----------- tie! -------------\n~a\n" (board->string 1 board))
           (go)]
          [else
           (printf "\n~a moved; ~a's turn\n~a\n" who-moved who (board->string 1 board))
           (let ([start (current-inexact-milliseconds)]
                 [m ((make-search (if (= BOARD-SIZE 3)
                                      make-3x3-rate-board
                                      make-4x4-rate-board)
                                  (if (= BOARD-SIZE 3)
                                      make-3x3-no-canned-moves
                                      make-4x4-canned-moves)) 
                     timeout steps depth
                     who board history)])
             (printf "[~a secs]\n" (/ (- (current-inexact-milliseconds) start)
                                      1000.0))
             (loop (apply-play board m) (other who) who (cons board history)))])))))

(invoke-unit
 (compound-unit/infer
  (import)
  (export)
  (link
   [((CONFIG : config^)) board-config@]
   [((MODEL : model^)) model-unit@]
   [((HEURISTICS : heuristics^)) heuristics-unit@]
   [((EXPLORE : explore^)) explore-unit@]
   [() robot-unit@])))
