;; Plays automatic games, often useful when learning is enabled in "explore.rkt"

(module robot mzscheme
  (require mzlib/unitsig
           mzlib/etc
           mzlib/list
           "sig.rkt"
           "model.rkt"
           "explore.rkt"
           "heuristics.rkt")
  
  (define board-size 3)
  (define steps 2)
  (define depth 3)
  
  (define timeout 3.0)
  (define cannon-size +inf.0)
  
  (invoke-unit/sig
   (compound-unit/sig
     (import)
     (link
      [CONFIG : config^ ((unit/sig config^
                           (import)
                           (define BOARD-SIZE board-size)))]
      [MODEL : model^ (model-unit CONFIG)]
      [HEURISTICS : heuristics^ (heuristics-unit CONFIG MODEL EXPLORE)]
      [EXPLORE : explore^ (explore-unit CONFIG MODEL)]
      [ROBOT : () ((unit/sig ()
                     (import config^ explore^ model^ heuristics^)
                     
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
                         ;; (random-seed 12)
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
                   CONFIG EXPLORE MODEL HEURISTICS)])
     (export))))
