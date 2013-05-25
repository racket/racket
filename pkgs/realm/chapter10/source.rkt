#lang racket

#| 
   The Dice of Doom game, the eager version  
   ----------------------------------------

   The Dice of Doom game is a turn-based game for two players sharing one keyboard. 
   Since this implementation employs an eager strategy to build the complete game 
   tree of all possible moves, it is only a step in the right direction. 

   Each player owns hexagonal territories, which are arranged into a planar game
   board. A territory comes with a number of dice. When it is a player's turn, 
   she marks one of her territories as a launching pad for an attack at a 
   neigboring territory of the other player. Such an attack is enabled only if 
   her chosen territory has more dice than the territory of the other player. 
   The effect of the attack is that the territory changes ownership and that all
   but one of the attack dice are moved to the newly conquered territory. A 
   player may continue her turn as long as she can launch attacks. Optionally, 
   she may choose to pass after her first attack is executed, meaning she ends 
   her turn. At the end of a turn, a number of dices are distributed across the 
   players' territories. The game is over when a player whose turn it is cannot
   attack on her first move. 

   A player can use the following five keys to play the game:
    -- with ← and → (arrow keys), the player changes the territory focus 
    -- with enter, the player marks a territory the launching pad for an attack
    -- with the "d" key, the player unmarks a territory 
    -- with the "p" key the player passes. 
   Once a player passes, the game announces whose turn it is next. 

   Play
   ----
 
   Run and evaluate 
     (roll-the-dice)
   This will pop up a window that the game board, and instructions. 
|#

(require 2htdp/image (except-in 2htdp/universe left right)) 

;                                                                                  
;                                                                                  
;                                                                                  
;                                                                                  
;   ;;;;       ;                            ;;; ;;;                   ;;        ;; 
;    ;  ;                                    ;   ;                     ;         ; 
;    ;   ;   ;;;     ;;; ;   ;;;;            ;   ;   ;;;;   ;; ;;;     ;     ;;; ; 
;    ;   ;     ;    ;   ;;  ;    ;           ; ; ;  ;    ;   ;;        ;    ;   ;; 
;    ;   ;     ;    ;       ;;;;;;           ; ; ;  ;    ;   ;         ;    ;    ; 
;    ;   ;     ;    ;       ;                ; ; ;  ;    ;   ;         ;    ;    ; 
;    ;  ;      ;    ;    ;  ;                ; ; ;  ;    ;   ;         ;    ;   ;; 
;   ;;;;     ;;;;;   ;;;;    ;;;;;            ; ;    ;;;;   ;;;;;    ;;;;;   ;;; ;;
;                                                                                  
;                                                                                  
;                                                                                  
;                                                                                  

;; ---------------------------------------------------------------------------------------------------
;; Data

(struct dice-world (src board gt) #:transparent)
;; DiceWorld = (dice-world (U #false Natural) Board GameTree)
;; in (dice-world i b gt)
;; -- if i is a Natural, it is an index for the territory that the player has marked for an attack
;; -- if i is #f, no territory has been marked yet 
;; b is the current board
;; gt is the game-tree for the given i and b 

(struct game (board player moves) #:transparent)
;; GameTree = (game Board Player [Listof Move]) 
;; in (game-tree b p lm)
;; -- b is the current board 
;; -- p is the current player 
;; -- lm is the list of moves that that player may execute 

;; Board = [List-of Territory]
;; the first field in the list is the currently marked  territory

;; Player ∈ [0, PLAYER#) | Natural 

(struct move (action gt) #:transparent)
;; Move = (move Action GameTree)
;; in (move a gt)
;; -- a represents the actione to be takem 
;; -- gt is the game-tree resulting from that action 

;; Action is one of:
;; -- '()                      a passing move
;; -- (list Natural Natural)   the move where the first attacks the second

(struct territory (index player dice x y) #:transparent)
;; Territory = (territory Natural Player Dice Integer Integer)
;; in (territory i p d x y)
;; -- i is a unique identifier for the territory; it also determines its initial location
;; -- p is the player who owns this territory
;; -- d is the number of dice on this board
;; -- x is the x coordiate of this territory in pixels
;; -- y is the y coordiate of this territory in pixels

;; Territory Natural -> Territory 
;; updates number of dice on territory 
(define (territory-set-dice t d)
  (territory (territory-index t) (territory-player t) d (territory-x t) (territory-y t)))

;; Territory Player -> Territory 
;; updates owner of territory 
(define (territory-set-player t p)
  (territory (territory-index t) p (territory-dice t) (territory-x t) (territory-y t)))

;; ---------------------------------------------------------------------------------------------------
;; sample game tree for BOOK 

(define b1
  (list (territory 1 0 1 'a 'b)
        (territory 0 0 1 'x 'y)))

(define b1-alternative 
  (list (territory 0 0 1 'x 'y)
        (territory 1 0 1 'a 'b)))

(define b3
  (list (territory 0 0 2 'x 'y)
        (territory 1 1 1 'a 'b)))

(define gt1 (game b1 1 '()))

(define mv2 (move '() gt1))

(define gt2 (game b1-alternative 0 (list mv2)))

(define mv3 (move '(0 1) gt2))

(define gt3 (game b3 0 (list mv3)))

;; ---------------------------------------------------------------------------------------------------
;; Constants

; initalization constants
(define PLAYER# 2)
(define DICE# 3)
(define BOARD 2)
(define GRID (* BOARD BOARD))
(define INIT-PLAYER 0)
(define INIT-SPARE-DICE 10)
; The depth at which to limit the gametree
(define AI-DEPTH 4)
(define AI 1)

; graphical constants: territories
(define DICE-OFFSET 6)
(define SIDE 75)
(define OFFSET0 (* 2 SIDE))
(define ROTATION 30)
(define HEX 6)
(define (hexagon color)
  (rotate ROTATION (regular-polygon SIDE HEX "solid" color)))
(define X-OFFSET (image-width (hexagon "black")))
(define Y-OFFSET (* (image-height (hexagon "black")) 3/4))

; graphical constants
(define COLORS 
  (list (make-color 255 0 0 100) 
        (make-color 0 255 0 100) 
        (make-color 0 0 255 100)))
(define FOCUS (rotate ROTATION (regular-polygon SIDE 6 "outline" "black")))
(define D1 (bitmap "graphics/dice1.png"))
(define D2 (bitmap "graphics/dice2.png"))
(define D3 (bitmap "graphics/dice3.png"))
(define D4 (bitmap "graphics/dice4.png"))
(define IMG-LIST (list D1 D2 D3 D4)) 

(define TEXT-SIZE 25)
(define TEXT-COLOR "black")
(define INSTRUCT 
  "← and → to move among territories, <enter> to mark, <d> to unmark, and <p> to pass")
(define AI-TURN "It's the Mighty AI's turn")
(define YOUR-TURN "It's your turn")
(define INFO-X-OFFSET 100)
(define INFO-Y-OFFSET 50)

(define INSTRUCTIONS (text INSTRUCT TEXT-SIZE TEXT-COLOR))
(define WIDTH (+ (image-width INSTRUCTIONS) 50))
(define HEIGHT 600)
(define (PLAIN)
  (define iw (image-width INSTRUCTIONS))
  (define bw (* SIDE 2 BOARD))
  (set! WIDTH  (+ (max iw bw) 50))
  (set! HEIGHT (+ (* SIDE 2 BOARD) 50))
  (empty-scene WIDTH HEIGHT))
(define (ISCENE)
  (define mt (PLAIN))
  (when (or (> (image-width mt) 1280) (> (image-height mt) 800))
    (error 'scene "it is impossible to draw a ~s x ~s game scene for a 1280 x 800 laptop screen" (image-width mt) (image-height mt)))
  (place-image INSTRUCTIONS (* .5 WIDTH) (* .9 HEIGHT) mt))

;                                  
;                                  
;                                  
;                                  
;   ;;; ;;;            ;           
;    ;; ;;                         
;    ;; ;;   ;;;;    ;;;    ;; ;;  
;    ; ; ;  ;    ;     ;     ;;  ; 
;    ; ; ;   ;;;;;     ;     ;   ; 
;    ;   ;  ;    ;     ;     ;   ; 
;    ;   ;  ;   ;;     ;     ;   ; 
;   ;;; ;;;  ;;; ;;  ;;;;;  ;;; ;;;
;                                  
;                                  
;                                  
;                                  

;; ---------------------------------------------------------------------------------------------------

;; start the game 
(define (roll-the-dice)
  (big-bang (create-world-of-dice-and-doom)             
            (on-key interact-with-board)
            (to-draw draw-dice-world)
            (stop-when no-more-moves-in-world? 
                       draw-end-of-dice-world)))

;;  -> DiceWorld
;; Returns a randomly generated world. If the world that
;; has been generated starts as a tie, the world is regenerated.
;; property: world is not in endgame state (no-more-moves? returns false)
(define (create-world-of-dice-and-doom)
  (define board (territory-build))
  (define gamet (game-tree board INIT-PLAYER INIT-SPARE-DICE))
  (define new-world (dice-world #f board gamet))
  (if (no-more-moves-in-world? new-world) 
      (create-world-of-dice-and-doom)
      new-world))

;; DiceWorld Key -> DiceWorld
;; Handles key events from a player
(define (interact-with-board w k)
  (cond [(key=? "left" k)
         (refocus-board w left)]
        [(key=? "right" k)
         (refocus-board w right)]
        [(key=? "p" k)
         (pass w)]
        [(key=? "\r" k)
         (mark w)]
        [(key=? "d" k)
         (unmark w)]
        [else w]))

;; Diceworld -> Scene
;; draws the world
(define (draw-dice-world w)
  (add-player-info 
   (game-player (dice-world-gt w)) 
   (add-board-to-scene w (ISCENE))))

;; DiceWorld -> Boolean
;; is it possible to play any moves from this world state? 
(define (no-more-moves-in-world? w)
  (define tree (dice-world-gt w))
  (define board (dice-world-board w))
  (define player (game-player tree))
  (or (no-more-moves? tree)
      (for/and ((t board)) (= (territory-player t) player))))

;; DiceWorld -> Image
;; render the endgame screen
(define (draw-end-of-dice-world w)
  (define board (dice-world-board w))
  (define message (text (won board) TEXT-SIZE TEXT-COLOR))
  (define background (add-board-to-scene w (PLAIN)))
  (overlay message background))

;; Board -> String
;; Which player has won the game -- eager is for N human players 
(define (won board)
  (define-values (best-score w) (winners board))
  (if (cons? (rest w)) "It's a tie." "You won."))

;                                  
;                                  
;                                  
;                                  
;    ;;;;;             ;           
;      ;                     ;     
;      ;    ;; ;;    ;;;    ;;;;;  
;      ;     ;;  ;     ;     ;     
;      ;     ;   ;     ;     ;     
;      ;     ;   ;     ;     ;     
;      ;     ;   ;     ;     ;   ; 
;    ;;;;;  ;;; ;;;  ;;;;;    ;;;  
;                                  
;                                  
;                                  
;                                  

;; ---------------------------------------------------------------------------------------------------
;; Making A Board

;; -> Board
;; Creates a list of territories the size of GRID with given x and y coordinates
;; properties: dice is (0,MAX-DICE]
;;             returns list of size GRID
(define (territory-build)
  (for/list ([n (in-range GRID)])
    (territory n (modulo n PLAYER#) (dice) (get-x n) (get-y n))))

;; -> Natural
;; the number of initial die on a territory
(define (dice)
  (add1 (random DICE#)))

;; Natural -> Number
;; the x coordinate for territory n of a board 
(define (get-x n)
  (+ OFFSET0
     (if (odd? (get-row n)) 0 (/ X-OFFSET 2))
     (* X-OFFSET (modulo n BOARD))))

;; Natural -> Number 
;; the y coordinate for territory n of a board 
(define (get-y n)
  (+ OFFSET0 (* Y-OFFSET (get-row n))))

;; ---------------------------------------------------------------------------------------------------
;; Making a Game Tree

;; Board Player Natural -> GameTree
;; creates a complete game-tree from the given board, player, and spare dice
(define (game-tree board player dice)
  ;; create tree of attacks from this position; add passing move
  (define (attacks board) 
    (for*/list ([src board] 
                [dst (neighbors (territory-index src))]
                #:when (attackable? board player src dst))
      (define from (territory-index src))
      (define dice (territory-dice src))
      (define newb (execute board player from dst dice))
      (define attacks-from-newb 
        (game newb player (cons (passes newb) (attacks newb))))
      (move (list from dst) attacks-from-newb)))
  ;; create a passing move , distribute dice, continue
  (define (passes board)
    (define-values (new-dice newb) (distribute board player dice))
    (move '() (game-tree newb (switch player) new-dice)))
  ;; -- START: -- 
  (game board player (attacks board)))

;; Player -> Player
;; switches from one player to the next
(define (switch player)
  (modulo (+ player 1) PLAYER#))

;; Board Player Natural -> Natural Board
;; adds reinforcements to the game board
;; > (add-new-dice (list (territory 0 2 2 9 0)) 2 2))
;; (list (territory 0 2 2 9 0))
(define (distribute board player spare-dice)
  (for/fold ([dice spare-dice] [new-board '()]) ([t board])
    (if (and (= (territory-player t) player)
             (< (territory-dice t) DICE#)
             (not (zero? dice)))
        (values (- dice 1) (cons (add-dice-to t) new-board))
        (values dice (cons t new-board)))))

;; Territory -> Territory 
;; adds one dice to the given territory
(define (add-dice-to t)
  (territory-set-dice t (add1 (territory-dice t))))

;; Board Player Territory Natural -> Boolean
;; can player attack dst from src?
(define (attackable? board player src dst)
  (define dst-t 
    (findf (lambda (t) (= (territory-index t) dst)) board))
  (and dst-t
       (= (territory-player src) player)
       (not (= (territory-player dst-t) player))
       (> (territory-dice src) (territory-dice dst-t))))

;; Board Natural Natural Natural Natural -> Board
;; Creates a new board after an attack
;; updates only src and dst
(define (execute board player src dst dice)
  (for/list ([t board])
    (define idx (territory-index t))
    (cond [(= idx src) (territory-set-dice t 1)]
          [(= idx dst) 
           (define s (territory-set-dice t (- dice 1)))
           (territory-set-player s player)]
          [else t])))

;; ---------------------------------------------------------------------------------------------------
;; Getting Neigbors

;; Natural -> [List-of Natural]
;; returns the neighbors of the current spot
;; > (neighbors 0)
;; '(1 2 3)
(define (neighbors pos)  
  (define top?      (< pos BOARD))
  (define bottom?   (= (get-row pos) (sub1 BOARD)))
  (define even-row? (zero? (modulo (get-row pos) 2)))
  (define right?    (zero? (modulo (add1 pos) BOARD)))
  (define left?     (zero? (modulo pos BOARD)))
  (if even-row?
      (even-row pos top? bottom? right? left?)
      (odd-row  pos top? bottom? right? left?)))

;; Natural Boolean Boolean Boolean Boolean -> [Listof Naturals]
;; gets the neighbors for a territory on an even row
(define (even-row pos top? bottom? right? left?)
  (append (add (or top? right?)    (add1 (- pos BOARD)))
          (add (or bottom? right?) (add1 (+ pos BOARD)))
          (add top?                (- pos BOARD))
          (add bottom?             (+ pos BOARD))
          (add right?              (add1 pos))
          (add left?               (sub1 pos))))

;; Natural Boolean Boolean Boolean Boolean -> [Listof Naturals]
;; gets the neighbors for a territory on an even odd
(define (odd-row pos top? bottom? right? left?)
  (append (add top?               (- pos BOARD))
          (add bottom?            (+ pos BOARD))
          (add (or top? left?)    (sub1 (- pos BOARD)))
          (add (or bottom? left?) (sub1 (+ pos BOARD)))
          (add right?             (add1 pos))
          (add left?              (sub1 pos))))

;; Boolean X -> [Listof X]
;; returns (list x) if (not b) else empty
(define (add b x)
  (if b '() (list x)))

;                                                                          
;                                                                          
;                                                                          
;                                                                          
;   ;;; ;;;                 ;;;;;;                                         
;    ;   ;                   ;   ;                           ;             
;    ;  ;    ;;;;   ;;; ;;;  ; ;   ;;;  ;;;  ;;;;   ;; ;;   ;;;;;    ;;;;; 
;    ; ;    ;    ;   ;   ;   ;;;    ;    ;  ;    ;   ;;  ;   ;      ;    ; 
;    ;;;    ;;;;;;   ;   ;   ; ;     ;  ;   ;;;;;;   ;   ;   ;       ;;;;  
;    ;  ;   ;         ; ;    ;       ;  ;   ;        ;   ;   ;           ; 
;    ;   ;  ;         ; ;    ;   ;    ;;    ;        ;   ;   ;   ;  ;    ; 
;   ;;;  ;;  ;;;;;     ;    ;;;;;;    ;;     ;;;;;  ;;; ;;;   ;;;   ;;;;;  
;                      ;                                                   
;                    ;;;                                                   
;                                                                          
;                                                                          

;; ---------------------------------------------------------------------------------------------------
;; Territory Focusing and Marking 

;; DiceWorld [Board -> Board] -> World
;; Creates a new World that has a rotated territory list
;; > (define lterritory (territory 0 0 1 9 2))
;; > (define rterritory (territory 0 0 1 9 0))
;; > (refocus-board-action (dice-world -1 (list rterritory lterritory ...) GT) left)
;; (dice-world -1 (list lterritory ... rterritory) GT)
;; > (refocus-board-action (dice-world -1 (list lterritory ... rterritory) GT) left)
;; (dice-world -1 (list rterritory lterritory ...) GT)
(define (refocus-board w direction)
  (define source (dice-world-src w))
  (define board  (dice-world-board w))
  (define tree   (dice-world-gt w))
  (define player (game-player tree))
  (define (owner? tid)
    (if source (not (= tid player)) (= tid player)))
  (define new-board (rotate-until owner? board direction))
  (dice-world source new-board tree))

;; [Player -> Boolean] Board (Board -> Board) -> Board 
;; rotate until the first element of the list satisfies owned-by 
(define (rotate-until owned-by board rotate)
  (define next-list (rotate board))
  (if (owned-by (territory-player (first next-list))) 
      next-list
      (rotate-until owned-by next-list rotate)))

;; Board -> Board
;; rotate a list to the left 
(define (left l)
  (append (rest l) (list (first l))))

;; Board -> Board
;; rotate a list to the right
(define (right l)
  (reverse (left (reverse l))))

;; ---------------------------------------------------------------------------------------------------
;; Handling Moves

;; DiceWorld -> DiceWorld
;; executes a passing move on the world state
;; THIS DEFINITION IS NOT USED FOR THE ABSTRACT VERSION OF THE MODULE. 
(define (pass.10 w)
  (define m (find-move (game-moves (dice-world-gt w)) '()))
  (cond [(not m) w]
        [else ;; (no-more-moves? m)
         (dice-world #f (game-board m) m)]))

;; DiceWorld -> DiceWorld
;; unmarks a marked territory
(define (unmark w)
  (dice-world #f (dice-world-board w) (dice-world-gt w)))

;; DiceWorld -> DiceWorld
;; marks a territory as the launching pad for an attack or launches the attack 
(define (mark w)
  (define tree   (dice-world-gt w))
  (define board  (dice-world-board w))
  (define source (dice-world-src w))
  (define focus  (territory-index (first board)))
  (if source 
      (attacking w source focus)
      (dice-world focus board tree)))

;; DiceWorld Natural Natural -> DiceWorld
(define (attacking w source target)
  (define feasible (game-moves (dice-world-gt w)))
  (define attack   (list source target))
  (define next     (find-move feasible attack))
  (if next (dice-world #f (game-board next) next) w))

;; [List-of Moves] [or '() [List Natural Natural]] -> [or #f Game-tree] 
;; find the move from the current list of moves
(define (find-move moves a)
  (define m (findf (lambda (m) (equal? (move-action m) a)) moves))
  (and m (move-gt m)))

;; Game -> Boolean 
;; are there any moves in this game record? 
(define (no-more-moves? g)
  (empty? (game-moves g)))

;                                                                          
;                                                                          
;                                                                          
;                                                                          
;   ;;;;;                       ;;                     ;                   
;    ;   ;                       ;                                         
;    ;   ;   ;;;;   ;; ;;    ;;; ;   ;;;;   ;; ;;;   ;;;    ;; ;;    ;;; ;;
;    ;   ;  ;    ;   ;;  ;  ;   ;;  ;    ;   ;;        ;     ;;  ;  ;   ;; 
;    ;;;;   ;;;;;;   ;   ;  ;    ;  ;;;;;;   ;         ;     ;   ;  ;    ; 
;    ;  ;   ;        ;   ;  ;    ;  ;        ;         ;     ;   ;  ;    ; 
;    ;   ;  ;        ;   ;  ;   ;;  ;        ;         ;     ;   ;  ;   ;; 
;   ;;;   ;  ;;;;;  ;;; ;;;  ;;; ;;  ;;;;;  ;;;;;    ;;;;;  ;;; ;;;  ;;; ; 
;                                                                        ; 
;                                                                    ;;;;  
;                                                                          
;                                            

;; Player Scene-> Scene
;; Draws the world
(define (add-player-info player s)
  (define str (whose-turn player))
  (define txt (text str TEXT-SIZE TEXT-COLOR))
  (place-image txt (- WIDTH INFO-X-OFFSET) INFO-Y-OFFSET s))

;; DiceWorld Scene -> Scene
;; folds through the board and creates an image representation of it
(define (add-board-to-scene w s)
  (define board   (dice-world-board w))
  (define player  (game-player (dice-world-gt w)))
  (define focus?  (dice-world-src w))
  (define trtry1  (first board))
  (define p-focus (territory-player trtry1))
  (define t-image (draw-territory trtry1))
  (define image (draw-focus focus? p-focus player t-image))
  (define base-s  (add-territory trtry1 image s))
  (for/fold ([s base-s]) ([t (rest board)])
    (add-territory t (draw-territory t) s)))

;; Nat Player Player Image -> Image 
;; add focus marker to territory if needed 
(define (draw-focus marked? p-in-focus p t-image)
  (if (or (and (not marked?) (= p-in-focus p))
          (and marked? (not (= p-in-focus p))))
      (overlay FOCUS t-image)
      t-image))

;; Image Territory Image -> Image 
(define (add-territory t image scene)
  (place-image image (territory-x t) (territory-y t) scene))

;; Territory -> Image
;; renders a single territory
(define (draw-territory t)
  (define color (color-chooser (territory-player t)))
  (overlay (hexagon color) (draw-dice (territory-dice t))))

;; Natural -> Image
;; renders all n >= 1 dice as a stack of dice 
(define (draw-dice n)
  (define first-die  (get-dice-image 0))
  (define height-die (image-height first-die))
  (for/fold ([s first-die]) ([i (- n 1)])
    (define dice-image (get-dice-image (+ i 1)))
    (define y-offset  (* height-die (+ .5 (* i .25))))
    (overlay/offset s 0 y-offset dice-image)))

;; Player -> Color
;; Determines a color for each player
(define (color-chooser p)
  (list-ref COLORS p))

;; -> Image
;; returns an image from the list of dice images 
(define (get-dice-image i)
  (list-ref IMG-LIST (modulo i (length IMG-LIST))))

;                                                  
;                                                  
;                                                  
;                                                  
;   ;;;;;;              ;;     ;                   
;    ;   ;               ;                         
;    ; ;    ;; ;;    ;;; ;   ;;;    ;; ;;    ;;; ;;
;    ;;;     ;;  ;  ;   ;;     ;     ;;  ;  ;   ;; 
;    ; ;     ;   ;  ;    ;     ;     ;   ;  ;    ; 
;    ;       ;   ;  ;    ;     ;     ;   ;  ;    ; 
;    ;   ;   ;   ;  ;   ;;     ;     ;   ;  ;   ;; 
;   ;;;;;;  ;;; ;;;  ;;; ;;  ;;;;;  ;;; ;;;  ;;; ; 
;                                                ; 
;                                            ;;;;  
;                                                  
;                                                  

;; Board ->* Natural [non-empty-listof Player]
;; gives the number of winning territories and the players(s) who have them
;; > (winners (list (territory 0 0 1 9 0) (territory 0 0 1 9 1)))
;; (values 2 '(0))
;; > (winners (list (territory 0 1 1 9 0) (territory 0 0 1 9 1)))
;; (values 1 '(0 1))
(define (winners board)
  (for/fold ([best 0][winners '()]) ([p PLAYER#])
    (define p-score (sum-territory board p))
    (cond [(> p-score best) (values p-score (list p))]
          [(< p-score best) (values best winners)]
          [(= p-score best) (values best (cons p winners))])))

;; Board Player -> Natural
;; counts the number of territorys the player owns
;; > (sum-territory (list (territory 0 1 1 9 0) (territory 0 1 1 9 1)) 1)
;; 2
(define (sum-territory board player)
  (for/fold ([result 0]) ([t board])
    (if (= (territory-player t) player) (+ result 1) result)))


;                      
;                      
;                      
;                      
;                      
;     ;;;     ;;;;;;;  
;      ;;        ;     
;     ;  ;       ;     
;     ;  ;       ;     
;     ;  ;       ;     
;    ;;;;;;      ;     
;    ;    ;      ;     
;   ;      ;     ;     
;  ;;;    ;;; ;;;;;;;  
;                      
;                      
;                      
;                      

;; Player -> {AI-TURN, YOUR-TURN}
;; THIS REQUIRES A DIFFERENT DEFINITION FOR PLAIN CHAPTER 10. 
(define (whose-turn player)
  (if (= player AI) AI-TURN YOUR-TURN))

;; DiceWorld -> DiceWorld
;; executes a passing move on the world state
;; THIS REQUIRES A DIFFERENT DEFINITION FOR PLAIN CHAPTER 10.
(define (pass w)
  (define m (find-move (game-moves (dice-world-gt w)) '()))
  (cond [(not m) w]
        [(or (no-more-moves? m) (not (= (game-player m) AI)))
         (dice-world #f (game-board m) m)]
        [else
         (define ai (the-ai-plays m))
         (dice-world #f (game-board ai) ai)]))

;; GameTree -> GameTree
;; Computer calls this function until it is no longer the player
(define (the-ai-plays tree)
  (define ratings  (rate-moves tree AI-DEPTH))
  (define the-move (first (argmax second ratings)))
  (define new-tree (move-gt the-move))
  (if (= (game-player new-tree) AI)
      (the-ai-plays new-tree)
      new-tree))

;; GameTree Natural -> [List-of (List Move Number)]
;; assigns a value to each move that is being considered
;; and return those values in a list
(define (rate-moves tree depth)
  (for/list ((move (game-moves tree)))
    (list move (rate-position (move-gt move) (- depth 1)))))

;; GameTree Natural -> Number
;; Returns a number that is the best move for the given player.
(define (rate-position tree depth)
  (cond [(or (= depth 0) (no-more-moves? tree))
         (define-values (best w) (winners (game-board tree)))
         (if (member AI w) (/ 1 (length w)) 0)]
        [else 
         (define ratings (rate-moves tree depth))
         (apply (if (= (game-player tree) AI) max min)
                (map second ratings))]))

;                                  
;                                  
;                                  
;                                  
;     ;;                           
;      ;                           
;     ; ;   ;;  ;;  ;;  ;;   ;;;;; 
;     ; ;    ;   ;   ;  ;   ;    ; 
;     ; ;    ;   ;    ;;     ;;;;  
;     ;;;    ;   ;    ;;         ; 
;    ;   ;   ;  ;;   ;  ;   ;    ; 
;   ;;; ;;;   ;; ;; ;;  ;;  ;;;;;  
;                                  
;                                  
;                                  
;                                  

;; Natural -> Natural
;; gets the row that territory is on, indexed from 0
;; [test vary on current board-size]
(define (get-row pos)
  (quotient pos BOARD))


;                                                    
;                                                    
;                                                    
;                                                    
;                                                    
;   ;;;;;;;                        ;                 
;   ;  ;  ;                        ;                 
;   ;  ;  ;     ;;;      ;;;; ;   ;;;;;;     ;;;; ;  
;   ;  ;  ;    ;   ;    ;    ;;    ;        ;    ;;  
;      ;      ;     ;   ;          ;        ;        
;      ;      ;;;;;;;    ;;;;;     ;         ;;;;;   
;      ;      ;               ;    ;              ;  
;      ;       ;    ;   ;     ;    ;    ;   ;     ;  
;    ;;;;;      ;;;;    ;;;;;;      ;;;;    ;;;;;;   
;                                                    
;                                                    
;                                                    
;                                 

;; ---------------------------------------------------------------------------------------------------

;; Natural -> Void 
;; make the board larger 
(define (set-grid n)
  (set! BOARD n)
  (set! GRID (* n n)))

(module+ test 
  
  (require rackunit rackunit/text-ui)
  
  ;; (-> any) -> void
  ;; runs the thunk PROP-NUM times
  (define (check-property t)
    (test-begin (for ((i 50)) (t))))
  
  ;; Properties
  (define (property:starting-world-playable)
    (unless (and (= BOARD 2) (= PLAYER# 2))
      (error 'starting-world-playable "BOARD-SIZE != 2 or PLAYERS# != 2"))
    (check-false (no-more-moves-in-world? (create-world-of-dice-and-doom))))
  
  (define (property:dice-in-range)
    (check-true (andmap (λ (b) (>= DICE# (territory-dice b) 1)) (territory-build))
                "dice out of range"))
  
  (define (property:board-correct-size)
    (check-equal? (length (territory-build)) GRID
                  "board incorrect-size"))
  
  (define (property:no-pass-on-first-move)
    (define (move-action? m) (equal? (move-action m) '()))
    (check-true (not (memf move-action? (game-moves (game-tree (territory-build) 0 0))))
                "no pass on first move"))
  
  ;; ---------------------------------------------------------------------------------------------------
  
  
  ;; testing game initialization
  
  (check-equal? (territory-index (first (territory-build))) 0)
  (check-equal? (territory-player (first (territory-build))) 0)
  (check-equal? (territory-index (second (territory-build))) 1)
  (check-equal? (territory-player (second (territory-build))) 1)
  (check-equal? (territory-index (third (territory-build))) 2)
  (check-equal? (territory-player (third (territory-build))) 0)
  (check-equal? (territory-index (fourth (territory-build))) 3)
  (check-equal? (territory-player (fourth (territory-build))) 1)
  
  (check-property property:starting-world-playable)
  (check-property property:board-correct-size)
  (check-property property:dice-in-range)
  (check-property property:no-pass-on-first-move)
  
  ;; ---------------------------------------------------------------------------------------------------
  ;; testing territory manipulation
  
  ;; legal?
  (check-true 
   (and (attackable? (list (territory 0 0 2 9 0) (territory 3 1 1 9 0)) 0 (territory 0 0 2 9 0) 3) #t))
  (check-false
   (attackable? (list (territory 0 0 2 9 0) (territory 3 1 1 9 0)) 0 (territory 0 0 2 9 0) 0))
  (check-false
   (attackable? (list (territory 0 0 2 9 0) (territory 5 1 1 9 0)) 1 (territory 0 0 2 9 0) 5))
  
  ;; get-row
  (check-equal? (get-row 0) 0)
  (check-equal? (get-row 1) 0)
  (check-equal? (get-row 2) 1)
  (check-equal? (get-row 3) 1)
  (check-equal? (get-row 12) 6) ;; checks math. actually invalid on board of size 2
  (check-equal? (get-row 11) 5) ;; checks math. actually invalid on board of size 2
  (check-equal? (get-row 13) 6) ;; checks math. actually invalid on board of size 2
  (check-equal? (get-row 14) 7) ;; checks math. actually invalid on board of size 2
  
  ;; ---------------------------------------------------------------------------------------------------
  (define board3 
    (list (territory 0 1 1 9 0) (territory 1 1 1 8 0) (territory 2 1 3 43.5 5) (territory 3 1 1 6 5)))
  (define b1+0+3
    (list (territory 0 0 2 9 0) (territory 1 1 1 8 0) (territory 2 0 2 43.5 5) (territory 3 1 1 6 5)))
  (define b2+1+2
    (list (territory 0 0 1 9 0) (territory 1 1 3 8 0) (territory 2 0 2 43.5 5) (territory 3 1 2 6 5)))
  (define board6
    (list (territory 0 0 1 9 0) (territory 1 1 2 8 0) (territory 2 0 3 43.5 5) (territory 3 1 2 6 5)))
  (define bard6+
    (list (territory 0 0 1 9 0) (territory 1 1 2 8 0) (territory 2 0 3 43.5 5) (territory 3 1 2 6 5)))
  
  (define (distribute/list a b c)
    (define-values (x y) (distribute a b c))
    (list x y))
  
  (define board0 
    (list (territory 0 0 1 9 0) (territory 1 1 2 8 0) (territory 2 0 2 43.5 5) (territory 3 1 1 6 5)))
  (define board1 
    (list (territory 0 0 1 9 0) (territory 1 1 1 8 0) (territory 2 0 1 43.5 5) (territory 3 1 1 6 5)))
  (define b1+1+2 
    (list (territory 0 0 1 9 0) (territory 1 1 2 8 0) (territory 2 0 1 43.5 5) (territory 3 1 2 6 5)))
  (define board2 
    (list (territory 0 0 1 9 0) (territory 1 1 1 8 0) (territory 2 0 3 43.5 5) (territory 3 1 1 6 5)))
  
  (define g-tree1 (game board1 0 '()))
  (define g-tree2 (game-tree board0 0 0))
  
  ; (define world31 (dice-world #f board1 g-tree1))
  (define world2 (dice-world #f board2 g-tree2))
  
  ;; testing book tree
  
  (check-equal? (game-tree (list (territory 0 0 2 'x 'y)
                                 (territory 1 1 1 'a 'b))
                           0
                           0)
                gt3)
  
  
  ;; testing tree generation
  
  (define (property:attack-location-valid)
    (define moves (game-moves (game-tree (territory-build) 0 0)))
    (check-true (and (for/and ([m moves])
                       (define m1 (move-action m))
                       (member (second m1) (neighbors (first m1))))
                     #t)
                "invalid attack location"))
  
  (define (property:add-to-territory-always-up-one)
    (define r (random 10000))
    (check-equal? (add-dice-to (territory 0 0 r 0 0))
                  (territory 0 0 (add1 r) 0 0)
                  "add to territory always up one"))
  
  (define (property:attackable?-does-not-need-neighbores-check)
    (define (check-attackable? gt)
      (for/and ([move (game-moves gt)]
                #:when (not (empty? (move-action move))))
        (define action (move-action move))
        (define gt (move-gt move))
        (and (member (second action) (neighbors (first action)))
             (check-attackable? gt))))
    
    ;;start
    (define old-size BOARD)
    (set-grid 2)
    (define testing-gt (dice-world-gt (create-world-of-dice-and-doom)))
    (check-true (check-attackable? testing-gt) "An attack move between non-neighbores was created")
    (set-grid old-size))
  
  
  ;; game-tree
  (check-equal? (game-tree board1 0 0) g-tree1)
  (check-equal? (game-tree board3 1 0) (game board3 1 '()))
  (check-equal? (game-tree board3 0 0) (game board3 0 '()))
  (check-property property:attackable?-does-not-need-neighbores-check)
  
  ;; find-move
  (check-false (find-move '() '()))
  (check-equal? (find-move (list (move '() (game '() 0 '()))) '()) (game '() 0 '()))
  ;; Attacking-Moves
  (check-property property:attack-location-valid)
  
  ;; switch-players
  (check-equal? (switch 0) 1)
  (check-equal? (switch 1) 0)
  
  ;; Add-New-Dice
  (check-equal? (distribute/list (game-board g-tree1) 0 3) (list 1 (reverse b1+0+3)))
  (check-equal? (distribute/list (game-board g-tree1) 1 2) (list 0 (reverse b1+1+2)))
  (check-equal? (distribute/list (game-board g-tree2) 1 2) (list 0 (reverse b2+1+2)))
  (check-equal? (distribute/list board6 0 0) (list 0 (reverse bard6+)))
  
  ;; add-to-territory
  (check-equal? (add-dice-to (territory 0 1 2 9 0)) (territory 0 1 3 9 0))
  (check-equal? (add-dice-to (territory 0 1 1 9 0)) (territory 0 1 2 9 0))
  (check-equal? (add-dice-to (territory 0 1 5 9 0)) (territory 0 1 6 9 0))
  (check-property property:add-to-territory-always-up-one)
  
  ;; ---------------------------------------------------------------------------------------------------
  (define board7
    (list (territory 0 0 1 9 0) (territory 1 1 1 8 0) (territory 2 0 2 43.5 5) (territory 3 1 1 6 5)))
  (define board8
    (list (territory 0 1 1 9 0) (territory 1 1 1 8 0) (territory 2 0 3 43.5 5) (territory 3 1 1 6 5)))
  (define board9
    (list (territory 0 0 1 9 0) (territory 1 1 1 8 0) (territory 2 0 2 43.5 5) (territory 3 0 1 6 5)))
  (define board10
    (list (territory 0 0 1 9 0) (territory 1 1 3 8 0) (territory 2 0 2 43.5 5) (territory 3 1 1 6 5)))
  
  ;; testing attacks
  
  (check-equal? 
   (execute board7 0 2 1 2)
   (list (territory 0 0 1 9 0) (territory 1 0 1 8 0) (territory 2 0 1 43.5 5) (territory 3 1 1 6 5)))
  
  (check-equal? 
   (execute board8 0 2 1 3)
   (list (territory 0 1 1 9 0) (territory 1 0 2 8 0) (territory 2 0 1 43.5 5) (territory 3 1 1 6 5)))
  
  (check-equal? 
   (execute board9 0 2 1 2)
   (list (territory 0 0 1 9 0) (territory 1 0 1 8 0) (territory 2 0 1 43.5 5) (territory 3 0 1 6 5)))
  
  (check-equal?
   (execute board10 1 1 0 3)
   (list(territory 0 1 2 9 0) (territory 1 1 1 8 0) (territory 2 0 2 43.5 5) (territory 3 1 1 6 5)))
  
  ;; Neighbors
  (check-equal? (neighbors 2) '(0 3))
  (check-equal? (neighbors 0) '(3 2 1))
  (check-equal? (neighbors 1) '(3 0)) 
  (check-equal? (neighbors 3) '(1 0 2))
  
  ;; ---------------------------------------------------------------------------------------------------
  (define board20
    (list (territory 0 0 1 9 2) (territory 1 0 1 9 0) (territory 2 2 1 9 0)))
  (define board21 
    (list (territory 0 1 1 9 0) (territory 1 1 1 8 0) (territory 2 1 1 43.5 5) (territory 3 1 1 6 5)))
  
  ;; testing focus manipulation 
  ;; interact-with-board
  (check-equal? 
   (interact-with-board world2 "\r")
   (dice-world (territory-index (car (dice-world-board world2))) (dice-world-board world2) g-tree2))
  
  (check-equal? (interact-with-board world2 "p") world2)
  
  ;; refocus-board-action
  (check-equal? 
   (refocus-board (dice-world #f (list (territory 0 0 1 9 0) (territory 0 0 1 9 2)) g-tree1) left)
   (dice-world #f (list (territory 0 0 1 9 2) (territory 0 0 1 9 0)) g-tree1))
  
  (check-equal? 
   (refocus-board (dice-world #f (list (territory 0 0 1 9 2) (territory 0 1 1 9 0)) g-tree1) right)
   (dice-world #f (list (territory 0 0 1 9 2) (territory 0 1 1 9 0)) g-tree1))
  
  (check-equal? 
   (refocus-board (dice-world 0 board20 g-tree1) left)
   (dice-world 0 (list (territory 2 2 1 9 0) (territory 0 0 1 9 2) (territory 1 0 1 9 0)) g-tree1))
  
  (check-equal? 
   (refocus-board (dice-world 0 (list (territory 0 0 1 9 2) (territory 0 1 1 9 0)) g-tree1) left)
   (dice-world 0 (list  (territory 0 1 1 9 0) (territory 0 0 1 9 2)) g-tree1))
  
  (check-equal? 
   (refocus-board (dice-world 0 (list(territory 0 0 1 9 2) (territory 0 1 1 9 0)) g-tree1) right)
   (dice-world 0 (list  (territory 0 1 1 9 0) (territory 0 0 1 9 2)) g-tree1))
  
  ;;unmark
  (check-equal? (unmark (dice-world 1 board21 g-tree1)) (dice-world #f board21 g-tree1))
  
  (check-equal? (unmark (dice-world 1 (list (territory 0 1 1 9 0) (territory 1 1 1 8 0)) g-tree1))
                (dice-world #f (list (territory 0 1 1 9 0) (territory 1 1 1 8 0)) g-tree1))
  (check-equal? (unmark (dice-world 0 (list (territory 0 1 1 9 0)) g-tree1))
                (dice-world #f (list (territory 0 1 1 9 0)) g-tree1))
  (check-equal? (unmark (dice-world #f (list (territory 0 1 1 9 0)) g-tree1))
                (dice-world #f (list (territory 0 1 1 9 0)) g-tree1))
  
  ;; ---------------------------------------------------------------------------------------------------
  (define (winners/list w)
    (define-values (a b) (winners w))
    (cons a b))
  
  ;; testing functions that determine 'winning' and declare the winner
  
  ;; winners
  (check-equal? (winners/list (list (territory 0 0 1 9 0) (territory 0 0 1 9 1))) (list 2 0))
  (check-equal? (winners/list (list (territory 0 1 1 9 0) (territory 0 0 1 9 1))) (list 1 1 0))
  
  ;; sum-territory
  (check-equal? (sum-territory (list (territory 0 0 1 9 0) (territory 0 0 1 9 1)) 0) 2)
  (check-equal? (sum-territory (list (territory 0 0 1 9 0) (territory 0 0 1 9 1)) 1) 0)
  (check-equal? (sum-territory (list (territory 0 0 1 9 0) (territory 0 0 1 9 1)) 2) 0)
  (check-equal? (sum-territory (list (territory 0 1 1 9 0) (territory 0 0 1 9 1)) 1) 1)
  (check-equal? (sum-territory (list (territory 0 1 1 9 0) (territory 0 0 1 9 1)) 0) 1)
  
  ;; ---------------------------------------------------------------------------------------------------
  ;; testing the AI 
  
  (define tree0
    (game-tree (list (territory 0 1 3 0 0)
                     (territory 1 0 2 0 0)
                     (territory 2 0 2 0 0) 
                     (territory 3 0 2 0 0))
               1 15))
  
  (define territory1 (territory 3 0 3 280 262.5))
  
  (define board31 
    (list territory1
          (territory 2 0 3 150 262.5)
          (territory 1 1 2 345 150)
          (territory 0 0 2 215 150)))
  
  (define world1 
    (dice-world #f board31 (game board31 1 '())))
  
  ;; testing the AI functions 
  
  ;; MF: one of these two tests should fail! 
  (check-true (and (attackable? board31 0 territory1 1) #t))
  (check-true (no-more-moves-in-world? world1))
  
  (check-equal? (interact-with-board (dice-world 3 '() '()) "d")
                (dice-world #f '() '()))
  
  (check-equal? (game-board (the-ai-plays tree0))
                (list (territory 3 1 3 0 0)
                      (territory 2 0 2 0 0)
                      (territory 1 0 2 0 0) 
                      (territory 0 1 2 0 0)))
  
  (check-equal? (game-player (the-ai-plays tree0))
                0)
  
  (check-equal? (game-board (move-gt (first (game-moves tree0))))
                (list (territory 0 1 1 0 0)
                      (territory 1 0 2 0 0)
                      (territory 2 0 2 0 0)
                      (territory 3 1 2 0 0)))
  
  (check-equal? (game-player (move-gt (first (game-moves tree0))))
                1)
  
  (check-equal? (rate-position tree0 AI-DEPTH) 1/2)
  (check-equal? (rate-position (move-gt (first (game-moves tree0))) AI-DEPTH)
                1/2)
  
  "all tests run")
