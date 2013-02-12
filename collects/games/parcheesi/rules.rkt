(module rules racket
  (provide show-rules)
  
  (require "board.rkt"
           "moves.rkt"
           "gui.rkt"
           xml
           browser/htmltext
           racket/gui)

  (define board-size 250)
  
  (define (show-rules)
    (let* ([f (new frame% (label "Parcheesi Rules") (width 600) (height 600))]
           [t (new (html-text-mixin text%))]
           [ec (new editor-canvas% (parent f) (editor t) (style '(no-hscroll resize-corner)))])
      (send f show #t)
      (send t begin-edit-sequence)
      (let-values ([(in out) (make-pipe)])
        (thread
         (lambda ()
           (display-xml/content (xexpr->xml (parcheesi-rules)) out)
           (close-output-port out)))
        (render-html-to-text in t #t #f))
      (replace-!!s t)
      (send t auto-wrap #t)
      (send t lock #t)
      (send t hide-caret #t)
      (send t end-edit-sequence)
      (send ec focus)))
  
  (define (parcheesi-rules)
    `(div 
      (p "Parcheesi is a race between four players. Each player moves four pawns "
         "from their starting point, around the board and then into the center. "
         "The first player to get all four pawns into the center wins. ")
      
      (p "Initially, the board looks like this, with each player's pawns "
         "in their home circle."
         (center ,(moves-make-image/link '() "initial")))
      
      (p "On each turn, a player rolls two dice and moves their pawns according to these rules: "
         (ul
          (li ,(heading "Entering")
              "If one of the dice has a five, or both dice together sum to five, one of the pieces "
              "may be moved out of the start area into the main board (moving out consumes the five). "
              "The entering positions are the positions on the board that are half purple and half "
              "the color of the player. "
              "For example, if green rolls 1 and a 4 on the first turn, the board looks like this: "
              (center
               ,(moves-make-image/link (list (make-enter-piece (make-pawn 'green 0)))
                                       "enter")))
          (li ,(heading "Normal Move")
              "With the exception of doubles, when a player rolls the dice, the player moves "
              "their pieces by the numbers indicated by the pips. Pieces move in the "
              "counter-clockwise direction around the board, on the purple and light blue squares. "
              "If, for example, green rolls "
              "a 5 and a 3 on the first move, the board looks like this:"
              (center
               ,(moves-make-image/link (list (make-enter-piece (make-pawn 'green 0))
                                             (make-move-piece-main (make-pawn 'green 0) 5 3))
                                       "simple-move"))
              "then, if green rolls a 4 and a 6 on their next move (and the other players don't enter) "
              "green might first move the 4, resulting in the board on the left "
              "and then move the 6, resulting in the board on the right."
              (center
               ,(moves-make-image/link (list (make-enter-piece (make-pawn 'green 0))
                                             (make-move-piece-main (make-pawn 'green 0) 5 3)
                                             (make-move-piece-main (make-pawn 'green 0) 8 4))
                                       "simple-move2")
               nbsp nbsp nbsp nbsp
               ,(moves-make-image/link (list (make-enter-piece (make-pawn 'green 0))
                                             (make-move-piece-main (make-pawn 'green 0) 5 3)
                                             (make-move-piece-main (make-pawn 'green 0) 8 4)
                                             (make-move-piece-main (make-pawn 'green 0) 12 6))
                                       "simple-move3")))
          (li ,(heading "Double Bonus")
              "When a player rolls doubles and all of their pieces are out of the starting area, "
              "the player moves by the tops and bottoms of the dice. That is, if the player "
              "rolls double 1s, the player moves two pieces 1 square and two pieces 6 squares "
              "(of course, this can be all the same piece). The opposite sides of the die always "
              "sum to seven, so the total number of spaces moved is always 14.")
          (li ,(heading "Double Repeats")
              "When a player rolls doubles (no matter if all of the pieces are out or not) "
              "they take another turn. If they roll doubles a second time, they take a third turn. "
              "On the third turn, if they roll doubles, their turn is forfeit and the most pawn that is "
              "furthest along must be moved back to the starting circle. "
              "If they do not roll doubles on the third turn, "
              "they take the third turn as normal.")
          (li ,(heading "Blockades")
              "Two pawns of the same color on a space form a blockade. "
              "A blockade cannot be passed by any other pawn (even one of the same color). "
              "A blockade cannot be moved together (this rule only affects doubles). "
              "As a simple example, if red were to roll a 2 and a 1 in the following board, "
              "only the freshly entered pawn could advance. "
              (center
               ,(moves-make-image/link (list (make-enter-piece (make-pawn 'green 0))
                                             (make-enter-piece (make-pawn 'green 1))
                                             (make-move-piece-main (make-pawn 'green 0) 5 22)
                                             (make-move-piece-main (make-pawn 'green 1) 5 22)
                                             (make-enter-piece (make-pawn 'red 0))
                                             (make-enter-piece (make-pawn 'red 1))
                                             (make-move-piece-main (make-pawn 'red 0) 22 4))
                                       "blockade")))
          
          (li ,(heading "Individual Die Rolls")
              "Each die roll should be thought of as an individual \"mini-move\". "
              "That is, when a player rolls a 1 and a 6, for example, the player may move "
              "one pawn 1 squares and another pawn 6 squares. This may be the same pawn, but "
              "this is not the same as moving that pawn 7 squares directly. "
              "For example, in the following board, if red has a 1 and a 6, red cannot move, "
              "even though the spot seven spaces away is safe to land on. "
              (center
               ,(moves-make-image/link (list (make-enter-piece (make-pawn 'green 0))
                                             (make-enter-piece (make-pawn 'green 1))
                                             (make-move-piece-main (make-pawn 'green 0) 5 24)
                                             (make-move-piece-main (make-pawn 'green 1) 5 29)
                                             (make-enter-piece (make-pawn 'red 0))
                                             (make-move-piece-main (make-pawn 'red 0) 22 6))
                                       "individual-die-rolls")))
          
          (li ,(heading "Bop")
              "If a pawn is by itself on a light blue square "
              "and a pawn of a different color lands on the square it occupies "
              "(by an exact count of the dice) the original pawn is bopped, "
              "which means it is sent back to the "
              "starting square. The player that bopped now may move any one of its pawns by 20. "
              "This bonus acts just like an extra \"mini-move\" as described above. "
              "For example, if red rolls a 1 and a 2 in the board on the left, after moving the board "
              "looks like on the right."
              (center
               ,(moves-make-image/link (list (make-enter-piece (make-pawn 'green 0))
                                             (make-move-piece-main (make-pawn 'green 0) 5 20)
                                             (make-enter-piece (make-pawn 'red 0)))
                                       "bop-before")
               nbsp nbsp nbsp nbsp
               ,(moves-make-image/link (list (make-enter-piece (make-pawn 'red 0))
                                             (make-move-piece-main (make-pawn 'red 0) 22 3)
                                             (make-move-piece-main (make-pawn 'red 0) 25 20))
                                       "bop-after")))
          
          
          (li ,(heading "Safety")
              "Purple squares are safety squares. The only way a pawn can be bopped on a purple "
              "square is if the bopping pawn is entering from the start. A safety square cannot be "
              "occupied by two different colored pawns. One pawn on a safety does not constitute a "
              "blockade, however, so other colors can pass by. "
              "For example, if red rolls a 2 and a 3 in the situation below, it can take "
              "the 2 and then the 3, but not the 3 and then the 2."
              (center
               ,(moves-make-image/link (list (make-enter-piece (make-pawn 'green 0))
                                             (make-move-piece-main (make-pawn 'green 0) 5 24)
                                             (make-enter-piece (make-pawn 'red 0))
                                             (make-move-piece-main (make-pawn 'red 0) 22 4))
                                       "no-bop"))
              "If, however, red rolls a 1 and a 4, in the picture on the left, red bops "
              "green and we get the picture on the right."
              (center
               ,(moves-make-image/link (list (make-enter-piece (make-pawn 'green 0))
                                             (make-move-piece-main (make-pawn 'green 0) 5 17))
                                       "bop-enter-before")
               nbsp nbsp nbsp nbsp
               ,(moves-make-image/link (list (make-enter-piece (make-pawn 'red 0))
                                             (make-move-piece-main (make-pawn 'red 0) 22 20))
                                       "bop-enter-after")))
          (li ,(heading "Home Row")
              "When a pawn makes a nearly complete circuit, it turns off into its "
              "correspondingly colored home row. For example, if "
              "green, after travelling all the way around the board to "
              "the bottom left, rolls a 4 and a 3, it moves into the green "
              "section of the board:"
              (center
               ,(moves-make-image/link (list (make-enter-piece (make-pawn 'green 0))
                                             (make-move-piece-main (make-pawn 'green 0) 5 60))
                                       "enter-home-before")
               nbsp nbsp nbsp nbsp
               ,(moves-make-image/link (list (make-enter-piece (make-pawn 'green 0))
                                             (make-move-piece-main (make-pawn 'green 0) 5 60)
                                             (make-move-piece-main (make-pawn 'green 0) 65 7))
                                       "enter-home-after")))
          
          (li ,(heading "Home")
              "Pawns must enter home by an exact count. "
              "For example, in the board below, green must roll a 1 "
              "in order to move the piece home."
              (center
               ,(moves-make-image/link (list (make-enter-piece (make-pawn 'green 0))
                                             (make-move-piece-main (make-pawn 'green 0) 5 60)
                                             (make-move-piece-main (make-pawn 'green 0) 65 10))
                                       "going-home")))
          
          (li ,(heading "Home Bonus")
              "When a piece moves home, the player whose piece moved home gets a bonus "
              "of 10. "
              "This bonus acts just like an extra \"mini-move\" as described above. "
              "For example, if green rolls a 1 and a 2 in the board on the left "
              "the move results in the board on the right."
              (center
               ,(moves-make-image/link (list (make-enter-piece (make-pawn 'green 0))
                                             (make-enter-piece (make-pawn 'green 1))
                                             (make-move-piece-main (make-pawn 'green 0) 5 60)
                                             (make-move-piece-main (make-pawn 'green 0) 65 10))
                                       "home-bonus-before")
               nbsp nbsp nbsp
               ,(moves-make-image/link (list (make-enter-piece (make-pawn 'green 0))
                                             (make-enter-piece (make-pawn 'green 1))
                                             (make-move-piece-main (make-pawn 'green 0) 5 60)
                                             (make-move-piece-main (make-pawn 'green 0) 65 10)
                                             (make-move-piece-home (make-pawn 'green 0) 6 1)
                                             (make-move-piece-main (make-pawn 'green 1) 5 10)
                                             (make-move-piece-main (make-pawn 'green 1) 15 2))
                                       "home-bonus-after")))
          
          (li ,(heading "Cell Occupancy")
              "Each square in the main ring and in the home rows can only have zero, one or two pawns on it. "
              "If it contains two pawns, those pawns must be the same color (and would form a blockade, as above)")
          
          (li ,(heading "Use all Dice")
              "A player must play as many dice as possible. More precisely, if a player makes a move and "
              "then finishes their turn, there must not be any further moves possible. "
              "Parcheesi typically requires a player to enter the board, if possible. That is not required "
              "here however." )))))
      
  (define table (make-hash))
  
  (define (replace-!!s t)
    (let loop ([starts (reverse (send t find-string-all "!!"))]
               [ends (reverse (send t find-string-all "::"))])
      (cond
        [(null? starts) (void)]
        [else (let* ([start (car starts)]
                     [end (car ends)]
                     [name (send t get-text (+ start 2) end)])
                (send t delete start (+ end 2) #f)
                (send t insert (new board-snip% (board (hash-ref table name))) start start #f)
                (loop (cdr starts) (cdr ends)))])))

  (define scroll-step-pixels 12)
  
  (define board-snip%
    (class snip%
      (init-field board)
      (define/override (find-scroll-step y) (inexact->exact (round (/ y scroll-step-pixels))))
      (define/override (get-num-scroll-steps) (quotient board-size scroll-step-pixels))
      (define/override (get-scroll-step-offset step) (* step scroll-step-pixels))
      (define/override (get-extent dc x y w h descent space lspace rspace)
        (set-box/f! w board-size)
        (set-box/f! h board-size)
        (set-box/f! descent 0)
        (set-box/f! space 0)
        (set-box/f! lspace 0)
        (set-box/f! rspace 0))
      (define/override (draw dc x y left top right bottom dx dy draw-caret)
        (draw-board board dc board-size board-size x y #t))
      (super-new)
      (inherit set-snipclass)
      (set-snipclass dummy-snipclass)))
  
  (define (set-box/f! b v) (when (box? b) (set-box! b v)))
  
  (define dummy-snipclass (new snip-class%))
  
  (define (heading name)
    `(font ((color "forestgreen") (size "+2")) (b ,name)))
  
  (define (moves-make-image/link moves name)
    (let-values ([(board bonuses) (make-moves (new-board) moves)])
      (hash-set! table name board)
      (format " !!~a:: " name))))
