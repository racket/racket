(module moves mzscheme
  (require "board.rkt"
           mzlib/contract
           mzlib/list)

  ;; a move is either:
  ;;  - (make-enter-piece pawn)
  ;;  - (make-move-piece-main pawn start distance)
  ;;  - (make-move-piece-home pawn start distance)
  (define-struct move () (make-inspector))
  (define-struct (enter-piece move) (pawn) (make-inspector))
  (define-struct (move-piece-main move) (pawn start distance) (make-inspector))
  (define-struct (move-piece-home move) (pawn start distance) (make-inspector))

  (provide/contract
   (struct enter-piece ((pawn pawn?)))
   (struct move-piece-main ([pawn pawn?] [start number?] [distance number?]))
   (struct move-piece-home ([pawn pawn?] [start number?] [distance number?])))

  (provide take-turn
           bad-move
           make-moves
           move?

           board-enter-piece
           board-move-piece-main
           board-move-piece-home
           
           blockade-moved?
           find-end-spot
           board-doubles-penalty
           
           make-one-move
           
           get-move-id
           get-move-color
           
           has-entering-roll?
           entering-blockade?
           exn:bad-move?
           
           board-all-in?
           <=/m
           possible-to-move)
  
  (define bop-bonus 20)
  (define home-bonus 10)
         
  ;; moves-dice : moves -> (listof number)
  ;; does not return the die moves that correspond to entering pawns
  (define (moves-dice moves)
    (let loop ([moves moves]
               [dice null])
      (cond
        [(null? moves) dice]
        [else 
         (let ([move (car moves)])
           (cond
             [(move-piece-main? move) 
              (loop (cdr moves) (cons (move-piece-main-distance move) dice))]
             [(move-piece-home? move) 
              (loop (cdr moves) (cons (move-piece-home-distance move) dice))]
             [else (loop (cdr moves) dice)]))])))
  
  ;; board-doubles-penalty : board color -> board
  (define (board-doubles-penalty board color)
    (let home-row-loop ([i board-home-row-size])
      (cond
        [(zero? i)
         (let main-board-loop ([i (get-enter-pos color)]
                               [first-time? #t])
           (cond
             [(and (not first-time?) (= i (get-enter-pos color)))
              board]
             [else
              (let* ([next-i (modulo (- i 1) board-main-size)]
                     [ent (board-main-i board next-i)])
                (if (and (pair? ent)
                         (eq? (pawn-color (car ent)) color))
                    (move-piece board (car ent) 'start)
                    (main-board-loop next-i #f)))]))]
        [else 
         (let ([ent (board-home-row-i board color (- i 1))])
           (if (null? ent)
               (home-row-loop (- i 1))
               (move-piece board (car ent) 'start)))])))
  
  ;; take-turn : color board (listof number) (listof move) -> board
  ;; raises an exception if the turn is illegal
  (define (take-turn color original-board original-dice original-moves)
    (unless (andmap (lambda (x) (eq? color (get-move-color x))) original-moves)
      (bad-move "attempted to move two different colors"))
    (let loop ([moves original-moves]
               [board original-board]
               [dice original-dice])
      (cond
        [(null? moves) 
         
         (when (and (has-entering-roll? dice)
                    (memf (lambda (pawn) (eq? (pawn-color pawn) color))
                          (board-start board)) ;; has pieces in start
                    (not (entering-blockade? board color)))
           (bad-move "can still enter a pawn"))
         
         (let ([used-dice (moves-dice original-moves)])
           (for-each (lambda (die) 
                       (let ([potential-board (possible-to-move color board die)])
                         (when potential-board
                           (unless (blockade-moved? original-board potential-board color)
                             (bad-move "die roll ~a can still be used" die)))))
                     dice)
           
           (when (blockade-moved? original-board board color)
             (bad-move "cannot move blockade together")))
         
         board]
        [else 
         (let ([move (car moves)])
           (let-values ([(new-board bonus new-dice)
                         (make-move/dice board move dice)])
             (let ([new-new-dice (if bonus
                                     (cons bonus new-dice)
                                     new-dice)])
               (loop (cdr moves)
                     new-board
                     new-new-dice))))])))
  
  ;; get-move-color : move -> symbol
  ;; extracts the moved color from the move
  (define (get-move-color move) (pawn-color (get-move-pawn move)))
  
  ;; get-move-id : move -> number
  (define (get-move-id move) (pawn-id (get-move-pawn move)))
  
  (define (get-move-pawn move)
    (cond
      [(enter-piece? move) (enter-piece-pawn move)]
      [(move-piece-main? move) (move-piece-main-pawn move)]
      [(move-piece-home? move) (move-piece-home-pawn move)]))
  
  ;; blocakde-moved? : board board color -> boolean
  (define (blockade-moved? original-board new-board color)
    (let ([original-blockades (find-blockades/color original-board color)]
          [new-blockades (find-blockades/color new-board color)])
      (ormap (lambda (new-blockade) (memf (same-blockade-different-place? new-blockade) original-blockades))
             new-blockades)))
  
  (define ((same-blockade-different-place? b1) b2)
    (and (equal? (blockade-p1 b1) (blockade-p1 b2))
         (equal? (blockade-p2 b1) (blockade-p2 b2))
         (not (equal? (blockade-loc b1) (blockade-loc b2)))))
  
  ;; make-move/dice : board move (listof number) number -> (values board bonus (listof number))
  ;; makes the given move, removing the used dice from the dice list.
  ;; raises an error if the move isn't legal.
  ;; check for: using a five to move when there are pieces to come in
  ;;            moving without the matching roll
  (define (make-move/dice board move dice)
    (cond
      [(enter-piece? move)
       (let ([new-dice (cond
                         [(memq 5 dice) (remq 5 dice)]
                         [(and (memq 1 dice) (memq 4 dice))
                          (remq 1 (remq 4 dice))]
                         [(and (memq 2 dice) (memq 3 dice))
                          (remq 2 (remq 3 dice))]
                         [else (bad-move "entered without having a 5")])])
         (let-values ([(board bonus) (board-enter-piece board (enter-piece-pawn move))])
           (values board bonus new-dice)))]
      [(move-piece-main? move)
       (do-move/dice/moving board dice 
                            (move-piece-main-distance move)
                            (move-piece-main-pawn move)
                            (move-piece-main-start move)
                            board-move-piece-main)]
      [(move-piece-home? move)
       (do-move/dice/moving board dice 
                            (move-piece-home-distance move)
                            (move-piece-home-pawn move)
                            (move-piece-home-start move)
                            board-move-piece-home)]))
  
  ;; helper function to collapse last two cases of make-move/dice
  (define (do-move/dice/moving board dice die pawn start board-move-piece)
    (let ([new-dice (remq die dice)])
      (unless (memq die dice)
        (bad-move "tried to move ~a squares but dice read ~a" die dice))
      (let-values ([(new-board bonus)
                    (board-move-piece board pawn start die)])
        (values new-board bonus new-dice))))
  
  ;; entering-blocade? : board symbol -> boolean
  (define (entering-blockade? board color) 
    (let ([ent (board-main-i board (get-enter-pos color))])
      (and (pair? ent) (pair? (cdr ent)))))
  
  (define (no-blockades board start end)
    (let ([ind (find-blockade/between board start end)])
      (cond
        [(not ind) (void)]
        [(number? ind)
         (bad-move "there is a blockade at ~a in the main ring" ind)]
        [(home-row-loc? ind)
         (bad-move "there is a blockade at ~a in the ~a home row" 
                   (home-row-loc-num ind)
                   (home-row-loc-color ind))]
        [else (bad-move "blockade in the way")])))
  
  ;; has-entering-roll? : (listof number) -> boolean
  (define (has-entering-roll? dice)
    (or (memq 5 dice)
        (and (memq 1 dice) (memq 4 dice))
        (and (memq 2 dice) (memq 3 dice))))
  
  ;; possible-to-move : symbol board number -> (union #f board)
  ;; indicates if there are any moves that could happen with the
  ;; given die, for the given color in the given board.
  ;; doesn't consider entering moves
  (define (possible-to-move color board die)
    (let/ec k
      (for-each-pawn/loc
       board
       (lambda (pawn loc)
         (when (and (eq? color (pawn-color pawn))
                    (not (symbol? loc)))
           (with-handlers ([exn:bad-move? (lambda (x) #f)])
             (cond
               [(number? loc) 
                (let-values ([(board bonus) (board-move-piece-main board pawn loc die)])
                  (k board))]
               [(home-row-loc? loc) 
                (let-values ([(board bonus) (board-move-piece-home board pawn (home-row-loc-num loc) die)])
                  (k board))])))))
      #f))
    
  ;; make-moves : board (listof move) -> board (listof number)
  ;; only checks that each move, in isloation, would be possible
  (define (make-moves board moves)
    (let loop ([board board]
               [bonus '()]
               [moves moves])
      (cond
        [(null? moves) (values board bonus)]
        [else 
         (let-values ([(new-board new-bonus) (make-one-move board (car moves))])
           (loop new-board 
                 (if new-bonus (cons new-bonus bonus) bonus)
                 (cdr moves)))])))
  
  ;; make-one-move : board move -> board
  (define (make-one-move board move)
    (cond
      [(enter-piece? move) (board-enter-piece board (enter-piece-pawn move))]
      [(move-piece-main? move) (board-move-piece-main board
                                                      (move-piece-main-pawn move)
                                                      (move-piece-main-start move)
                                                      (move-piece-main-distance move))]
      [(move-piece-home? move) (board-move-piece-home board
                                                      (move-piece-home-pawn move)
                                                      (move-piece-home-start move)
                                                      (move-piece-home-distance move))]))
  
  
  
  (define (board-all-in? board color) 
    (not (memf (lambda (pawn) (eq? (pawn-color pawn) color))
               (board-start board))))
  
  ;; enter-piece : board pawn -> (values board (union #f number))
  (define (board-enter-piece orig-board pawn)
    (unless (member pawn (board-start orig-board))
      (bad-move "~a's pawn ~a is already on the board" (pawn-color pawn) (pawn-id pawn)))
    ;; move the color out of the starting area
    (let* ([pos (get-enter-pos (pawn-color pawn))]
           [old-ent (board-main-i orig-board pos)])
      (when (= 2 (length old-ent))
        (bad-move "cannot move out into a blockade"))
      (cond
        ;; no bop
        [(or (null? old-ent)
             (eq? (pawn-color (car old-ent)) (pawn-color pawn)))
         (values (move-piece orig-board pawn pos)
                 #f)]
        ;; bop
        [else
         (values (move-piece2
                  orig-board
                  pawn
                  pos
                  (car old-ent)
                  'start)
                 bop-bonus)])))
  
  ;; board-move-piece-home : board pawn number number -> (values board (union #f number))
  ;; result of #f indicates no bop; result of a color indicates who got bopped
  (define (board-move-piece-home board pawn start distance)
    (let* ([color (pawn-color pawn)]
           [old (board-home-row-i board color start)])
      (unless (member pawn old)
        (bad-move "color ~a is not in the home row on ~a" (pawn-color pawn) start))
      (unless (and (<= 0 start) (< start board-home-row-size))
        (error 'boad-move-piece-home "bad start argument ~e" start))
      (unless (<= 0 start (+ start distance) (+ board-home-row-size 1))
        (bad-move "moved too far, off the end of the board"))
      
      (let ([finish (+ start distance)])
        (cond
          [(= finish board-home-row-size)
           (when (< start (- finish 1))
             ;; if only moving one square, then we don't need to check blockades
             ;; this lets us satisfy the inputs to no-blockades
             (no-blockades board
                           (make-home-row-loc (+ start 1) color)
                           (make-home-row-loc (- finish 1) color)))
           (values (move-piece board pawn 'home)
                   home-bonus)]
          [(< finish board-home-row-size)
           (no-blockades board
                         (make-home-row-loc (+ start 1) color)
                         (make-home-row-loc finish color))
           
           (let ([old-ent (board-home-row-i board color finish)])
             (cond
               [(or (null? old-ent)
                    (null? (cdr old-ent)))
                (values (move-piece board 
                                    pawn
                                    (make-home-row-loc finish color))
                        #f)]
               [else
                (bad-move "moved onto a blockade in the home row")]))]
          [else
           (bad-move "moved off of the end of the board")]))))
  
  ;; board-move-piece-main : board pawn number number -> (values board (union #f number))
  ;; result of #f indicates no bop; result of a color indicates who got bopped
  (define (board-move-piece-main board pawn start distance)
    (unless (member pawn (board-main-i board start))
      (bad-move "color ~a (piece #~a) is not on square ~a" 
                (pawn-color pawn)
                (pawn-id pawn)
                start))
    (let* ([color (pawn-color pawn)]
           [landed (find-end-spot color start distance)]
           [exit (get-exit-pos color)])
      (cond
        [(eq? landed 'too-far) (bad-move "moved off of the board")]
        [(eq? landed 'home)
         (no-blockades board
                       (modulo (+ start 1) board-main-size)
                       (make-home-row-loc (- board-home-row-size 1) color))
         (values (move-piece board
                             pawn
                             'home)
                 10)]
        [(eq? (car landed) 'home-row)
         ;; turned onto the exit ramp
         
         (let* ([final-spot (cdr landed)])
           (no-blockades board
                         (next-pos color start)
                         (make-home-row-loc final-spot color))
           
           (let ([old (board-home-row-i board color final-spot)])
             (when (and (pair? old)
                        (pair? (cdr old)))
               (bad-move "cannot move onto a blockade"))
             (values (move-piece board pawn (make-home-row-loc final-spot color))
                     #f)))]
        [else
         ;; stayed on the main board
         (let ([end (cdr landed)])
           (let ([start+1 (modulo (+ start 1) board-main-size)])
             (unless (= start+1 end)
               (no-blockades board start+1 end)))
           (let ([old-contents (board-main-i board end)])
         
             (cond
               ;; no one there
               [(null? old-contents)
                (values (move-piece board pawn end)
                        #f)]
               
               [(and (pair? old-contents)
                     (pair? (cdr old-contents)))
                (bad-move "cannot move directly onto a blockade")]
               
               ;; already one of the same color on that spot
               [(eq? (pawn-color (car old-contents)) color)
                (values (move-piece board
                                    pawn
                                    end)
                        #f)]
               
               ;; attempt to bop on a safety -- illegal
               [(safety? end)
                (bad-move "cannot move onto a safety if someone else is already there")]
               
               ;; successful bop
               [else
                (values
                 (move-piece2 board
                              pawn
                              end
                              (car old-contents)
                              'start)
                 bop-bonus)])))])))
  
  ;; next-pos : color number -> (union number home-row-loc)
  ;; given a position on the main ring, it finds the next position
  ;; for the given color on the board.
  (define (next-pos color pos)
    (cond
      [(= pos (get-exit-pos color))
       (make-home-row-loc color 0)]
      [else
       (modulo (+ pos 1) board-main-size)]))

  ;; find-end-spot : color number number -> (union 'too-far 'home (cons 'home-row number) (cons 'main number)))
  (define (find-end-spot color start distance)
    (let ([exit (get-exit-pos color)]
          [end (modulo (+ start distance) board-main-size)])
      (cond
        [(and (<=/m start exit end)
              (not (= exit end)))
         (let* ([distance-to-exit (modulo (- exit start) board-main-size)]
                [final-spot (- distance distance-to-exit 1)])
           (cond
             [(final-spot . = . board-home-row-size)
              'home]
             [(final-spot . < . board-home-row-size)
              (cons 'home-row final-spot)]
             [else
              'too-far]))]
        [else
         (cons 'main end)])))
           
           
  (define (<=/m one two three)
    (or (<= one two three)
        (<= two three one)
        (<= three one two)))
  
  (define-struct (exn:bad-move exn) ())
  
  (define bad-move
    (case-lambda
      [(str) (raise (make-exn:bad-move str (current-continuation-marks)))]
      [args (raise (make-exn:bad-move (apply format args)
                                      (current-continuation-marks)))])))
