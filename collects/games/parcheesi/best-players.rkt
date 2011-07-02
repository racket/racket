#|

player characteristics
  - all things being equal, prefer to bop opponents (even if no bonus comes from it)
  - player that prefers to set himself up to bop opponents

agressive annie
balanced bob
careful charlie
(random ron)
|#

(module best-players mzscheme
  (require "board.rkt"
           "moves.rkt"
           "interfaces.rkt"
           "test.rkt"
           mzlib/list
           mzlib/etc
           mzlib/class
           mzlib/pretty)
  
  (provide random-player%
           agressive-player%
           careful-player%
           best-player%
           polite-player%
           reckless-player%
           
           search
           (struct state (moves dice board)))

  (define candidates-record '())
  (provide average-move-count)
  (define (average-move-count)
    (if (null? candidates-record)
	'no-games-played
	(list (/ (apply + candidates-record) (length candidates-record) 1.0)
	      (length candidates-record))))
  
  ;; moves : (listof move) -- what got us here
  ;; dice : (listof dice) -- what we have left to use
  ;; board : board -- the state of the board after taking the moves
  (define-struct state (moves dice board) (make-inspector))
  
  (define base-player%
    (class* object% (player<%>)
      (init-field name)
      (define/public (score board) (error 'score "abstract method"))
      
      (field [color #f])
      (define/public (start-game _color)
        (set! color _color)
        name)
      (define/public (doubles-penalty) (void))
      
      (define/public (do-move orig-board unsorted-dice)
        (let ([dice (sort unsorted-dice <)])
          (let* ([before (current-process-milliseconds)]
                 [candidates (search orig-board color dice)]
                 [ms (- (current-process-milliseconds) before)])
	    (set! candidates-record (cons (length candidates) candidates-record))
            (when (ms . > . 4000)
              (printf "\nmsec ~s\n" ms)
              (printf "candidates ~s (avg moves ~s)\n" 
                      (length candidates)
                      (let ([nums (map (lambda (x) (length (state-moves x))) candidates)])
                        (/ (apply + nums)
                           (length nums)
                           1.0)))
              (print-struct #t)
              (printf "color ~s dice ~s\n" color dice)
              (pretty-print orig-board))
            (find-best candidates))))

      (define/private (find-best moves)
        (cond
          [(null? moves) '()]
          [else
           (let loop ([states (cdr moves)]
                      [best (list (car moves))]
                      [best-score (score (state-board (car moves)))])
             (cond
               [(null? states) (state-moves (list-ref best (random (length best))))]
               [else (let* ([state (car states)]
                            [this-score (score (state-board state))])
                       (cond
                         [(= this-score best-score)
                          (loop (cdr states)
                                (cons state best)
                                best-score)]
                         [(< this-score best-score) 
                          (loop (cdr states)
                                best
                                best-score)]
                         [else (loop (cdr states)
                                     (list state)
                                     this-score)]))]))]))
      
      (super-new)))
  
  (define random-player%
    (class base-player%
      (init [name "Random Ron"])
      (define/override (score board) (random 10000))
      (super-new [name name])))
  
  (define agressive-player%
    (class base-player%
      (inherit-field color)
      (define/override (score board) (- 500 (find-distance board color)))
      (super-new [name "Agressive Annie"])))
  
  (define polite-player%
    (class base-player%
      (inherit-field color)
      (define/override (score board) (find-distance board color))
      (super-new [name "Polite Polly"])))
  
  (define best-player%
    (class base-player%
      (inherit-field color)
      (define/override (score board) 
        (let ([dist (- 500 (find-distance board color))]
              [bop-chance (- 36 (find-bop-chance board color))])
          (+ (* dist 100) bop-chance)))
      (super-new [name "Amazing Grace"])))
  
  (define reckless-player%
    (class base-player%
      (inherit-field color)
      (define/override (score board) (find-bop-chance board color))
      (super-new [name "Reckless Renee"])))
  
  (define careful-player%
    (class base-player%
      (inherit-field color)
      (define/override (score board) (- 36 (find-bop-chance board color)))
      (super-new [name "Careful Charlie"])))
  
  ;; search : board color (listof number) -> (listof state)
  (define (search orig-board color dice)
    (define candidate-ht (make-hash-table 'equal))
    (define (move-candidate candidate)
      (hash-table-put! candidate-ht (state-board candidate) candidate))
    (define (get-candidates) (hash-table-map candidate-ht (lambda (x y) y)))
    
    ;; main : -> void
    (define (main)
      ;; ht : board -o> true
      (let ([ht (make-hash-table 'equal)])
        (let loop ([state (make-state '() dice orig-board)])
          (let* ([board (state-board state)]
                 [dice (state-dice state)]
                 [key (cons dice board)])
            (cond
              [(hash-table-get ht key (lambda () #f))
               (void)]
              [else 
               (hash-table-put! ht key #t)
               (let* ([possible-moves (find-moves board dice)]
                      [valid-next-states (find-valid-states state orig-board board possible-moves)])
                 (cond
                   [(null? valid-next-states) (move-candidate state)]
                   [else (for-each loop valid-next-states)]))])))))
    
    (define (find-valid-states state orig-board board moves)
      (let loop ([moves moves])
        (cond
          [(null? moves) null]
          [else (let ([move (car moves)])
                  (with-handlers ([exn:bad-move? (lambda (x) (loop (cdr moves)))])
                    (let-values ([(new-board bonus) (make-one-move board move)])
                      (if (blockade-moved? orig-board new-board color)
                          (loop (cdr moves))
                          (let ([removed-dice (remove-used-rolls move (state-dice state))])
                            (if removed-dice
                                (cons (make-state (append (state-moves state) (list move))
                                                  (if bonus (insert bonus removed-dice) removed-dice)
                                                  new-board)
                                      (loop (cdr moves)))
                                (loop (cdr moves))))))))])))
    
    (define (remove-used-rolls move dice)
      (cond
        [(move-piece-main? move) (mem/rem (move-piece-main-distance move) dice)]
        [(move-piece-home? move) (mem/rem (move-piece-home-distance move) dice)]
        [(enter-piece? move)
         (or (mem/rem 5 dice)
             (and (memq 3 dice)
                  (memq 2 dice)
                  (remq 3 (remq 2 dice)))
             (and (memq 1 dice)
                  (memq 4 dice)
                  (remq 1 (remq 4 dice))))]))
    
    (define (mem/rem x ls) (and (memq x ls) (remq x ls)))
    
    (define (find-moves board dice)
      (let ([moves '()])
        (for-each-pawn/loc
         board
         (lambda (pawn loc)
           (when (eq? (pawn-color pawn) color)
             (cond
               [(eq? loc 'start) (set! moves (cons (make-enter-piece pawn) moves))]
               [(number? loc)
                (set! moves (append (map (lambda (die) (make-move-piece-main pawn loc die))
                                         dice)
                                    moves))]
               [(home-row-loc? loc)
                (set! moves (append (map (lambda (die) (make-move-piece-home pawn (home-row-loc-num loc) die))
                                         dice)
                                    moves))]
               [(eq? loc 'home) (void)]))))
        moves))
    
    (define (extend vec i make-move-piece dice)
      (let ([ent (vector-ref vec i)])
        (cond
          [(and (pair? ent)
                (eq? (pawn-color (car ent)) color))
           (all-moves make-move-piece ent i dice)]
          [else '()])))
    
    (define (all-moves make-move-piece ent i dice)
      (let d-loop ([dice dice])
        (cond
          [(null? dice) '()]
          [else   
           (append
            (let loop ([ent ent])
              (cond
                [(null? ent) null]
                [else (cons (make-move-piece (car ent) i (car dice))
                            (loop (cdr ent)))]))
            (d-loop (cdr dice)))])))
    
    (main)
    (get-candidates))
  
  ;; find-bop-chance : board color -> number[0-36]
  (define (find-bop-chance board color)
    
    ;; add-chances : number[distance] -> void
    (define (add-chances i)
      (case i
        [(1)
         (add-single-roll-chances 1)]
        [(2) 
         (add-single-roll-chances 1)
         (add-chance 1 1)]
        [(3) 
         (add-single-roll-chances 1)
         (add-chance 1 2)]
        [(4) 
         (add-single-roll-chances 1)
         (add-chance 1 3)
         (add-chance 2 2)]
        [(5) 
         (add-single-roll-chances 1)
         (add-chance 1 4)
         (add-chance 2 3)]
        [(6) 
         (add-single-roll-chances 1)
         (add-chance 1 5)
         (add-chance 2 4)
         (add-chance 3 3)]
        [(7) 
         (add-chance 1 6)
         (add-chance 2 5)
         (add-chance 3 4)]
        [(8) 
         (add-chance 2 6)
         (add-chance 3 5)
         (add-chance 4 4)]
        [(9) 
         (add-chance 3 6)
         (add-chance 4 5)]
        [(10) 
         (add-chance 4 6)
         (add-chance 5 5)]
        [(11) 
         (add-chance 5 6)]
        [(12) 
         (add-chance 6 6)]
        [else (void)]))
    
    (define (add-single-roll-chances i)
      (add-chance i 1)
      (add-chance i 2)
      (add-chance i 3)
      (add-chance i 4)
      (add-chance i 5)
      (add-chance i 6))
    
    (define chances (build-vector
                     6
                     (lambda (i) (make-vector 6 #f))))
    (define (add-chance d1 d2)
      (vector-set! (vector-ref chances (- d1 1)) (- d2 1) #t)
      (vector-set! (vector-ref chances (- d2 1)) (- d1 1) #t))

    (let ([my-blockades (map blockade-loc (find-blockades/color board color))])
      (for-each-pawn/loc
       board
       (lambda (pawn my-loc)
         (when (and (eq? (pawn-color pawn) color)
                    (number? my-loc))
           (unless (member my-loc my-blockades)
             (for-each-pawn/loc
              board
              (lambda (pawn their-loc)
                (unless (eq? (pawn-color pawn) color)
                  (cond
                    [(eq? their-loc 'start)
                     (let ([their-enter-spot (get-enter-pos (pawn-color pawn))])
                       ;; this code assumes that the enter-pos's are not within 6 of
                       ;; where the board indices wrap around.
                       (cond
                         [(= my-loc their-enter-spot)
                          (add-single-roll-chances 5)
                          (add-chance 2 3)
                          (add-chance 1 4)]
                         [(= my-loc (+ their-enter-spot 1))
                          (add-chance 5 1)]
                         [(= my-loc (+ their-enter-spot 2))
                          (add-chance 5 2)]
                         [(= my-loc (+ their-enter-spot 3))
                          (add-chance 5 3)]
                         [(= my-loc (+ their-enter-spot 4))
                          (add-chance 5 4)]
                         [(= my-loc (+ their-enter-spot 5))
                          (add-chance 5 5)]
                         [(= my-loc (+ their-enter-spot 6))
                          (add-chance 5 6)]))]
                    [(number? their-loc)
                     (unless (safety? my-loc)
                       (unless (find-blockade/between board their-loc my-loc)
                         (add-chances (dist-from their-loc my-loc))))])))))))))
    
    (length (filter values (apply append (map vector->list (vector->list chances))))))
  
  (define (dist-from them me)
    (cond
      [(< them me) (- me them)]
      [else (- (+ me board-main-size) them)]))
  
  ;; find-distance : board color -> number
  ;; finds the cumulative distance that the pawns have
  ;; to travel before they can make it home.
  (define (find-distance board color)
    (define distance 0)
    
    (for-each-pawn/loc
     board
     (lambda (pawn loc)
       (when (eq? (pawn-color pawn) color)
         (set! distance (+ (find-loc-distance loc color) distance)))))
    distance)
  
  ;; find-loc-distance : loc color -> number
  (define (find-loc-distance loc color)
    (cond
      [(eq? loc 'start) 100]
      [(eq? loc 'home) 0]
      [(number? loc) 
       (let* ([entry (get-enter-pos color)]
              [exit (get-exit-pos color)]
              [sub-from 73]
              [dist-to-entry
               (cond
                 [(<= entry loc) (- loc entry)]
                 [else (- (+ loc board-main-size) entry)])])
         (- sub-from dist-to-entry))]
      [else (- board-home-row-size (home-row-loc-num loc))]))
    
  (define (insert x l)
    (cond
      [(empty? l) (list x)]
      [(<= x (first l)) (cons x l)]
      [else (cons (first l) (insert x (rest l)))]))

  (define everywhere-board
    (make-board (list (make-pawn 'red 2)
                      (make-pawn 'blue 0)
                      (make-pawn 'green 1)
                      (make-pawn 'yellow 3))
                `#68(() () () () () (,(make-pawn 'green 0)) () () 
                        () () () () () () () () 
                        ()
                        () () () () () (,(make-pawn 'red 1)) () ()
                        () () () () () () () () 
                        ()
                        () () () () () (,(make-pawn 'blue 3)) () () 
                        () () () () () () () () 
                        ()
                        () () () () () (,(make-pawn 'yellow 2)) () () 
                        () () () () () () () ())
                (list (cons 'green `#7((,(make-pawn 'green 2)) ()))
                      (cons 'red `#7(() (,(make-pawn 'red 3)) ()))
                      (cons 'blue `#7(() () (,(make-pawn 'blue 1)) ()))
                      (cons 'yellow `#7(() () () (,(make-pawn 'yellow 0)) ())))
                (list (make-pawn 'red 0)
                      (make-pawn 'blue 2)
                      (make-pawn 'green 3)
                      (make-pawn 'yellow 1))))
  
  (define (run-test)
    (define (build-state moves dice board)
      (make-state moves
                  dice
                  (let loop ([moves moves]
                             [board board])
                    (cond
                      [(null? moves) board]
                      [else 
                       (let-values ([(new-board bonus) (make-one-move board (car moves))])
                         (loop (cdr moves) new-board))]))))
    
    (test-list (search (new-board) 'green (list 2 3))
               (list (build-state (list (make-enter-piece (make-pawn 'green 0)))
                                  '()
                                  (new-board))
                     (build-state (list (make-enter-piece (make-pawn 'green 1)))
                                  '()
                                  (new-board))
                     (build-state (list (make-enter-piece (make-pawn 'green 2)))
                                  '()
                                  (new-board))
                     (build-state (list (make-enter-piece (make-pawn 'green 3)))
                                  '()
                                  (new-board))))
    
    (test-list (search (new-board) 'green (list 5 2))
               (list (build-state (list (make-enter-piece (make-pawn 'green 0))
                                        (make-move-piece-main (make-pawn 'green 0) 5 2))
                                  '()
                                  (new-board))
                     (build-state (list (make-enter-piece (make-pawn 'green 1))
                                        (make-move-piece-main (make-pawn 'green 1) 5 2))
                                  '()
                                  (new-board))
                     (build-state (list (make-enter-piece (make-pawn 'green 2))
                                        (make-move-piece-main (make-pawn 'green 2) 5 2))
                                  '()
                                  (new-board))
                     (build-state (list (make-enter-piece (make-pawn 'green 3))
                                        (make-move-piece-main (make-pawn 'green 3) 5 2))
                                  '()
                                  (new-board))))
    
    (test (find-loc-distance 'start 'red) 100)
    (test (find-loc-distance 'home 'red) 0)
    (test (find-loc-distance (make-home-row-loc 1 'red) 'red) 6)
    (test (find-loc-distance (get-enter-pos 'red) 'red) 73)
    (test (find-loc-distance (get-exit-pos 'red) 'red) (- 73
                                                          board-main-size 
                                                          (- (get-exit-pos 'red)
                                                             (get-enter-pos 'red))))

    (test (find-distance (new-board) 'green)
          (* 4 100))
    (test (let-values ([(board bonus) (board-enter-piece (new-board) (make-pawn 'green 0))])
            (find-distance board 'green))
          (+ (* 3 100) 73))
    (test (let-values ([(board bonus) (board-enter-piece (new-board) (make-pawn 'red 0))])
            (find-distance board 'red))
          (+ (* 3 100) 73))
    (test (let-values ([(board bonus) (board-enter-piece (new-board) (make-pawn 'yellow 0))])
            (find-distance board 'yellow))
          (+ (* 3 100) 73))
    (test (let-values ([(board bonus) (board-enter-piece (new-board) (make-pawn 'blue 0))])
            (find-distance board 'blue))
          (+ (* 3 100) 73))
    (test (let-values ([(board bonus) (board-enter-piece (new-board) (make-pawn 'yellow 0))])
            (let-values ([(board bonus) (board-move-piece-main board (make-pawn 'yellow 0) 56 60)])
              (let-values ([(board bonus) (board-move-piece-main board (make-pawn 'yellow 0) 48 3)])
                (find-distance board 'yellow))))
          (+ (* 3 100) 10))
    (test (let-values ([(board bonus) (board-enter-piece (new-board) (make-pawn 'yellow 0))])
            (let-values ([(board bonus) (board-move-piece-main board (make-pawn 'yellow 0) 56 60)])
              (let-values ([(board bonus) (board-move-piece-main board (make-pawn 'yellow 0) 48 4)])
                (find-distance board 'yellow))))
          (+ (* 3 100) 7))
    (test (let-values ([(board bonus) (board-enter-piece (new-board) (make-pawn 'yellow 0))])
            (let-values ([(board bonus) (board-move-piece-main board (make-pawn 'yellow 0) 56 60)])
              (let-values ([(board bonus) (board-move-piece-main board (make-pawn 'yellow 0) 48 11)])
                (find-distance board 'yellow))))
          300)
    (test (let-values ([(board bonus) (board-enter-piece (new-board) (make-pawn 'yellow 0))])
            (let-values ([(board bonus) (board-move-piece-main board (make-pawn 'yellow 0) 56 60)])
              (let-values ([(board bonus) (board-move-piece-main board (make-pawn 'yellow 0) 48 10)])
                (find-distance board 'yellow))))
          301)
    (test (let-values ([(board bonus) (board-enter-piece (new-board) (make-pawn 'yellow 0))])
            (let-values ([(board bonus) (board-move-piece-main board (make-pawn 'yellow 0) 56 40)])
              (find-distance board 'yellow)))
          333)
    
    (test (let-values ([(board bonus) (board-enter-piece (new-board) (make-pawn 'blue 0))])
            (let-values ([(board bonus) (board-enter-piece board (make-pawn 'blue 1))])
              (let-values ([(board bonus) (board-move-piece-main board (make-pawn 'blue 0) 39 1)])
                (let-values ([(board bonus) (board-move-piece-main board (make-pawn 'blue 1) 39 1)])
                  (let-values ([(board bonus) (board-enter-piece board (make-pawn 'blue 2))])
                    (let-values ([(board bonus) (board-enter-piece board (make-pawn 'blue 3))])
                      (find-distance board 'blue)))))))
          (+ 73 73 72 72))

    (test (find-bop-chance (mb) 'green) 0)
    (test (find-bop-chance (mb (make-enter-piece (make-pawn 'green 0))
                               (make-enter-piece (make-pawn 'red 0))
                               (make-move-piece-main (make-pawn 'green 0) 5 16))
                           'green)
          0)
    (test (find-bop-chance (mb (make-enter-piece (make-pawn 'green 0))
                               (make-enter-piece (make-pawn 'green 1))
                               (make-move-piece-main (make-pawn 'green 0) 5 1))
                           'green)
          0)
    (test (find-bop-chance (mb (make-enter-piece (make-pawn 'green 0))
                               (make-move-piece-main (make-pawn 'green 0)
                                                     5
                                                     17))
                           'green)
          15)
    (test (find-bop-chance (mb (make-enter-piece (make-pawn 'green 0))
                               (make-move-piece-main (make-pawn 'green 0)
                                                     5
                                                     18))
                           'green)
          2)
    (test (find-bop-chance (mb (make-enter-piece (make-pawn 'green 0))
                               (make-move-piece-main (make-pawn 'green 0)
                                                     5
                                                     19))
                           'green)
          2)
    (test (find-bop-chance (mb (make-enter-piece (make-pawn 'green 0))
                               (make-move-piece-main (make-pawn 'green 0)
                                                     5
                                                     20))
                           'green)
          2)
    (test (find-bop-chance (mb (make-enter-piece (make-pawn 'green 0))
                               (make-move-piece-main (make-pawn 'green 0)
                                                     5
                                                     21))
                           'green)
          2)
    (test (find-bop-chance (mb (make-enter-piece (make-pawn 'green 0))
                               (make-move-piece-main (make-pawn 'green 0)
                                                     5
                                                     22))
                           'green)
          1)
    (test (find-bop-chance (mb (make-enter-piece (make-pawn 'green 0))
                               (make-move-piece-main (make-pawn 'green 0)
                                                     5
                                                     23))
                           'green)
          2)
    (test (find-bop-chance (mb (make-enter-piece (make-pawn 'green 0))
                               (make-move-piece-main (make-pawn 'green 0)
                                                     5
                                                     27))
                           'green)
          0)
    (test (find-bop-chance (mb (make-enter-piece (make-pawn 'green 0))
                               (make-enter-piece (make-pawn 'red 0))
                               (make-move-piece-main (make-pawn 'green 0) 5 16))
                           'red)
          0)
    (test (find-bop-chance (mb (make-enter-piece (make-pawn 'green 0))
                               (make-enter-piece (make-pawn 'red 0))
                               (make-move-piece-main (make-pawn 'green 0) 5 16)
                               (make-move-piece-main (make-pawn 'red 0) 22 1))
                           'red)
          11)
    
    (test (find-bop-chance (mb (make-enter-piece (make-pawn 'green 0))
                               (make-enter-piece (make-pawn 'red 0))
                               (make-enter-piece (make-pawn 'red 1))
                               (make-move-piece-main (make-pawn 'green 0) 5 16)
                               (make-move-piece-main (make-pawn 'red 0) 22 1))
                           'red)
          11)
    
    (test (find-bop-chance (mb (make-enter-piece (make-pawn 'green 0))
                               (make-enter-piece (make-pawn 'red 0))
                               (make-enter-piece (make-pawn 'red 1))
                               (make-move-piece-main (make-pawn 'green 0) 5 16)
                               (make-move-piece-main (make-pawn 'red 0) 22 1)
                               (make-move-piece-main (make-pawn 'red 1) 22 1))
                           'red)
          0)
    
    (test (find-bop-chance (mb (make-enter-piece (make-pawn 'green 0))
                               (make-enter-piece (make-pawn 'red 0))
                               (make-enter-piece (make-pawn 'red 1))
                               (make-move-piece-main (make-pawn 'green 0) 5 16)
                               (make-move-piece-main (make-pawn 'red 0) 22 1)
                               (make-move-piece-main (make-pawn 'red 1) 22 1)
                               (make-enter-piece (make-pawn 'red 2)))
                           'red)
          0)
    
    (test (find-bop-chance (mb (make-enter-piece (make-pawn 'green 0))
                               (make-enter-piece (make-pawn 'red 0))
                               (make-enter-piece (make-pawn 'red 1))
                               (make-move-piece-main (make-pawn 'green 0) 5 16)
                               (make-move-piece-main (make-pawn 'red 0) 22 1)
                               (make-move-piece-main (make-pawn 'red 1) 22 2)
                               (make-enter-piece (make-pawn 'red 2))
                               (make-move-piece-main (make-pawn 'red 2) 22 1))
                           'red)
          0)
    
    (test-results))

  (define (mb . moves)
    (let-values ([(board bonus) (make-moves (new-board) moves)])
      board))

  #;
  (begin
    (define problem-board
      (make-board (list (make-pawn 'yellow 0)
                        (make-pawn 'blue 2)
                        (make-pawn 'blue 3)
                        (make-pawn 'red 3)
                        (make-pawn 'yellow 3))
                  `#68(() () () () () () () () () () () () 
                          (,(make-pawn 'green 2)) () () () () ()
                          (,(make-pawn 'blue 1)) () 
                          (,(make-pawn 'green 1)) () () () () () 
                          (,(make-pawn 'red 2)) () () () () () 
                          (,(make-pawn 'green 0)) () () () () () 
                          (,(make-pawn 'green 3)) () () () () () () 
                          (,(make-pawn 'blue 0)) () () () () () 
                          (,(make-pawn 'yellow 1)) () 
                          (,(make-pawn 'red 0)) () () () () 
                          (,(make-pawn 'red 1))
                          (,(make-pawn 'yellow 2)) ()) 
                  '((blue . #7(())) (green . #7(())) (red . #7(())) (yellow . #7(()))) 
                  '()))
    
    #|
first cut in new board representation
  cpu time: 42710 real time: 47086 gc time: 1400
  1353 candidates

blockade move checking optimized:
  cpu time: 5580 real time: 5828 gc time: 0
  1353 candidates

use move-piece instead of removing and adding:
  cpu time: 4700 real time: 4939 gc time: 0
  1353 candidates

optimize blockade-in-the-way checking and macro-inline matching-pawns
  cpu time: 2600 real time: 2779 gc time: 0
  1353 candidates

another new board representation (vector of loc's)
  cpu time: 1710 real time: 1799 gc time: 0
  1353 candidates
|#
    
    (let ([candidates (time (search problem-board 'green (list 1 1 6 6)))])
      (printf "~s candidates\n" (length candidates))))

  
  ;color green dice (2 2 5 5)
  ;candidates 2893 (avg moves 6.952298651918424)
  #;
  (begin
    (define problem-board2
      (let loop ([board (new-board)]
                 [ents (list
                        (list (make-pawn 'blue 1) 'start)
                        (list (make-pawn 'red 2) 'start)
                        (list (make-pawn 'yellow 0) 'start)
                        (list (make-pawn 'yellow 1) 'start)
                        (list (make-pawn 'yellow 3) 'start)
                        (list (make-pawn 'green 1) 5)
                        (list (make-pawn 'green 3) 5)
                        (list (make-pawn 'blue 0) 17)
                        (list (make-pawn 'green 0) 21)
                        (list (make-pawn 'red 1) 23)
                        (list (make-pawn 'red 0) 25)
                        (list (make-pawn 'red 3) 26)
                        (list (make-pawn 'green 2) 27)
                        (list (make-pawn 'blue 3) 45)
                        (list (make-pawn 'blue 2) 47)
                        (list (make-pawn 'yellow 2) 63))])
        (cond
          [(null? ents) board]
          [else (loop (move-piece board (car (car ents)) (cadr (car ents)))
                      (cdr ents))])))
    
    #;
    (require xml
             "parse.rkt")
    #;
    (define (dump-out-candidates candidates)
      (let loop ([i 0]
                 [candidates candidates])
        (unless (null? candidates)
          (let ([candidate (car candidates)])
            (call-with-output-file (build-path "problem-board-nexts"
                                               (format "board~a.xml" i))
              (lambda (port)
                (write-xml/content (xexpr->xml (unparse-board (state-board candidate))) port))
              'truncate
              'text))
          (loop (+ i 1) (cdr candidates)))))
  
    (define candidates2 (time (search problem-board2 'green (list 2 2 5 5))))
    ;(dump-out-candidates candidates2)
    (printf "~s candidates\n" (length candidates2)))
  
  
  ;(require "gui.rkt") (show-board problem-board)
  #|
cpu time: 12680 real time: 13855 gc time: 380
candidates 2797 (avg moves 6.9878441186986056)
board #5(struct:board (#3(struct:pawn yellow 0) #3(struct:pawn blue 2) #3(struct:pawn blue 3) #3(struct:pawn red 3) #3(struct:pawn yellow 3)) #68(() () () () () () () () () () () () (#3(struct:pawn green 2)) () () () () () (#3(struct:pawn blue 1)) () (#3(struct:pawn green 1)) () () () () () (#3(struct:pawn red 2)) () () () () () (#3(struct:pawn green 0)) () () () () () (#3(struct:pawn green 3)) () () () () () () (#3(struct:pawn blue 0)) () () () () () (#3(struct:pawn yellow 1)) () (#3(struct:pawn red 0)) () () () () (#3(struct:pawn red 1)) (#3(struct:pawn yellow 2)) ()) ((blue . #7(())) (green . #7(())) (red . #7(())) (yellow . #7(()))) ())
|#

  #| more problem boards:

msec 5280
candidates 2441 (avg moves 6.699303564113069)
color blue dice (1 1 6 6)
#(struct:board
  #16(54 63 39 54 start start start 40 start start 14 start 60 15 6 0))

msec 4750
candidates 2709 (avg moves 6.959025470653378)
color blue dice (1 1 6 6)
#(struct:board
  #16(42 39 53 41 11 25 27 35 start start 40 start start 61 59 60))

msec 4100
candidates 1492 (avg moves 7.138739946380697)
color red dice (3 3 4 4)
#(struct:board
  #16(39
      61
      start
      58
      20
      0
      62
      start
      37
      37
      42
      #(struct:home-row-loc 4 red)
      57
      start
      start
      41))
|#
  )
  
