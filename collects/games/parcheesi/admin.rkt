(module admin mzscheme
  (require "board.rkt"
           "moves.rkt"
           "interfaces.rkt"
           mzlib/class
           mzlib/list)
  
  (provide game%
           game-observer<%>)
  
  (define all-colors '(green red blue yellow))
  
  (define game-observer<%>
    (interface ()
      introduce ;; color string -> void
      taking-turn ;; color dice -> void
      took-turn ;; color board -> void
      game-over)) ;; (union string #f) (union color #f) -> void
  
  (define game%
    (class* object% (game<%>)      
      (define die (new die%))
      (define players '())
      (define colors all-colors)
      (define board (new-board))
      
      (define observer #f)
      (define/public (set-observer wf)
        (unless (is-a? wf game-observer<%>)
          (error 'set-observer "expected a game-observer<%> object, got ~e" wf))
        (set! observer wf))
      
      (define/public (register player)
        (when (null? colors)
          (error 'add-player "cannot add more than four players"))
        (set! players (append players (list (new splayer% (player player) (color (car colors))))))
        (set! colors (cdr colors)))
      
      ;; -> (union player #f)
      ;; #f indicates that everyone cheated
      (define/public (start)
        (unless (= 4 (length players))
          (error 'start "expected 4 players to be registered, but there are ~a" (length players)))

        (for-each (lambda (player) (send player start-game observer)) players)
        
        (let loop ()
          (for-each (lambda (player) 
                      (unless (winner)
                        (take-player-turn player)))
                    players)
          (unless (winner)
            (loop)))
        
        (let ([winner (winner)])
          (when observer
            (if (object? winner)
                (send observer game-over (send winner get-name) (send winner get-color))
                (send observer game-over #f #f)))
          (list (if (object? winner)
                    (send winner get-name)
                    #f)
                (map (lambda (x) (send x get-name))
                     (filter (lambda (x) (send x get-cheated?)) players)))))
      
      (define/private (take-player-turn player)
        (let doubles-loop ([count 1])
          (let-values ([(doubles? roll) (send die roll board (send player get-color))])
            (when observer
              (send observer taking-turn (send player get-color) roll))
            (cond
              [(and doubles? (= count 3))
               (set! board (send player doubles-penalty board))
               (when observer
                 (send observer took-turn (send player get-color) board))]
              [else
               (set! board (send player do-move board roll))
               (when observer
                 (send observer took-turn (send player get-color) board))
               (when doubles?
                 (doubles-loop (+ count 1)))]))))
      
      (define/private (winner)
        (cond
          [(ormap (lambda (player) (and (send player won? board) player))
                  players)
           =>
           (lambda (x) x)]
          [(andmap (lambda (player) (send player get-cheated?))
                   players)
           'everyone-cheated]
          [else #f]))
      
      (super-new)))
  
  (define splayer%
    (class object%
      (init-field [player player]
                  [color color])
      (define name #f)
      (define cheated? #f) 
      (define/public (get-cheated?) cheated?)
      (define/public (get-color) color)
      (define/public (get-name) name)
      (define/public (won? board)
        (equal? 4 (length (filter (lambda (x) (eq? (pawn-color x) color)) (board-home board)))))
      
      (define/private (cheated . args)
        (display (string-append (format "~s cheated! " color)
                                (apply format args)
                                "\n"))
        (set! cheated? #t))
      
      (define/public (start-game observer) 
        (unless cheated?
          (with-handlers ([exn? (lambda (x) (cheated "start-game error ~a" (exn-message x)))])
            (let ([res-name (send player start-game color)])
              (cond
                [(string? res-name)
                 (set! name res-name)
                 (when observer
                   (send observer introduce color name))
                 name]
                [else (cheated "expected a string for the name, got ~s" name)])))))
      
      (define/public (do-move board dice)
        (cond
          [cheated? board]
          [else
           (with-handlers ([exn:bad-move? 
                            (lambda (x)
                              (cheated "~s" (exn-message x))
                              (remove-player board))])
             (let ([moves (with-handlers ([exn? (lambda (x) (list 'error (exn-message x)))])
                            (send player do-move board dice))])
               (cond
                 [(and (list? moves) (andmap move? moves))
                  (take-turn color board dice moves)]
                 [else
                  (cheated "wrong result ~s" moves)
                  (remove-player board)])))]))
      
      (define/private (remove-player board)
        (board-doubles-penalty
         (board-doubles-penalty
          (board-doubles-penalty
           (board-doubles-penalty board color)
           color)
          color)
         color))
                  
      (define/public (doubles-penalty board)
        (cond
          [cheated? board]
          [else
           (with-handlers ([exn? (lambda (x) 
                                   (cheated "doubles-penalty: ~a\n" (exn-message x))
                                   (void))])
             (send player doubles-penalty))
           (board-doubles-penalty board color)]))
      
      (super-new)))
  
  (define die%
    (class object%
      (define/public (roll board color)
        (let* ([die1 (+ 1 (random 6))]
               [die2 (+ 1 (random 6))]
               [doubles? (= die1 die2)])
          (if (and doubles? (board-all-in? board color))
              (values doubles? (list die1 die2 (- 7 die1) (- 7 die2)))
              (values doubles? (list die1 die2)))))
      (super-new))))
