#lang racket

;; This module implements the server for the Hungry Henry game

(provide 
 bon-appetit ;; -> Void 
 ;; launch the server for Hungry Henry 
 )

(require "shared.rkt" 2htdp/universe) 

#| -----------------------------------------------------------------------------
The server is responsible for: 
-- starting the game 
-- moving Henrys 
-- have Henrys eat, remove food on collision 
-- collecting and broadcasting information about the movement of players
-- ending games
|#

;                                                                                      
;                                                                                      
;                                                                                      
;   ;   ;                                            ;   ;                             
;   ;   ;                                            ;   ;                             
;   ;   ;  ;   ;  ; ;;    ;; ;  ; ;;;  ;   ;         ;   ;   ;;;   ; ;;   ; ;;;  ;   ; 
;   ;   ;  ;   ;  ;;  ;  ;  ;;  ;;  ;  ;   ;         ;   ;  ;   ;  ;;  ;  ;;  ;  ;   ; 
;   ;;;;;  ;   ;  ;   ;  ;   ;  ;       ;  ;         ;;;;;  ;   ;  ;   ;  ;       ;  ; 
;   ;   ;  ;   ;  ;   ;  ;   ;  ;       ; ;          ;   ;  ;;;;;  ;   ;  ;       ; ;  
;   ;   ;  ;   ;  ;   ;  ;   ;  ;       ; ;          ;   ;  ;      ;   ;  ;       ; ;  
;   ;   ;  ;  ;;  ;   ;  ;  ;;  ;        ;           ;   ;  ;      ;   ;  ;        ;   
;   ;   ;   ;; ;  ;   ;   ;; ;  ;        ;           ;   ;   ;;;;  ;   ;  ;        ;   
;                            ;           ;                                         ;   
;                         ;;;          ;;                                        ;;    
;                                                                                      


;; Init Constants
(define TICK .1)
(define PLAYER-LIMIT 2)
(define START-TIME 0)
(define WAIT-TIME 250)

(define FOOD*PLAYERS 5)

(define WEIGHT-FACTOR 2.1)
(define BASE-SPEED (/ (expt PLAYER-SIZE 2) WEIGHT-FACTOR))

;; Data Definitions 
(struct join (clients [time #:mutable]) #:transparent)
(struct play (players food spectators) #:transparent #:mutable)

;; plus some update primitives: 

;; JoinUniverse Player -> JoinUniverse
(define (join-add-player j new-p)
  (join (cons new-p (join-clients j)) (join-time j)))

;; PlayUniverse IP -> PlayUniverse
(define (play-add-spectator pu new-s)
  (define players (play-players pu))
  (define spectators (play-spectators pu))
  (play players (play-food pu) (cons new-s spectators)))

;; PlayUniverse IWorld -> PlayUniverse
;; removes player that uses iworld
(define (play-remove p iw)
  (define players (play-players p))
  (define spectators (play-spectators p))
  (play (rip iw players) (play-food p) (rip iw spectators)))

;; JoinUniverse IWorld -> JoinUniverse 
;; removes players and spectators that use iw from this world 
(define (join-remove j iw)
  (join (rip iw (join-clients j)) (join-time j)))

;; IWorld [Listof Player] -> [Listof Player]
;; remove player that contains the given IWorld 
(define (rip iw players)
  (remove iw players (lambda (iw p) (iworld=? iw (ip-iw p)))))

;; LIKE: 
;; (struct ip ip? ip-id ip-iw ip-body ip-waypoints ip-player)
(define-values 
  (ip ip? ip-id ip-iw ip-body ip-waypoints ip-player)
  (let ()
    (struct ip (id iw body waypoints player) #:transparent)
    (define (create iw id body waypoints)
      (ip id iw body waypoints (player id body waypoints)))
    (values 
     create ip? ip-id ip-iw ip-body ip-waypoints ip-player)))

;; ServerState is one of
;; -- JoinUniverse
;; -- PlayUniverse
;; JoinUniververse = (join [Listof IPs] Nat)
;; interpretation: 
;; -- the first field lists the currently connected client-player
;; -- the second field is the number of ticks since the server started
;; PlayUniverse    = (play [Listof IPs] [Listof Food] [Listof IP])
;; interpretation: 
;; -- the first field lists all participating players
;; -- the second field lists the cupcakes 
;; --- the third field enumerates the spectating players 
;; IP              = (ip Id IWorld Body [Listof Complex] Feaster)
;; interpretation: 
;; the struct represents the Universe's perspective of a connected player 
;; -- the first field is the assigned unique Id 
;; -- the second field is the IWorld representing the remote connection to the client 
;; -- the third field is the Body of the player 
;; -- the fourth field is the list of player-chosen Waypoints, 
;;     ordered from oldest click to most-recent
;;     meaning the first one has to be visited first by the Henry 
;; -- the fifth field is the serialized representation of the first four fields 

(define JOIN0 (join empty START-TIME))

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

(define (bon-appetit) 
  (universe JOIN0 
            (on-new connect)
            (on-msg handle-goto-message)
            (on-tick tick-tock TICK)
            (on-disconnect disconnect)))

;; ServerState IWorld -> Bundle
;; adds a new connection to a JoinUniverse and ticks. Ignores otherwise
(define (connect s iw)
  (cond [(join? s) (add-player s iw)]
        [(play? s)   (add-spectator s iw)]))

;; ServerState IWorld Sexpr -> Bundle
;; accepts goto messages from clients
(define (handle-goto-message s iw msg)
  (cond [(and (play? s) (goto? msg)) (goto s iw msg)]
        [else                        (empty-bundle s)]))

;; ServerState -> Bundle
;; handle a tick event
(define (tick-tock s)
  (cond [(join? s) (wait-or-play s)]
        [(play? s) (move-and-eat s)]))

;; ServerState IWorld -> Bundle
;; handles loss of a client
(define (disconnect s iw)
  (cond [(join? s) (drop-client s iw)]
        [(play? s) (drop-player s iw)]))

;                                                   
;                                                   
;                                                   
;  ;     ;          ;             ;                 
;  ;     ;                ;                         
;  ;     ; ;;;;   ;;;    ;;;;;  ;;;    ; ;;    ;; ; 
;  ;  ;  ;     ;    ;     ;       ;    ;;  ;  ;  ;; 
;  ;  ;  ;     ;    ;     ;       ;    ;   ;  ;   ; 
;   ;; ;;   ;;;;    ;     ;       ;    ;   ;  ;   ; 
;   ;; ;;  ;   ;    ;     ;       ;    ;   ;  ;   ; 
;   ;   ;  ;  ;;    ;     ;       ;    ;   ;  ;  ;; 
;   ;   ;   ;;  ;   ;      ;;;    ;    ;   ;   ;; ; 
;                                                 ; 
;                                              ;;;  
;                                                   

;; JoinUniverse -> Bundle
;; count down and might transition
(define (wait-or-play j)
  (cond [(keep-waiting? j) (keep-waiting j)]
        [else              (start-game j)]))

;; JoinUniverse -> Boolean
;; is it time to start?
(define (keep-waiting? j)
  (or (> PLAYER-LIMIT (length (join-clients j)))
      (> WAIT-TIME (join-time j))))

;; JoinUniverse -> [Bundle JoinUniverse]
(define (keep-waiting j)
  (set-join-time! j (+ (join-time j) 1))
  (time-broadcast j))

;; JoinUniverse -> [Bundle JoinUniverse]
;; broadcasts the new load time fraction to the players
(define (time-broadcast j)
  (define iworlds (map ip-iw (join-clients j)))
  (define load%   (min 1 (/ (join-time j) WAIT-TIME)))
  (make-bundle j (broadcast iworlds load%) empty))

;; JoinUniverse -> [Bundle PlayUniverse]
;; starts the game
(define (start-game j)
  (define clients  (join-clients j))
  (define cupcakes (bake-cupcakes (length clients)))
  (broadcast-universe (play clients cupcakes empty)))

;; Number -> [Listof Food]
;; creates the amount of food for that number of players
(define (bake-cupcakes player#)
  (for/list ([i (in-range (* player# FOOD*PLAYERS))])
    (create-a-body CUPCAKE)))

;                                                   
;                                                   
;          ;;;                                      
;   ;;;;     ;                    ;                 
;   ;   ;    ;                                      
;   ;   ;    ;    ;;;;   ;   ;  ;;;    ; ;;    ;; ; 
;   ;  ;     ;        ;  ;   ;    ;    ;;  ;  ;  ;; 
;   ;;;      ;        ;   ;  ;    ;    ;   ;  ;   ; 
;   ;        ;     ;;;;   ; ;     ;    ;   ;  ;   ; 
;   ;        ;    ;   ;   ; ;     ;    ;   ;  ;   ; 
;   ;        ;    ;  ;;    ;      ;    ;   ;  ;  ;; 
;   ;        ;     ;;  ;   ;      ;    ;   ;   ;; ; 
;                          ;                      ; 
;                        ;;                    ;;;  
;                                                   

;; PlayUniverse -> Bundle
;; moves everything. eats. may end game
(define (move-and-eat pu)
  (define nplayers  (move-player* (play-players pu)))
  (define nfood (feed-em-all nplayers (play-food pu)))
  (progress nplayers nfood (play-spectators pu)))

;; [Listof IP] -> [Listof IP]
;; moves all players
(define (move-player* players)
  (for/list ([p players])
    (define waypoints (ip-waypoints p))
    (cond [(empty? waypoints) p]
          [else (define body  (ip-body p))
                (define nwpts 
                  (move-toward-waypoint body waypoints))
                (ip (ip-iw p) (ip-id p) body nwpts)])))

;; Body [Listof Complex] -> [Listof Complex]
;; effect: set body's location 
;; determine new waypoints for player 
;; pre: (cons? waypoints)
(define (move-toward-waypoint body waypoints)
  (define goal  (first waypoints))
  (define bloc  (body-loc body))
  (define line  (- goal bloc))
  (define dist  (magnitude line))
  (define speed (/ BASE-SPEED (body-size body)))
  (cond
    [(<= dist speed) 
     (set-body-loc! body goal)
     (rest waypoints)]
    [else ; (> distance speed 0)
     (set-body-loc! body (+ bloc (* (/ line dist) speed)))
     waypoints]))

;; [Listof Player] [Listof Food] -> [Listof Food]
;; feeds all players and removes food
(define (feed-em-all players foods)
  (for/fold ([foods foods]) ([p players])
    (eat-all-the-things p foods)))

;; IP [Listof Food] -> [Listof Food]
;; effect: fatten player as he eats 
;; determine left-over foods 
(define (eat-all-the-things player foods)
  (define b (ip-body player))
  (for/fold ([foods '()]) ([f foods])
    (cond
      [(body-collide? f b)
       (set-body-size! b (+ PLAYER-FATTEN-DELTA (body-size b)))
       foods]
      [else (cons f foods)])))

;; body body -> Boolean
;; Have two bodys collided?
(define (body-collide? s1 s2)
  (<= (magnitude (- (body-loc s1) (body-loc s2)))
      (+ (body-size s1) (body-size s2))))

;; [Listof Ip] [Listof Food] [Listof IP] -> Bundle
;; moves all objects. may end game
(define (progress pls foods spectators)
  (define p (play pls foods spectators))
  (cond [(empty? foods) (end-game-broadcast p)]
        [else (broadcast-universe p)]))

;; PlayUniverse -> [Bundle JoinUniverse]
;; ends the game, and restarts it
(define (end-game-broadcast p)
  (define iws (get-iws p))
  (define msg (list SCORE (score (play-players p))))
  (define mls (broadcast iws msg)) 
  (make-bundle (remake-join p) mls empty))

;; Play-Universe -> JoinUniverse
;; Readies the ServerState for a new game
(define (remake-join p)
  (define players (refresh (play-players p)))
  (define spectators (play-spectators p))
  (join (append players spectators) START-TIME))

;; [Listof Players] -> [Listof Players]
;; creates new players for new game
(define (refresh players)
  (for/list ([p players])
    (create-player (ip-iw p) (ip-id p))))

;; [Listof IP] -> [Listof (list Id Score)]
;; makes the endgame message informing clients of all the size
(define (score ps)
  (for/list ([p ps])
    (list (ip-id p) (get-score (body-size (ip-body p))))))

;                                                                  
;                                                                  
;                                                                  
;                                                                  
;   ;;; ;;;                                                        
;    ;; ;;                                                         
;    ;; ;;   ;;;;    ;;;;;   ;;;;;   ;;;;    ;;; ;;  ;;;;    ;;;;; 
;    ; ; ;  ;    ;  ;    ;  ;    ;  ;    ;  ;   ;;  ;    ;  ;    ; 
;    ; ; ;  ;;;;;;   ;;;;    ;;;;    ;;;;;  ;    ;  ;;;;;;   ;;;;  
;    ;   ;  ;            ;       ;  ;    ;  ;    ;  ;            ; 
;    ;   ;  ;       ;    ;  ;    ;  ;   ;;  ;   ;;  ;       ;    ; 
;   ;;; ;;;  ;;;;;  ;;;;;   ;;;;;    ;;; ;;  ;;; ;   ;;;;;  ;;;;;  
;                                                ;                 
;                                            ;;;;                  
;                                                                  
;                                                                  

;; -----------------------------------------------------------------------------
;; Play Universe

;; Message -> Boolean
;; checks if message is a drag
(define (goto? msg)
  (and (list? msg)
       (= GOTO-LENGTH (length msg))
       (symbol? (first msg))
       (number? (second msg))
       (number? (third msg))
       (symbol=? GOTO (first msg))
       (<= 0 (second msg) WIDTH)
       (<= 0 (third msg) HEIGHT)))

;; PlayUniverse IWorld GotoMessage -> PlayUniverse
;; handles a player clicking. checks for collisions, updates score, removes food
;; Effect: changes a player's waypoints
(define (goto p iw msg)
  (define c (make-rectangular (second msg) (third msg)))
  (set-play-players! p (add-waypoint (play-players p) c iw))
  (broadcast-universe p))

;; [Listof IPs] Complex IWorld -> [Listof IPs]
;; adds that complex to the waypoints of the given players
(define (add-waypoint ps c iw)
  (for/list ([p ps])
    (cond [(iworld=? (ip-iw p) iw)
           (ip (ip-iw p)
               (ip-id p) 
               (ip-body p) 
               (append (ip-waypoints p) (list c)))]
          [else p])))

;                                                                                  
;                                                                                  
;                                                                                  
;                                                                                  
;     ;;;;                                                     ;                   
;    ;   ;                                           ;                             
;   ;        ;;;;   ;; ;;   ;; ;;    ;;;;    ;;; ;  ;;;;;    ;;;     ;;;;   ;; ;;  
;   ;       ;    ;   ;;  ;   ;;  ;  ;    ;  ;   ;;   ;         ;    ;    ;   ;;  ; 
;   ;       ;    ;   ;   ;   ;   ;  ;;;;;;  ;        ;         ;    ;    ;   ;   ; 
;   ;       ;    ;   ;   ;   ;   ;  ;       ;        ;         ;    ;    ;   ;   ; 
;    ;   ;  ;    ;   ;   ;   ;   ;  ;       ;    ;   ;   ;     ;    ;    ;   ;   ; 
;     ;;;    ;;;;   ;;; ;;; ;;; ;;;  ;;;;;   ;;;;     ;;;    ;;;;;   ;;;;   ;;; ;;;
;                                                                                  
;                                                                                  
;                                                                                  
;                                                                                  


;; -----------------------------------------------------------------------------
;; Join Universe

;; [Universe Player -> Universe] -> [Universe IWorld -> [Bundle Universe]]
;; creates a function that deals with a new connection during join or play phase 
(define (make-connection adder)
  (lambda (u iw)
    (define player (named-player iw))
    (define mails  (list (make-mail iw (ip-id player))))
    (make-bundle (adder u player) mails empty)))

;; JoinUniverse IWorld ID -> [Bundle JoinUniverse]
;; creates an internal player for the IWorld, adds it to Universe as waiting player 
(define add-player (make-connection join-add-player))

;; PlayUniverse IWorld -> [Bundle PlayUniverse]
;; creates an internal player for the IWorld, adds it to Universe as spectator 
(define add-spectator (make-connection play-add-spectator))

;; [Listof IP] IWorld ->* Player 
(define (named-player iw)
  (create-player iw (symbol->string (gensym (iworld-name iw)))))

;                                                                          
;                                                                          
;                                                                          
;                                                                          
;    ;;; ;                     ;              ;;       ;                   
;   ;   ;;                                     ;                           
;   ;        ;;;;   ;; ;;;   ;;;     ;;;;      ;     ;;;     ;;;;;   ;;;;  
;    ;;;;   ;    ;   ;;        ;    ;    ;     ;       ;     ;  ;   ;    ; 
;        ;  ;;;;;;   ;         ;     ;;;;;     ;       ;       ;    ;;;;;; 
;        ;  ;        ;         ;    ;    ;     ;       ;      ;     ;      
;   ;;   ;  ;        ;         ;    ;   ;;     ;       ;     ;   ;  ;      
;   ; ;;;    ;;;;;  ;;;;;    ;;;;;   ;;; ;;  ;;;;;   ;;;;;   ;;;;;   ;;;;; 
;                                                                          
;                                                                          
;                                                                          
;                                                                          

;; PlayUniverse -> [Bundle PlayUniverse [Listof [Mail StateMessage]]]
;; bundle this universe, serialize it, broadcast it, and drop noone
(define (broadcast-universe p)
  (define mails (broadcast (get-iws p) (serialize-universe p)))
  (make-bundle p mails empty))

;; [Listof IWorlds] Message -> [Listof Mail]
;; sends mail to all clients
(define (broadcast iws msgs)
  (map (lambda (iw) (make-mail iw msgs)) iws))

;; PlayUniverse -> (list s [Listof SerializedPlayer] [Listof SerializedFood])
;; prepairs a message for an update world/ServerState state
(define (serialize-universe p)
  (define serialized-players (map ip-player (play-players p)))
  (list SERIALIZE serialized-players (play-food p)))

;                                                                                  
;                                                                                  
;                                                                                  
;                                                                                  
;   ;;;;       ;                                                                   
;    ;  ;                                                                    ;     
;    ;   ;   ;;;     ;;;;;   ;;; ;   ;;;;   ;; ;;   ;; ;;    ;;;;    ;;; ;  ;;;;;  
;    ;   ;     ;    ;    ;  ;   ;;  ;    ;   ;;  ;   ;;  ;  ;    ;  ;   ;;   ;     
;    ;   ;     ;     ;;;;   ;       ;    ;   ;   ;   ;   ;  ;;;;;;  ;        ;     
;    ;   ;     ;         ;  ;       ;    ;   ;   ;   ;   ;  ;       ;        ;     
;    ;  ;      ;    ;    ;  ;    ;  ;    ;   ;   ;   ;   ;  ;       ;    ;   ;   ; 
;   ;;;;     ;;;;;  ;;;;;    ;;;;    ;;;;   ;;; ;;; ;;; ;;;  ;;;;;   ;;;;     ;;;  
;                                                                                  
;                                                                                  
;                                                                                  
;                                                                                  

;; JoinUniverse IWorld -> Bundle
;; remove that iworld from list of clients
(define (drop-client j iw)
  (empty-bundle (join-remove j iw)))

;; PlayUniverse IWorld -> Bundle
;; removes a player from the ServerState and tells the players
(define (drop-player p iw)
  (broadcast-universe (play-remove p iw)))

;                          
;                          
;                          
;                          
;     ;;                   
;      ;                   
;     ; ;   ;;  ;;  ;;  ;; 
;     ; ;    ;   ;   ;  ;  
;     ; ;    ;   ;    ;;   
;     ;;;    ;   ;    ;;   
;    ;   ;   ;  ;;   ;  ;  
;   ;;; ;;;   ;; ;; ;;  ;; 
;                          
;                          
;                          
;                          

;; Number -> Body
;; creates a random body, that does not touch the edge
(define (create-a-body size)
  (define x (+ size (random (- WIDTH size))))
  (define y (+ size (random (- HEIGHT size))))
  (body size (make-rectangular x y)))

;; PlayUniverse -> [Listof IWorlds]
;; gets the iworlds of all players
(define (get-iws p)
  (map ip-iw (append (play-players p) (play-spectators p))))

;; ServerState -> Bundle
;; makes a bundle that sends no messages and disconnects noone
(define (empty-bundle s)
  (make-bundle s empty empty))  

;; IWorld Id -> IP
;; creates a player with that idnumber
(define (create-player iw n)
  (ip iw n (create-a-body PLAYER-SIZE) empty))

;                                          
;                                          
;                                          
;                                          
;   ;;;;;;;                                
;   ;  ;  ;                  ;             
;      ;     ;;;;    ;;;;;  ;;;;;    ;;;;; 
;      ;    ;    ;  ;    ;   ;      ;    ; 
;      ;    ;;;;;;   ;;;;    ;       ;;;;  
;      ;    ;            ;   ;           ; 
;      ;    ;       ;    ;   ;   ;  ;    ; 
;     ;;;    ;;;;;  ;;;;;     ;;;   ;;;;;  
;                                          
;                                          
;                                          
;           

(module+ test 
  (require rackunit rackunit/text-ui)
  
  (define PROP-NUM 500)
  (define do-prop (make-parameter #t))
  (do-prop #f)
  
  ;; thunk -> void
  ;; runs the thunk PROP-NUM times
  (define (check-property t)
    (when (do-prop) (test-begin (doo PROP-NUM t))))
  
  ;; doo : number thunk ->
  ;; does the thunk n times
  (define (doo n l)
    (l)
    (unless (zero? n)
      (doo (sub1 n) l)))
  
  ;; testing main server
  
  ;; new-connection
  
  ;; drop-client
  (check-equal? (drop-client (join (list (ip iworld1 "player1" (body 10 1+10i) empty)
                                         (ip iworld2 "player2" (body 10 1+10i) empty)
                                         (ip iworld3 "player3" (body 10 1+10i) empty)) 100)
                             iworld1)
                (empty-bundle (join (list (ip iworld2 "player2" (body 10 1+10i) empty)
                                          (ip iworld3 "player3" (body 10 1+10i) empty))
                                    100)))
  (check-equal? (drop-client (join (list (ip iworld1 "player1" (body 10 1+10i) empty)
                                         (ip iworld2 "player2" (body 10 1+10i) empty)
                                         (ip iworld3 "player3" (body 10 1+10i) empty)) 100)
                             iworld2)
                (empty-bundle (join (list (ip iworld1 "player1" (body 10 1+10i) empty)
                                          (ip iworld3 "player3" (body 10 1+10i) empty)) 100)))
  (check-equal? (drop-client (join (list (ip iworld1 "player1" (body 10 1+10i) empty)
                                         (ip iworld2 "player2" (body 10 1+10i) empty)
                                         (ip iworld3 "player3" (body 10 1+10i) empty)) 100)
                             iworld3)
                (empty-bundle (join (list (ip iworld1 "player1" (body 10 1+10i) empty)
                                          (ip iworld2 "player2" (body 10 1+10i) empty)) 100)))
  
  ;; remove-player
  (check-equal? (drop-player
                 (play (list (ip iworld1 "player10" (body 10 1+10i) empty)
                             (ip iworld2 "player345" (body 56 3+45i) empty))
                       (list (body 87 67+23i)
                             (body 5 3+4i))
                       empty)
                 iworld1)
                (let ([remd (play-remove 
                             (play (list (ip iworld1 "player10" (body 10 1+10i) empty)
                                         (ip iworld2 "player345" (body 56 3+45i) empty))
                                   (list (body 87 67+23i)
                                         (body 5 3+4i))
                                   empty)
                             iworld1)])
                  (broadcast-universe remd)
                  #;
                  (make-bundle remd (serial/broadcast-univ remd) empty)))
  
  (check-equal? (drop-player 
                 (play (list (ip iworld2 "player345" (body 56 3+45i) empty))
                       (list (body 87 67+23i)
                             (body 5 3+4i))
                       (list (ip iworld1 "player10" (body 10 1+10i) empty)))
                 iworld1)
                (let ([remd (play-remove
                             (play (list (ip iworld1 "player10" (body 10 1+10i) empty)
                                         (ip iworld2 "player345" (body 56 3+45i) empty))
                                   (list (body 87 67+23i)
                                         (body 5 3+4i))
                                   empty)
                             iworld1)])
                  (broadcast-universe remd)
                  #;
                  (make-bundle remd (serial/broadcast-univ remd) empty)))
  
  ;; ready-to-go
  (check-false (keep-waiting? (join (list (create-player iworld1 "player")
                                          (create-player iworld2 "player"))
                                    250)))
  (check-false (keep-waiting? (join (list (create-player iworld1 "player")
                                          (create-player iworld1 "player")
                                          (create-player iworld2 "player"))
                                    456345132135213))) 
  (check-true  (keep-waiting? (join (list (create-player iworld2 "player")) -234)))
  (check-true  (keep-waiting? (join (list  (create-player iworld2 "player")) 10)))
  
  
  
  ;; handle-join
  ;; name
  ;; update-player
  
  ;; remove-player-by-iworld
  (check-equal? (play-remove
                 (play (list (ip iworld1 "player10" (body 10 1+10i) empty)
                             (ip iworld2 "player324" (body 56 3+45i) empty))
                       (list (body 87 67+23i)
                             (body 5 3+4i))
                       empty)
                 iworld1)
                (play (list (ip iworld2 "player324" (body 56 3+45i) empty))
                      (list (body 87 67+23i)
                            (body 5 3+4i))
                      empty)
                empty)
  (check-equal? (play-remove
                 (play (list (ip iworld2 "player324" (body 56 3+45i) empty))
                       (list (body 87 67+23i)
                             (body 5 3+4i))
                       empty)
                 iworld2)
                (play (list)
                      (list (body 87 67+23i)
                            (body 5 3+4i))
                      empty))
  
  ;; testing messaging
  
  ;; goto?
  
  (check-true (goto? '(goto 3 2)))
  (check-true (goto? '(goto 3 2)))
  (check-true (goto? '(goto 0 2)))
  (check-true (goto? '(goto 6 2)))
  (check-false (goto? `(goto ,(add1 WIDTH) 0)))
  (check-false (goto? `(goto 0 ,(add1 HEIGHT))))
  (check-false (goto? '(goto -1 0)))
  (check-false (goto? '(goto 0 -1)))
  (check-false (goto? '(goto 1)))
  (check-false (goto? '(drag 6+2i)))
  (check-false (goto? '(drag 1)))
  (check-false (goto? '(6+1i)))
  (check-false (goto? '(1 2)))
  (check-false (goto? '(goto 6+2i)))
  (check-false (goto? '(drag 1 2)))
  (check-false (goto? 'click))
  (check-false (goto? "click"))
  (check-false (goto? #t))
  
  ;;add-waypoint
  
  (check-equal? (add-waypoint `(,(ip iworld1 "player10" (body 10 1+10i) empty)) 8+9i iworld1)
                (list (ip iworld1 "player10" (body 10 1+10i) '(8+9i))))
  (check-equal? (add-waypoint `(,(ip iworld1 "player10" (body 10 1+10i) '(23+45i))) 8+9i iworld1)
                (list (ip iworld1 "player10" (body 10 1+10i) '(23+45i 8+9i))))
  
  ;; goto
  
  (check-equal? (goto (play (list (ip iworld1 "player10" (body 10 1+10i) empty)
                                  (ip iworld2 "player345" (body 56 3+45i) empty))
                            (list (body 87 67+23i)
                                  (body 5 3+4i))
                            empty)
                      iworld1 '(goto 1 1))
                (let ([state (play (list (ip iworld1 "player10" (body 10 1+10i)'(1+1i))
                                         (ip iworld2 "player345" (body 56 3+45i) empty))
                                   (list (body 87 67+23i)
                                         (body 5 3+4i))
                                   empty)])
                  (broadcast-universe state)
                  #;
                  (make-bundle state (serial/broadcast-univ state) empty)))
  
  (check-equal? (goto (play (list (ip iworld1 "player10" (body 10 1+10i) '(1+4i))
                                  (ip iworld2 "player345" (body 56 3+45i) empty))
                            (list (body 87 67+23i)
                                  (body 5 3+4i))
                            empty)
                      iworld1 '(goto 1 1))
                (let ([state (play (list (ip iworld1 "player10" (body 10 1+10i) '(1+4i 1+1i))
                                         (ip iworld2 "player345" (body 56 3+45i) empty))
                                   (list (body 87 67+23i)
                                         (body 5 3+4i))
                                   empty)])
                  (broadcast-universe state)
                  #;
                  (make-bundle state (serial/broadcast-univ state) empty)))
  
  ;; eat-all-the-things
  (check-equal? (eat-all-the-things  (ip iworld1 "player10" (body 11 0) '(1+10i)) (list (body 10 0)))
                empty)
  (check-equal? (eat-all-the-things  (ip iworld1 "player10" (body 10 0) '(1+10i)) (list (body 10 40+5i)))
                (list (body 10 40+5i)))
  
  ;; testing initialization
  
  ;; property of no motion to same point in move-body
  ;; also checks for divide by zero error in move-player*
  (define (property:no-same-point)
    (define (random-near n)
      (define ε 1/1000000)
      (+ n (* (random 10) ε (sub1 (* 2 (random 2))))))
    
    (define test-body (create-a-body 1))
    
    (define waypoints
      (for/list ([r (in-range (add1 (random 100)))])
        (define x (real-part (body-loc test-body)))
        (define y (imag-part (body-loc test-body)))
        (make-rectangular (random-near x) (random-near y))))
    
    (define random-p (ip iworld1 "nope" test-body waypoints))
    
    (define (test p)
      (cond [(empty? (ip-waypoints p))
             #t]
            [(= (first (ip-waypoints p)) 
                (body-loc (ip-body p)))
             #f]
            [else (test (move-player* (list p)))]))
    
    (check-true (test random-p)))
  
  ;; does spawn food create the nessecary amount of food?
  (define (property:player/food-number-correct)
    (define players (random 50))
    (check-equal? (length (bake-cupcakes players))
                  (* FOOD*PLAYERS players)))
  
  ;; is random-body on the board?
  (define (test-body-in-bounds)
    (define size 10)
    (define body (create-a-body size))
    (check-true (and (< size (real-part (body-loc body)) (- WIDTH size))
                     (< size (imag-part (body-loc body)) (- HEIGHT size)))
                "body out of bounds"))
  
  
  
  
  ;;create-name
  ;; (check-equal? (create-name empty "john") "john")
  ;; (check-equal? (create-name (list (ip iworld1 "player10" (body 10 0) '(1+10i))) "player10") "player10*")
  #;
  (check-equal? (create-name (list (ip iworld1 "player10" (body 10 0) '(1+10i))
                                   (ip iworld1 "player10*" (body 10 0) '(1+10i)))
                             "player10")
                "player10**")
  #;
  (check-property property:unique-name)
  
  ;; spawn-food
  (check-property property:player/food-number-correct)
  
  ;; random-body
  (check-property test-body-in-bounds)
  
  ;; testing clock tick handling
  
  (define tbody1 (body 100 1+3i))
  (define tbody2 (body 100 1))
  (define tbody3 (body 100 0+3i))
  (define tbody4 (body 100 101))
  
  (define waypoints1 '(1+3i 1 0+3i 10+10i))
  (define waypoints2 '(100))
  
  ;; move-player*
  (check-equal? (move-player*
                 (list (ip iworld1 "player10" (body 10 1+10i) '(1+10.01i))))
                (list (ip iworld1 "player10" (body 10 1+10.01i) empty)))
  (check-property property:no-same-point)
  ;; move-twards-waypoint
  
  
  (test-begin
   (check-equal? (move-toward-waypoint tbody1 waypoints1)
                 (rest waypoints1)
                 "waypoint removal failed")
   (check-equal? tbody1 (body 100 1+3i) "movement failed")
   (set! tbody1 (body 100 1+3i)))
  
  (test-begin
   ;; test dependent on (< BASE-SPEED 100)
   (check-equal? (move-toward-waypoint tbody2 waypoints2)
                 waypoints2
                 "waypoint removal failed")
   (check-equal? tbody2 (body 100 (+ 1 (make-rectangular (/ BASE-SPEED 100) 0)))
                 "movement failed")
   (set! tbody2 (body 100 1)))
  
  (test-begin
   (check-equal? (move-toward-waypoint tbody4 waypoints2)
                 '())
   (check-equal? tbody4 (body 100 100))
   (set! tbody4 (body 100 101)))
  
  ;; countdown
  (check-equal? (wait-or-play (join (list (ip iworld1 "player10" (body 10 1+10i) empty))  0))
                (make-bundle
                 (join (list (ip iworld1 "player10" (body 10 1+10i) empty)) 1)
                 (broadcast (list iworld1) (/ 1 WAIT-TIME))
                 empty))
  (check-equal? (wait-or-play (join empty  0))
                (empty-bundle (join empty 1)))
  
  ;;countdown
  (check-equal? (wait-or-play (join (list (ip iworld1 "player10" (body 10 1+10i) empty)
                                          (ip iworld1 "player345" (body 56 3+45i) empty))
                                    100))
                (make-bundle (join (list (ip iworld1 "player10" (body 10 1+10i) empty)
                                         (ip iworld1 "player345" (body 56 3+45i) empty))
                                   101)
                             (broadcast (list iworld1 iworld1) (/ 101 WAIT-TIME))
                             empty))
  (check-equal? (wait-or-play (join (list (ip iworld1 "player10" (body 10 1+10i) empty)
                                          (ip iworld1 "player345" (body 56 3+45i) empty))
                                    1))
                (make-bundle (join (list (ip iworld1 "player10" (body 10 1+10i) empty)
                                         (ip iworld1 "player345" (body 56 3+45i) empty))
                                   2)
                             (broadcast (list iworld1 iworld1) (/ 2 WAIT-TIME))
                             empty))
  ;; progress
  (check-equal? (progress 
                 (list (ip iworld1 "player10" (body 10 1+10i) empty)
                       (ip iworld1 "player345" (body 56 3+45i) empty))
                 (list (body 87 67+23i)
                       (body 5 3+4i))
                 empty)
                (broadcast-universe 
                 (play (list (ip iworld1 "player10" (body 10 1+10i) empty)
                             (ip iworld1 "player345" (body 56 3+45i) empty))
                       (list (body 87 67+23i)
                             (body 5 3+4i))
                       empty))
                #;
                (make-bundle
                 (play (list (ip iworld1 "player10" (body 10 1+10i) empty)
                             (ip iworld1 "player345" (body 56 3+45i) empty))
                       (list (body 87 67+23i)
                             (body 5 3+4i))
                       empty)
                 (serial/broadcast-univ (play (list (ip iworld1 "player10" (body 10 1+10i) empty)
                                                    (ip iworld1 "player345" (body 56 3+45i) empty))
                                              (list (body 87 67+23i)
                                                    (body 5 3+4i))
                                              empty))
                 empty))
  
  ;; body-collide?
  (check-true (body-collide? (body 10 10+10i) (body 10 10+10i)))
  (check-true (body-collide? (body 10 10+10i) (body 10 0+10i)))
  (check-true (body-collide? (body 10 10+10i) (body 10 10)))
  (check-true (body-collide? (body 10 10+10i) (body 10 20)))
  (check-true (body-collide? (body 10 10+10i) (body 10 0+20i)))
  
  (check-false (body-collide? (body 1 10+10i) (body 1 10+13i)))
  (check-false (body-collide? (body 1 10+10i) (body 1 0+10i)))
  (check-false (body-collide? (body 1 10+10i) (body 1 10)))
  (check-false (body-collide? (body 1 10+10i) (body 1 20)))
  (check-false (body-collide? (body 1 10+10i) (body 1 0+20i)))
  
  ;; serial/broadcast-univ
  #;
  (check-equal? (serial/broadcast-univ 
                 (play (list (ip iworld1 "player10" (body 10 1+10i) empty)
                             (ip iworld2 "player345" (body 56 3+45i) empty))
                       (list (body 87 67+23i)
                             (body 5 3+4i))
                       empty))
                (let ([serialized (serialize-universe (play (list (ip iworld1 "player10" (body 10 1+10i) empty)
                                                                  (ip iworld2 "player345" (body 56 3+45i) empty))
                                                            (list (body 87 67+23i)
                                                                  (body 5 3+4i))
                                                            empty))])
                  (list (make-mail iworld1 serialized)
                        (make-mail iworld2 serialized))))
  
  ;; time-broadcast
  (let ([j (join '() 100)])
    (check-equal? (time-broadcast j)
                  (make-bundle j '() '())))
  (let ([j (join `(,(ip iworld1 "sallyjoe" (body 0 0+0i) '())) 100)])
    (check-equal? (time-broadcast j)
                  (make-bundle j `(,(make-mail iworld1 (/ 100 WAIT-TIME))) '())))
  
  ;; testing auxiliary functions 
  (check-equal? (score `(,(ip iworld1 "foo" (body 1000 +inf.0) '())
                         ,(ip iworld1 "bar" (body 0 +inf.0) '())))
                `(("foo" ,(get-score 1000))
                  ("bar" ,(get-score 0))))
  ;; get-iws
  ;; empty-bundle
  (check-equal? (empty-bundle (join (list (ip iworld1 "player10" (body 10 1+10i) empty) 
                                          (ip iworld2 "player345" (body 56 3+45i) empty)) 132))
                (make-bundle (join (list (ip iworld1 "player10" (body 10 1+10i) empty) 
                                         (ip iworld2 "player345" (body 56 3+45i) empty)) 132) empty empty))
  (check-equal? (empty-bundle (join (list (ip iworld1 "player10" (body 10 1+10i) empty) 
                                          (ip iworld2 "player345" (body 56 3+45i) empty)) 345))
                (make-bundle (join (list (ip iworld1 "player10" (body 10 1+10i) empty) 
                                         (ip iworld2 "player345" (body 56 3+45i) empty)) 345) empty empty))
  (check-equal? (empty-bundle (play (list (ip iworld1 "player1" (body 87 67+23i) empty))
                                    (list (body 87 67+23i)
                                          (body 89 32+345i))
                                    empty))
                (make-bundle
                 (play (list (ip iworld1 "player1" (body 87 67+23i) empty))
                       (list (body 87 67+23i)
                             (body 89 32+345i))
                       empty)
                 empty
                 empty))
  
  ;; get-iws
  (check-equal? (get-iws (play (list (ip iworld1 "player10" (body 10 1+10i) empty)
                                     (ip iworld2 "player345" (body 56 3+45i) empty))
                               (list (body 87 67+23i)
                                     (body 5 3+4i))
                               empty))
                (list iworld1 iworld2))
  (check-equal? (get-iws (play (list (ip iworld1 "player10" (body 10 1+10i) empty))
                               empty
                               empty))
                (list iworld1))
  ;; broadcast
  (check-equal? (broadcast (list iworld1 iworld3 iworld2)
                           '(testing testing 1 2 3))
                (let ([message '(testing testing 1 2 3)])
                  (list (make-mail iworld1
                                   message)
                        (make-mail iworld3
                                   message)
                        (make-mail iworld2
                                   message))))
  (check-equal? (broadcast (list iworld1)
                           '(testing testing 1 2 3))
                (let ([message '(testing testing 1 2 3)])
                  (list (make-mail iworld1
                                   message))))
  (check-equal? (broadcast (list iworld1 iworld3)
                           9)
                (let ([message 9])
                  (list (make-mail iworld1
                                   message)
                        (make-mail iworld3
                                   message))))
  
  ;; broadcast-state
  (let ([state (play (list (ip iworld1 "player10" (body 10 1+10i) empty)
                           (ip iworld2 "player345" (body 56 3+45i) empty))
                     (list (body 87 67+23i)
                           (body 5 3+4i))
                     empty)])
    (check-equal? (broadcast-universe state)
                  (broadcast-universe state)))
  
  "server: all tests run")
