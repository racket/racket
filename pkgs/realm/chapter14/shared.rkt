#lang racket

;; This module describes the shared vocabulary and knowledge for the server 
;; and client modules of the Hungry Henry game. 

(provide ;; type Id 
         id?   ;; Any -> Boolean : Id 
         id=?  ;; Id Id -> Boolean 
         ;; type GOTO 
         ;; type SOTO = Time | Ackn | State | Score 
         ;; type Food 
         ;; type Feaster 
         ;; type Body 
         (struct-out player) ;; 
         (struct-out body)   ;; 
         get-score ;; Nat -> Nat 
         PLAYER-FATTEN-DELTA
         WIDTH HEIGHT CUPCAKE PLAYER-SIZE 
         SCORE GOTO SERIALIZE
         GOTO-LENGTH)

#|  -----------------------------------------------------------------------------

;; --- Tasks --------------------------------------------------------------------

The game server keeps track of the entire game state [to avoid cheating by 
lients]. It collects waypoints, moves the avatars on behalf of the clients, 
detects collisions with cupcakes, has avatars eat and grow, and discovers the
end of the game. As events occur, it informs all clients about all actions and,
at the end of the game, tallies the scores. 

Each client displays the current state of the game as broadcast by the server. 
It also records and sends all mouse clicks to the server. 

;; --- Messages and Protocol ---------------------------------------------------

The server and the client exchange messages to inform each other about 
the events in the game. 

Client To Server Message: 
------------------------

 GOTO = (list GOTO PositiveNumber PositiveNumber)
 represents the coordinates of player's latest waypoint, 
 obtained via a mouse click.
 Constraint: in (list GOTO x y), (and (<= 0 x WIDTH) (<= 0 y HEIGHT))
 
Server to Client Message:
-------------------------

 SOTO is one of: 
 -- Number ∈ [0,1]
    called a Time message 
    repreents the percentage of loading time left
 -- ID
    called an Ackn message 
    represents the unique id that the server assigns to the client, 
    based on the client's name 
 -- (list SERIALIZE [Listof Feaster] [Listof Food])
    called a State message
    represents the complete current state of the game 
 -- (list SCORE [Listof (list Id Natural)])
    called a Score message 
    informs clients that the game is over and the sizes of each player. 
|#
;; Shared Data Definitions for Messages 

(struct player (id body waypoints) #:prefab)
(struct body (size loc) #:prefab #:mutable)
;; Food     = Body
;; Feaster  = (player Id Body [Listof Complex])
;; interpretation: 
;; -- id is the player's id
;; -- body is the player's size and location
;; -- loc are the player's waypoints, ordered from first to last
;; Body     = (body PositiveNumber Complex)
;; interpretation: any 'body' on the playing field, both players and cupcakes 
;; -- the postive number specifies the body's size
;; -- the complex number represents the body's location
;; PlayerId = String
(define id? string?)
(define id=? string=?)

;; Message ID Constants
(define SCORE 'score)
(define SERIALIZE 'state)
(define GOTO 'goto)
(define GOTO-LENGTH 3)

#| --- Protocol ----------------------------------------------------------------

     Client1        Client2         Server
       |               |              |
       | register(name1)              |   [universe protocol]
       |----------------------------->|
       |               |              |  
       |               |       ID     |    an identifier message 
       |<-----------------------------|
       |               |       t      |    percentage of wait time 
       |<-----------------------------|
       |<-----------------------------|
       |<-----------------------------|
       |               |              |    
       |               | register(name2) 
       |               |------------->|
       |               |              |  
       |               |       ID     |  
       |               |<-------------|
       |               |       t      |    percentage of wait time 
       |<-----------------------------|
       |               |<-------------|
       |<-----------------------------|
       |               |<-------------|
       |               |              |  <==== end of wait time [clock, players]
       |             state msg        |  
       |<-----------------------------|    `(state (,feaster1 ,feaster2) ,foods)
       |               |<-------------|
       |               |              |  
 click |  GOTO 	       |	      |    `(goto ,x ,y)
 ====> |----------------------------->|    new state 
       |               |              |  
       |             state msg        |     
       |<-----------------------------|    `(state (,feaster1 ,feaster2) ,foods)
       |               |<-------------|
       |               |              |  
       |               |              |    move, eat:
       |<-----------------------------|    `(state (,feaster1 ,feaster2) ,foods)
       |               |<-------------|
       |               |              |    
       |        click  | GOTO	      |   `(goto ,x ,y)
       |	====>  |------------->|  
       |               |              |  
       |             state msg        |     
       |<-----------------------------|    `(state (,feaster1 ,feaster2) ,foods)
       |               |<-------------|
       |               |              |
       |             score msg        |    all food is eaten: 
       |<-----------------------------|    `(score ((,id ,score) ...))
       |               |<-------------|
       |               |              |
      ---             ---            ---

|#

;; Shared Logical Constants
(define WIDTH 1000)
(define HEIGHT 700)
(define CUPCAKE 15)
(define PLAYER-SIZE (* 3 CUPCAKE)) 
(define PLAYER-FATTEN-DELTA 5)

;; Number -> Number ;; move to serer 
;; gets aplayers score given its fatness
(define (get-score f)
  (/ (- f PLAYER-SIZE) PLAYER-FATTEN-DELTA))

