#lang racket

#|
   Hungry Henry, a multi-player, distributed game 
   -----------------------------------------------

   This game is a multi-player competition for cupcakes. Each player owns an 
   avatar, called a "Henry", and competes for a limited number of cupcakes,
   distributed over a rectangular space. A player launches her Henry via 
   a series of mouse clicks, so-called waypoints. Her Henry moves from waypoint
   to waypoint. If it gets close enough to a cupcake, he eats the cupcake and 
   fattens up. As a Henry fattens up, he slows down. When all cupcakes are 
   consumed, the fattest Henry wins. 

   Notes: 
   1. The cupcakes remain in place until they are eaten. 
   2. Once a waypoiny is recorded, it cannot be removed. 
   3. Waypoints are visited in a first-come, first-serve order. 

   Play
   ----

   Click Run. Evaluate 

     (serve-dinner)

   in the Interactions Panel. This will pop up three windows: 
    -- Matthias, a game window 
    -- David, another game window 
    -- Universe, the game server's console

   Play. You can play the part of both participants. Alternatively, click 
   the David or Matthias window (to obtain focus) and click again to choose
   a way point for David's or Matthias's "hungry henry". Watch the hungry 
   henries go for the cup cake and eat them up. You can make either one of them 
   win or you can force a tie. 

   To run the game on two distinct computers: 

     -- copy this folder to another computer, determine its IP number "12.345.67.98"
     -- open run.rkt 
     -- evaluate 
          (bon-appetit)
     
     -- on your own computer, open run.rkt and run 
     -- evaluate 
          (lets-eat SomeNameAsAString "12.345.67.98")
|#

(require (only-in "server.rkt" bon-appetit)
         (only-in "client.rkt" lets-eat)
         2htdp/universe)

;; launch server worlds for playtesting
(define (serve-dinner)
  (launch-many-worlds
   (bon-appetit)
   (lets-eat "Matthias" LOCALHOST)
   (lets-eat "David"    LOCALHOST)))
