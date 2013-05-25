#lang racket

#|
   The Guess My Number game, a distributed version with a GUI
   -----------------------------------------------------------
   
   You pick a number. The program guesses the nunber, 
   by asking you questions. Your responses are "too 
   small" "too large" or "you guessed it". 

   In the Distributed Guess My Number game a player uses a client to connect 
   to the server. The Server attempts to guess what number the client is thinking
   of. Each time the server guesses, the client must use the arrow keys to tell
   the server if it is right, too small, or too large.

   Play
   ----

   Click Run. Pick a number X between <n> and <m>. 
   Evaluate 
     (run)
   This will pop up three windows: 
 
    -- Adam: with instructions for interacting with the program

    -- Universe: the console for the central server 
        it displays the messages that it receives and sends 

    -- your server's state: a window that displays the server's internal state. 

   Play and watch the two latter window to understand how the server and client 
   interact in response to your actions. 

   To run the game on two distinct computers: 

     -- copy this folder to another computer, determine its IP number "12.345.67.98"
     -- open run.rkt 
     -- evaluate 
          (launch-guess-server) 
     
     -- on your own computer, open run.rkt and run 
     -- evaluate 
          (launch-guess-client "12.345.67.98")
|#

(require 2htdp/universe "client.rkt" "server.rkt")

;; play the game as "Adam" 
(define (run)
  (launch-many-worlds (launch-guess-client "Adam" LOCALHOST)
                      (launch-guess-server)))

;; what happens if two players sign up with the server simultaneously 
(define (bad)
  (launch-many-worlds (launch-guess-client "Adam" LOCALHOST) 
                      (launch-guess-server)
                      (launch-guess-client "Beatrice" LOCALHOST)))
