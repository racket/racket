#lang racket

;; This module describes the shared vocabulary and knowledge for the server 
;; and client modules of the distributed Guess My Number game. 

#|
 Message Formats 
 ---------------

 StoCMessage is the set of numbers between LOWER and UPPER (inclusive). 
 The numbers represent the guess. 

 CtoSMessage is one of the following two strings: 
   -- "up"
   -- "down" 
 with the obvious meaning. 


 Message Exchanges
 -----------------

  server      client 
    |             |    
    | register    |    
    |<------------|    
    |             | 
    |             | <----- guess ("up", "down")
    | CtoSMessage | 
    |<------------|
    |             | 
    | StoCMessage | 
    |------------>|
    |             | 
    |             |
|#


(provide
 ;; the to-be-guessed number must be in [LOWER,UPPER]
 UPPER
 LOWER)

;                                          
;                                          
;                                          
;                                          
;                                          
;   ;;;;;;               ;                 
;    ;    ;              ;                 
;    ;     ;    ;;;;    ;;;;;;      ;;;;   
;    ;     ;   ;    ;    ;         ;    ;  
;    ;     ;        ;    ;              ;  
;    ;     ;   ;;;;;;    ;         ;;;;;;  
;    ;     ;  ;     ;    ;        ;     ;  
;    ;    ;   ;    ;;    ;    ;   ;    ;;  
;   ;;;;;;     ;;;; ;;    ;;;;     ;;;; ;; 
;                                          
;                                          
;                                          
;                                          

;; prefined upper and lower limits for a game.
(define UPPER 100)
(define LOWER 0)
