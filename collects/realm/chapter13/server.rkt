#lang racket

;; the server for distributed Guess my Number

(provide
 ;; starts the distributed guess my number game
 ;; -> GmNState
 launch-guess-server)

(require 2htdp/image 2htdp/universe "shared.rkt")

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

;; A GmNState is one of: 
;;  -- #f 
;;  -- GuessRange 

(struct interval (small big) #:transparent)
;; A GuessRange is (interval Number Number)
;; always true: (interval l u) means (<= l u)

(define u0 (interval LOWER UPPER))

;                                          
;                                          
;                                          
;                          ;               
;                          ;               
;  ;;;   ;;;                               
;   ;;   ;;                                
;   ; ; ; ;     ;;;;     ;;;      ;; ;;;   
;   ; ; ; ;    ;    ;      ;       ;;   ;  
;   ; ; ; ;         ;      ;       ;    ;  
;   ;  ;  ;    ;;;;;;      ;       ;    ;  
;   ;     ;   ;     ;      ;       ;    ;  
;   ;     ;   ;    ;;      ;       ;    ;  
;  ;;;   ;;;   ;;;; ;;  ;;;;;;;   ;;;  ;;; 
;                                          
;                                          
;                                          
;                                          

(define (launch-guess-server)
  (universe #f
            (state #t)
            (on-new connect)
            (on-msg handle-msg)))

;; GmNState IWorld -> [Bundle GmNState [Listof [Mail IWorld Nat]] [Listof IWorld]]
;; handles all new connections. It only accepts one connection.
(define (connect u client)
  (if (false? u)
      (make-bundle u0 (list (make-mail client (guess u0))) '())
      (make-bundle u empty (list client))))

;; GmNState IWorld CtoSMessage -> [Bundle GmNState [List [Mail IWorld Nat]] Empty]
;; handles a message from the client.
(define (handle-msg u client msg)
  (define w (next-interval u msg))
  (make-bundle w (list (make-mail client (guess w))) '()))

;; GmNState CtoSMessage -> GmNState 
;; creates the new universe for a responce
(define (next-interval u msg)
  (cond [(not (string? msg))   u]
        [(string=? "up" msg)   (bigger u)]
        [(string=? "down" msg) (smaller u)]
        [else u]))

;                                          
;                                          
;                                          
;                                          
;     ;; ;                                 
;    ;  ;;                                 
;   ;       ;;  ;;   ;;;;    ;;;;;   ;;;;; 
;   ;        ;   ;  ;    ;  ;    ;  ;    ; 
;   ;  ;;;;  ;   ;  ;;;;;;   ;;;;    ;;;;  
;   ;    ;   ;   ;  ;            ;       ; 
;    ;   ;   ;  ;;  ;       ;    ;  ;    ; 
;     ;;;     ;; ;;  ;;;;;  ;;;;;   ;;;;;  
;                                          
;                                          
;                                          
;                                          

;; GuessRange -> Boolean
;; Does the interval represent a single number?
;; > (single? (interval 1 1))
;; #t
(define (single? w)
  (= (interval-small w) (interval-big w)))

;; GuessRange -> Number
;; Calculates a guess based on the given interval
;; > (guess (interval 0 100))
;; 50
(define (guess w)
  (quotient (+ (interval-small w) (interval-big w)) 2))

;; GuessRange -> GuessRange
;; Recreates a GuessRange that lowers the upper bound
;; > (smaller (interval 0 100))
;; (interval 0 50)
(define (smaller w)
  (interval (interval-small w) (max (interval-small w) (sub1 (guess w)))))

;; GuessRange -> GuessRange
;; Recreates a interval that raises the lower bound
;; > (bigger (0 100)
;; (interval 51 100)
(define (bigger w)
  (interval (min (interval-big w) (add1 (guess w))) (interval-big w)))


;                                     
;                                     
;                                     
;                                     
;    ;                    ;           
;   ;;;;;   ;;;    ;;;;  ;;;;;   ;;;; 
;    ;     ;   ;  ;       ;     ;     
;    ;     ;;;;;   ;;     ;      ;;   
;    ;     ;         ;    ;        ;  
;    ;     ;          ;   ;         ; 
;     ;;;   ;;;;  ;;;;     ;;;  ;;;;  
;                                     
;                  

(module+ test 
  
  (require rackunit rackunit/text-ui)
  
  (define 51-100 (interval 51 100))
  
  ;; testing the server's main function 
  (check-equal? (connect #f iworld1)
                (make-bundle (interval 0 100) `(,(make-mail iworld1 50)) '()))
  (check-equal? (handle-msg (interval 0 100) iworld1 "up")
                (make-bundle 51-100 `(,(make-mail iworld1 (guess 51-100))) '()))
  
  
  ;; testing the server's handlers
  
  (check-true (single? (interval 50 50)))
  (check-false (single? (interval 50 51)))
  
  (check-equal? (guess (interval 0 100)) 50)
  (check-equal? (guess (interval 50 100)) 75)
  (check-equal? (guess (interval 0 50)) 25)
  
  (check-equal? (smaller (interval 0 100)) (interval 0 49))
  (check-equal? (smaller (interval 0 000)) (interval 0 0))
  (check-equal? (smaller (interval 0 50)) (interval 0 24))
  (check-equal? (smaller (interval 50 100)) (interval 50 74))
  (check-equal? (smaller (bigger (bigger (interval 0 100))))
                (interval 76 87))
  
  (check-equal? (bigger (interval 0 100)) (interval 51 100))
  (check-equal? (bigger (interval 0 000)) (interval 0 0))
  (check-equal? (bigger (interval 0 100)) (interval 51 100))
  (check-equal? (bigger (interval 51 100)) (interval 76 100))
  (check-equal? (bigger (interval 0 50)) (interval 26 50))
  
  "server: all tests run")
