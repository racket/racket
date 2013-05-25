#lang racket

#|
   The Guess My Number game, played at DrRacket's REPL
   ---------------------------------------------------
   
   You pick a number. The program guesses the nunber, 
   by asking you questions. Your responses are "too 
   small" "too large" or "you guessed it". 

   Play
   ----

   Click Run. Pick a number X between <n> and <m>. 
   Evaluate 
     (start <n> <m>)
   The program will respond with a number.
   Use
     (bigger)
   and
     (smaller)
    to let it know what you think of its guess.
|#

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

;; Example:
;; > (start 0 100)       ; [0,100]
;; 50
;; > (bigger)            ; [51,100]
;; 75
;; > (bigger)            ; [76,100]
;; 88
;; > (smaller)           ; [76,87]
;; 82

;; Number Number -> Number
;; Start a new game in [n,m] and make a guess.
;; > (start 0 100)
;; 50
(define (start n m)
  (set! lower (min n m))
  (set! upper (max n m))
  (guess))

;; Lower and upper bounds on the number
(define lower 1)
(define upper 100)

;; -> Nuber Number
;; Guess half-way between lower and upper bounds.
;; > (begin (start 0 100) (guess))
;; 50
(define (guess) 
  (quotient (+ lower upper) 2))

;; -> Number
;; Lower the upper bound and guess again.
;; > (begin (start 0 100) (smaller))
;; 24
(define (smaller)
  (set! upper (max lower (sub1 (guess))))
  (guess))

;; -> Number
;; Raise the lower bound and guess again.
;; > (begin (start 0 100) (bigger))
;; 75
(define (bigger) 
  (set! lower (min upper (add1 (guess))))
  (guess))

;                                                    
;                                                    
;                                                    
;                                                    
;                                                    
;   ;;;;;;;                        ;                 
;   ;  ;  ;                        ;                 
;   ;  ;  ;     ;;;      ;;;; ;  ;;;;;;;     ;;;; ;  
;   ;  ;  ;    ;   ;    ;    ;;    ;        ;    ;;  
;      ;      ;     ;   ;          ;        ;        
;      ;      ;;;;;;;    ;;;;;     ;         ;;;;;   
;      ;      ;               ;    ;              ;  
;      ;       ;    ;   ;     ;    ;    ;   ;     ;  
;    ;;;;;      ;;;;    ;;;;;;      ;;;;    ;;;;;;   
;                                                    
;                                                    
;                                                    
;                                                    


(module+ test 
  
  (require rackunit rackunit/text-ui)
  
  ;; basic guesses 
  
  (check-equal? (guess) 50)
  
  (check-equal? (start 0 100) 50)
  
  (check-equal? (begin (start 0 100) lower) 0)
  (check-equal? (begin (start 0 100) upper) 100)
  (check-equal? (begin (start 0 100) (smaller)) 24)
  (check-equal? (begin (start 0 000) (smaller)) 0)
  (check-equal? (begin (start 0 100) (smaller) lower) 0)
  (check-equal? (begin (start 0 100) (smaller) upper) 49)
  (check-equal? (begin (start 0 100) (bigger)) 75)
  (check-equal? (begin (start 0 000) (bigger)) 0)
  
  ;; testing a sequence of interactions with expected intermediate states 
  
  (test-begin (start 0 100)        
              (bigger)
              (check-equal? lower 51)
              (check-equal? upper 100)
              (bigger)
              (check-equal? lower 76)
              (check-equal? upper 100)
              (smaller)
              (check-equal? lower 76)
              (check-equal? upper 87))
  
  ;; doing it all over for negative numbers 
  
  (check-equal? (start -100 0) -50)
  
  (check-equal? (begin (start -100 0) lower) -100)
  (check-equal? (begin (start -100 0) upper) 0)
  (check-equal? (begin (start -100 0) (smaller)) -75)
  (check-equal? (begin (start -100 0) (smaller)) -75)
  (check-equal? (begin (start -100 0) (smaller) lower) -100)
  (check-equal? (begin (start -100 0) (smaller) upper) -51)
  (check-equal? (begin (start -100 0) (bigger)) -24)
  (check-equal? (begin (start -100 0) (bigger)) -24)  
  
  (test-begin (start -100 0)        
              (bigger)
              (check-equal? lower -49)
              (check-equal? upper 0)
              (bigger)
              (check-equal? lower -23)
              (check-equal? upper 0)
              (smaller)
              (check-equal? lower -23)
              (check-equal? upper -12))
  
  
  ;; ---------------------------------------------------------------------------
  ;; testing random properties of our functions 
  
  ;; Property:
  ;; For all games starting in [n,m] after any number of moves,
  ;; lower <= upper.
  (define (prop:ordered n m i)
    (check-true 
     (begin (start n m)
            (random-moves i)
            (<= lower upper))))
  
  ;; Property:
  ;; For all games starting in [n,m], for any number of moves,
  ;; lower grows up, upper grows down, or they are equal.
  (define (prop:monotonic n m i)
    (check-true
     (begin (start n m)
            (for/and ([i (in-range i)])
              (define l lower)
              (define u upper)
              (random-move)
              (or (and (< l lower) (= u upper))
                  (and (= l lower) (> u upper))
                  (and (= l lower) (= u upper)))))))
  
  ;; Number -> Void
  ;; Move randomly n times.
  (define (random-moves i)
    (unless (zero? i)
      (random-move)
      (random-moves (sub1 i))))
  
  ;; -> Void
  ;; Move randomly once.
  (define (random-move)
    (if (zero? (random 2))
        (smaller) 
        (bigger)))
  
  ;; property tests
  (for ([i (in-range 1000)])
    (prop:ordered (random 1000) (random 1000) (random 100))
    (prop:monotonic (random 1000) (random 1000) (random 100)))
  
  ;; reset the boundaries to defaults 
  (start 1 100)

  "all tests run")
