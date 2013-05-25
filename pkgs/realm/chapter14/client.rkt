#lang racket

;; This module implements the client for the Hungry Henry game

(provide 
 lets-eat ;; String String[IP Address] -> Meal
 ;; launch single client and register at specified host 
 )

(require "shared.rkt" 2htdp/universe 2htdp/image)

;                                     
;                                     
;                                     
;   ;   ;                             
;   ;   ;                             
;   ;   ;   ;;;   ; ;;   ; ;;;  ;   ; 
;   ;   ;  ;   ;  ;;  ;  ;;  ;  ;   ; 
;   ;;;;;  ;   ;  ;   ;  ;       ;  ; 
;   ;   ;  ;;;;;  ;   ;  ;       ; ;  
;   ;   ;  ;      ;   ;  ;       ; ;  
;   ;   ;  ;      ;   ;  ;        ;   
;   ;   ;   ;;;;  ;   ;  ;        ;   
;                                 ;   
;                               ;;    
;                                     


;; Image Constants
(define FOOD-IMG (bitmap "graphics/cupcake.gif"))
(define PLAYER-IMG (bitmap "graphics/hungry-henry.gif"))
(define BASE (empty-scene WIDTH HEIGHT))
(define WAYPOINT-NODE (circle 3 'solid 'black))
;; Color Constants
(define PLAYER-COLOR "red")
(define MY-COLOR "blue")
(define WAYPOINT-COLOR "green")
;; Text Constants
(define LOADING... "Waiting For Server")
(define TEXT-SIZE 20)
(define SCORE-SIZE 20)
(define TEXT-COLOR "black")
(define END-OPEN-TEXT "your score was: ")
(define END-CLOSE-TEXT ", the winner was player ")
(define LOADING-OPEN-TEXT "\nYou are ")
(define SEPERATOR ": ")
;; PBAR constants
(define PBAR-HEIGHT 35)
(define PBAR-LOC (- HEIGHT PBAR-HEIGHT))
(define PBAR-COLOR "red")
(define PBAR-TEXT (text "loading..." 20 "black"))
;; Message ID Constants
(define UPDATE-LENGTH 3)
(define SPLAYER-LENGTH 3)
(define SBODY-LENGTH 2)
(define END-LENGTH 2)
(define SCORE-LIST-LENGTH 2)
;; Init Constants
(define ZERO% 0)
(define LOADING (text LOADING... 20 "black"))

;; -----------------------------------------------------------------------------
;; State of Client 

(struct app (id img countdown) #:transparent)
(struct entree (id players food) #:transparent)

;; Meal is one of
;; - Appetizer
;; - Entree
;; Appetizer = (app [or Id #f] Image Number∈[0,1])
;; interpretation: 
;; -- the first field is this players id, #f if it hasnt been sent yet
;; -- the second is the loading image
;; -- the third is the %%% of loading time passed, represents the loading state
;; Entree     = (entree Id [Listof Feaster] [Listof Food])
;; interpretation: 
;; -- the first field is this player's id
;; -- the second field represents complete information about all players
;; -- the third field specifies the location of the cupcakes 

(define INITIAL (app #f LOADING ZERO%))

;                                          
;                                          
;                                          
;                           ;              
;                           ;              
;  ;;;    ;;;                              
;   ;;    ;;                               
;   ; ;  ; ;    ;;;;      ;;;     ;; ;;;   
;   ; ;  ; ;   ;    ;       ;      ;;   ;  
;   ; ;  ; ;        ;       ;      ;    ;  
;   ;  ;;  ;   ;;;;;;       ;      ;    ;  
;   ;  ;;  ;  ;     ;       ;      ;    ;  
;   ;      ;  ;     ;       ;      ;    ;  
;   ;      ;  ;    ;;       ;      ;    ;  
;  ;;;    ;;;  ;;;; ;;   ;;;;;;;  ;;;  ;;; 
;                                          
;                                          
;                                          
;                                          
;                                          

(define (lets-eat label server)
  (big-bang INITIAL
            (to-draw render-the-meal)
            (on-mouse set-waypoint)
            (on-receive handle-server-messages)
            (register server)
            (name label)))

;; Meal Message -> Meal
;; handles incomming messages
(define (handle-server-messages meal msg)
  (cond [(app? meal) (handle-appetizer-message meal msg)]
        [(entree? meal) (handle-entree-message meal msg)]))

;; Meal Number Number MouseEvent -> Meal
;; handles what happends on a click
(define (set-waypoint meal x y event)
  (if (and (entree? meal) (string=? event "button-down"))
      (make-package meal (list GOTO x y))
      meal))

;; Meal -> Image
;; deals with draw some kind of meal
(define (render-the-meal meal)
  (cond [(app? meal)   (render-appetizer meal)]
        [(entree? meal) (render-entree meal)]))

;                                                   
;                                                   
;                                                   
;   ;;;;                          ;                 
;   ;   ;                                           
;   ;   ;   ;;;    ;;;;   ;;;   ;;;   ;     ;  ;;;  
;   ;   ;  ;   ;  ;      ;   ;    ;    ;   ;  ;   ; 
;   ;;;;   ;   ;  ;      ;   ;    ;    ;   ;  ;   ; 
;   ;  ;   ;;;;;  ;      ;;;;;    ;    ;; ;;  ;;;;; 
;   ;  ;   ;      ;      ;        ;     ; ;   ;     
;   ;   ;  ;      ;      ;        ;     ; ;   ;     
;   ;   ;   ;;;;   ;;;;   ;;;;    ;      ;     ;;;; 
;                                                   
;                                                   
;                                                   

;; -----------------------------------------------------------------------------
;; Appetizer

;; Appetizer Message -> Meal
;; starts the game if the message is valid
(define (handle-appetizer-message s msg)
  (cond [(id? msg)    (app msg (app-img s) (app-countdown s))]
        [(time? msg)  (app (app-id s) (app-img s) msg)]
        [(state? msg) (switch-to-entree s msg)]
        ;; fault tolerant 
        [else s]))

;; Appetizer State -> Meal 
(define (switch-to-entree s m)
  (apply entree (app-id s) (rest m)))

;; -----------------------------------------------------------------------------
;; Appetizer

;; Entree Message -> Meal
;; either updates the world or ends the game
(define (handle-entree-message s msg)
  (cond [(state? msg) (update-entree s msg)]
        [(score? msg) (restart s msg)]
        [else s]))

;; Entree State -> Entree
;; creates a new entree based on the update mesg
(define (update-entree s state-msg)
  (apply entree (entree-id s) (rest state-msg)))

;; Entree EndMessage -> Appetizer
;; Tranistion to start state
(define (restart s end-msg)
  (define score-image (render-scores end-msg))
  (app (entree-id s) (above LOADING score-image) ZERO%))

;; -----------------------------------------------------------------------------
;; predicates for recognizing network messages 

;; Message -> Boolean
;; checks if message is a valid update message
(define (state? msg)
  (and (list? msg)
       (= UPDATE-LENGTH (length msg))
       (symbol? (first msg))
       (list? (second msg))
       (list? (third msg))
       (symbol=? SERIALIZE (first msg))
       (andmap player? (second msg))
       (andmap body? (third msg))))

;; Message -> Boolean
;; checks if message is a valid time message
(define (time? msg)
  (and (real? msg) (<= 0 msg 1)))

;; Message -> Boolean
;; checks if is end game message
(define (score? msg)
  (and (list? msg)
       (= END-LENGTH (length msg))
       (symbol? (first msg))
       (list? (second msg))
       (symbol=? SCORE (first msg))
       (score-list? (second msg))))

;; List -> Boolean
;; is this a list binding names to scores?
(define (score-list? l)
  (for/and ([s l])
    (and (list? s)
         (= SCORE-LIST-LENGTH (length s))
         (id? (first s))
         (number? (second s)))))

;                                                                        
;                                                                        
;                                                                        
;                                               ;                        
;                                               ;                        
;   ;;;;;;                                                               
;    ;    ;                                                              
;    ;     ;   ;;  ;;     ;;;;   ;;;     ;;   ;;;     ;; ;;;      ;;; ;; 
;    ;     ;    ;;;  ;   ;    ;   ;       ;     ;      ;;   ;    ;   ;;  
;    ;     ;    ;             ;   ;   ;   ;     ;      ;    ;   ;     ;  
;    ;     ;    ;        ;;;;;;    ;  ;  ;      ;      ;    ;   ;     ;  
;    ;     ;    ;       ;     ;    ; ; ; ;      ;      ;    ;   ;     ;  
;    ;     ;    ;       ;     ;    ; ; ; ;      ;      ;    ;   ;     ;  
;    ;    ;     ;       ;    ;;    ; ; ; ;      ;      ;    ;    ;   ;;  
;   ;;;;;;     ;;;;;;    ;;;; ;;    ;   ;    ;;;;;;;  ;;;  ;;;    ;;; ;  
;                                                                     ;  
;                                                                    ;;  
;                                                                ;;;;    
;                                                                        
;                                                                        

;; -----------------------------------------------------------------------------
;; Appetizer Drawing

;; Appetizer -> Image
;; tells the player that we're waiting for the server. shows id
(define (render-appetizer app)
  (add-progress-bar (render-id+image app) (app-countdown app)))

;; Image Number∈[0,1] -> Image
;; draws the progress bar 
(define (add-progress-bar base count)
  (place-image (render-progress count) (/ WIDTH 2) PBAR-LOC base))

;; Number∈[0,1] -> Image
;; draw a progress bar that is count percent complete
(define (render-progress count)
  (overlay PBAR-TEXT (rectangle (* count WIDTH) PBAR-HEIGHT "solid" PBAR-COLOR)))

;; Appetizer -> Image
;; gets the text to display on the loading screen
(define (render-id+image app)
  (define id (app-id app))
  (define base-image (app-img app))
  (overlay
   (cond
     [(boolean? id) base-image]
     [else (define s (string-append LOADING-OPEN-TEXT id))
           (above base-image (text s TEXT-SIZE TEXT-COLOR))])
   BASE))

;; -----------------------------------------------------------------------------
;; Entree Drawing

;; Entree -> Image
;; draws a Entree
(define (render-entree entree)
  (define id (entree-id entree))
  (define pl (entree-players entree))
  (define fd (entree-food entree))
  (add-path id pl (add-players id pl (add-food fd BASE))))

;; [Listof Food] Image -> Image
;; draws all the food
(define (add-food foods base-scene)
  (for/fold ([scn base-scene]) ([f foods])
    (place-image FOOD-IMG (body-x f) (body-y f) scn)))

;; Id [Listof Feaster] Image -> Image
;; draws all players
(define (add-players id lof base-scene)
  (for/fold ([scn base-scene]) ([feaster lof])
    (place-image (render-avatar id feaster)
                 (feaster-x feaster) (feaster-y feaster)
                 scn)))

;; Id Feaster -> Image
;; gets an image for the player
(define (render-avatar id player)
  (define size (body-size (player-body player)))
  (define color
    (if (id=? id (player-id player)) MY-COLOR PLAYER-COLOR))
  (above
   (render-text (player-id player))
   (overlay (render-player-score player) 
            PLAYER-IMG
            (circle size 'outline color))))

;; Feaster -> Image
;; Draw the players score
(define (render-player-score player)
  (render-text (number->string (get-score (body-size (player-body player)))))) 

;; Id [Listof Feaster] Image -> Image
;; draws the path of the player whose id is passed in
(define (add-path id players base-scene)
  (define player 
    (findf (lambda (x) (id=? id (player-id x))) players))
  (if (boolean? player)
      base-scene
      (add-waypoint* player base-scene)))

;; Feaster Image -> Image
;; draws the list of way points to the scene
(define (add-waypoint* player base-scene)
  (define loc  (body-loc (player-body player)))
  (define ways (player-waypoints player))
  (define-values (resulting-scene _)
    (for/fold ([scn base-scene][from loc]) ([to ways])
      (values (add-waypoint from to scn) to)))
  resulting-scene)

;; Complex Complex Image -> Image
;; Add a waypoint to the scene at those coordinates
(define (add-waypoint from to s)
  (define x-from (real-part from))
  (define y-from (imag-part from))
  (define x-to (real-part to))
  (define y-to (imag-part to))
  (define with-line (add-line s x-to y-to x-from y-from WAYPOINT-COLOR))
  (place-image WAYPOINT-NODE x-to y-to with-line))

;; -----------------------------------------------------------------------------
;; render the end 

;; Score -> Image
;; draws the end of the game
(define (render-scores msg)
  (define scores (sort (second msg) < #:key second))
  (for/fold ([img empty-image]) ([name-score scores])
    (define txt (get-text name-score))
    (above (render-text txt) img)))

;; (list ID Natural) -> string
;; builds a string for that winning pair
(define (get-text name-score)
  (define-values (name score) (apply values name-score))
  (string-append name SEPERATOR (number->string score)))


;                                
;                                
;                                
;                                
;                                
;   ;;;;;                        
;      ;;                        
;     ;  ;    ;;   ;;   ;;;  ;;; 
;     ;  ;     ;    ;    ;    ;  
;     ;  ;     ;    ;     ;  ;   
;    ;    ;    ;    ;      ;;    
;    ;;;;;;    ;    ;      ;;    
;    ;    ;    ;    ;     ;  ;   
;   ;      ;   ;   ;;    ;    ;  
;  ;;;    ;;;   ;;; ;;  ;;;  ;;; 
;                                
;                                
;                                
;                                
;                                

;; String -> Image
;; draws the text
(define (render-text txt)
  (text txt TEXT-SIZE TEXT-COLOR))

;; player -> Number 
;; Gets the X coord of a entrees
(define (feaster-x feaster)
  (body-x (player-body feaster)))

;; player -> Number 
;; Gets the Y coord of a entrees
(define (feaster-y feaster)
  (body-y (player-body feaster)))

;; body -> Number
;; gets the X coord of a body
(define (body-x body)
  (real-part (body-loc body)))

;; body -> Number
;; gets the Y coord of a body
(define (body-y body)
  (imag-part (body-loc body)))

;                                                    
;                                                    
;                                                    
;                                                    
;                                                    
;   ;;;;;;;;;                       ;                
;   ;   ;   ;                       ;                
;   ;   ;   ;   ;;;;     ;;;; ;   ;;;;;;;    ;;;; ;  
;   ;   ;   ;  ;    ;   ;    ;;     ;       ;    ;;  
;       ;     ;      ;  ;           ;       ;        
;       ;     ;;;;;;;;   ;;;;;      ;        ;;;;;   
;       ;     ;               ;     ;             ;  
;       ;     ;               ;     ;             ;  
;       ;      ;     ;  ;     ;     ;    ;  ;     ;  
;     ;;;;;     ;;;;;   ;;;;;;       ;;;;   ;;;;;;   
;                                                    
;                                                    
;                                                    
;                                                    
;                                                    

(module+ test 
  
  (require rackunit rackunit/text-ui)
  
  ;; testing main client
  (check-equal? (switch-to-entree (app "foo" 'blah 1) '(STATE () ()))
                (entree "foo" '()'()))
  (check-equal? (handle-server-messages (app #f 'ksajfhsdkjhfr 1) .5) 
                (handle-appetizer-message (app #f 'ksajfhsdkjhfr 1) .5))
  ;;dispatch-mouse
  (check-equal? (set-waypoint (app 1 LOADING 0) 1 1 "button-down")
                (app 1 LOADING 0))
  (check-equal? (set-waypoint (app 1 LOADING 0) 1 1 "button-up")
                (app 1 LOADING 0))
  (check-equal? (set-waypoint (app #f LOADING 0) 1 1 "button-down")
                (app #f LOADING 0))
  (check-equal? (set-waypoint (app #f LOADING 0) 1 1 "button-up")
                (app #f LOADING 0))
  (check-equal? (set-waypoint (entree "player1" (list (player "player1" (body 1 1+1i) empty)) empty) 1 1 "button-up")
                (entree "player1" (list (player "player1" (body 1 1+1i) empty)) empty))
  (check-equal? (set-waypoint (entree "player1" (list (player "player1" (body 1 1+1i) empty)) empty)
                              1 1 "button-down")
                (make-package (entree "player1" (list (player "player1" (body 1 1+1i) empty)) empty)
                              (list 'goto 1 1)))
  ;;render-the-meal
  
  ;; testing message receipt
  ;; app-may-start
  ;; entree-msg
  ;; update-msg?
  
  (check-true (state? `(,SERIALIZE (,(player "player1" (body 1+4i 234) `()) ,(player "player1" (body 3 3) `()))
                                   (,(body 1+i 2) ,(body 2 2)))))
  (check-true (state? `(,SERIALIZE (,(player "player1" (body 1+4i 234) `()))
                                   (,(body 1+i 2) ,(body 2 2)))))
  (check-true (state? `(,SERIALIZE ()
                                   (,(body 1+i 2) ,(body 2 2)))))
  (check-true (state? `(,SERIALIZE (,(player "player1" (body 1+4i 234) `()) ,(player "player1" (body 3 3) `()))
                                   ())))
  
  (check-false (state? `(,SERIALIZE (("player1" (1+4i 234) ()) ("player1" (3 3) ()))
                                    ((1+i 2) (2 2)))))
  (check-false (state? `(,SERIALIZE (("player1" (1+4i 234) ()))
                                    ((1+i 2) (2 2)))))
  (check-false (state? `(,SERIALIZE ()
                                    ((1+i 2) (2 2)))))
  (check-false (state? `(,SERIALIZE (("player1" (1+4i 234) ()) ("player1" (3 3) ()))
                                    ())))
  (check-true (state? `(,SERIALIZE ()
                                   ())))
  (check-false (state? `(,SERIALIZE (("player1" (1+4i 234) ()) ("player1" (3 3) ()))
                                    ((1+i 2) (2 2)))))
  (check-false (state? `(,SERIALIZE (("player1" (1+4i 234) ()))
                                    ((1+i 2) (2 2)))))
  (check-false (state? `(,SERIALIZE ()
                                    ((1+i 2) (2 2)))))
  (check-false (state? `(,SERIALIZE (("player1" (1+4i 234) ()) ("player1" (3 3) ()))
                                    ())))
  
  (check-false (state? '(u ((1 1+4i 234))
                           ((1+i 2) (2 2)))))
  (check-false (state? '(((1 1+4i 234))
                         ((1+i 2) (2 2)))))
  (check-false (state? '(u ((1 1+4i))
                           ((1+i 2) (2 2)))))
  (check-false (state? '(u ((1 1+4i 234))
                           ((1+i 2) (2 b)))))
  (check-false (state? '(u ((1 1+4i 234)))))
  (check-false (state? '(u ((1+i 2) (2 2)))))
  (check-false (state? '(((1+i 2) (2 2)))))
  (check-false (state? 4))
  (check-false (state? 'f))
  ;; score-list?
  (check-true (score-list? '(("s" 0) ("l" 0) ("sdf" 0))))
  (check-true (score-list? empty))
  (check-true (score-list? '(("s" 0) ("l" 0))))
  (check-false (score-list? '(('s 0) ('l 0) ('sdf 0))))
  (check-false (score-list? '((s 0) (l 0))))
  (check-false (score-list? '((s) (l))))
  (check-false (score-list? '((s 0) (l 0))))
  ;; update-entree
  (check-equal? (update-entree (entree "player10" '() '())
                               `(s (,(player "player1" (body 10 10) `(3 4+9i))
                                    ,(player "player10" (body 103 10+4i) `(3 5+78i)))
                                   (,(body 5 10) ,(body 30 30))))
                (entree "player10" (list (player "player1" (body 10 10) (list 3 4+9i))
                                         (player "player10" (body 103 10+4i) (list 3 5+78i)))
                        (list (body 5 10) (body 30 30))))
  
  
  ;; testing rendering the client 
  
  ;; draw-app
  (check-equal? (render-appetizer (app #f LOADING 0))
                (add-progress-bar (overlay LOADING
                                           BASE)
                                  0))
  ;; draw-entree
  
  
  ;; draw-players
  
  (check-equal? (add-players "player0" 
                             (list (player "player1" (body 40 23+34i) empty)
                                   (player "player0" (body 50 1+3i) empty))
                             BASE)
                (place-image (render-avatar "player0" (player "player0" (body 50 1+3i) empty))
                             1 3
                             (place-image (render-avatar "player0" (player "player1" (body 40 23+34i) empty))
                                          23 34
                                          BASE)))
  (check-equal? (add-players "player0"
                             (list (player "player1" (body 40 23+34i) empty))
                             BASE)
                (place-image (render-avatar "player0" (player "player1" (body 40 23+34i) empty))
                             23 34
                             BASE))
  
  ;; draw-player
  
  ;; get-player-image
  (check-equal? (render-avatar "player0" (player "player0" (body 30 1+3i) empty))
                (above (render-text "player0")
                       (overlay (text (number->string (get-score 30)) 20 'black)
                                PLAYER-IMG (circle 30 "outline" MY-COLOR))))
  (check-equal? (render-avatar "player0" (player "player1" (body 30 1+3i) empty))
                (above (render-text "player1")
                       (overlay (text (number->string (get-score 30)) 20 'black)
                                PLAYER-IMG (circle 30 "outline" PLAYER-COLOR))))
  
  ;; draw-food
  (check-equal? (add-food (list (body 34 54+3i)
                                (body 9 45+23i))
                          BASE)
                (place-image FOOD-IMG
                             45 23
                             (place-image 
                              FOOD-IMG
                              54 3
                              BASE)))
  (check-equal? (add-food (list (body 34 54+3i))
                          BASE)
                (place-image 
                 FOOD-IMG
                 54 3
                 BASE))
  
  
  ;; testing auxiliary functions 
  ;; player-x
  (check-equal? (feaster-x (player 20 (body 3 1+3i) empty))
                1)
  (check-equal? (feaster-x (player 20 (body 3 4+3i) empty))
                4)
  (check-equal? (feaster-x (player 20 (body 3 4+67i) empty))
                4)
  ;; player-y
  (check-equal? (feaster-y (player 20 (body 3 1+3i) empty))
                3)
  (check-equal? (feaster-y (player 20 (body 3 4+3i) empty))
                3)
  (check-equal? (feaster-y (player 20 (body 3 4+67i) empty))
                67)
  
  ;; body-x
  (check-equal? (body-x (body 20  1+2i))
                1)
  (check-equal? (body-x (body 20  4+2i))
                4)
  (check-equal? (body-x (body 20  3+2i))
                3)
  ;; body-y
  (check-equal? (body-y (body 20  4+1i))
                1)
  (check-equal? (body-y (body 20  1+4i))
                4)
  (check-equal? (body-y (body 20  3))
                0)
  
  "client: all tests run")

