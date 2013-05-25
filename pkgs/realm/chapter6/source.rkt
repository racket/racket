#lang racket

#|
   The Snake game 
   --------------

   The Snake game revolves around a room filled with pieces of radioactive goo
   and a snake that can remove this goo. 

   When the snake eats the goo, it grows and new goo appears. Like all 
   radioactive material, goo decays over time. Eventually it expires, but 
   fortunately for the snake, a new piece of goo appears elsewhere.

   The player is in control of a snake, and the objective is to grow the snake as 
   large as possible. She may change the direction of the snake by pressing one of 
   the four arrow keys. When the snake gets close to a piece of goo, it eats the 
   goo and grows a new segment. If the snake runs into itself or one of the four 
   walls, the game is over. The length of the snake is the player's score. 

   Play
   ----
 
   Run and evaluate 
     (start-snake)
   This will pop up a window with instructions for interacting with the program. 
   Watch how qiuckly the program guesses X. 
|#

;
;
;
;
;                                 ;;
;     ;;; ;                        ;
;    ;   ;;                        ;
;    ;    ;   ;; ;;;      ;;;;     ;  ;;;     ;;;
;    ;         ;;   ;    ;    ;    ;  ;      ;   ;
;     ;;;;     ;    ;         ;    ; ;      ;     ;
;         ;    ;    ;    ;;;;;;    ;;;      ;;;;;;;
;    ;    ;    ;    ;   ;     ;    ;  ;     ;  
;    ;;   ;    ;    ;   ;    ;;    ;   ;     ;    ;
;    ; ;;;    ;;;  ;;;   ;;;; ;;  ;;  ;;;;    ;;;;
;
;
;
;

(require 2htdp/image 2htdp/universe)
;; -----------------------------------------------------------------------------
;; Data Definitions

;; A Pit is a (pit Snake (Listof Goo))
(struct pit (snake goos) #:transparent)

;; A Snake is a (make-snake Direction (cons Seg [Listof Seg]))
(struct snake (dir segs) #:transparent)
;; The head of the snake is the first element in the list of segs.
;; Each segment of a snake is located with:
;;  - x in (0,SIZE),
;;  - y in (0,SIZE).
;; And is SEG-SIZE aligned (x and y are multiples of SEG-SIZE).

;; A Seg is a (posn Number Number)

;; A Goo is a (goo Posn Number)
(struct goo (loc expire) #:transparent)
;; The expire field is a Natural Number that represents the number
;; of ticks until the goo expires. A goo is expired when this field is 1

;; A Direction is one of "up" "down" "left" "right"

;; A Posn is (posn number number)
(struct posn (x y) #:transparent)
;; Represents a two dimensional point.

;; -----------------------------------------------------------------------------
;; Constants

;; Tick Rate 
(define TICK-RATE 1/10)

;; Board Size Constants
(define SIZE 30)

;; Snake Constants
(define SEG-SIZE 15)

;; Goo Constants
(define MAX-GOO 5)
(define EXPIRATION-TIME 150)

;; GRAPHICAL BOARD
(define WIDTH-PX  (* SEG-SIZE 30))
(define HEIGHT-PX (* SEG-SIZE 30))

;; Visual constants
(define MT-SCENE (empty-scene WIDTH-PX HEIGHT-PX))
(define GOO-IMG (bitmap "graphics/goo.gif"))
(define SEG-IMG  (bitmap "graphics/body.gif"))
(define HEAD-IMG (bitmap "graphics/head.gif"))

(define HEAD-LEFT-IMG HEAD-IMG)
(define HEAD-DOWN-IMG (rotate 90 HEAD-LEFT-IMG))
(define HEAD-RIGHT-IMG (flip-horizontal HEAD-LEFT-IMG))
(define HEAD-UP-IMG (flip-vertical HEAD-DOWN-IMG))

(define ENDGAME-TEXT-SIZE 15)


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
;; -----------------------------------------------------------------------------

;; Start the Game
(define (start-snake)
  (big-bang (pit (snake "right" (list (posn 1 1)))
                 (list (fresh-goo)
                       (fresh-goo)
                       (fresh-goo)
                       (fresh-goo)
                       (fresh-goo)
                       (fresh-goo)))
            (on-tick next-pit TICK-RATE)
            (on-key direct-snake)
            (to-draw render-pit)
            (stop-when dead? render-end)))

;; Pit -> Pit
;; Take one step: eat or slither
(define (next-pit w)
  (define snake (pit-snake w))
  (define goos  (pit-goos w))
  (define goo-to-eat (can-eat snake goos))
  (if goo-to-eat
      (pit (grow snake) (age-goo (eat goos goo-to-eat)))
      (pit (slither snake) (age-goo goos))))

;; Pit KeyEvent -> Pit
;; Handle a key event
(define (direct-snake w ke)
  (cond [(dir? ke) (world-change-dir w ke)]
        [else w]))

;; Pit -> Scene
;; Render the world as a scene
(define (render-pit w)
  (snake+scene (pit-snake w)
               (goo-list+scene (pit-goos w) MT-SCENE)))

;; Pit -> Boolean
;; Is the snake dead?
(define (dead? w)
  (define snake (pit-snake w))
  (or (self-colliding? snake) (wall-colliding? snake)))

;; Pit -> Scene
;; produces a gameover image
(define (render-end w)
  (overlay (text "Game over" ENDGAME-TEXT-SIZE "black")
           (render-pit w)))

;                                                                                          
;                                                                                          
;                                                                                          
;     ;;;;    ;;                    ;;              ;;;;;;     ;            ;;             
;    ;   ;     ;                     ;              ;  ;                     ;             
;   ;          ;     ;;;;    ;;; ;   ; ;;;;            ;     ;;;     ;;; ;   ; ;;;;  ;;;;; 
;   ;          ;    ;    ;  ;;  ;;   ;  ;              ;       ;    ;;  ;;   ;  ;   ;    ; 
;   ;          ;    ;    ;  ;        ;;;               ;       ;    ;        ;;;     ;;;;  
;   ;          ;    ;    ;  ;        ; ;               ;       ;    ;        ; ;         ; 
;    ;   ;     ;    ;    ;  ;    ;   ;  ;              ;       ;    ;    ;   ;  ;   ;    ; 
;     ;;;    ;;;;;   ;;;;    ;;;;   ;;  ;;;           ;;;    ;;;;;   ;;;;   ;;  ;;; ;;;;;  
;                                                                                          
;                                                                                          
;                                                                                          
;                                                                                          

;; -----------------------------------------------------------------------------
;; Movement and Eating

;; -----------------------------------------------------------------------------
;; Eating and Growth

;; Snake [Listof Goo] -> Goo or #f
;; Can the snake eat any of the goos?
;; > (can-eat (snake "right" `(,(posn 3 3))) `(,(goo (posn 3 3) 130)))
;; (goo (posn 3 3) 130)
(define (can-eat snake goos)
  (cond [(empty? goos) #f]
        [else (if (close? (snake-head snake) (first goos))
                  (first goos)
                  (can-eat snake (rest goos)))]))

;; [Listof Goo] Goo -> [Listof Goo]
;; Eat and replenish a goo.
;; > (eat (list (goo (posn 5 5) 5)) (goo (posn 5 5) 5))
;; (list (new-goo))
(define (eat goos goo-to-eat)
  (cons (fresh-goo) (remove goo-to-eat goos)))

;; Seg Goo -> Boolean
;; Is the segment close to the goo?
;; > (close? (posn 1 2) (goo (posn 1 2) 4))
;; #t
(define (close? s g)
  (posn=? s (goo-loc g)))

;; Grow the snake one segment.
;; Snake -> Snake
;; > (grow snake0)
;; (snake "right" `(,(posn 2 1) ,(posn 1 1)))
(define (grow sn)
  (snake (snake-dir sn) (cons (next-head sn) (snake-segs sn))))

;; -----------------------------------------------------------------------------
;; Movement

;; Snake -> Snake
;; Slither the snake forward one segment.
;; > (slither snake0)
;; (snake "right" (posn 2 1))
(define (slither sn)
  (snake (snake-dir sn)
         (cons (next-head sn) (all-but-last (snake-segs sn)))))

;; Snake -> Seg
;; Compute the next head position of the snake.
;; > (next-head snake0)
;; (snake "right" (list (posn 2 1)))
(define (next-head sn)
  (define head (snake-head sn))
  (define dir (snake-dir sn))
  (cond [(string=? dir "up") (posn-move head 0 -1)]
        [(string=? dir "down") (posn-move head 0 1)]
        [(string=? dir "left") (posn-move head -1 0)]
        [(string=? dir "right") (posn-move head 1 0)]))

;; Posn Number Number -> Posn
;; Move the position by dx, dy.
;; > (posn-move (posn 1 1) 2 3)
;; (posn 3 4)
(define (posn-move p dx dy)
  (posn (+ (posn-x p) dx)
        (+ (posn-y p) dy)))

;; (cons X [Listof X]) -> [Listof X]
;; Returns a List that is does not contain the last element of the given list.
;; > (all-but-last '(1 2 3 4))
;; '(1 2 3)
(define (all-but-last segs)
  (cond [(empty? (rest segs)) empty]
        [else (cons (first segs) 
                    (all-but-last (rest segs)))]))

;; -----------------------------------------------------------------------------
;; Rotting Goo

;; [Listof Goo] -> [Listof Goo]
;; Renew and rot goos.
(define (age-goo goos)
  (rot (renew goos)))

;; [Listof Goo] -> [Listof Goo]
;; Renew any rotten goos.
(define (renew goos)
  (cond [(empty? goos) empty]
        [(rotten? (first goos))
         (cons (fresh-goo) (renew (rest goos)))]
        [else
         (cons (first goos) (renew (rest goos)))]))

;; [Listof Goo] -> [Listof Goo]
;; Rot all of the goos.
(define (rot goos)
  (cond [(empty? goos) empty]
        [else (cons (decay (first goos))
                    (rot (rest goos)))]))

;; Goo -> Boolean
;; has the goo expired?
;; > (rotten? (goo 1 2) 0)
;; #t
(define (rotten? g)
  (zero? (goo-expire g)))

;; Goo -> Goo
;; decreases the expire field of goo by one
;; > (decay (goo (posn 1 2) 2))
;; (goo (posn 1 2) 1)
(define (decay g)
  (goo (goo-loc g) (sub1 (goo-expire g))))

;; -> Goo
;; Create random goo with fresh expiration.
;; Property: The position of the goo is:
;;  - x in (0,WIDTH),
;;  - y in (0,HEIGHT).
(define (fresh-goo)
  (goo (posn (add1 (random (sub1 SIZE)))
             (add1 (random (sub1 SIZE))))
       EXPIRATION-TIME))

;                                                                                                      
;                                                                                                      
;                                                                                                      
;                                                                                                      
;                                                                                                      
;   ;;; ;;;;
;    ;   ;
;    ;  ;       ;;;    ;;;   ;;;   ;;;; ;
;    ; ;       ;   ;    ;     ;   ;    ;;
;    ;;;;     ;     ;    ;   ;    ;
;    ;   ;    ;;;;;;;    ;   ;     ;;;;;
;    ;   ;    ;           ; ;           ;
;    ;    ;    ;    ;     ; ;     ;     ;
;   ;;;   ;;    ;;;;       ;      ;;;;;;
;                          ;
;                         ;
;                      ;;;;;
;                                                                                                      
;; -----------------------------------------------------------------------------

;; String -> Boolean
;; Is the given value a direction?
;; > (dir? "up")
;; #t
(define (dir? x)
  (or (string=? x "up") 
      (string=? x "down") 
      (string=? x "left") 
      (string=? x "right")))

;; Pit Direction-> Pit
;; Change the direction (if not opposite current snake dir)
;; > (world-change-dir world0 "up")
;; (pit snake1 (list goo0)) 
(define (world-change-dir w d)
  (define the-snake (pit-snake w))
  (cond [(and (opposite-dir? (snake-dir the-snake) d) 
              ;; consists of the head and at least one segment:
              (cons? (rest (snake-segs the-snake))))
         (stop-with w)]
        [else 
         (pit (snake-change-dir the-snake d) 
              (pit-goos w))]))

;; Direction Direction -> Boolean
;; Are d1 and d2 opposites?
;; > (opposite-dir? "up" "down")
;; #t
(define (opposite-dir? d1 d2)
  (cond [(string=? d1 "up")    (string=? d2 "down")]
        [(string=? d1 "down")  (string=? d2 "up")]
        [(string=? d1 "left")  (string=? d2 "right")]
        [(string=? d1 "right") (string=? d2 "left")]))


;
;
;
; 
;                                      ;;
;   ;;;;;;                              ;
;    ;    ;                             ;
;    ;    ;     ;;;     ;; ;;;      ;;; ;     ;;;      ;;  ;;;
;    ;    ;    ;   ;     ;;   ;    ;   ;;    ;   ;      ;;;
;    ;;;;;    ;     ;    ;    ;   ;     ;   ;     ;     ;
;    ;  ;     ;;;;;;;    ;    ;   ;     ;   ;;;;;;;     ;
;    ;   ;    ;          ;    ;   ;     ;   ;           ;
;    ;    ;    ;    ;    ;    ;    ;   ;;    ;    ;     ;
;   ;;;   ;;    ;;;;    ;;;  ;;;    ;;; ;;    ;;;;     ;;;;;
; 
; 
; 
;                                                                                            
;; -----------------------------------------------------------------------------

;; Snake Scene -> Scene
;; Draws the snake onto the scene
;; > (snake+scene snake0 MT-SCENE)
;; (place-image SEG-IMG 8 8 MT-SCENE)
(define (snake+scene snake scene)
  (define snake-body-scene
    (img-list+scene  (snake-body snake) SEG-IMG scene))
  (define dir (snake-dir snake))
  (img+scene (snake-head snake) 
             (cond [(string=? "up" dir) HEAD-UP-IMG]
                   [(string=? "down" dir) HEAD-DOWN-IMG]
                   [(string=? "left" dir) HEAD-LEFT-IMG]
                   [(string=? "right" dir) HEAD-RIGHT-IMG])
             snake-body-scene))

;; [Listof Goo] Scene -> Scene
;; draws all of the goo to a scene
;; > (goo-list+scene (list goo0) MT-SCENE)
;; (place-image GOO-IMG 32 32 MT-SCENE)
(define (goo-list+scene goos scene)
  ;; [Listof Goo] -> [Listof Posn]
  ;; gets the posns of all the goo 
  ;; > (get-posns-from-goo (list (goo (posn 2 2) 1) (goo (posn 3 3) 1))
  ;; (list (posn 2 2) (posn 3 3))
  (define (get-posns-from-goo goos)
    (cond [(empty? goos) empty]
          [else (cons (goo-loc (first goos))
                      (get-posns-from-goo (rest goos)))]))
  (img-list+scene (get-posns-from-goo goos) GOO-IMG scene))

;; [Listof Posn] Image Scene -> Scene
;; Draws a the image to each posn in the list
;; > (img-list+scene (list (posn 1 1)) GOO-IMG MT-SCENE)
;; (place-image GOO-IMG 8 8
;;              (img-list+scene empty GOO-IMG MT-SCENE))
(define (img-list+scene posns img scene)
  (cond [(empty? posns) scene]
        [else (img+scene (first posns)
                         img 
                         (img-list+scene (rest posns) img scene))]))

;; Posn Image Scene -> Scene
;; Draws a the given image onto the scene at the posn.
;; > (img+scene (posn 2 2) GOO-IMG MT-SCENE)
;; (place-image GOO-IMG 32 32 MT-SCENE)
(define (img+scene posn img scene)
  (place-image img 
               (* (posn-x posn) SEG-SIZE)
               (* (posn-y posn) SEG-SIZE)
               scene))

;                                                                                  
;                                                                                  
;                                                                                  
;                                                                                  
;                            ;;                                                    
;   ;;;;;;;                   ;               ;;;; ;                               
;    ;    ;                   ;              ;    ;;                               
;    ;    ;   ;; ;;;      ;;; ;             ;           ;;;;   ;; ;  ;      ;;;    
;    ;  ;      ;;   ;    ;   ;;             ;          ;    ;   ;; ;; ;    ;   ;   
;    ;;;;      ;    ;   ;     ;             ;               ;   ;  ;  ;   ;     ;  
;    ;  ;      ;    ;   ;     ;             ;   ;;;;;  ;;;;;;   ;  ;  ;   ;;;;;;;  
;    ;    ;    ;    ;   ;     ;             ;      ;  ;     ;   ;  ;  ;   ;        
;    ;    ;    ;    ;    ;   ;;              ;     ;  ;    ;;   ;  ;  ;    ;    ;  
;   ;;;;;;;   ;;;  ;;;    ;;; ;;              ;;;;;    ;;;; ;; ;;; ;; ;;    ;;;;   
;                                                                                  
;                                                                                  
;                                                                                  
;                                                                                  
;; -----------------------------------------------------------------------------

;; Snake -> Boolean
;; Determine if the snake is colliding with itself.
;; > (self-colliding? (snake "up" (list (posn 1 1) (posn 2 1)
;;                                      (posn 2 2) (posn 1 2)
;;                                      (posn 1 1))))
;; #t
(define (self-colliding? sn)
  (cons? (member (snake-head sn) (snake-body sn))))

;; Snake -> Boolean
;; Determine if the snake is colliding with any of the walls.
;; > (wall-colliding? (snake "up" (list (posn 0 1))))
;; #t
(define (wall-colliding? sn)
  (define x (posn-x (snake-head sn)))
  (define y (posn-y (snake-head sn)))
  (or (= 0 x) (= x SIZE)
      (= 0 y) (= y SIZE)))



;                                                                                            
;                                                                                            
;                                                                                            
;                                                                                            
;                                                                                            
;     ;;;     ;;;  ;;; ;;;   ;;;  ;;;;;;;   ;;;;;     ;;;;;;;     ;;;     ;;;;;;   ;;;   ;;; 
;      ;;      ;    ;   ;     ;      ;        ;          ;         ;;      ;    ;   ;     ;  
;     ;  ;     ;    ;    ;   ;       ;        ;          ;        ;  ;     ;    ;    ;   ;   
;     ;  ;     ;    ;     ; ;        ;        ;          ;        ;  ;     ;    ;     ; ;    
;     ;  ;     ;    ;      ;         ;        ;          ;        ;  ;     ;;;;;       ;     
;    ;;;;;;    ;    ;     ; ;        ;        ;    ;     ;       ;;;;;;    ;  ;        ;     
;    ;    ;    ;    ;    ;   ;       ;        ;    ;     ;       ;    ;    ;   ;       ;     
;   ;      ;   ;    ;   ;     ;      ;        ;    ;     ;      ;      ;   ;    ;      ;     
;  ;;;    ;;;   ;;;;   ;;;   ;;;  ;;;;;;;   ;;;;;;;;  ;;;;;;;  ;;;    ;;; ;;;   ;;   ;;;;;   
;                                                                                            
;                                                                                            
;                                                                                            
;
;; -----------------------------------------------------------------------------
;; Posn Posn -> Boolean
;; Are the two posns are equal?
;; > (posn=? (posn 1 1) (posn 1 1))
;; true
(define (posn=? p1 p2)
  (and (= (posn-x p1) (posn-x p2))
       (= (posn-y p1) (posn-y p2))))

;; Access the head position of the snake.
;; snake-head : Snake -> Seg
;; > (snake-head (snake "right" (list (posn 1 1) (posn 2 1)))
;; (posn 1 1)
(define (snake-head sn)
  (first (snake-segs sn)))

;; Snake -> [Listof Segs]
;; returns the snake's body.
;; That is everyting that isn't the snake's head.
(define (snake-body sn)
  (rest (snake-segs sn)))

;; Snake Direction -> Snake 
(define (snake-change-dir sn d)
  (snake d (snake-segs sn)))


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
;; -----------------------------------------------------------------------------

;; Initial Structures

(define goo-list (build-list MAX-GOO (lambda (x) (fresh-goo))))
(define snake0 (snake "right" (list (posn 1 1)))) ;; BUG? << -- moving this define into the test module blows up
(define world0 (pit snake0 goo-list))

(define left-snake0 (snake "left" (list (posn 1 1))))
(define left-world0 (pit left-snake0 goo-list))

;; Test Constants

(define snake1 (snake "left" (list (posn 5 5))))
(define snake2 (snake "up" (list (posn 2 2) (posn 2 3) (posn 2 4))))
(define wall-snake (snake "right" (list (posn 0 1) (posn 1 1))))
(define self-eating-snake (snake "up" (list
                                       (posn 19 3)
                                       (posn 19 4)
                                       (posn 20 4)
                                       (posn 21 4)
                                       (posn 22 4)
                                       (posn 22 3)
                                       (posn 21 3)
                                       (posn 20 3)
                                       (posn 19 3)
                                       (posn 18 3))))
(define goo1 (goo (posn 5 5) 45))
(define goo2 (goo (posn 4 8) 1))
(define goo3 (goo (posn 6 9) 40))
(define goo4 (goo (posn 1 1) 120))
(define goo5 (goo (posn 1 9) 58))
(define goo-list1 (list goo1 goo2 goo3 goo4 goo5))
(define world1 (pit snake1 goo-list1))
(define world2 (pit snake2 goo-list1))

(define right-snake1 (snake "right" (list (posn 5 5))))
(define right-world1 (pit right-snake1 goo-list1))

(module+ test 
  
  (require rackunit rackunit/text-ui)
  
  ;; test the major basic snake functions
  (check-equal? (pit-snake (next-pit world2))
                (snake "up" (list (posn 2 1) (posn 2 2) (posn 2 3))))
  (check-equal? (pit-snake (next-pit world1))
                (snake "left" (list (posn 4 5) (posn 5 5))))
  (check-true (let ([f (pit-goos (next-pit world1))])
                (= (length f) MAX-GOO)))
  (check-equal? (pit-snake (next-pit world0))
                (snake "right" (list (posn 2 1))))
  (check-equal? (pit-snake (next-pit (pit snake0 (list (goo (posn 1 1) 130)))))
                (snake "right" (list (posn 2 1) (posn 1 1))))
  
  (check-equal? (direct-snake world0 "down") 
                (world-change-dir world0 "down"))
  (check-equal? (direct-snake world0 "a")
                world0)
  
  (check-equal? (render-pit world0)
                (snake+scene snake0
                             (goo-list+scene goo-list MT-SCENE)))
  (check-equal? (render-pit world1)
                (snake+scene snake1 (goo-list+scene goo-list1 MT-SCENE)))
  (check-equal? (render-pit world2)
                (snake+scene snake2 (goo-list+scene goo-list1 MT-SCENE)))
  
  (check-true (dead? (pit wall-snake '())))
  (check-true (dead? (pit self-eating-snake  '())))
  (check-false (dead? (pit snake1  '())))
  (check-false (dead? (pit snake2  '())))
  (check-false (dead? world0))
  
  (check-equal? (render-end world1)
                (overlay (text "Game over" 15 "black")
                         (render-pit world1)))
  (check-equal? (render-end world2)
                (overlay (text "Game over" 15 "black")
                         (render-pit world2)))
  
  ;; Properties
  ;; -----------------------------------------------------------------------------
  
  ;; Property: each goo in the list has its 'expire' field decremented by 1
  (define (prop:goo-rot-- i)
    (test-begin 
     (for ([i (in-range i)])
       (define goos (list-of-n-goo MAX-GOO))
       (define goo-initial-expire (map goo-expire goos))
       (check-equal? (map sub1 goo-initial-expire)
                     (map goo-expire (age-goo goos))))))
  
  ;; Property: The position of the goo is:
  ;;  - x in (0,WIDTH-SEGS),
  ;;  - y in (0,HEIGHT-SEGS).
  (define (prop:new-goo-range i)
    (test-begin 
     (for ([i (in-range i)])
       (define f (fresh-goo))
       (check-true (and (< 0 (posn-x (goo-loc f)) SIZE)
                        (< 0 (posn-y (goo-loc f)) SIZE))))))
  
  ;; Number -> [Listof Goo]
  ;; creates a list of randomly selected goo that is n long.
  (define (list-of-n-goo n)
    (cond [(zero? n) empty]
          [else (define rand (random 5))
                (cons (list-ref goo-list1 rand) (list-of-n-goo (sub1 n)))]))
  
  ;; testing pit-snake event handling 
  
  (check-equal? (pit-snake (world-change-dir (pit snake1 "foobar") "down"))
                (snake "down" (list (posn 5 5))))
  (check-equal? (pit-snake (world-change-dir (pit snake2 "left") "left"))
                (snake "left" (list (posn 2 2) (posn 2 3) (posn 2 4))))
  
  (prop:goo-rot-- 1000)
  
  (check-equal? (grow snake0)
                (snake "right" (list (posn 2 1) (posn 1 1))))
  (check-equal? (grow snake1)
                (snake "left" (list (posn 4 5) (posn 5 5))))
  (check-equal? (grow snake0)
                (snake "right" (list (posn 2 1) 
                                     (posn 1 1))))
  
  (prop:new-goo-range 1000)
  
  (check-equal? (can-eat (snake "right" `(,(posn 3 3))) `(,(goo (posn 3 3) 130)))
                (goo (posn 3 3) 130))
  (check-false (can-eat (snake "right" `(,(posn 3 3))) `(,(goo (posn 3 4) 130)
                                                         ,(goo (posn 2 2) 0))))
  (check-equal? (can-eat snake0 (list (goo (posn 1 1) 1)))
                (goo (posn 1 1) 1))
  (check-false (can-eat snake0 (list (goo (posn 2 1) 1))))
  
  (check-equal? (slither snake0) (snake "right" (list (posn 2 1))))
  (check-equal? (slither (snake "right" (list (posn 4 4) 
                                              (posn 4 5)
                                              (posn 4 6))))
                (snake "right" (list (posn 5 4) (posn 4 4) (posn 4 5))))
  (check-equal? (slither snake0)
                (snake "right" (list (posn 2 1))))
  
  (check-equal? (length (eat (list (goo (posn 1 1) 130)) (goo (posn 1 1) 130)))
                1)
  (check-equal? (grow (snake "left" (list (posn 1 1))))
                (snake "left" (list (posn 0 1) (posn 1 1))))
  
  (check-equal? (next-head snake0) (posn 2 1))
  (check-equal? (next-head (snake "left" (list (posn 1 1))))
                (posn 0 1))
  (check-equal? (next-head (snake "up" (list (posn 1 1))))
                (posn 1 0))
  (check-equal? (next-head (snake "down" (list (posn 1 1)))) 
                (posn 1 2))
  (check-equal? (next-head snake0) (posn 2 1))
  
  (check-equal? (posn-move (posn 1 1) 2 3) (posn 3 4))
  (check-equal? (posn-move (posn 3 4) 6 0) (posn 9 4))
  (check-equal? (posn-move (posn 2 8) 0 5) (posn 2 13))
  (check-equal? (posn-move (posn 2 3) 0 0) (posn 2 3))
  
  (check-equal? (all-but-last '(1 2 3 4 5 6))
                '(1 2 3 4 5))
  (check-equal? (all-but-last (snake-segs snake2))
                `(,(posn 2 2) ,(posn 2 3)))
  (check-equal? (all-but-last (list 0)) empty)
  (check-equal? (all-but-last (list 0 1 2)) (list 0 1))
  
  ;; testing snake-key-handling
  
  (check-true (dir? "up"))
  (check-true (dir? "down"))
  (check-true (dir? "left"))
  (check-true (dir? "right"))
  (check-false (dir? "f"))
  (check-true (dir? "right"))
  
  (check-equal? (world-change-dir world1 "left") world1)
  (check-equal? (world-change-dir world1 "right") right-world1)
  (check-equal? (world-change-dir world0 "left") left-world0)
  (check-equal? (world-change-dir world0 "right") 
                (pit (snake "right" (snake-segs (pit-snake world0)))
                     (pit-goos world0)))
  (check-equal? (world-change-dir world0 "down")
                (pit (snake "down" (snake-segs (pit-snake world0)))
                     (pit-goos world0)))
  
  (check-true (opposite-dir? "up" "down"))
  (check-true (opposite-dir? "left" "right"))
  (check-true (opposite-dir? "right" "left"))
  (check-true (opposite-dir? "down" "up"))
  (check-false (opposite-dir? "left" "down"))
  (check-false (opposite-dir? "right" "down"))
  (check-false (opposite-dir? "down" "left"))
  (check-false (opposite-dir? "up" "right"))
  (check-true (opposite-dir? "up" "down"))
  (check-true (opposite-dir? "down" "up"))
  (check-false (opposite-dir? "up" "up") false)
  (check-equal? (opposite-dir? "right" "left") true)
  (check-equal? (opposite-dir? "left" "right") true)
  
  ;; testing snake rendering
  
  (check-equal? (snake+scene snake1 MT-SCENE)
                (place-image HEAD-LEFT-IMG (* 5 SEG-SIZE)
                             (* 5 SEG-SIZE) MT-SCENE))
  (check-equal? (snake+scene snake2 MT-SCENE)
                (img+scene (posn 2 2) HEAD-UP-IMG 
                           (img+scene (posn 2 3) SEG-IMG 
                                      (img+scene (posn 2 4) SEG-IMG MT-SCENE))))
  (check-equal? (snake+scene (snake "up" (list (posn 1 1))) MT-SCENE)
                (img+scene (posn 1 1) HEAD-UP-IMG MT-SCENE))
  
  (check-equal? (goo-list+scene (list goo1) MT-SCENE)
                (place-image GOO-IMG (* 5 SEG-SIZE)
                             (* 5 SEG-SIZE) MT-SCENE))
  (check-equal? (goo-list+scene goo-list1 MT-SCENE)
                (img-list+scene (list (posn 5 5) (posn 4 8) (posn 6 9) (posn 1 1) (posn 1 9))
                                GOO-IMG MT-SCENE))
  
  (check-equal? (img-list+scene (list (posn 3 3) (posn 4 4)) SEG-IMG MT-SCENE)
                (place-image SEG-IMG (* 3 SEG-SIZE) (* 3 SEG-SIZE) 
                             (place-image SEG-IMG (* 4 SEG-SIZE) (* 4 SEG-SIZE) MT-SCENE)))
  (check-equal? (img-list+scene (list (posn 1 1) (posn 10 10)) SEG-IMG MT-SCENE)
                (place-image SEG-IMG (* 1 SEG-SIZE) (* 1 SEG-SIZE)  
                             (place-image SEG-IMG (* 10 SEG-SIZE) (* 10 SEG-SIZE) MT-SCENE)))
  (check-equal? (img-list+scene (list (posn 1 1)) GOO-IMG MT-SCENE)
                (place-image GOO-IMG SEG-SIZE SEG-SIZE
                             (img-list+scene empty GOO-IMG MT-SCENE)))
  
  (check-equal? (img+scene (posn 4 3) SEG-IMG MT-SCENE)
                (place-image SEG-IMG (* 4 SEG-SIZE) (* 3 SEG-SIZE)  MT-SCENE))
  (check-equal? (img+scene (posn 5 2) GOO-IMG MT-SCENE)
                (place-image GOO-IMG (* 5 SEG-SIZE) (* 2 SEG-SIZE)  MT-SCENE))
  (check-equal? (img+scene (posn 1 1) SEG-IMG MT-SCENE)
                (place-image SEG-IMG SEG-SIZE SEG-SIZE MT-SCENE))
  
  ;; testing the endgame
  (check-false (self-colliding? snake1))
  (check-false (self-colliding? snake2))
  (check-false (self-colliding? wall-snake))
  (check-true (self-colliding? self-eating-snake))
  (check-false (self-colliding? snake0))
  (check-true (self-colliding? (snake (snake-dir snake0)
                                      (cons (posn 1 1) 
                                            (snake-segs snake0)))))
  
  (check-false (wall-colliding? snake1))
  (check-false (wall-colliding? snake2))
  (check-false (wall-colliding? self-eating-snake))
  (check-true (wall-colliding? wall-snake))
  (check-true 
   (wall-colliding? (snake "right" (list (posn (/ WIDTH-PX SEG-SIZE) 0)))))
  (check-true 
   (wall-colliding? (snake "down" (list (posn 0 (/ HEIGHT-PX SEG-SIZE))))))
  (check-true 
   (wall-colliding? (snake "up" (list (posn 1 0)))))
  (check-equal? (wall-colliding? (snake "right" 
                                        (list (posn 0 1))))
                true)
  (check-equal? (wall-colliding? (snake "right" 
                                        (list (posn 1 0))))
                true)
  (check-equal? (wall-colliding? (snake "right" 
                                        (list (posn 1 1))))
                false)
  (check-true (wall-colliding? (snake "right" (list (posn 1 SIZE)))))
  
  
  ;; testing utilities functions 
  
  (check-false (posn=? (posn 1 1) (posn 2 2)))
  (check-false (posn=? (posn 1 2) (posn 2 1)))
  (check-true (posn=? (posn 3 4) (posn 3 4)))
  (check-true (posn=? (posn 2 2) (posn 2 2)))
  (check-equal? (posn=? (posn 1 2) (posn 1 1)) false)
  (check-equal? (posn=? (posn 1 2) (posn 1 2)) true)
  (check-equal? (posn-move (posn 0 0) 2 3) (posn 2 3))    
  
  (check-equal? (snake-head snake1) (posn 5 5))
  (check-equal? (snake-head snake2) (posn 2 2))
  (check-equal? (snake-head snake0) (posn 1 1))
  
  (check-equal? (snake-body snake1) empty)
  (check-equal? (snake-body snake0) empty)
  (check-equal? (snake-body snake2) (list (posn 2 3) (posn 2 4)))
  
  (check-equal? (snake-change-dir snake0 "up") 
                (snake "up" (list (posn 1 1))))
  (check-equal? (snake-change-dir snake1 "down") 
                (snake "down" (list (posn 5 5))))
  (check-equal? (snake-change-dir snake2 "left") 
                (snake "left" (list (posn 2 2) (posn 2 3) (posn 2 4))))
  
  (check-true (rotten? (goo (posn 1 2) 0)))
  (check-true (rotten? (goo (posn 6 9) 0)))
  (check-true (rotten? (goo (posn 23 2) 0)))
  
  (check-false (rotten? (goo (posn 1 2) 2)))
  (check-false (rotten? (goo (posn 3 45) 45334534)))
  (check-false (rotten? (goo (posn 2 4) 9)))
  
  (check-equal? (decay (goo (posn 1 2) 2))
                (goo (posn 1 2) 1))
  (check-equal? (decay (goo (posn 132 0) 2))
                (goo (posn 132 0) 1))
  (check-equal? (decay (goo (posn 1 2) 10))
                (goo (posn 1 2) 9))
  (check-equal? (decay (goo (posn 3 5) 8))
                (goo (posn 3 5) 7))
  
  "all tests run")
