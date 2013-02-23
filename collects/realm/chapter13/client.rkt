#lang racket

;; the client for distributed Guess my Number
(require 2htdp/image 2htdp/universe "shared.rkt")

(provide launch-guess-client)

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

;; ClientState = String 
(define ClientState0  "no guess available")

;; Constants
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define TEXT-SIZE 11)
(define HELP-TEXT 
  (text "↑ for larger numbers, ↓ for smaller ones" 
        TEXT-SIZE 
        "blue"))
(define HELP-TEXT2 
  (text "Press = when your number is guessed; q to quit." 
        TEXT-SIZE 
        "blue"))
(define WIDTH (+ (image-width HELP-TEXT2) 10))
(define HEIGHT 150)
(define COLOR "red")
(define SIZE 72)
(define TEXT-X 3)
(define TEXT-UPPER-Y 10)
(define TEXT-LOWER-Y 135)
(define MT-SC 
  (place-image/align 
   HELP-TEXT TEXT-X TEXT-UPPER-Y 
   "left" "top" 
   (place-image/align 
    HELP-TEXT2 
    TEXT-X TEXT-LOWER-Y "left" "bottom"
    (empty-scene WIDTH HEIGHT))))


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


;; String -> ClientState 
;; Launch the Client.
(define (launch-guess-client n host)
  (big-bang ClientState0
            (on-draw draw-guess)
            (on-key handle-keys)
            (name n)
            (register host)
            (on-receive handle-msg)))

;; handle-keys: ClientState Key -> [Package ClientState CtoSMessage] or ClientState 
;; if the key is "up" or "down", ask the server for a different guess 
(define (handle-keys w key)
  (cond [(key=? key "up")   (make-package w "up")]
        [(key=? key "down") (make-package w "down")]
        [(key=? key "q")    (stop-with w)]
        [(key=? key "=")    (stop-with w)]
        [else w]))

;; handle-msg: ClientState StoCMessage -> ClientState 
;; if the message is a number, you got a new guess
(define (handle-msg c msg)
  (number->string msg))

;; draw-guess: ClientState -> Scene
;; renders the state as an image 
(define (draw-guess c)
  (overlay (text c SIZE COLOR) MT-SC))

;                                                    
;                                                    
;                                                    
;                                                    
;                                                    
;   ;;;;;;;                        ;                 
;   ;  ;  ;                        ;                 
;   ;  ;  ;     ;;;      ;;;; ;   ;;;;;;     ;;;; ;  
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
  
  ;; testing the client's key-handling
  
  (check-equal? (handle-keys "55" "up")   (make-package "55" "up"))
  (check-equal? (handle-keys "47" "down") (make-package "47" "down"))
  (check-equal? (handle-keys "10" "=")    (stop-with "10"))
  (check-equal? (handle-keys "66" "k")    "66")
  
  ;; testing the client's message handling
  
  (check-equal? (handle-msg "100" 99) "99")
  (check-equal? (handle-msg "30" -34) "-34")
  
  ;; testing the client's rendering function 
  
  (check-equal? (draw-guess ClientState0) (overlay (text ClientState0 SIZE COLOR) MT-SC))
  (check-equal? (draw-guess "50") (overlay (text "50" SIZE COLOR) MT-SC))
  (check-equal? (draw-guess "25") (overlay (text "25" SIZE COLOR) MT-SC))
  
  "client: all tests run")
