#lang scheme/gui

(require htdp/error
         lang/prim
         mzlib/unitsig
         mzlib/etc
         mzlib/class
         mzlib/list)

(provide 
 guess-with-gui
 guess-with-gui-3
 guess-with-gui-list
 )

(define-higher-order-primitive guess-with-gui guess-with-gui/proc 
  (check-guess))
(define-higher-order-primitive guess-with-gui-3 guess-with-gui-3/proc 
  (check-guess))
(define-higher-order-primitive guess-with-gui-list guess-with-gui-list/proc 
  (_ check-guess-list))


;                                                                  
;                                                                  
;                                                                  
;     ;;;;                                                         
;    ;    ;                                                        
;   ;                               ;                   ;          
;   ;         ;;;;   ; ;;;    ;;;; ;;;;   ;;;   ; ;;;  ;;;;   ;;;; 
;   ;        ;    ;  ;;   ;  ;      ;    ;   ;  ;;   ;  ;    ;     
;   ;        ;    ;  ;    ;  ;      ;        ;  ;    ;  ;    ;     
;   ;        ;    ;  ;    ;   ;;;   ;     ;;;;  ;    ;  ;     ;;;  
;   ;        ;    ;  ;    ;      ;  ;    ;   ;  ;    ;  ;        ; 
;    ;    ;  ;    ;  ;    ;      ;  ;    ;   ;  ;    ;  ;        ; 
;     ;;;;    ;;;;   ;    ;  ;;;;    ;;   ;;;;; ;    ;   ;;  ;;;;  
;                                                                  
;                                                                  
;                                                                  

(define TITLE "Bobby's Game")

(define WELCOME "Welcome to Bobby's Game") 

(define DIGITS (build-list 10 (lambda (i) (number->string i))))

(define BUT-SIZE 10)

(define WIDTH BUT-SIZE) ; (* 5 BUT-SIZE)

(define HIGHT BUT-SIZE)

(define STOPS (list 'Perfect 'perfect 'perfect! 'perfect_guess))

(define TRUMPET
  (make-object bitmap% 
    (build-path (collection-path "icons") "trumpet.xbm")
    'xbm))


;                                     
;                                     
;                                     
;     ;;;;                            
;    ;    ;                           
;   ;                                 
;   ;         ;;;   ; ;;; ;;     ;;;  
;   ;        ;   ;  ;;  ;;  ;   ;   ; 
;   ;            ;  ;   ;   ;   ;   ; 
;   ;     ;   ;;;;  ;   ;   ;   ;;;;; 
;   ;     ;  ;   ;  ;   ;   ;   ;     
;    ;    ;  ;   ;  ;   ;   ;   ;     
;     ;;;;;   ;;;;; ;   ;   ;    ;;;; 
;                                     
;                                     
;                                     



#| ------------------------------------------------------------------------
     
     ------------------------------------------------------------------
     |       
     |   CB1  CB2   CB3   CB4   .......   CB*   CB*   CB*   CB*   CB* 
     | 
     |          CB-GUESS1    ...    CB-GUESS*
     | 
     |                     ONE-MESSAGE
     ------------------------------------------------------------------

     Two horizontal panels: 
       the first one with all the colors (as buttons)
       the second is a sequence of colored buttons 
     |#

;; ------------------------------------------------------------------------
;; Setting up the buttons

(define (init-game number-of-digits convert check-guess)
  (local ((define GUESS number-of-digits)
          
          ;; layout
          (define frame (make-object frame% TITLE #f WIDTH HIGHT))
          
          (define verti (make-object vertical-panel% frame))
          
          (define panel
            (let ([panel (make-object horizontal-panel% verti)])
              (send panel set-alignment 'center 'center)
              panel))
          
          (define guess-panel
            (let ([guess-panel (make-object horizontal-panel% verti)])
              (send guess-panel set-alignment 'center 'center)
              guess-panel))
          
          (define message-panel
            (let ([message-panel (make-object horizontal-panel% verti)])
              (send message-panel set-alignment 'center 'center)
              message-panel))
          
          ;; message : a field for displaying basic messages about state of game 
          (define message  (make-object message% WELCOME message-panel))
          
          ;; guesses: status vector, record the choice of digit when set
          (define guesses (make-vector GUESS 0))
          
          ;; the-number : randomly chosen 
          (define the-number 0)
          
          ;; new-game : -> void
          ;; effect: set up new target number, send message that game's ready 
          (define (new-game) 
            (set! the-number (random (expt 10 GUESS)))
            (send message-panel change-children (lambda (x) (list message)))
            (send message set-label WELCOME))
          
          ;; call-back : _ _ -> void
          ;; check status and announce result, possibly set winner
          (define (call-back x y)
            (let ((response (check-guess (convert guesses) the-number)))
              (send message set-label (symbol->string response))
              (when (memq response STOPS)
                ;; announce winner and set up new game
                (send message-panel change-children (lambda (x) empty))
                (make-object message% TRUMPET message-panel)
                (make-object button% "New Game?" message-panel 
                  (lambda (x y) (new-game)))
                (make-object button% "CLOSE?" message-panel
                  (lambda (x y) (send frame show #f)))))))
    
    ;; making the menu choices 
    (for-each (lambda (i)
                (local ((define n (- GUESS i 1)))
                  (make-object choice% #f DIGITS panel 
                    (lambda (x y)
                      (vector-set! guesses n (send x get-selection))))))
              (build-list GUESS identity))
    
    (new-game)
    (make-object button% "Check" guess-panel call-back)
    (send frame show #t)
    #t))


;                                                  
;                                                  
;                                                  
;   ;;;;;                                          
;   ;                                              
;   ;                                   ;          
;   ;     ;     ;  ; ;;;    ;;;;   ; ;;;;;;   ;;;; 
;   ;;;;   ;   ;   ;;   ;  ;    ;  ;;   ;    ;     
;   ;       ; ;    ;    ;  ;    ;  ;    ;    ;     
;   ;        ;     ;    ;  ;    ;  ;    ;     ;;;  
;   ;       ; ;    ;    ;  ;    ;  ;    ;        ; 
;   ;      ;   ;   ;;   ;  ;    ;  ;    ;        ; 
;   ;;;;; ;     ;  ; ;;;    ;;;;   ;     ;;  ;;;;  
;                  ;                               
;                  ;                               
;                  ;                               


;; convert : (vector DIGIT) -> number
;; to convert a vector of digits into a number 
;; 0-th digit is right-most digit in number, 
;; N-th digit is left-most digit in number
(define (convert guesses:vec)
  (local ((define (convert digits)
            (cond
              ((empty? digits) 0)
              (else (+ (first digits) (* (convert (rest digits)) 10))))))
    (convert (vector->list guesses:vec))))

;; guess-with-gui : (num num -> num) -> void
;; effect: init target, init frame, set the check-guess function and show the frame
(define (guess-with-gui/proc cg)
  (check-proc 'guess-with-gui cg 2 'first "two arguments")
  (init-game 5 convert cg))

;; guess-with-gui-3 : (digit digit digit num -> num) -> void
;; effect: init target, init frame, set the check-guess function and show the frame
(define (guess-with-gui-3/proc cg)
  (check-proc 'guess-with-gui-3 cg (+ 3 1) 'first "four arguments")
  (init-game 3 vector->list
             (lambda (lod target) (apply cg (append lod (list target))))))

;; guess-with-gui-list : num ((listof digit) num -> num) -> void
;; effect: init target, init frame, set the check-guess function and show the frame
(define (guess-with-gui-list/proc n cg)
  (check-arg  'guess-with-gui-list
              (and (number? n) (integer? n) (>= n 1)) "positive integer" '1st n)
  (check-proc 'guess-with-gui-list cg 2 'first "two arguments")
  (unless  (<= (expt 10 n) 2147483647)
    (error 'guess-with-gui-list "the given number of digits (~a) is too large" n))
  (init-game n vector->list cg))

