#lang scheme

(require htdp/image
         htdp/error
         (only-in lang/htdp-beginner image?))

(provide (all-defined-out))

(define INSET  5)     ;; the space around the image in the canvas
(define RATE   1/30)   ;; the clock tick rate 
(define TRIES  3)     ;; how many times should register try to connect to the server 
(define PAUSE  1/2)     ;; # secs to wait between attempts to connect to server 
(define SQPORT 4567) ;; the port on which universe traffic flows

(define (K w . r) w)
(define (False w) #f)
(define (True w) #t)

;                                                                               
;                                                                               
;                                                                               
;    ;;;                                              ;;;                       
;   ;   ;                                            ;   ;                      
;   ;   ;                                            ;   ;                      
;   ;       ;;;   ;;;;;  ;;;;;   ;;;   ;;;;          ;   ;  ;   ;  ;   ;        
;   ;      ;   ;  ; ; ;  ; ; ;  ;   ;  ;   ;         ;;;;;  ;   ;   ; ;         
;   ;      ;   ;  ; ; ;  ; ; ;  ;   ;  ;   ;         ;   ;  ;   ;    ;          
;   ;      ;   ;  ; ; ;  ; ; ;  ;   ;  ;   ;         ;   ;  ;   ;    ;          
;   ;   ;  ;   ;  ; ; ;  ; ; ;  ;   ;  ;   ;         ;   ;  ;  ;;   ; ;    ;;   
;    ;;;    ;;;   ; ; ;  ; ; ;   ;;;   ;   ;         ;   ;   ;; ;  ;   ;   ;;   
;                                                                               
;                                                                               
;                                                                               

;; -----------------------------------------------------------------------------

;; Any -> Boolean 
(define (scene? i)
  (and (image? i) (internal-scene? i)))

;; Image -> Boolean 
(define (internal-scene? i) 
  (and (= 0 (pinhole-x i)) (= 0 (pinhole-y i))))

;; Number -> Integer
(define (number->integer x)
  (inexact->exact (floor x)))

;; -----------------------------------------------------------------------------
;; Nat Nat ->String 
;; converts i to a string, adding leading zeros, make it at least as long as L
(define (zero-fill i L)
  (let ([n (number->string i)])
    (string-append (make-string (max (- L (string-length n)) 0) #\0) n)))

;; -----------------------------------------------------------------------------

;; MouseEvent% -> [List Nat Nat MouseEventType]
;; turn a mouse event into its pieces 
(define (mouse-event->parts e)
  (define x (- (send e get-x) INSET))
  (define y (- (send e get-y) INSET))
  (values x y 
          (cond [(send e button-down?) "button-down"]
                [(send e button-up?)   "button-up"]
                [(send e dragging?)    "drag"]
                [(send e moving?)      "move"]
                [(send e entering?)    "enter"]
                [(send e leaving?)     "leave"]
                [else ; (send e get-event-type)
                 (let ([m (send e get-event-type)])
                   (error 'on-mouse (format "Unknown event: ~a" m)))])))

;; KeyEvent% -> String
(define (key-event->parts e)
  (define x (send e get-key-code))
  (cond
    [(char? x) (string x)]
    [(symbol? x) (symbol->string x)]
    [else (error 'on-key (format "Unknown event: ~a" x))]))

;; -----------------------------------------------------------------------------
;; Any -> Symbol 
(define (name-of draw tag)
  (define fname  (object-name draw))
  (if fname fname tag))

;; -----------------------------------------------------------------------------
;; Any -> Boolean
(define (sexp? x)
  (cond
    [(empty? x) true]
    [(string? x) true]
    [(symbol? x) true]
    [(number? x) true]
    [(char? x) true]
    [(pair? x) (and (list? x) (andmap sexp? x))]
    [else false]))

(define (no-newline? x)
  (not (member #\newline (string->list x))))

;; -----------------------------------------------------------------------------
;; exchange one-line messages between worlds and the server

(define tcp-eof (gensym 'tcp-eof))

;; Any -> Boolean 
(define (tcp-eof? a) (eq? tcp-eof a))

;; OutPort Sexp -> Void
(define (tcp-send out msg)
  (write msg out)
  (newline out)
  (flush-output out))

;; InPort -> Sexp
(define (tcp-receive in)
  (with-handlers ((exn? (lambda (x) (raise tcp-eof))))
    (define x (read in))
    (if (eof-object? x) 
        (raise tcp-eof)
        (begin
          (read-line in) ;; read the newline 
          x))))

;                                                   
;                                                   
;                                                   
;    ;;;                         ;;;   ;      ;     
;   ;   ;                       ;   ;  ;      ;     
;   ;   ;                       ;   ;  ;      ;     
;   ;   ;  ; ;;    ;;;;         ;      ;;;;   ;  ;  
;   ;;;;;  ;;  ;  ;   ;         ;      ;   ;  ; ;   
;   ;   ;  ;   ;  ;   ;         ;      ;   ;  ;;    
;   ;   ;  ;      ;   ;         ;      ;   ;  ; ;   
;   ;   ;  ;      ;   ;         ;   ;  ;   ;  ;  ;  
;   ;   ;  ;       ;;;;          ;;;   ;   ;  ;   ; 
;                     ;                             
;                 ;   ;                             
;                  ;;;                              

;; Symbol Any String -> Void
(define (check-pos t c r)
  (check-arg 
   t (and (number? c) (>= (number->integer c) 0)) "positive integer" r c))

;; Symbol Any String String *-> Void
(define (check-image tag i rank . other-message)
  (if (and (pair? other-message) (string? (car other-message)))
      (check-arg tag (image? i) (car other-message) rank i)
      (check-arg tag (image? i) "image" rank i)))

;; Symbol Any String -> Void
(define (check-scene tag i rank)
  (define error "image with pinhole at (~s,~s)")
  (if (image? i)
      (check-arg tag (internal-scene? i) "scene" rank (image-pins i))
      (check-arg tag #f         "scene" rank i)))

;; Symbol Any -> Void 
(define (check-scene-result tname i)
  (if (image? i) 
      (check-result tname internal-scene? "scene" i (image-pins i))
      (check-result tname (lambda (x) (image? x)) "scene" i)))

(define (image-pins i)
  (format "image with pinhole at (~s,~s)" (pinhole-x i) (pinhole-y i)))


;; Symbol Any String -> Void
(define (check-color tag width rank)
  (check-arg tag (or (symbol? width) (string? width)) 
             "color symbol or string" rank width))

;; Symbol (union Symbol String) Nat -> Void
(define (check-mode tag s rank)
  (check-arg tag (or (eq? s 'solid)
                     (eq? s 'outline)
                     (string=? "solid" s)
                     (string=? "outline" s)) "mode (solid or outline)" rank s))

