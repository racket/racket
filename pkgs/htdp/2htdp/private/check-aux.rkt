#lang racket/base

(require racket/class
         racket/list
         racket/bool
         racket/match
         htdp/error)

(provide (all-defined-out))

(define INSET  5)     ;; the space around the image in the canvas
(define RATE   1/30)   ;; the clock tick rate 
(define TRIES  3)     ;; how many times should register try to connect to the server 
(define PAUSE  1/2)     ;; # secs to wait between attempts to connect to server 
(define SQPORT 4567) ;; the port on which universe traffic flows

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

;; ---------------------------------------------------------------------------------------------------

;; Any -> Boolean
(define (nat? x)
  (and (number? x) (integer? x) (>= x 0)))

;; Number Symbol Symbol -> Integer
(define (number->integer x [t ""] [p ""])
  (check-arg t (and (number? x) (real? x)) "real number" p x)
  (inexact->exact (floor x)))

;; ---------------------------------------------------------------------------------------------------
;; Nat Nat ->String 
;; converts i to a string, adding leading zeros, make it at least as long as L
(define (zero-fill i L)
  (let ([n (number->string i)])
    (string-append (make-string (max (- L (string-length n)) 0) #\0) n)))

;; ---------------------------------------------------------------------------------------------------

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

;; KeyEvent% -> String
(define (key-release->parts e)
  (define x (send e get-key-release-code))
  (cond
    [(char? x) (string x)]
    [(symbol? x) (symbol->string x)]
    [else (error 'on-key (format "Unknown event: ~a" x))]))

;; ---------------------------------------------------------------------------------------------------
;; Any -> Symbol 
(define (name-of draw tag)
  (define fname  (object-name draw))
  (if fname fname tag))

;; ---------------------------------------------------------------------------------------------------
;; Any -> Boolean
(define (sexp? x)
  (cond
    [(empty? x) true]
    [(string? x) true]
    [(symbol? x) true]
    [(number? x) true]
    [(boolean? x) true]
    [(char? x) true]
    [(pair? x) (and (list? x) (andmap sexp? x))]
    [(and (struct? x) (prefab-struct-key x)) (for/and ((i (struct->vector x))) (sexp? i))]
    [else false]))

; tests:
;(struct s (t) #:prefab)
;(unless (sexp? (list (s (list 'a))))
;  (error 'prefab "structs should be sexp?"))

(define (no-newline? x)
  (not (member #\newline (string->list x))))

;; ---------------------------------------------------------------------------------------------------
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

;; InPort OutPort (X -> Y) -> (U Y Void)
;; process a registration from a potential client, invoke k on name if it is okay
(define (tcp-process-registration in out k)
  (define next (tcp-receive in))
  (match next
    [`(REGISTER ((name ,name)))
     (tcp-send out '(OKAY))
     (k name)]))
  
;; InPort OutPort (U #f String) -> Void 
;; register with the server, send the given name or make up a symbol 
(define (tcp-register in out name)
  (define msg `(REGISTER ((name ,(if name name (gensym 'world))))))
  (tcp-send out msg)
  (define ackn (tcp-receive in))
  (unless (equal? ackn '(OKAY))
    (raise tcp-eof)))

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
   t (and (real? c) (>= (number->integer c t r) 0)) "positive integer" r c))
