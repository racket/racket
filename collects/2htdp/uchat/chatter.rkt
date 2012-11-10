;; The first three lines of this file were inserted by DrScheme. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname chatter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
(require 2htdp/universe)
(require htdp/image)
(require "auxiliaries.rkt")

#|

 +------------------------------------------------------------------+
 | from: text text text text text text                              |
 | from*: text text text text text text                             |
 | ...                                                              |
 | ...                                                              |
 +------------------------------------------------------------------+
 | to: text text text text text text                                |
 | *: text text text text text text                                 |
 | to2: text blah text[]                                            |
 | ...                                                              |
 +------------------------------------------------------------------+

 Convention: the names of participants may not contain ":". 
 The first typed ":" separates the addresses from the message.
 
 TODO:
   -- the editing of too-tall send messages is a bit off screen. 
   
|#

;                                                          
;                                                          
;      ;;                                 ;;           ;;; 
;       ;          ;                       ;          ;    
;    ;; ;   ;;;   ;;;;;   ;;;           ;; ;   ;;;   ;;;;; 
;   ;  ;;  ;   ;   ;     ;   ;         ;  ;;  ;   ;   ;    
;   ;   ;   ;;;;   ;      ;;;;         ;   ;  ;;;;;   ;    
;   ;   ;  ;   ;   ;     ;   ;         ;   ;  ;       ;    
;   ;   ;  ;   ;   ;   ; ;   ;         ;   ;  ;       ;    
;    ;;;;;  ;;;;;   ;;;   ;;;;;         ;;;;;  ;;;;  ;;;;; 
;                                                          
;                                                          
;                                                          
;                                                          

(define-struct world (todraft mmdraft from to))
(define-struct messg (addr text))

;; World = (make-world StrFl StrFl (Listof Line) (Listof Line))
;; StrFl = String or false 
;; Line  = (make-messg String String)
;; WorldPackage = (make-package World (list String String))

;; NOTE: the from and to fields never contain lists that would 
;;   create images taller than MID-2. We enforce this via send
;;   and receive. 

;; visual constants 

(define WIDTH 400)
(define HEIGHT 300) ;; at least 200 
(define MID (/ HEIGHT 2))

(define FTSZ 11)
(define FTCL "black")

(define SP " ")

(define MT (scene+line (empty-scene WIDTH HEIGHT) 0 MID WIDTH MID FTCL))
(define BLANK (rectangle WIDTH FTSZ "outline" "white"))

(define CURSOR (rectangle 3 FTSZ "solid" "red"))

;                                            
;                                            
;                           ;;               
;                            ;               
;   ;; ;;   ;;;  ;; ;;    ;; ;   ;;;   ;; ;; 
;    ;;    ;   ;  ;;  ;  ;  ;;  ;   ;   ;;   
;    ;     ;;;;;  ;   ;  ;   ;  ;;;;;   ;    
;    ;     ;      ;   ;  ;   ;  ;       ;    
;    ;     ;      ;   ;  ;   ;  ;       ;    
;   ;;;;;   ;;;; ;;; ;;;  ;;;;;  ;;;;  ;;;;; 
;                                            
;                                            
;                                            
;                                            

;; World -> Scene 
;; render the world as a scene 
(define (render w)
  (local [(define fr (line*-render (world-from w)))
          (define t1 (line*-render (world-to w)))
          (define last-to-line
            (line-render-cursor (world-todraft w) (world-mmdraft w)))
          (define tt (image-stack t1 last-to-line))]
    (place-image fr 1 1 (place-image tt 1 MID MT))))

;; -----------------------------------------------------------------------------
;; [Listof Line] -> Image
;; render this list of lines 
(define (line*-render lines)
  (cond
    [(empty? lines) (circle 0 "solid" "red")]
    [else
     (local ((define fst (first lines)))
       (image-stack (line-render (messg-addr fst) (messg-text fst))
                    (line*-render (rest lines))))]))

;; -----------------------------------------------------------------------------
;; String String -> Image
;; render a single display line 

(define result0 (text (string-append SP "ada: hello") FTSZ FTCL))

(check-expect (line-render "ada" "hello") result0)

(define (line-render addr msg)
  (text (string-append SP addr ": " msg) FTSZ FTCL))

;; -----------------------------------------------------------------------------
;; StrFl StrFl -> Image
;; render a single display line, with a cursor at current 'editing' position

(check-expect (line-render-cursor false false) 
              (image-append (text SP FTSZ FTCL) CURSOR))

(check-expect (line-render-cursor "ada" false) 
              (image-append (text (string-append SP "ada") FTSZ FTCL) CURSOR))

(check-expect (line-render-cursor "ada" "hello") 
              (image-append result0 CURSOR))

(define (line-render-cursor addr msg)  
  (cond
    [(and (boolean? addr) (boolean? msg)) 
     (image-append (text SP FTSZ FTCL) CURSOR)]
    [(and (string? addr) (boolean? msg)) 
     (image-append (text (string-append SP addr) FTSZ FTCL) CURSOR)]
    [else 
     (image-append (text (string-append SP addr ": " msg) FTSZ FTCL) CURSOR)]))

;                                                   
;                                                   
;                   ;                               
;                                                   
;    ;; ;;;;  ;;  ;;;            ;;;  ;;  ;; ;;  ;; 
;   ;  ;;  ;   ;    ;           ;   ;  ;   ;  ;  ;  
;   ;   ;  ;   ;    ;            ;;;;  ;   ;   ;;   
;   ;   ;  ;   ;    ;           ;   ;  ;   ;   ;;   
;   ;   ;  ;  ;;    ;           ;   ;  ;  ;;  ;  ;  
;    ;;;;   ;; ;; ;;;;;          ;;;;;  ;; ;;;;  ;; 
;       ;                                           
;    ;;;                                            
;                                                   
;                                                   

;; Image Image -> Image 
;; stack two images along left vertical 

(check-expect (image-stack (rectangle 10 20 "solid" "red") 
                           (rectangle 10 20 "solid" "red"))
              (put-pinhole (rectangle 10 40 "solid" "red") 0 0))

(define (image-stack i j)
  (overlay/xy (put-pinhole i 0 0) 0 (image-height i) (put-pinhole j 0 0)))

;; Image Image -> Image 
;; append two images along bottom line

(check-expect (image-append (rectangle 10 20 "solid" "red") 
                            (rectangle 10 20 "solid" "red"))
              (put-pinhole (rectangle 20 20 "solid" "red") 0 0))

(check-expect (image-append (rectangle 10 20 "solid" "red") 
                            (rectangle 10 10 "solid" "red"))
              (overlay/xy (put-pinhole (rectangle 10 20 "solid" "red") 0 0)
                          10 10
                          (put-pinhole (rectangle 10 10 "solid" "red") 0 0)))

(define (image-append i j)
  (local ((define hi (image-height i))
          (define hj (image-height j)))
    (overlay/xy (put-pinhole i 0 0) (image-width i) (- hi hj) (put-pinhole j 0 0))))

;                                                   
;                                                   
;                                 ;                 
;                                                   
;   ;; ;;   ;;;    ;;;;   ;;;   ;;;   ;;; ;;;  ;;;  
;    ;;    ;   ;  ;   ;  ;   ;    ;    ;   ;  ;   ; 
;    ;     ;;;;;  ;      ;;;;;    ;    ;   ;  ;;;;; 
;    ;     ;      ;      ;        ;     ; ;   ;     
;    ;     ;      ;   ;  ;        ;     ; ;   ;     
;   ;;;;;   ;;;;   ;;;    ;;;;  ;;;;;    ;     ;;;; 
;                                                   
;                                                   
;                                                   
;                                                   

;; World Message -> World 
;; receive a message, append to end of received messages 

(define w0 (make-world false false '() '()))
(define w1 (make-world false false (list (make-messg "bob*" "hello")) '()))

(check-expect (receive w0 (list "bob*" "hello")) w1)
(check-expect (receive w1 (list "angie" "world")) 
              (make-world false false 
                          (list (make-messg "bob*" "hello")
                                (make-messg "angie" "world"))
                          '()))

(define (receive w m)
  (make-world (world-todraft w)
              (world-mmdraft w)
              (enqueue 2 (world-from w) (first m) (second m))
              (world-to w)))

;                                                          
;                                                          
;    ;;      ;                                             
;     ;                                                    
;     ;    ;;;   ;; ;;    ;;;           ;;;  ;;  ;; ;;  ;; 
;     ;      ;    ;;  ;  ;   ;         ;   ;  ;   ;  ;  ;  
;     ;      ;    ;   ;  ;;;;;          ;;;;  ;   ;   ;;   
;     ;      ;    ;   ;  ;             ;   ;  ;   ;   ;;   
;     ;      ;    ;   ;  ;             ;   ;  ;  ;;  ;  ;  
;   ;;;;;  ;;;;; ;;; ;;;  ;;;;          ;;;;;  ;; ;;;;  ;; 
;                                                          
;                                                          
;                                                          
;                                                          

;; Nat [Listof Line] String String -> [Listof Line]
;; generative: add the line at end of list; if small enough, okay. 

;; this tests adding one too many items to the list
(check-expect 
 (enqueue 2 (build-list 11 (lambda (i) (make-messg "a*" (number->string i))))
          "ada" "hello world")
 (build-list 11 (lambda (i) 
                  (if (<= i 9)
                      (make-messg "a*" (number->string (+ i 1)))
                      (make-messg "ada" "hello world")))))
                      

(define (enqueue h from* from msg)
  (local ((define candidate (append from* (list (make-messg from msg))))
          (define rendered  (line*-render candidate)))
    (cond
      [(<= (image-height rendered) (- MID h)) candidate]
      [else (enqueue h (rest from*) from msg)])))
    


;                                                                               
;                                                                               
;             ;;    ;                                                        ;; 
;              ;          ;                                                   ; 
;    ;;;    ;; ;  ;;;    ;;;;;           ;;           ;;;;   ;;;  ;; ;;    ;; ; 
;   ;   ;  ;  ;;    ;     ;             ;            ;   ;  ;   ;  ;;  ;  ;  ;; 
;   ;;;;;  ;   ;    ;     ;             ;             ;;;   ;;;;;  ;   ;  ;   ; 
;   ;      ;   ;    ;     ;            ; ; ;             ;  ;      ;   ;  ;   ; 
;   ;      ;   ;    ;     ;   ;        ;  ;          ;   ;  ;      ;   ;  ;   ; 
;    ;;;;   ;;;;; ;;;;;    ;;;          ;; ;         ;;;;    ;;;; ;;; ;;;  ;;;;;
;                                                                               
;                                                                               
;                                                                               
;                                                                               

;; World String -> World u WorldPackage
;; add char to address (unless ":"); otherwise to message draft
;; if line is too wide to display, send off the message

;; edit a text: one char at a time until 
;; -- (1) ":" which is the name or 
;; -- (2) "\r"/rendering a line is wider than the window
;; WHAT HAPPENS IF THE LINE BECOMES WIDER THAN THE BUFFER BEFORE ":" ?

(define WIDE-STRING (replicate WIDTH "m"))

(check-expect (react w0 ":") w0)
(check-expect (react w0 " ") w0)
(check-expect (react w0 "a") (make-world "a" false '() '()))
(check-expect (react (make-world "a" false '() '()) "d") 
              (make-world "ad" false '() '()))
(check-expect (react (make-world "ada" false '() '()) ":")
              (make-world "ada" "" '() '()))
(check-expect (react (make-world "ada" false '() '()) "left")
              (make-world "ada" false '() '()))
(check-expect (react (make-world "ada" "" '() '()) " ")
              (make-world "ada" " " '() '()))
(check-expect (react (make-world "ada" "" '() '()) "left")
              (make-world "ada" "" '() '()))
(check-expect (react (make-world "ada" "" '() '()) "h")
              (make-world "ada" "h" '() '()))
(check-expect (react (make-world "ada" "h" '() '()) ":")
              (make-world "ada" "h:" '() '()))
(check-expect (react w0 "\r") w0)
(check-expect (react (make-world "ada" "x" '() '()) "\r")
              (send "ada" "x" '() '()))
(check-expect (react (make-world "ada" false '() '()) "\r")
              (send "ada" "" '() '()))
(check-expect (react (make-world "ada" WIDE-STRING '() '()) "x")
              (send "ada" WIDE-STRING '() '()))
(check-expect (react (make-world WIDE-STRING false '() '()) "x")
              (send WIDE-STRING "" '() '()))

;; -------------------------------
;; editing keys, not required 

(check-expect (react (make-world "abc" false '() '()) "\b")
              (make-world "ab" false '() '()))
(check-expect (react (make-world false false '() '()) "\b")
              (make-world false false '() '()))
(check-expect (react (make-world "abc" "def" '() '()) "\b")
              (make-world "abc" "de" '() '()))

(define (react w key)
  (local ((define to (world-todraft w))
          (define mm (world-mmdraft w))
          (define from* (world-from w))
          (define to* (world-to w)))
    (cond
      [(key=? "\r" key) 
       (if (boolean? to) w (send to (if (boolean? mm) "" mm) from* to*))]
      ;; -------------------------------
      ;; editing keys; not required 
      [(key=? "\b" key)
       (cond
         [(boolean? to) w]
         [(string? mm) (world-mmdraft! w (shorten mm))]
         ;; (boolean? mm) ^ (not (boolean? to)) => (string? mm)
         [else (world-todraft! w (shorten to))])]       
      ;; -------------------------------
      [(key=? ":" key)
       (cond
         [(boolean? to) w]
         [(boolean? mm) (world-mmdraft! w "")]
         ;; (and (string? to) (string? mm))
         ;; so this string belongs to the end of mm
         [else (world-mmdraft! w (string-append mm ":"))])]
      [else 
       (cond
         [(and (boolean? to) (boolean? mm))
          ;; the key belongs into the address; it can't possibly be too wide
          (cond
            [(bad-name-key? key) w]
            [else (world-todraft! w key)])]
         [(and (string? to) (boolean? mm))
          ;; the key also belongs into address 
          (local ((define to-new (string-append to key)))
            (cond
              [(bad-name-key? key) w]
              [(too-wide? to-new mm) (send to "" from* to*)]
              [else (world-todraft! w to-new)]))]
         ; [(and (boolean? to) (string? mm)) (error 'react "can't happen")]
         [else                          ; (and (string? to) (string? mm))
          ;; the key belongs into the message text 
          (local ((define new-mm (string-append mm key)))
            (cond
              [(bad-msg-key? key) w]
              [(too-wide? to new-mm) (send to mm from* to*)]
              [else (world-mmdraft! w new-mm)]))])])))

;; -----------------------------------------------------------------------------
;; String -> String 
;; take off last character 
(check-expect (shorten "") "")
(check-expect (shorten "abc") "ab")
(define (shorten to)
  (if (= (string-length to) 0)
      to
      (substring to 0 (- (string-length to) 1))))

;; -----------------------------------------------------------------------------
;; String -> Boolean 
;; is this key bad for text messages?

(check-expect (bad-msg-key? " ") false)
(check-expect (bad-msg-key? "right") true)

(define (bad-msg-key? key)
  (or (string=? "\b" key) (>= (string-length key) 2)))

;; -----------------------------------------------------------------------------
;; String -> Boolean 
;; is the key bad (special key, or space or ":") for names

(check-expect (bad-name-key? "x") false)
(check-expect (bad-name-key? ":") true)
(check-expect (bad-name-key? "false") true)

(define (bad-name-key? key)
  (or (string=? " " key) (string=? ":" key) (>= (string-length key) 2)))

;; -----------------------------------------------------------------------------
;; String String [Listof Line] [Listof Line] -> WorldPackage
;; add (make-messg addr msg) to from list, send (list addr msg)

(check-expect (send "ada" "hello" '() '())
              (make-package
               (make-world false false '() (list (make-messg "ada" "hello")))
               (list "ada" "hello")))

(define (send addr msg from* to*)
  (local ((define new-to* (enqueue (+ (image-height CURSOR) 2) to* addr msg)))
    (make-package (make-world false false from* new-to*) (list addr msg))))

;; -----------------------------------------------------------------------------
;; World String -> World 
;; create world from old fiels, use str for mmdraft

(check-expect (world-mmdraft! (make-world false "" '() '()) ":") 
              (make-world false ":" '() '()))

(define from0 (list (make-messg "ada" "hello world")))
(check-expect (world-mmdraft! (make-world false "" '() from0) ":")
              (make-world false ":" '() from0))

(define (world-mmdraft! w str)
  (make-world (world-todraft w) str (world-from w) (world-to w)))

;; -----------------------------------------------------------------------------
;; World String -> World 
;; create world from old fiels, use str for todraft

(check-expect (world-todraft! (make-world false false '() '()) "x") 
              (make-world "x" false '() '()))

(check-expect (world-todraft! (make-world "xy" false '() from0) "xyz")
              (make-world "xyz" false '() from0))

(define (world-todraft! w str)
  (make-world str (world-mmdraft w) (world-from w) (world-to w)))

;; -----------------------------------------------------------------------------
;; String String -> Boolean

(check-expect (too-wide? "" (replicate WIDTH "m")) true)
(check-expect (too-wide? "ada" "hello") false) ; must succeed 

(define (too-wide? addr msg)
  (>= (image-width (line-render-cursor addr msg)) (- WIDTH 2)))

;                                     
;                                     
;                                     
;    ;                    ;           
;   ;;;;;   ;;;    ;;;;  ;;;;;   ;;;; 
;    ;     ;   ;  ;   ;   ;     ;   ; 
;    ;     ;;;;;   ;;;    ;      ;;;  
;    ;     ;          ;   ;         ; 
;    ;   ; ;      ;   ;   ;   ; ;   ; 
;     ;;;   ;;;;  ;;;;     ;;;  ;;;;  
;                                     
;                                     
;                                     
;                                     

(define world0 (make-world false false '() '()))
(define world1 (make-world  
                false false 
                (list (make-messg "ada*" "hello")) 
                (list (make-messg "ada" "world"))))
(define world2 (make-world  
                false false 
                (list (make-messg "ada*" "hello")
                      (make-messg "bob" "secret code")
                      (make-messg "cynthia" "more secrets")
                      (make-messg "doug" "it's all the same to me"))
                (list (make-messg "ada" "world") 
                      (make-messg "*" "world!!!"))))

(render world0)
(render world1)
(render world2)

(define (main n)
  (big-bang world0 
            (on-key react)
            (on-draw render)
            (on-receive receive)
            (check-with world?)
            (name n)
            (state true)
            (register LOCALHOST)))

(define (run* _)
  (launch-many-worlds (main "carl") (main "sam")))

(run* 0)
