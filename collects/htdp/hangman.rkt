#lang mzscheme
(require "error.rkt"
         "draw-sig.rkt"
         "big-draw.rkt"
         mzlib/class
         mzlib/unit
         mzlib/etc
         lang/prim
         mred)

(provide
 hangman
 hangman-list
 )

(define-higher-order-primitive hangman hangman/proc (make-word reveal draw-next))
(define-higher-order-primitive hangman-list hangman-list/proc (reveal-list draw-next))

(provide-signature-elements draw^)

#| ------------------------------------------------------------------------
  The Basic Constants |#

(define TITLE "Hangman")
(define WELCOME "Welcome to Hangman") 
(define WINNER  "We have a winner!") 
(define LOSER   "This is the end, my friend. The word was ~a.") 

(define SMALL_A (char->integer #\a))
(define LETTERS
  (build-list 26 (lambda (i) (format "~a" (integer->char (+ SMALL_A i))))))

(define TRUMPET
  (make-object bitmap% 
    (build-path (collection-path "icons") "trumpet.xbm")
    'xbm))

(define PARTS (vector 'right-leg 'left-leg 'left-arm 'right-arm 'body 'head 'noose))

(define NUMBER-OF-GUESSES (vector-length PARTS))

;; char->symbol : char -> symbol 
(define (char->symbol c)
  (string->symbol (format "~a" c)))

;; word->list : symbol -> (listof letter)
(define (word->list sym)
  (map char->symbol (string->list (symbol->string sym))))

;; WORDS : (listof (list letter letter letter))
(define WORDS
  (map word->list
       '(and
         are
         but
         cat
         cow
         dog
         eat
         fee
         gal
         hat
         inn
         jam
         kit
         lit
         met
         now
         owl
         pet
         rat
         sea
         the
         usa
         vip
         was
         zoo)))

;; WORDS2 : (listof (listof letter))
(define WORDS2
  (append (map word->list
               '(apple
                 bottle
                 cattle
                 drscheme
                 elephant
                 folder
                 gift
                 hangman
                 idle
                 jet
                 knowledge
                 length
                 macintosh
                 north
                 ottoman
                 public
                 queue
                 rattlesnake
                 snow
                 toddler
                 under
                 virus
                 xylaphon
                 yellow
                 zombie))
          WORDS))

;; ------------------------------------------------------------------------
;; The GUI

;; two communication channels to GUI; init with setup-gui
(define status-message #f)
(define message-panel #f)

;; setup-gui : str ->* message% panel%
;; to produce a status message and a panel where winning/losing can be announced
;; effect: set up a new frame, arrange the GUI, and display (blank) status word 
(define (setup-gui status)
  (local (#| --------------------------------------------------------------
             The GUI Layout: (computed as a function of constants)
             
             ------------------------------------------------
             |                                              |
             |   a ... z    "Check"  "Status"   word        |
             |   choice%     button%  message%  message%    |
             |                                              |
             |         Welcome/Winner/Loser                 |
             |            message%                          |
             ------------------------------------------------
             |#
          
          (define frame (make-object frame% TITLE #f 100 50))
          (define verti (make-object vertical-panel% frame))
          
          (define panel (make-object horizontal-panel% verti))
          (define _1 (send panel set-alignment 'center 'center))
          
          (define choice (make-object choice% "Guess:" LETTERS panel void))    
          
          ; (make-object message% "  " panel);; added for looks
          (define check (make-object button% "Check" panel 
                          (lambda (x y)
                            (check-guess
                             (char->symbol
                              (list-ref LETTERS
                                        (send choice get-selection)))))))
          
          (define _2 (make-object message% " Status: " panel))
          (define m (make-object message% (uncover status) panel))
          
          (define p (make-object horizontal-panel% verti))
          (define _3 (send p set-alignment 'center 'center))
          (define _4 (make-object message% WELCOME p)))
    (set! status-message m)
    (set! message-panel p)
    (send frame show #t)))

;; ------------------------------------------------------------------------
;; The functions for communicating with the GUI

;; a-winner! : -> void
;; effect: signal win and disable game 
(define (a-winner!)
  (send message-panel change-children (lambda (x) null))
  (make-object message% WINNER message-panel)
  (make-object message% TRUMPET message-panel))

;; a-loser! : -> void
;; effect: signal loss and disable game 
(define (a-loser!)
  (send message-panel change-children (lambda (x) null))
  (make-object message% (format LOSER (uncover chosen)) message-panel))

;; ------------------------------------------------------------------------
;; The functions for playing the game

;; check-guess : symbol -> word 
;; to check whether guess occurs in the chosen word, using reveal 
;; effect: update the status word 
(define (check-guess guess)
  (let ((result (reveal chosen status guess)))
    (cond
      [(equal? result chosen) 
       (send status-message set-label (uncover chosen))
       (a-winner!)]
      [(equal? result status) 
       (draw-next-part (select-piece!))
       (when (the-end?) (a-loser!))]
      [else
       (set! status result)
       (send status-message set-label (uncover status))])))

;; uncover : word -> string
;; to translate the current word into a string, 
;; using abstraction breaking struct-ref
(define (uncover a-word)
  (error 'hangman "impossible"))

;; pieces-left : index into PARTS 
(define pieces-left NUMBER-OF-GUESSES)
(define (init-guesses)
  (set! pieces-left NUMBER-OF-GUESSES))

;; select-piece! : -> void
;; effect: to decrease pieces-left and to pick the next thing to be drawn 
(define (select-piece!)
  (if (> pieces-left 0)
      (begin
        (set! pieces-left (sub1 pieces-left))
        (vector-ref PARTS pieces-left))
      ;; (<= pieces-left 0)
      (vector-ref PARTS 1)))
;; the-end? : -> boolean
;; to check whether the hangman is complet 
(define (the-end?) (zero? pieces-left))

;; USER INTERFACE to student

;; chosen : word 
(define chosen 10)
;; status : word 
(define status 10)
;; reveal :  (word word letter -> word)
(define (reveal chosen status guess)
  (error 'hangman "appply hangman first!"))
;; draw-next-part :  (symbol -> #t)
(define (draw-next-part s)
  (error 'hangman "appply hangman first!"))

;; hangman :
;;   (letter letter letter -> word)
;;   (word word letter -> word)
;;   (symbol -> true)
;;   ->
;;   true
;; effects: set up game status, draw noose, show frame
;; depends on: words are structures 
(define (hangman/proc mw rv dr)
  (check-proc 'hangman mw 3 '1st "3 arguments")
  (check-proc 'hangman rv 3 '2nd "3 arguments")
  (check-proc 'hangman dr 1 '3rd "1 argument")
  (set! chosen (apply mw (list-ref WORDS (random (length WORDS)))))
  (set! status (mw '_ '_ '_))
  ;; make uncover work for structs
  (set! uncover
        (lambda (a-word)
          ;; abstraction breaking hack.
          (parameterize ([current-inspector
                          (with-handlers ([exn:fail? 
                                           (λ (x)
                                             (error 
                                              'hangman
                                              "only works from within DrRacket when using the teaching languages via the languages menu (original exn msg: ~s)"
                                              (exn-message x)))])
                            (dynamic-require ''drscheme-secrets 'drscheme-inspector))])
            (unless (struct? a-word)
              (error 'hangman "expected a struct, got: ~e" a-word))
            (let ([word-vec (struct->vector a-word)])
              (unless (= (vector-length word-vec) 5)
                (error 'hangman "expected words to be structures with three fields, found ~a fields"
                       (- (vector-length word-vec) 2)))
              (format "~a~a~a" 
                      (vector-ref word-vec 1)
                      (vector-ref word-vec 2)
                      (vector-ref word-vec 3))))))
  (initialize rv dr status))

;; word2 = (listof letter)
;; hangman-list : (word2 word2 letter -> word2) (symbol -> #t) -> void
;; effects: set up game status, draw noose, show frame
(define (hangman-list/proc rv dr)
  (check-proc 'hangman-list rv 3 '1st "3 arguments")
  (check-proc 'hangman-list dr 1 '2nd "1 argument")
  (set! chosen (list-ref WORDS2 (random (length WORDS2))))
  (set! status (build-list (length chosen) (lambda (x) '_)))
  ;; make uncover work for lists
  (set! uncover
        (lambda (word)
          (apply string-append (map (lambda (x) (format "~a" x)) word))))
  (initialize (check-fun-res rv list? "list of symbols")
              (check-fun-res dr boolean? "boolean")
              status))


;; initialize : as it says ...
;; the gui, the guess, the picture, the reveal function the draw-next function
(define (initialize rv dr status)
  (init-guesses)
  (set! reveal rv)
  (set! draw-next-part dr)
  (setup-gui status)
  (draw-next-part (select-piece!)))

