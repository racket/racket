#| TODO
   1. use chars for letters; admit letters only 
   2. write new exercises 
   3. compare error messages for word to beginner language 
   4. change messages at end to just display the word 
|#
#lang scheme

  (require htdp/world
           htdp/error
           lang/prim
           mzlib/contract
           mzlib/etc
           mzlib/list)
  
  (define (letter? s) (and (symbol? s) (pair? (member s LETTERS))))
  (define LETTERS '(a b c d e f g h i j k l m o p q r s t u v w x y z _))
  
  (define-struct word (one two three))
  
  (provide/contract
   [letter? (any/c . -> . boolean?)]
   [word? (any/c . -> . boolean?)]
   [make-word (letter? letter? letter? . -> . word?)]
   [word-one (word? . -> . letter?)]
   [word-two (word? . -> . letter?)]
   [word-three (word? . -> . letter?)])
  
  (provide-higher-order-primitive
   ;; Letter = Symbol
   ;; type Word 
   
   ;; (Letter Letter Letter -> Word) 
   ;; (Word Word Letter -> Word) 
   ;; (Symbol Scene -> Scene)
   ;;  ->
   ;;  true
   ;; given a function that makes letters from words, and 
   ;; a function that compares the chosen word to the status word with current guess,
   ;; and a function that adds a body part to a Scene, start the world and set up 
   ;; an event handler to play hangman 
   hangman (reveal draw-body))
  
  (define (hangman rv dr)
    (check-proc 'hangman rv 3 'first "3 arguments")
    (check-proc 'hangman dr 2 'second "2 arguments")
    (local ((define (reveal-list ch st gu)
              (local ((define w ; status @ t+1
                        (rv (apply make-word ch) (apply make-word st) gu)))
                (list (word-one w) (word-two w) (word-three w)))))
      (hangman-list reveal-list dr)))
  
  (provide-higher-order-primitive
   ;; Word = [Listof Symbol]
   
   ;; (Word Word Symbol -> Symbol) (Symbol Scene -> Scene) -> true
   ;; given a function that compares the chosen word, the status word, and 
   ;; the current guess, start the world and install a event handler for 
   ;; characters that plays hangman
   hangman-list (reveal add-body-part))
  
  (provide
   ;; [Listof Symbols]
   body-parts)
  
  (define body-parts
    {list 'noose 'head 'right-arm 'left-arm 'body 'right-leg 'left-leg})
  
  (define (hangman-list reveal-list add-next-part)
    (check-proc 'hangman-list reveal-list 3 'first "3 arguments")
    (check-proc 'hangman-list add-next-part 2 'second "2 arguments")
    (local ((define chosen (list-ref WORDS (random (length WORDS))))
            (define status (build-list (length chosen) (lambda (x) '_)))
            (define world0 (list chosen status body-parts))
            ;; World KeyEvent -> World 
            (define (click world ke)
              (define pcs (third world))
              (define wrd (first world))
              (define sta (second world))
              (define cmp (reveal-list wrd sta (char->symbol ke)))
              (cond
                [(symbol? ke) world]
                [(equal? sta cmp) (list wrd sta (rest pcs))]
                [else (list wrd cmp pcs)]))
            ;; World -> Scene
            (define (image world)
              (define wrd (first world))
              (define cmp (second world))
              (define pcs (third world))
              (define scn
                (place-image (text (list-word->string (second world)) 18 'red) 20 100 
                             (add-up-to body-parts pcs (empty-scene 200 200))))
              (cond
                [(equal? wrd cmp) 
                 (place-image (text "Congratulations!" 11 'red) 10 10 scn)]
                [(empty? pcs) 
                 (place-image
                  (text (string-append "This is the end, my friend: "
                                       (list-word->string chosen)) 
                        11 'red)
                  10 10 scn)]
                [else scn]))
            ;; World -> Boolean 
            (define (stop? world)
              (or (empty? (third world)) (equal? (first world) (second world))))
            ;; [Listof Symbol] [Listof Symbol] Scene -> Scene 
            (define (add-up-to parts pcs s)
              (cond 
                [(empty? parts) s]
                [(and (cons? pcs) (eq? (first parts) (first pcs))) s]
                [else (add-up-to (rest parts) pcs (add-next-part (first parts) s))])))
      ;; --- go world go ---
      (and 
       (big-bang 200 200 .1 world0)
       (on-redraw image)
       (on-key-event click)
       (stop-when stop?))))
  
  ;; Char -> Symbol 
  (define (char->symbol c) (string->symbol (format "~a" c)))
  
  ;; Symbol -> Char 
  (define (symbol->char c) (string-ref (symbol->string c) 0))
  
  ;; Symbol -> Word
  (define (word->list s) (map char->symbol (string->list (symbol->string s))))
  
  ;; Word -> String 
  (define (list-word->string w) (list->string (map symbol->char w)))
  
  ;; a list of symbolic words 
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

