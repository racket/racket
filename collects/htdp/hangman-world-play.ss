(module hangman-world-play mzscheme 
  (require "hangman-world.ss"
           (lib "world.ss" "htdp")
           (lib "etc.ss"))
  
  #| ------------------------------------------------------------------------
  add-next-part :
  { 'noose 'head 'right-arm 'left-arm 'body 'right-leg 'left-leg } scene -> scene
  result: #t if things went okay
  effect: to add the specified body part in a canvas of size W x H
  credit: John Clements 
  |#
  (define (add-next-part body-part s)
    (cond ((eq? body-part 'body)
           (add-line s 100 60 100 130 'black))
          ((eq? body-part 'right-leg)
           (add-line s 100 130 30 170 'black))
          ((eq? body-part 'left-leg)
           (add-line s 100 130 170 170 'black))
          ((eq? body-part 'right-arm)
           (add-line s 100 75 40 65 'black))
          ((eq? body-part 'left-arm)
           (add-line s 100 75 160 65 'black))
          ((eq? body-part 'head)
           (place-image (circle 10 'outline 'black) 100 50 s))
          ((eq? body-part 'noose)
           (local ((define s1 (add-line s  100 30 100 10 'black))
                   (define s2 (add-line s1 100 10   0 10 'black))
                   (define s3 (add-line s2 115 35 123 43 'black))
                   (define s4 (add-line s3 123 35 115 43 'black))
                   (define s5 (add-line s4 131 40 139 48 'black))
                   (define s6 (add-line s5 139 40 131 48 'black)))
             (place-image (circle 30 'outline 'red) 120 50 s6)))
          (else (error 'ouch))))
  
  ;; reveal-list : list-of-letters list-of-letters letter -> list-of-letters
  (define (reveal-list l1 l2 gu)
    (map (lambda (x1 x2)
           (cond
             [(symbol=? x1 gu) gu]
             [else x2]))
         l1 l2))
  
  (define (go-list x) (hangman-list reveal-list add-next-part))
  
  ;; reveal : Word Words Letter -> Word 
  (define (reveal l1 l2 gu)
    (make-word 
     (reveal1 (word-one l1) (word-one l2) gu)
     (reveal1 (word-two l1) (word-two l2) gu)
     (reveal1 (word-three l1) (word-three l2) gu)))
  
  (define (reveal1 x1 x2 gu)
    (cond
      [(symbol=? x1 gu) gu]
      [else x2]))
  
  (define (go x) (hangman reveal add-next-part))
)
