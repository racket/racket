; (load "tester.ss")
;; by hand, bottom

;; ------------------------------------------------------------------------
;; testing repl3
;; teachpack: guess.ss

;; check-guess3 : digit digit digit number -> symbol
;; to determine how three guess digits and target relate to each other 
(define (check-guess3 d0 d1 d2 target)
  (check-guess (convert3 d0 d1 d2) target))

;; convert3: digit digit digit -> number
;; to convert three digits, from right to left, into a number 
(define (convert3 d0 d1 d2) 
  (+ (* 100 d2)(+ (* 10 d1) d0)))

;; check-guess : number number -> symbol 
;; to determine how guess and target relate to each other 
(define (check-guess guess target)
  (cond
    ((< target guess) 'TooLarge)
    ((= target guess) 'Perfect)
    ((> target guess) 'TooSmall)))

;; Tests for check-guess3:
(= (convert3 0 0 5) 500)
(= (convert3 1 2 3) 321)

;; Tests for check-guess3:
(eq? (check-guess3 0 0 5 631) 'TooSmall)
(eq? (check-guess3 0 0 7 631) 'TooLarge)
(eq? (check-guess3 1 3 6 631) 'Perfect)

;; Test with GUI: set lib to guess-lib.ss
(guess-with-gui-3 check-guess3)

; (define (foo x) x) (guess-with-gui-3 foo) 
; (guess-with-gui-3 'a)
       
