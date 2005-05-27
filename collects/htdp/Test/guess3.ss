; (load "tester.ss")
;; by hand, bottom

;; ------------------------------------------------------------------------
;; testing repl-list
;; teachpack: guess.ss

;; check-guess-for-list : (listof DIGIT) number -> symbol
;; to determine how guess digits and target relate to each other 
(define (check-guess-for-list guess-list target)
  (check-guess (convert guess-list)target))

;; check-guess : number number -> symbol 
;; to determine how guess and target relate to each other 
(define (check-guess guess target)
  (cond
    ((< target guess) 'TooLarge)
    ((= target guess) 'Perfect)
    ((> target guess) 'TooSmall)))

;; convert : (listof DIGIT) -> number
;; to convert a vector of digits into a number 
;; 0-th digit is right-most digit in number, 
;; N-th digit is left-most digit in number
(define (convert digits)
  (cond
    ((empty? digits) 0)
    (else (+ (first digits) (* (convert (rest digits)) 10)))))

;; EXAMPLES/TESTS for convert:
(= (convert (cons 1 (cons 2 (cons 3 empty)))) 321)
(= (convert (cons 4 (cons 5 (cons 6 empty)))) 654)

;; EXAMPLES/TESTS for check-guess-for-list:
(eq? (check-guess-for-list (cons 0 (cons 0 (cons 5 empty))) 631) 'TooSmall)
(eq? (check-guess-for-list (cons 6 (cons 0 (cons 7 empty))) 631) 'TooLarge)
(eq? (check-guess-for-list (cons 1 (cons 3 (cons 6 empty))) 631) 'Perfect)

;; Test with GUI: set lib to guess-lib.ss
(guess-with-gui-list 2 check-guess-for-list)

