;; The first three lines of this file were inserted by DrScheme. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname guess1) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
(require htdp/guess)

;; check-guess : number number -> symbol 
;; to determine how guess and target relate to each other 
(define (check-guess guess target)
  (cond
    ((< target guess) 'TooLarge)
    ((= target guess) 'Perfect)
    ((> target guess) 'TooSmall)))

;; Tests: 
(eq? (check-guess 5000 5631) 'TooSmall)
(eq? (check-guess 6000 5631) 'TooLarge)
(eq? (check-guess 5631 5631) 'Perfect)

;; Test with GUI lib: set lib to guess-lib.ss
(check-expect (guess-with-gui check-guess) true)

; (guess-with-gui list)
; (define (foo x) x) (guess-with-gui foo)
; (guess-with-gui first)
