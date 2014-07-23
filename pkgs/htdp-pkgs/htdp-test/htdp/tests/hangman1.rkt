;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname hangman1) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
(require htdp/hangman)

; (load "tester.rkt")
;; by hand, Beginner for plain, Full for errors 


(define-struct word (a b c))
;; A word is a structure: (make-word letter letter letter)

;; DEFINITION: 
(define (reveal chosen status guess)
  (make-word (reveal1 (word-a chosen) (word-a status) guess)
             (reveal1 (word-b chosen) (word-b status) guess)
             (reveal1 (word-c chosen) (word-c status) guess)))

;; EXAMPLES:
;; reveal1: 'a 'a 'x => 'a 
;; reveal1: 'x '_ 'x => 'x
;; reveal1: 'x '_ 'y => '_

;; The inputs are atomic, which means we need domain knowledge.
;; The domain knowledge is given in the problem statement. 

;; DEFINITION: 
(define (reveal1 ch st gu) 
  (cond
    ((eq? ch st) ch)
    ((eq? ch gu) gu)
    (else st)))



;; TESTS:
(eq? (reveal1 'a 'a 'x) 'a)
(eq? (reveal1 'x '_ 'x) 'x)
(eq? (reveal1 'x '_ 'y) '_)

(equal? (reveal (make-word 'd 'e 'r) (make-word '_ '_ '_) 'd)
        (make-word 'd '_ '_))
(equal? (reveal (make-word 'd 'e 'r) (make-word '_ '_ '_) 'f)
        (make-word '_ '_ '_))
