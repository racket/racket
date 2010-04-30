;; The first three lines of this file were inserted by DrScheme. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname guess-gui) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
 ;; TeachPack : guess-gui.ss
;; Language: Beginning 

;; ------------------------------------------------------------------------
;; model : button% event% -> true
(define (model x y)
  (view (convert (list (control 0) (control 1) (control 2)))))

;; convert : (listof DIGIT) -> number 
;; to convert a list of digits into a number 
;; the leading digit is the least signifcant one
(define (convert alod)
  (cond
    [(empty? alod) 0]
    [else (+ (first alod) (* 10 (convert (rest alod))))]))

;; TEST: 
(connect model)



