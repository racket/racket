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



