;; TeachPack: graphing.ss
;; Language: Beginner

;; ------------------------------------------------------------------------

(define (fun1 x) (+ (* x x) 1))
(graph-fun fun1 'red)

(define (fun2 x) (+ (* -1 x x) 1))
(graph-fun fun2 'blue)

(define (line1 x) (+ (* +1 x) 10))
(graph-line line1 'black)

(define (line2 x) (+ (* -1 x) 10))
(graph-line line2 'green)
