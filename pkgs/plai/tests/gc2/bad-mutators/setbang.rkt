#lang plai/gc2/mutator
(allocator-setup "../good-collectors/good-collector.rkt" 1024)

;; xxx someday we should move this to good by fixing set! or rejecting
;; the program

(if (= (let ([f 0])
         (let ([g (lambda (n) f)])
           (set! f 1)
           (g 11)))
       1)
  "okay"
  (/ 1 0))

(if (= (let ([f (lambda (n) 'wrong-answer)])
         (let ([g (lambda (n) (f n))])
           (set! f (lambda (n) 'right-answer))
           (g 11)))
       'right-answer)
  "okay"
  (/ 1 0))
