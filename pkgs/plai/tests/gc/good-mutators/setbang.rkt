#lang plai/mutator
(allocator-setup "../good-collectors/good-collector.rkt" 40)

(test/value=?
 (let ([f 0])
   (let ([g (lambda (n) f)])
     (set! f 1)
     (g 11)))
 1)

(test/value=?
 (let ([f (lambda (n) 'wrong-answer)])
   (let ([g (lambda (n) (f n))])
     (set! f (lambda (n) 'right-answer))
     (g 11)))
 'right-answer)
