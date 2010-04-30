#lang plai/mutator
(allocator-setup "../good-collectors/good-collector.ss" 20)


(define car first)


'junk
'junk
'junk
(test/value=? (car (cons 'this-car 2)) 'this-car)