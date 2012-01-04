#lang plai/gc2mutator

(allocator-setup "../good-collectors/good-collector.rkt" 30)

(test/value=? (cons 1 empty) '(1))
(test/value=? (cons 1 empty) '(1))
(test/value=? (cons 1 empty) '(1))
(test/value=? (cons 1 empty) '(1))
(test/value=? (cons 1 empty) '(1))
(test/value=? (cons 1 empty) '(1))
(test/value=? (cons 1 empty) '(1))
(test/value=? (cons 1 empty) '(1))
(test/value=? (cons 1 empty) '(1))
(test/value=? (cons 1 empty) '(1))
