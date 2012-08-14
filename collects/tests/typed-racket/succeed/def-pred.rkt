#lang typed/scheme

(define-predicate int-or-bool? (U Integer Boolean))

(int-or-bool? 7)

(define-predicate int-list? (Rec List (Pair Integer (U '() List))))
(int-list? 1)
(int-list? '(1 2 3))
