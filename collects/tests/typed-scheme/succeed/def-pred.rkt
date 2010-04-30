#lang typed/scheme

(define-predicate int-or-bool? (U Integer Boolean))

(int-or-bool? 7)