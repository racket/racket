#lang racket
;; A compile-time table shared by eopl and sllgen:
(define sllgen-def (make-hasheq))
(provide sllgen-def)
