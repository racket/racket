#lang racket/base
(require "../common/check.rkt")

(provide filesystem-change-evt?
         filesystem-change-evt
         filesystem-change-evt-cancel)

(define (filesystem-change-evt? v) #f)

(define filesystem-change-evt
  (case-lambda
   [(p) (error 'filesystem-change-evt "unsupported")]
   [(p fail) (fail)]))

(define/who (filesystem-change-evt-cancel e)
  (check who filesystem-change-evt? e)
  (void))
