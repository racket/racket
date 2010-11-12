#lang racket/base
(require racket/class
         racket/draw/private/color)
(provide special-control-key
         special-option-key
         file-creator-and-type
         get-panel-background
         fill-private-color)

(define special-control-key? #f)
(define special-control-key
  (case-lambda
   [() special-control-key?]
   [(on?) (set! special-control-key? (and on? #t))]))

(define special-option-key? #f)
(define special-option-key
  (case-lambda
   [() special-option-key?]
   [(on?) (set! special-option-key? (and on? #t))]))

(define file-creator-and-type
  (case-lambda
   [(path cr ty) (void)]
   [(path) (values #"????" #"????")]))

(define (get-panel-background)
  (make-object color% "gray"))

(define (fill-private-color dc col)
  (send dc set-background col)
  (send dc clear))
