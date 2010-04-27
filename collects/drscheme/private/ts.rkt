#lang at-exp racket/base

(require scribble/decode
         scribble/manual)

(define (phase n)
  (make-splice
   @list{This function can only be called in
              phase @(number->string n) (see @secref["implementing-tools"] for details).}))

(provide phase)
