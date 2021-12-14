#lang racket/base

(provide error-message->string)

(define (error-message->string who msg)
  (error-message->adjusted-string who
                                  'racket/primitive
                                  msg
                                  'racket/primitive))
