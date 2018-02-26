#lang racket/base

(provide special-comment?
         make-special-comment
         special-comment-value)

(struct special-comment (value)
  #:authentic
  #:constructor-name make-special-comment)
