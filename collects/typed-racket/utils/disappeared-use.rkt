#lang racket/base

(provide (all-defined-out))
;; list of syntax objects that should count as disappeared uses
(define disappeared-use-todo (make-parameter '()))
(define (add-disappeared-use t)
  (disappeared-use-todo (cons t (disappeared-use-todo))))
(define disappeared-bindings-todo (make-parameter '()))
(define (add-disappeared-binding t)
  (disappeared-bindings-todo (cons t (disappeared-bindings-todo))))
