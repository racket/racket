#lang racket/base

(require racket/generic racket/set racket/stream rackunit)

(define-syntax-rule (delegate method params input wrap-output)
  (begin
    (define/generic generic-method method)
    (define (method . params) (wrap-output (generic-method . input)))))

(struct mask (s)
  #:methods gen:set
  [(delegate set-empty?  [m]   [(mask-s m)]   values)
   (delegate set-member? [m v] [(mask-s m) v] values)
   (delegate set-add     [m v] [(mask-s m) v] mask)
   (delegate set-remove  [m v] [(mask-s m) v] mask)
   (delegate set-first   [m]   [(mask-s m)]   values)
   (delegate set-rest    [m]   [(mask-s m)]   mask)])

;; ---

(check set=?
       (list 3 5 8)
       (stream->list (set->stream (mask (set 3 5 8)))))

(check set=?
       (list 2 4 8)
       (set->list (mask (set 2 4 8))))

