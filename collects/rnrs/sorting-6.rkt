#lang scheme/base

(require scheme/mpair)

(provide list-sort
         vector-sort
         vector-sort!)

(define (list-sort < l)
  ;; FIXME (performance): `sort' internally converts 
  ;; a list to an mlist!
  (list->mlist (sort (mlist->list l) <)))

(define (vector-sort < v)
  (list->vector (sort (vector->list v) <)))

(define (vector-sort! < v)
  (let ([v2 (vector-sort < v)])
    (vector-copy! v 0 v2)))
