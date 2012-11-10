#lang plai

(define-type Node
  (node (data string?) (adj list?)))

(define g
  (shared ([PVD (node "Providence" (list ORH BOS))]
           [ORH (node "Worcester" (list PVD BOS))]
           [BOS (node "Boston" (list PVD ORH))])
          (list PVD ORH BOS)))

g

(define PVD (first g))
(define ORH (second g))
(define BOS (third g))

PVD
ORH
BOS

(test (node-adj PVD) (list ORH BOS))
(test (node-adj ORH) (list PVD BOS))
(test (node-adj BOS) (list PVD ORH))
