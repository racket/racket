#lang racket/base

(provide (struct-out link))

;; A "link" represent a linklet reference, which is a name
;; (corresponds to a `resolved-module-path-name` result) plus a phase
(struct link (name phase) #:prefab)
