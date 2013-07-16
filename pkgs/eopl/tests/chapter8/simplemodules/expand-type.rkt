#lang eopl

(require "lang.rkt")
(require "static-data-structures.rkt")

(provide expand-type)
(provide expand-iface)

;;;;;;;;;;;;;;;; expand-type ;;;;;;;;;;;;;;;;

;; these are stubs.  They will be replaced by something more
;; interesting in abstract-types-lang.

(define expand-type (lambda (ty tenv) ty))
(define expand-iface (lambda (m-name iface tenv) iface))

