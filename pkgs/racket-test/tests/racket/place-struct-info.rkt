#lang racket/base
(require racket/place)

;; Some results from `struct-type-info' may be created on demand.
;; Check that any sharing of `struct:exn' across places handles
;; that on-demand creation correctly.

(define (go)
  (place 
   pch
   (define-values (sym init auto ref set! imms par skip?)
     (struct-type-info struct:exn))
   (unless (procedure? ref)
     (error "bad reference procedure"))
   (collect-garbage)))

(module+ main
  (void (place-wait (go)))
  (collect-garbage)
  (void (place-wait (go))))
